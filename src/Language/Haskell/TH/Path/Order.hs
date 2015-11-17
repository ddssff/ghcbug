{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts, FlexibleInstances, FunctionalDependencies,
             ImpredicativeTypes, MultiParamTypeClasses, ScopedTypeVariables, TemplateHaskell, TypeFamilies,
             UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Haskell.TH.Path.Order
    ( OrderKey(init, succ)
    , OrderMap(Order, next, empty, elems, order, putItem, fromPairs, view, insert, permute)
    , deleteItem
    , appendItem
    , insertItems
    , toPairs
    , toList
    , fromList
    , asList
    , find
    , lens_omat
    , view'
    , deriveOrder
    ) where

import Data.Data (Data)
import Control.Lens (Traversal', _Just, lens)
import Data.List as List (partition, elem, foldl, foldl', foldr, filter)
import qualified Data.ListLike as LL
import Data.Map as Map (Map, (!))
import qualified Data.Map as Map
import Data.SafeCopy (SafeCopy(..), contain, safeGet, safePut)
import Data.Typeable (Typeable)
import Language.Haskell.TH
import Prelude hiding (init, succ)
import qualified Prelude (succ)

class Ord k => OrderKey k where
    init :: k      -- ^ The initial key value (toEnum 0?)
    succ :: k -> k -- ^ The successor function for k

-- | The OrderMap instance is inferred from the OrderKey instance.
-- An OrderMap is a map from k to v (returned by 'elems') and an
-- ordering of keys [k] (returned by 'order'.)  This is a better data
-- structure to use in a collaborative application because the keys
-- are a stable way to address the element, not dependant on the
-- changing order of the list.  Even better would be to divorce the
-- keys and the ordering.
class OrderKey k => OrderMap k where
    data Order k :: * -> *
    -- | The next available key
    next :: Order k v -> k
    next = next'
    -- | Return the an empty Order.
    empty :: Order k v     -- ^ The empty Order
    empty = Order {elems' = mempty, order' = mempty, next' = init}
    -- | Return the key-value map
    elems :: Order k v -> Map k v
    elems = elems'
    -- | Return the list of keys in order.
    order :: Order k v -> [k]
    order = order'
    -- | Update the value of an existing item
    putItem :: k -> v -> Order k v -> Order k v
    putItem k a m =
        m {elems' = Map.alter f k (elems m)}
            where f Nothing = (error "putItem: bad key")
                  f (Just _) = Just a
    -- | Partition an Order into the element at k and the Order
    -- containing the remaining elements.
    view :: k -> Order k v -> Maybe (v, Order k v) -- like Data.Set.minView
    view k m = case Map.lookup k (elems m) of
                 Nothing -> Nothing
                 Just x -> Just (x, m {elems' = Map.delete k (elems m), order' = filter (/= k) (order m)})
    -- | Build an order from a list of (key, value) pairs.  No
    -- uniqueness check of the keys is performed.
    fromPairs :: [(k, v)] -> Order k v
    fromPairs prs =
      let ks = map fst prs in
      Order { elems' = Map.fromList prs
            {-, deleted = mempty-}
            , order' = ks
            , next' = succ (foldl1 max ks) }
    -- | Put a new element at the end of the Order, returning a pair
    -- containing the new Order and the new key.
    insert :: v -> Order k v -> (Order k v, k)
    insert a m = let k = next m in (m {next' = succ k, elems' = Map.insert k a (elems m), order' = order m ++ [k]}, k)
    -- | Replace the current ordering with the given key list.  The
    -- result is a triple: (new, missing, invalid).  Missing pairs are
    -- those not mentioned in the new list, invalid are those
    -- mentioned but not present in the old Order.
    permute :: [k] -> Order k v -> (Order k v, [(k, v)], [k])
    permute neworder m =
      reorder $ collect $ sanitize
      where
        -- Make sure the new key order doesn't have any unknown keys
        -- sanitize :: ([k], [k]) -- (known, invalid)
        sanitize = List.partition (`List.elem` (order' m)) neworder
        -- Collect the values that are missing from the new key order
        -- collect :: ([k], [k]) -> ([k], [k], [k]) -- (present, missing, invalid)
        collect (valid, invalid) =
            let deleted = List.filter (not . (`List.elem` invalid)) (order' m) in (valid, deleted, invalid)
        -- Reorder the OrderMap according to the now safe permutation,
        -- also return the portion of the OrderMap not mentioned in the
        -- new order and the list of invalid keys.
        -- reorder :: ([k], [k], [k]) -> (OrderMap k a, OrderMap k a, [k])
        reorder (valid, _missing, invalid) =
            let (validmap, missingmap) = Map.partitionWithKey (\ k _ -> List.elem k valid) (elems m) in
            (m {elems' = validmap, order' = valid},
             (Map.toList missingmap),
             invalid)

-- | Given any instance of OrderKey, we can create an instance of
-- OrderMap which includes an Order type containing a Map, a list
-- of keys, and a field to hold the next available key.
instance OrderKey k => OrderMap k where
    data Order k v = Order { elems' :: Map k v
                           -- , deleted :: Map k a
                           , order' :: [k]
                           , next' :: k
                           } deriving (Data, Typeable, Show)

instance OrderMap k => Monoid (Order k m) where
    mempty = empty
    mappend a b = foldr (\ x m -> fst (insert x m)) a (toList b)

-- Not sure how correct these three instances are in the presence of
-- randomly allocated keys and the like.
instance (OrderMap k, Eq a) => Eq (Order k a) where
    a == b = toList a == toList b

instance (OrderMap k, Eq a, Ord a) => Ord (Order k a) where
    compare a b = compare (toList a) (toList b)

instance (OrderMap k, Read a) => Read (Order k a) where
    -- readsPrec :: Int -> String -> [(OrderMap k a, String)]
    readsPrec _ s = let l = (read s :: [a]) in [(fromList l, "")]

instance (OrderKey k, Monoid (Order k a)) => LL.ListLike (Order k a) a where
    singleton x = fst $ insert x empty
    head m = case order m of
               (hd : _) -> elems m ! hd
               _ -> error "OrderMap.head"
    tail m = case order m of
               (hd : tl) -> m {order' = tl, elems' = Map.delete hd (elems m), next' = next m}
               _ -> error "OrderMap.tail"

instance (OrderKey k, Monoid (Order k a)) => LL.FoldableLL (Order k a) a where
    foldl f r0 xs = List.foldl f r0 (toList xs)
    foldr f r0 xs = List.foldr f r0 (toList xs)

instance (Ord a, Enum a) => OrderKey a where
    init = toEnum 1            -- Yeah, that's right, 1.  F**k zeroth elements.
    succ = Prelude.succ

instance (OrderKey k, SafeCopy k, SafeCopy a) => SafeCopy (Order k a) where
    putCopy m = contain $ do safePut (elems' m)
                             safePut (order' m)
                             safePut (next' m)
    getCopy = contain $ do elems_ <- safeGet
                           order_ <- safeGet
                           next_ <- safeGet
                           return $ Order {elems' = elems_, order' = order_, next' = next_}

-- | Remove the element at k if present.
deleteItem :: OrderMap k => k -> Order k v -> Order k v
deleteItem k m = maybe m snd (view k m)

-- | Put a new element at the end of the order, allocating a new key
-- for it.
appendItem :: OrderMap k => v -> Order k v -> Order k v
appendItem x = fst . insert x

insertItems :: forall k v. Enum k => OrderMap k => Order k v -> [v] -> ([k], Order k v)
insertItems om xs =
    foldr f ([], om) (reverse xs)
    where
      f x (ks, om') = let (om'', k) = insert x om' in ((k : ks), om'')

-- | Return the keys and values of the order.
toPairs :: OrderMap k => Order k v -> [(k, v)]
toPairs m = map (\ k -> (k, (elems m) ! k)) (order m)

-- | Return only the values of the order, discarding the keys.
toList :: OrderMap k => Order k v -> [v]
toList = map snd . toPairs

-- | Build an order from a list of values, allocating new all keys.
fromList :: OrderMap k => [v] -> Order k v
fromList xs = foldl' (flip appendItem) empty xs

-- | Perform an operation on a of an Order's (key, value) pairs,
-- reassembling the resulting pairs into a new Order.
asList :: OrderMap k => ([(k, v)] -> [(k, v)]) -> Order k v -> Order k v
asList f om = fromPairs . f . toPairs $ om

-- | Find the first value (along with the associated key) that
-- satisfies the predicate.
find :: forall k v. OrderMap k => (v -> Bool) -> Order k v -> Maybe (k, v)
find p m =
    find' (order m)
    where
      find' :: [k] -> Maybe (k, v)
      find' [] = Nothing
      find' (k : more) =
          case Map.lookup k (elems m) of
            Nothing -> find' more
            Just x | not (p x) -> find' more
            Just x -> Just (k, x)

-- | Build a lens to focus on the k element of an Order.
lens_omat :: forall k v. OrderMap k => k -> Traversal' (Order k v) v
lens_omat k =
    lens getter setter . _Just
    where
      getter :: Order k v -> Maybe v
      getter s = Map.lookup k $ elems s
      setter :: Order k v -> Maybe v -> Order k v
      setter s a = maybe s (\ a' -> putItem k a' s) a

-- | Like view, but discards the remainder list
view' :: OrderMap k => k -> Order k v -> v
view' i m = maybe (error "Order.view'") fst (view i m)

-- | Given the name of a type such as AbbrevPair, generate declarations
-- @@
--     newtype AbbrevPairID = AbbrevPairID {unAbbrevPairID :: IntJS} deriving (Eq, Ord, Read, Show, Data, Typeable)
--     type AbbrevPairs = Order AbbrevPairID AbbrevPair
--     instance Enum AbbrevPairID where
--       toEnum = AbbrevPairID . toEnum
--       fromEnum = fromEnum . unAbbrevPairID
-- @@
deriveOrder :: TypeQ -> Name -> Q [Dec]
deriveOrder ityp t = do
  let idname = mkName (nameBase t ++ "ID")
      unname = mkName ("un" ++ nameBase t ++ "ID")
      mpname = mkName (nameBase t ++ "s")
  idtype <- newtypeD (cxt []) idname [] (recC idname [varStrictType unname (strictType notStrict ityp) ]) [''Eq, ''Ord, ''Read, ''Show, ''Data, ''Typeable]
  insts <- [d| instance Enum $(conT idname) where
                 toEnum = $(conE idname) . toEnum
                 fromEnum = fromEnum . $(varE unname) |]
  -- It would be nice to build the PathInfo instance for idname, but a
  -- call to derivePathInfo would try to reify it, and its too soon
  -- for that.
  omtype <- tySynD mpname [] [t|Order $(conT idname) $(conT t)|]
  return $ [idtype, omtype] ++ insts
