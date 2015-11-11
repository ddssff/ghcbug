{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans -fno-warn-missing-signatures #-}
module Language.Haskell.TH.Path.Core
    ( -- * Type classes and associated types
      Path(toLens)
    , PathType
    , IdPath(idPath)

    -- * Basic Path Types
    , Path_Pair(..)
    , lens_mapPair
    , lens_mapFirst
    , lens_mapSecond
    , Path_Maybe(..)
    , lens_Maybe_Monoid
    , lens_Monoid_Maybe
    , Path_Map(..)
    , mat
    , Path_OMap(..)
    , Path_List(..)
    , at
    , lens_list
    , Path_Either(..)
    , eitherIso

    -- * Basic lenses
    , readOnlyLens
    , readShowLens
    , lens_trace
    -- , listLookupLens
    , lens_mrs
    , idLens
    , dummyLens
    , textLens
    , IsText(textLens')
    , stringLens
    , lens_UserIds_Text

    , pathTypeNames
    ) where

import Control.Applicative.Error (maybeRead)
import Control.Lens (set, Traversal', Lens', _Just, iso, lens, view, view)
import Data.Generics (Data, Typeable)
import Data.List as List (map)
import qualified Data.Map as M (Map, insert, lookup)
import Data.Set as Set (difference, fromList, Set)
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Text as Text (Text, pack, unpack, unwords, words)
import Data.UserId (UserId(..))
import Debug.Trace (trace)
import Language.Haskell.TH
import Language.Haskell.TH.Desugar (DsMonad)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Syntax (qReify)
import Language.Haskell.TH.TypeGraph.Prelude (pprint')
import Prelude hiding (exp)
import Safe (readMay)
import Web.Routes.TH (derivePathInfo)

-- | If there is an instance of 'Path' for a pair of types @s@ and
-- @a@, that means there is at least one way to obtain an @a@ from an
-- @s@.
class Path s a where
    type PathType s a
    -- ^ Each instance defines this type function which returns the
    -- path type.  Each value of this type represents a different way
    -- of obtaining the @a@ from the @s@.  For example, if @s@ is a
    -- record with two fields of type 'Int', the type @PathType s Int@
    -- would have distinct values for those two fields, and the lenses
    -- returned by 'toLens' would access those two fields.
    toLens :: PathType s a -> Traversal' s a
    -- ^ Function to turn a PathType into a lens to access (one of)
    -- the @a@ values.

class IdPath s where
    idPath :: s -- ^ The identity path for type s.  @toLens idPath@
                -- returns @iso id id@.

-- instance OrderKey k => Path (Order k a) a where
--     type PathType (Order k a) a = (Path_OMap k a)
--     toLens (Path_At k a) = lens_omat k . toLens a

-- Primitive path types

-- | A path type with constructors to extract either @fst@, @snd@, or
-- the pair itself.
data Path_Pair a b = Path_First a | Path_Second b | Path_Pair deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_Either a b = Path_Left a | Path_Right b | Path_Either deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_Invalid = Path_Invalid deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_Maybe a = Path_Just a | Path_Maybe deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_Map k v = Path_Look k v | Path_Map  deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_List a = Path_List deriving (Eq, Ord, Read, Show, Typeable, Data) -- No element lookup path - too dangerous, use OMap
data Path_OMap k a = Path_OMap | Path_At k a deriving (Eq, Ord, Read, Show, Typeable, Data)

instance IdPath (Path_Pair a b) where idPath = Path_Pair
instance IdPath (Path_Maybe a) where idPath = Path_Maybe
instance IdPath (Path_Either a b) where idPath = Path_Either
instance IdPath (Path_Map k v) where idPath = Path_Map
instance IdPath (Path_List a) where idPath = Path_List
instance IdPath (Path_OMap k a) where idPath = Path_OMap

$(derivePathInfo ''Path_Pair)
$(derivePathInfo ''Path_List)
$(derivePathInfo ''Path_Map)
$(derivePathInfo ''Path_Either)
$(derivePathInfo ''Path_Maybe)
$(derivePathInfo ''Path_OMap)

$(deriveSafeCopy 0 'base ''Path_Pair)
$(deriveSafeCopy 0 'base ''Path_List)
$(deriveSafeCopy 0 'base ''Path_Map)
$(deriveSafeCopy 0 'base ''Path_Either)
$(deriveSafeCopy 0 'base ''Path_Maybe)
$(deriveSafeCopy 0 'base ''Path_OMap)

idLens :: Lens' a a
idLens = iso id id

dummyLens :: b -> Lens' a b
dummyLens v = lens (const v) const

-- | A lens that assumes usable round trip Read/Show instances for
-- a.  Similar to an isomorphism, but fails silently.
readShowLens :: (Show a, Read a) => Lens' a String
readShowLens = lens show (\r v ->
                           case maybeRead v of
                             Nothing -> r
                             Just r' -> r')

-- | A lens for 'Maybe' values whose getter turns Nothing into the
-- empty string and whose setter returns Nothing whenever read fails.
lens_mrs :: (Show a, Read a) => Lens' (Maybe a) String
lens_mrs = lens getter setter
  where getter Nothing = ""
        getter (Just x) = show x
        setter _ x = maybeRead x

readOnlyLens :: Lens' a a
readOnlyLens = iso id (error "Lens.readOnlyLens: TROUBLE ignoring write to readOnlyLens")

mat :: forall k a. (Show k, Ord k) => k -> Traversal' (M.Map k a) a
mat k = lens (M.lookup k) (\ mp ma -> maybe mp (\ a -> M.insert k a mp) ma) . _Just

-- A list lens
at :: Integral i => i -> Traversal' [a] a
at ii = lens getter setter . _Just
    where getter :: [a] -> Maybe a
          getter = nth ii
          setter :: [a] -> Maybe a -> [a]
          setter = replace ii

nth :: Integral i => i -> [a] -> Maybe a
nth n _ | n < 0 = Nothing
nth _ [] = Nothing
nth 0 (x : _) = Just x
nth n (_ : xs) = nth (pred n) xs

replace :: Integral i => i -> [b] -> Maybe b -> [b]
replace 0 (_ : xs) (Just x) = x : xs
replace n (_ : xs) x | n > 0 = replace (pred n) xs x
replace _ xs _ = xs

lens_trace :: Show a => String -> Lens' a a
lens_trace s =
    iso getter setter
    where
      getter a = trace ("lens_trace " ++ s ++ " - get " ++ show a) a
      setter b = trace ("lens_trace " ++ s ++ " - set " ++ show b) b

eitherIso :: (a -> Bool) -> Lens' a (Either a a)
eitherIso p = iso (split p) merge
  where split f x | f x = Right x
                  | otherwise = Left x
        merge (Right x) = x
        merge (Left x) = x

lens_mapPair :: forall a b c d. Lens' a c -> Lens' b d -> Lens' (a, b) (c, d)
lens_mapPair lensAC lensBD =
    lens g s
    where
      g :: (a, b) -> (c, d)
      g (a, b) = (view lensAC a, view lensBD b)
      s :: (a, b) -> (c, d) -> (a, b)
      s (a, b) (c, d) = (set lensAC c a, set lensBD d b)

lens_mapFirst :: Lens' a b -> Lens' (a, c) (b, c)
lens_mapFirst l = lens_mapPair l (iso id id)

lens_mapSecond :: Lens' b c -> Lens' (a, b) (a, c)
lens_mapSecond l = lens_mapPair (iso id id) l

-- | Create a list lens from an element lens.  For the setter, extra
-- elements in the C list cause an error (unless elens is an iso.)
-- Extra elements in the B list are ignored.
lens_list :: forall b c. b -> Lens' b c -> Lens' [b] [c]
lens_list def elens =
    lens getter setter
    where
      getter :: [b] -> [c]
      getter bs = List.map (view elens) bs
      setter :: [b] -> [c] -> [b]
      setter bs cs = List.map
                       (\ (c, b) -> set elens c b)
                       (zip cs (bs ++ repeat def))

lens_Maybe_Monoid :: (Eq a, Monoid a) => Lens' (Maybe a) a
lens_Maybe_Monoid = lens (maybe mempty id) (\_ v -> if v == mempty then Nothing else Just v)

lens_Monoid_Maybe :: (Eq a, Monoid a) => Lens' a (Maybe a)
lens_Monoid_Maybe = lens (\a -> if a == mempty then Nothing else Just a)
                    (\_ v -> case v of
                               Nothing -> mempty
                               Just z -> z)

_lens_Maybe_Monoid_Tests :: [Bool]
_lens_Maybe_Monoid_Tests = [ Just [i 1,2,3]  == (set lens_Maybe_Monoid [1,2,3] $ (Just [1,2]))
                          , Nothing == (set lens_Maybe_Monoid [] $ (Just [i 1,2]))
                          , Just (Sum $ i 2) == (set lens_Maybe_Monoid (Sum 2) $ (Just (Sum 1)))
                          , Nothing == (set lens_Maybe_Monoid (Sum $ i 0) $ (Just (Sum 1)))
                          ]
  where i :: Int -> Int
        i = id

_lens_Monoid_Maybe_Tests :: [Bool]
_lens_Monoid_Maybe_Tests = [ [i 1,2,3] == ((set lens_Monoid_Maybe (Just [1,2,3])) [1,2])
                          , [] == (set lens_Monoid_Maybe Nothing [i 1,2])
                          ]
  where i :: Int -> Int
        i = id

textLens :: Lens' Text Text
textLens = iso id id

class IsText a where
    textLens' :: Lens' a Text
    stringLens :: IsText a => Lens' a String
    stringLens = textLens' . iso unpack pack

lens_UserIds_Text :: Lens' [UserId] Text
lens_UserIds_Text = iso (encode') (decode')
    where
      decode' :: Text -> [UserId]
      decode' t =
          catMaybes . List.map readId . Text.words $ t
          where readId :: Text -> Maybe UserId
                readId = fmap UserId . readMay . unpack

      encode' :: [UserId] -> Text
      encode' uids =
          Text.unwords . List.map showId $ uids
          where showId = Text.pack . show . _unUserId

-- | Find all the names of the (non-primitive) path types.
pathTypeNames :: DsMonad m => m (Set Name)
pathTypeNames = do
  (FamilyI (FamilyD TypeFam _pathtype [_,_] (Just StarT)) tySynInsts) <- qReify ''PathType
  return . flip Set.difference primitivePathTypeNames . Set.fromList . List.map (\(TySynInstD _ (TySynEqn _ typ)) -> doTySyn typ) $ tySynInsts
    where
      doTySyn (AppT x _) = doTySyn x
      doTySyn (ConT pathTypeName) = pathTypeName
      doTySyn x = error $ "Unexpected PathType: " ++ pprint' x ++ " (" ++ show x ++ ")"

primitivePathTypeNames :: Set Name
primitivePathTypeNames = Set.fromList [''Path_Pair, ''Path_List, ''Path_Either, ''Path_Map, ''Path_OMap, ''Path_Maybe]
