{-# OPTIONS -fno-warn-missing-signatures #-}
module Appraisal.Utils.List
    ( presperse
    , postsperse
    , terminate
    , modifyAt
    , moveUp
    , moveDown
    , deleteElt
    , insertElt
    , extendList
    , replaceElem
    , spanBy
    , ifEmpty
    , ifEmpty'
    , dropSuffix
    , dropTail
    , dropPrefix
    , indices
    -- , cartesianProduct
    ) where

import Data.List
import qualified Data.ListLike as LL (ListLike(null))

presperse :: a -> [a] -> [a]
presperse x xs = x : intersperse x xs

postsperse :: a -> [a] -> [a]
postsperse = terminate

terminate :: a -> [a] -> [a]
terminate x xs = intersperse x xs ++ [x]

-- makeAssocList fst snd [(1, 2), (2, 3), (1, 4), (2, 1)]
--	-> [(1, [2, 4]), (2, [3, 1])]
{-
makeAssocList :: (Ord b) => (a -> b) -> (a -> c) -> [a] -> [(b, [c])]
makeAssocList key nonKey xs =
    let groups = groupBy eq (sortBy cmp xs) in
    -- Each group will have at least one element
    map (\ group -> (key (head group), map nonKey group)) groups
    where 
      cmp a b = compare (key a) (key b)
      eq a b = cmp a b == EQ
-}

-- Insert an element at position n.
insertAt :: Int -> a -> [a] -> [a]
insertAt index x xs =
    let (a, b) = splitAt index xs in
    a ++ [x] ++ b

insertElt = insertAt

deleteElt :: Int -> [a] -> [a]
deleteElt n elems =
    case splitAt n elems of
      (hd, _ : tl) -> hd ++ tl
      _ -> fail "deleteElt"

modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt n f xs =
    case splitAt n xs of
      (_, []) -> fail $ "No element at position " ++ show n
      (a, x : b) -> a ++ [f x] ++ b

-- Replace the n'th element of a list.
replaceAt :: Int -> a -> [a] -> [a]
replaceAt index x xs = modifyAt index (const x) xs

moveUp :: Int -> [a] -> [a]
moveUp n elems =
    case splitAt (n - 2) elems of
      (hd, a : b : tl) -> hd ++ [b, a] ++ tl
      _ -> fail "moveUp"

moveDown :: Int -> [a] -> [a]
moveDown n elems =
    case splitAt (n - 1) elems of
      (hd, a : b : tl) -> hd ++ [b, a] ++ tl
      _ -> fail "moveDown"

{-
partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers xs =
    foldr f ([], []) xs
    where
      f (Left a) (as, bs) = (a : as, bs)
      f (Right b) (as, bs) = (as, b : bs)
-}

extendList :: Int -> a -> [a] -> [a]
extendList n x xs = xs ++ replicate (max 0 (n + 1 - length xs)) x

{-
appendElem :: a -> [a] -> [a]
appendElem x xs = xs ++ [x]
-}

--insertElem = insertAt
replaceElem = replaceAt
--modifyElem = modifyAt

-- Return groups of elements which begin with one satisfying p and
-- include the elements which directly follow and do not satisfy p.
-- The first group may not begin with an element satisfying p.  One
-- use for this function is when sorting a list where not all of the
-- elements have a sort order.
--   spanBy isDigit ['x', 'y', '4', '1', 'a', 'b', '2', '3', 'b'] -> ["xy","4","1ab","2","3b"]
spanBy :: (a -> Bool) -> [a] -> [[a]]
spanBy _ [] = []
spanBy p (x : xs) = let (spanned, xs') = span (not . p) xs in ((x : spanned) : spanBy p xs')

ifEmpty :: b -> ([a] -> b) -> [a] -> b
ifEmpty c _ [] = c
ifEmpty _ f xs = f xs

ifEmpty' :: LL.ListLike a c => a -> a -> a
ifEmpty' d t = if LL.null t then d else t

dropSuffix :: Eq a => [a] -> [a] -> [a]
dropSuffix suf str = if isSuffixOf suf str then take (length str - length suf) str else error "dropSuffix"

dropTail _ 0 l = l
dropTail e _ [] = e
dropTail _ n l = take (length l - n) l

dropPrefix :: Eq a => [a] -> [a] -> [a] -> [a]
dropPrefix e pre str = if isPrefixOf pre str then drop (length pre) str else e

indices :: Integral i => [a] -> [i]
indices xs = [0..pred n]
    where n = genericLength xs

{-
cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct xs ys = concatMap (\ x -> map (\ y -> (x, y)) ys) xs
-}
