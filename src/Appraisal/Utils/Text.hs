{-# OPTIONS -fno-warn-missing-signatures #-}
module Appraisal.Utils.Text
    ( toWords
    , read
    ) where

import Data.List (intersperse)
import Prelude hiding (read)

-- Convert a number to words.
toWords :: (Integral a) => a -> String
toWords x =
    if x < 0
    then "negative " ++ toWords (- x)
    else concat (intersperse " "
                 (billions (x `div` 1000000000) ++
                  millions (x `div` 1000000 `mod` 1000) ++
                  thousands (x `div` 1000 `mod` 1000) ++
                  ones (x `mod` 1000)))
    where
      billions :: (Integral a) => a -> [String]
      billions 0 = []
      billions n = ones n ++ ["billion"]
      millions :: (Integral a) => a -> [String]
      millions 0 = []
      millions n = ones n ++ ["million"]
      thousands :: (Integral a) => a -> [String]
      thousands 0 = []
      thousands n = ones n ++ ["thousand"]
      ones :: (Integral a) => a -> [String]
      ones n = hundreds (n `div` 100) ++ ones' (n `mod` 100)
      hundreds 0 = []
      hundreds n = ones' n ++ ["hundred"]
      ones' 0 = []
      ones' 1 = ["one"]
      ones' 2 = ["two"]
      ones' 3 = ["three"]
      ones' 4 = ["four"]
      ones' 5 = ["five"]
      ones' 6 = ["six"]
      ones' 7 = ["seven"]
      ones' 8 = ["eight"]
      ones' 9 = ["nine"]
      ones' 10 = ["ten"]
      ones' 11 = ["eleven"]
      ones' 12 = ["twelve"]
      ones' 13 = ["thirteen"]
      ones' 14 = ["fourteen"]
      ones' 15 = ["fifteen"]
      ones' 16 = ["sixteen"]
      ones' 17 = ["seventeen"]
      ones' 18 = ["eighteen"]
      ones' 19 = ["nineteen"]
      ones' n = tens' (n `div` 10) ++ ones' (n `mod` 10)
      tens' 2 = ["twenty"]
      tens' 3 = ["thirty"]
      tens' 4 = ["forty"]
      tens' 5 = ["fifty"]
      tens' 6 = ["sixty"]
      tens' 7 = ["seventy"]
      tens' 8 = ["eighty"]
      tens' 9 = ["ninety"]
      tens' _ = error "Internal error"

-- |A version of read that outputs its argument when it fails.
read s =
    case reads s of
      [] -> error $ "Extra.Text.read - no parse: " ++ show s
      ((x, "") : _) -> x
      ((_, extra) : _) -> error $ "Extra.Text.read - partial parse: " ++ show extra
