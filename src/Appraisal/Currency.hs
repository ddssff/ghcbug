{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables, TypeSynonymInstances #-}
{-# OPTIONS -fno-warn-missing-signatures #-}
module Appraisal.Currency
    ( toWords
    , read
    -- * Data
    , Priceable(..)
    , CashValue
    , isValue
    , cashValueScalar
    -- * Parse
    , parseCashValue
    -- * Markup
    , cashValueDigits
    , cashValueDigitsLaTeX
    , cashValueDigitsHtml
    , cashValueWords
    , cashValueWordsLaTeX
    , cashValueWordsHtml
    , addCashValues
    ) where

import Appraisal.Markup (Markup, rawHtml, protectLaTeX, runLaTeX)
import Data.List(intersperse)
import Data.Monoid ((<>))
import Data.Text as Text (Text, pack)
import Prelude hiding (read)
import Text.LaTeX.Base.Class (comm1)
import Text.LaTeX.Base.Texy (texy)
import Text.Printf(printf)
import Text.Regex(mkRegex, matchRegex)

class Priceable a where
    cashValue :: a -> CashValue

-- |Other currency types may be added as necessary.
data CashValue
    = UnspecifiedValue          -- ^ Value is not being supplied for some reason, e.g. it is not known.
    | InvalidValue String       -- ^ A failed attempt was made to create a value using a conversion function such as read.
    | USDollars Double          -- ^ A value given in terms of US dollars
--  | Euros Double              -- ^ Add more currencies here.  Hard coding these may eventually
                                -- become unwieldy, in some future world which we stand astride...
    deriving Show

-- |Predicate which is true for "real" values.
isValue UnspecifiedValue = False
isValue (InvalidValue _) = False
isValue _ = True

-- |Convert a cash value to a double containing the number of currency
-- units.
cashValueScalar :: CashValue -> Maybe Double
cashValueScalar (USDollars d) = Just d
cashValueScalar _ = Nothing

-- |Sum a list of cash values, giving a total amount and counts of the
-- number of unsupplied and the number of invalid elements.
addCashValues :: [CashValue] -> (CashValue, Int, Int)
addCashValues values =
    (USDollars sum', valued, unvalued)
    where
      (sum', valued, unvalued) = foldr f (0.0, 0, 0) values
      f (USDollars x) (t, n, m) = (x + t, n + 1, m)
      f UnspecifiedValue (t, n, m) = (t, n, m + 1)
      f (InvalidValue _) (t, n, m) = (t, n, m + 1)

-- Parse

-- |Convert a string such as "$10,335.25" to a cash value.
parseCashValue :: String -> CashValue
parseCashValue "" = UnspecifiedValue
parseCashValue s =
        case matchRegex (mkRegex "^([ \t]*\\$[ \t]*)?([0-9,]*\\.[0-9][0-9]|[0-9,]+)") s of
          Just [_, s'] -> case reads (filter (/= ',') s') :: [(Double, String)] of
                             [(x, "")] -> USDollars x
                             _ -> InvalidValue s
          _ -> InvalidValue s

-- Markup

cashValueWords :: CashValue -> Text
cashValueWords (USDollars value) =
    pack $
    toWords dollars ++ " dollars" ++
    case cents of
      0 -> ""
      _ -> " and " ++ toWords cents ++ " cents"
    where
      (dollars :: Int) = truncate value
      (cents :: Int) = round (value * 100.0 - (fromIntegral dollars) * 100.0)
cashValueWords UnspecifiedValue = "–"
cashValueWords (InvalidValue s) = pack s

cashValueWordsLaTeX :: CashValue -> Markup
cashValueWordsLaTeX x@(InvalidValue _) = runLaTeX (comm1 "uwave" (texy (cashValueWords x))) -- protectLaTeX (pack s)
cashValueWordsLaTeX x = protectLaTeX . cashValueWords $ x

cashValueWordsHtml :: CashValue -> Markup
cashValueWordsHtml x@(InvalidValue _) = rawHtml $ "<i>" <> cashValueWords x <> "</i>" -- protectLaTeX (pack s)
cashValueWordsHtml x = rawHtml . cashValueWords $ x

cashValueDigits :: CashValue -> Text
cashValueDigits (USDollars value) =
    pack $ "$" ++ concat (intersperse "," (threes a)) ++ b
    where
      s = printf "%.0f" value
      (a, b) = break (== '.') s

      threes xs =
          case length xs of
            0 -> []
            n -> let g = ((n - 1) `mod` 3) + 1 in
                 let (hd, tl) = splitAt g xs in
                 hd : threes tl
cashValueDigits UnspecifiedValue = "–"
cashValueDigits (InvalidValue s) = pack s

cashValueDigitsLaTeX x@(InvalidValue _) = runLaTeX (comm1 "uwave" (texy (cashValueDigits x)))
cashValueDigitsLaTeX x = protectLaTeX . cashValueDigits $ x

cashValueDigitsHtml x@(InvalidValue _) = rawHtml $ "<i>" <> cashValueDigits x <> "</i>"
cashValueDigitsHtml x = rawHtml . cashValueDigits $ x

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
