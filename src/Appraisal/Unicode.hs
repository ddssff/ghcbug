{-# LANGUAGE CPP, DeriveDataTypeable, TemplateHaskell #-}
module Appraisal.Unicode
    ( Unicode'(Unicode', unUnicode')
    -- , IsUnicode(fromUnicode)
    -- , rawUnicode
    , shouldBeUnicode
    , migrateToUnicode
    , fixUnicode
    , isCJK
    -- , mapChars
    -- , lines
    , tests
    ) where

import Appraisal.Utils.IsText (IsText(fromText))
import Control.Monad.Identity (runIdentity)
import qualified Data.ByteString as B
import Data.Char (ord)
import Data.Generics (Data, Typeable)
import Data.Monoid ((<>))
import Data.SafeCopy (base, deriveSafeCopy)
import Data.String (IsString(..))
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import Test.HUnit (Test(TestCase, TestList), assertEqual)

import Debug.Trace (trace)

newtype Unicode' = Unicode' {unUnicode' :: T.Text} deriving (Data, Typeable, Eq, Ord, Read, Show)

-- class IsUnicode a where
--     fromUnicode :: Unicode' -> a

instance Monoid Unicode' where
    mempty = Unicode' T.empty
    mappend (Unicode' a) (Unicode' b) = Unicode' (a <> b)

instance IsString Unicode' where
    fromString = fromText . T.pack

instance IsText Unicode' where
    -- It is an error for the contents of a Text value to be UTF-8
    -- encoded - such a value should be in a ByteString.  If we have
    -- a database with such Text values (and we do) we need to use
    -- migrateToUnicode.
    fromText t = runIdentity $ fixUnicode (\ _ u -> return u) (return (Unicode' t)) t

-- | Unsafely convert a Text into a Unicode.  It is possible that the
-- Text might be UTF-8 encoded, or even a JPEG file that has somehow been
-- packed into a Text.
rawUnicode :: T.Text -> Unicode'
rawUnicode = Unicode'

-- | Use this for stuff that should already be a unicode type but
-- I haven't gotten around to it
shouldBeUnicode :: T.Text -> Unicode'
shouldBeUnicode = rawUnicode

-- | Used to convert things from the Text values in (for example)
-- Report_v5 to the more correct values in the current version.
migrateToUnicode :: T.Text -> Unicode'
migrateToUnicode t = runIdentity $
    fixUnicode (\ old new -> trace ("Repaired a Unicode string: " ++ show old ++ " -> " ++ show new) (return new)) (return (Unicode' t)) t

-- | Consider whether this is a UTF-8 accidentally put into a Text
-- value.  If so, return the repaired Unicode.  This tries first to
-- convert the Text value into a ByteString.  If that succeeds
-- (because all the characters are <= 0xff) try to UTF-8 decode it.
-- If the result is different from the original the text was
-- incorrectly UTF-8 encoded, so the correctly decoded text is
-- returned.  Otherwise return Nothing.
fixUnicode :: Monad m =>
              (B.ByteString -> Unicode' -> m a) -- handler for repaired UTF8
           -> m a                              -- result when argument was already valid
           -> T.Text                           -- text to be repaired
           -> m a
fixUnicode repaired valid text =
    if all (<= 0xff) ords
    then case decodeUtf8' bytes of
           -- If there are no characters above 0xff and the utf8 decode
           -- fails, we have to assume that it is valid unicode, unless
           -- someone can show me otherwise.
           Right text' | text' /= text -> repaired bytes (Unicode' text')
           _ -> valid
    else valid
    where
      -- These computations are expensive - fixUnicode should not be
      -- used in production.
      bytes = B.pack (map fromIntegral ords)
      ords = map ord (T.unpack text)

-- mapChars :: (Char -> Char) -> Unicode' -> Unicode'
-- mapChars f u = rawUnicode $ T.map f (unUnicode' u)

-- lines :: Unicode' -> [Unicode']
-- lines = map rawUnicode . T.lines . unUnicode'

data UnicodeStatus
    = Valid Unicode'                 -- ^ Appears to already be valid unicode - no UTF8 control characters, perhaps some non-ASCII code points
    | Repaired B.ByteString Unicode' -- ^ Almost certainly some UTF8 text that was accidentally turned into a Text value
    deriving (Show, Eq)

tests :: Test
tests =
    let ascii = T.pack "Catalog Raisonne"
        unicode = T.pack "Catalog Raisonné"
        utf8 = T.pack "Catalog Raisonn\195\169"
        invalid1 = T.pack "Catalog Raisonn\195\5" -- Contains an ascii control character
        invalid2 = T.pack "Catalog Raisonn\195" -- Looks like truncated utf8
    in TestList
        [TestCase (assertEqual "All ASCII" (testUnicode ascii) (Valid (Unicode' ascii))),
         TestCase (assertEqual "Valid Unicode" (testUnicode unicode) (Valid (Unicode' unicode))),
         TestCase (assertEqual "Valid Unicode" (testUnicode invalid1) (Valid (Unicode' invalid1))),
         TestCase (assertEqual "Valid Unicode" (testUnicode invalid2) (Valid (Unicode' invalid2))),
         TestCase (assertEqual "Valid UTF-8" (testUnicode utf8) (Repaired (B.pack (map fromIntegral (map ord (T.unpack utf8)))) (Unicode' unicode)))]

testUnicode :: T.Text -> UnicodeStatus
testUnicode text = runIdentity $ fixUnicode (\ old new -> return (Repaired old new)) (return (Valid (Unicode' text))) text

-- | Is this character one that must be rendered with makexeCJKactive?
isCJK :: Char -> Bool
isCJK c =
    let n = ord c in
          (n >= 0x3400 && n < 0xa000)   -- CJK Unified Ideographs including extension A
       || (n >= 0xf900 && n < 0xfb00)   -- CJK Compatibility Ideographs
       || (n >= 0x20000 && n < 0x2a6e0) -- CJK Unified Ideographs extension B
       || (n >= 0x2f800 && n < 0x2fa20) -- CJK Compatibility Ideographs Supplement
       || (n >= 0xac00 && n < 0xd7b0)   -- Hangul

$(deriveSafeCopy 1 'base ''Unicode')
