module Appraisal.Utils.IsText
    ( IsText(fromText)
    ) where

import qualified Data.Text as T

-- | Like Data.String.IsString
class IsText a where
    fromText :: T.Text -> a

instance IsText T.Text where
    fromText = id
