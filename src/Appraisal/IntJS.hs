{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Appraisal.IntJS
    ( IntJS
    , ToIntJS(intJS)
    , deriveOrderJS

    , JSONText
    , iso_JSONText
    , gjsonIso
    , gjsonLens
    ) where

import Control.Lens (iso, Lens')
import Data.Aeson (ToJSON(toJSON), FromJSON(parseJSON))
import Data.Generics (Typeable)
import Data.Int (Int32)
import Data.Map as Map (fromList, Map, toList)
import Language.Haskell.TH
import Language.Haskell.TH.Path.Order (deriveOrder, fromPairs, OrderMap, Order, toPairs)
import Language.Javascript.JMacro (ToJExpr(toJExpr), JExpr(ValExpr), JVal(JInt))
import Prelude hiding (init, succ)
import Text.JSON.Generic (Data, decodeJSON, encodeJSON)

-- | Javascript can't handle Int64, so until there is an Int53 in
-- haskell this will have to do.
type IntJS = Int32

instance ToJExpr IntJS where
    toJExpr = ValExpr . JInt . fromIntegral

class ToIntJS k where
    intJS :: k -> IntJS

instance ToIntJS IntJS where
    intJS = id

instance (ToJSON k, ToJSON v) => ToJSON (Map k v) where
    toJSON mp = toJSON (Map.toList mp)

instance (Ord k, FromJSON k, FromJSON v) => FromJSON (Map k v) where
    parseJSON mp = Map.fromList <$> parseJSON mp

instance (OrderMap k, ToJSON k, ToJSON a) => ToJSON (Order k a) where
  toJSON = toJSON . toPairs

instance (OrderMap k, FromJSON k, FromJSON a) => FromJSON (Order k a) where
  parseJSON = fmap fromPairs . parseJSON

deriveOrderJS :: Name -> Q [Dec]
deriveOrderJS t = do
  decs <- deriveOrder [t|IntJS|] t
  let idname = mkName (nameBase t ++ "ID")
      unname = mkName ("un" ++ nameBase t ++ "ID")
  insts <- [d| instance ToIntJS $(conT idname) where
                 intJS = $(varE unname)
               instance ToJSON $(conT idname) where
                   toJSON = toJSON . $(varE unname)
               instance FromJSON $(conT idname) where
                   parseJSON = fmap $(conE idname) . parseJSON |]
  return $ decs ++ insts

newtype JSONText = JSONText {unJSONText :: String} deriving (Eq, Ord, Read, Show, Data, Typeable, Monoid)

iso_JSONText :: Lens' JSONText String
iso_JSONText = iso unJSONText JSONText

gjsonIso :: Data a => Lens' a JSONText
gjsonIso = iso (JSONText . encodeJSON) (decodeJSON . unJSONText)

gjsonLens :: Data a => Lens' a JSONText
gjsonLens = gjsonIso
