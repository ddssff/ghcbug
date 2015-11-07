{-# LANGUAGE PackageImports, ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleContexts     #-}
module Appraisal.Utils.Builders where

import Prelude (Int,Integer, Float, Double, map, undefined)   -- legitimate undefined, should be Proxy

{- Patched version of Data.Generics.Builders that handles Data.Text. -}

import Data.Int  (Int64, Int32)
import Debug.Trace
import Data.Data
import Data.Generics.Aliases (extB)
import qualified Data.Text as T (Text, empty)
import qualified Data.UUID as UUID

-- | Construct the empty value for a datatype. For algebraic datatypes, the
-- leftmost constructor is chosen.
empty :: forall a. Data a => a
empty = general 
      `extB` char 
      `extB` int
      `extB` int64
      `extB` int32
      `extB` integer
      `extB` float 
      `extB` double
      `extB` text
      `extB` uuid where
  -- Generic case
  general :: Data a => a
  general = fromConstrB empty (indexConstr (dataTypeOf general) 1)
  
  -- Base cases
  char    = '\NUL'
  int     = 0      :: Int
  int64   = 0      :: Int64
  int32   = 0      :: Int32
  integer = 0      :: Integer
  float   = 0.0    :: Float
  double  = 0.0    :: Double
  text    = T.empty :: T.Text
  uuid    = UUID.nil

-- | Return a list of values of a datatype. Each value is one of the possible
-- constructors of the datatype, populated with 'empty' values.
constrs :: forall a. Data a => [a]
constrs = general
      `extB` text
      `extB` int
      `extB` int64
      `extB` int32
      `extB` integer
      `extB` float
      `extB` double
      `extB` char
      `extB` uuid where
  -- Generic case
  general :: Data a => [a]
  general = map (fromConstrB empty)
              (dataTypeConstrs (dataTypeOf (unList general))) where
    -- legitimate undefined, should be Proxy
    unList :: Data a => [a] -> a
    unList = trace "Builders.unlist undefined" undefined

  -- Base cases
  char    = "\NUL"
  int     = [0   :: Int]
  int64   = [0   :: Int64]
  int32   = [0   :: Int32]
  integer = [0   :: Integer]
  float   = [0.0 :: Float]
  double  = [0.0 :: Double]
  text    = [T.empty :: T.Text]
  uuid    = [UUID.nil]
