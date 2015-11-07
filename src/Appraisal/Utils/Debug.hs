{-# LANGUAGE CPP, TemplateHaskell #-}
module Appraisal.Utils.Debug
    ( Ident(..)
    , ident
    , trace'
    , trace''
    , withIndent
    , indented
    , locMsg
    ) where

import Control.Exception (throw)
import Control.Monad.Trans (MonadIO(liftIO))
import Data.List (intercalate)
import Data.Maybe (fromMaybe, isJust, catMaybes)
import Debug.Trace (trace)
import Network.URI (URI)
import Language.Haskell.TH (ExpQ, Exp, Loc(..), location, Q)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Syntax (lift)
import System.Environment (getEnv, setEnv)
import System.IO.Error (catchIOError, isDoesNotExistError)
import System.IO.Unsafe (unsafePerformIO)
import Text.Groom (groom)

ident :: Ident a => a -> String
ident x = fromMaybe (show x) (ident' x)

class Show a => Ident a where
    ident' :: a -> Maybe String

instance Ident URI where
    ident' = Just . show

instance (Show a, Ident a) => Ident (Maybe a) where
    ident' Nothing = Nothing
    ident' (Just x) = ident' x

instance (Show a, Show b, Ident a, Ident b) => Ident (Either a b) where
    ident' (Left x) = maybe (Just (show x)) Just (ident' x)
    ident' (Right x) = maybe (Just (show x)) Just (ident' x)

instance Ident a => Ident [a] where
    ident' xs =
        let xs' = map ident' xs in
        if all isJust xs'
        then Just ("[" ++ intercalate "," (catMaybes xs') ++ "]")
        else Nothing

trace' :: (Show a) => String -> a -> a
trace' s a = trace (s ++ " " ++ groom a) a

tracing :: Bool
tracing = False
trace'' :: (Show a) => String -> a -> a
trace'' s a = if tracing then trace (s ++ " " ++ show a) a
              else a

indent :: MonadIO m => String -> m ()
indent i = liftIO $ modifyEnv "" "INDENT" (i ++)

undent :: MonadIO m => Int -> m ()
undent n = liftIO $ modifyEnv "" "INDENT" (drop n)

indented :: String -> String
indented s = unsafePerformIO $
    do i <- getEnvWithDefault "" "INDENT"
       return $ unlines $ map (i ++) (lines s)

withIndent :: MonadIO m => String -> m a -> m a
withIndent s task = do
  liftIO (indent s)
  r <- task
  liftIO (undent (length s))
  return r

modifyEnv :: String -> String -> (String -> String) -> IO ()
modifyEnv d v f = getEnvWithDefault d v >>= setEnv v . f

getEnvWithDefault :: String -> String -> IO String
getEnvWithDefault d v = getEnv v `catchIOError` (\ e -> if isDoesNotExistError e then return d else throw e)

__LOC__ :: Q Exp
__LOC__ = lift =<< location

locMsg :: String -> ExpQ
locMsg msg = [|locationToString $__LOC__ ++ ' ' : msg|]

-- turn the TH Loc loaction information into a human readable string
-- leaving out the loc_end parameter
locationToString :: Loc -> String
locationToString loc = (loc_package loc) ++ ':' : (loc_module loc) ++ 
  ' ' : (loc_filename loc) ++ ':' : (line loc) ++ ':' : (char loc)
  where
    line = show . fst . loc_start
    char = show . snd . loc_start
