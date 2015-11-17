{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             OverloadedStrings, ScopedTypeVariables, TemplateHaskell, TypeFamilies, TypeSynonymInstances #-}
{-# OPTIONS -Wall -fno-warn-orphans -fno-warn-name-shadowing #-}
module Appraisal.Markup
    ( Markup(..)
    , foldMarkup
    , markupText
    , markupNull

    , rawMarkdown
    , rawHtml
    , rawLaTeX
    , rawPandoc
    , rawMarkup

    , runLaTeX

    , protectMarkdown
    , protectHtml
    , protectLaTeX

    , mapChars

    , lines
    , blocks
    , strip
    , strip'

    , lens_CIString_Text

    , htmlify
    ) where

import Appraisal.Utils.CIString
import Appraisal.LaTeX ({- Ord, Data, Read -})
import Appraisal.Utils.IsText (fromText)
import Appraisal.Utils.Pandoc (pandocFromMarkdown)
import Control.Monad.Identity (Identity, runIdentity)
import Data.Generics (Data, Typeable)
import Control.Lens (Lens', iso)
import Data.List as List (map, foldr)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
--import Data.SafeCopy (base, deriveSafeCopy)
import Data.Text as T (Text, isPrefixOf, drop, empty, uncons, null, singleton, map, foldr, pack)
import qualified Data.Text as T (lines, strip)
import Language.Haskell.TH.Path.Graph (HideType)
import Prelude hiding (lines)
import Text.LaTeX.Base.Syntax (LaTeX(TeXEmpty, TeXRaw), protectText)
import Text.LaTeX.Base.Writer (LaTeXT, execLaTeXT)
import qualified Text.Pandoc as P

{-
$(deriveSafeCopy 1 'base ''P.Alignment)
$(deriveSafeCopy 1 'base ''P.Block)
$(deriveSafeCopy 1 'base ''P.Citation)
$(deriveSafeCopy 1 'base ''P.CitationMode)
$(deriveSafeCopy 1 'base ''P.Format)
$(deriveSafeCopy 1 'base ''P.Inline)
$(deriveSafeCopy 1 'base ''P.ListNumberDelim)
$(deriveSafeCopy 1 'base ''P.ListNumberStyle)
$(deriveSafeCopy 1 'base ''P.MathType)
$(deriveSafeCopy 1 'base ''P.Meta)
$(deriveSafeCopy 1 'base ''P.MetaValue)
$(deriveSafeCopy 1 'base ''P.Pandoc)
$(deriveSafeCopy 1 'base ''P.QuoteType)
-}

data Markup
    = Markdown Text
    | Html Text
    | LaTeX LaTeX
    | Pandoc P.Pandoc
    | Markup [Markup]
    deriving (Eq, Ord, Data, Typeable, Read)

-- Hiding these types will hide three fields of Markup we don't want
-- to appear in the UI.
instance HideType LaTeX
instance HideType [Markup]
instance HideType P.Pandoc

strip :: Markup -> Maybe Markup
strip = stripStart . stripEnd . Just

strip' :: Markup -> Markup
strip' x = fromMaybe (Markup []) . strip $ x

stripStart :: Maybe Markup -> Maybe Markup
stripStart (Just (Markdown text)) = Just $ Markdown $ T.strip text
stripStart (Just (Markup xs)) =
    stripStart' xs
    where
      stripStart' :: [Markup] -> Maybe Markup
      stripStart' xs' =
          case firstView xs' of
            Nothing -> Nothing
            Just (x, xs'') ->
                case stripStart (Just x) of
                  Nothing -> stripStart' xs''
                  Just x' -> Just $ Markup $ (x' : xs'')
stripStart x = x

stripEnd :: Maybe Markup -> Maybe Markup
stripEnd (Just (Markdown text)) = Just $ Markdown $ T.strip text
stripEnd (Just (Markup xs)) =
    stripEnd' xs
    where
      stripEnd' :: [Markup] -> Maybe Markup
      stripEnd' xs' =
          case lastView xs' of
            Nothing -> Nothing
            Just (xs'', x) ->
                case stripEnd (Just x) of
                  Nothing -> stripEnd' xs''
                  Just x' -> Just $ Markup $ (xs'' ++ [x'])
stripEnd x = x

firstView :: [a] -> Maybe (a, [a])
firstView (x : xs) = Just (x, xs)
firstView [] = Nothing

lastView :: [a] -> Maybe ([a], a)
lastView [] = Nothing
lastView xs = Just (init xs, last xs)

instance Show Markup where
    show (Markdown x) = "rawMarkdown " <> show x
    show (Html x) = "rawHtml " <> show x
    show (LaTeX x) = "rawLaTeX " <> show x
    show (Pandoc x) = "rawPandoc " <> show x
    show (Markup xs) = "rawMarkup " <> show xs

instance Monoid Markup where
    mempty = Markdown mempty
    mappend a b | markupNull a = b
    mappend a b | markupNull b = a
    mappend (Markup a) (Markup b) = Markup (a <> b)
    mappend (Markup a) b = Markup (a ++ [b])
    mappend a (Markup b) = Markup (a : b)
    mappend (Markdown a) (Markdown b) = Markdown (a <> b)
    mappend (Html a) (Html b) = Html (a <> b)
    mappend a b = Markup [a, b]

foldMarkup :: (Text -> a) -> (Text -> a) -> (LaTeX -> a) -> (P.Pandoc -> a) -> ([Markup] -> a) -> Markup -> a
foldMarkup markdownFn _ _ _ _ (Markdown x) = markdownFn x
foldMarkup _ htmlFn _ _ _     (Html x) =     htmlFn x
foldMarkup _ _ latexFn _ _    (LaTeX x) =    latexFn x
foldMarkup _ _ _ pandocFn _   (Pandoc x) =   pandocFn x
foldMarkup _ _ _ _ listFn     (Markup xs) =  listFn xs

markupText :: Markup -> Text
markupText (Markdown t) = t
markupText (Html t) = t
markupText (LaTeX _) = error "markupText LaTeX"
markupText (Pandoc _) = error "markupText Pandoc"
markupText (Markup xs) = mconcat (List.map markupText xs)

markupNull :: Markup -> Bool
markupNull (Markup []) = True
markupNull (Markdown s) = T.null s
markupNull (Html s) = T.null s
markupNull (LaTeX TeXEmpty) = True
markupNull _ = False

rawMarkdown :: Text -> Markup
rawMarkdown = Markdown

rawHtml :: Text -> Markup
rawHtml = Html

rawLaTeX :: LaTeX -> Markup
rawLaTeX = LaTeX

rawPandoc :: P.Pandoc -> Markup
rawPandoc = Pandoc

rawMarkup :: [Markup] -> Markup
rawMarkup = Markup

-- | Turn a LaTeX writer expression into markup (without abbreviation expansion)
runLaTeX :: LaTeXT Identity a -> Markup
runLaTeX = rawLaTeX . runIdentity . execLaTeXT

protectMarkdown :: Text -> Markup
protectMarkdown = Markdown . protect
    where
      protect t =
              case () of
                -- Hack to allow users to embed some html tags and abbreviation
                -- names in square brackets in their markdown.
                _ | T.isPrefixOf "<em>" t -> "\\<em\\>" <> T.drop 4 t
                _ | T.isPrefixOf "</em>" t -> "\\</em\\>" <> T.drop 5 t
                _ | T.isPrefixOf "<bf>" t -> "\\<bf\\>" <> T.drop 4 t
                _ | T.isPrefixOf "</bf>" t -> "\\</bf\\>" <> T.drop 5 t
                _ | T.isPrefixOf "<ol>" t -> "\\<ol\\>" <> T.drop 4 t
                _ | T.isPrefixOf "</ol>" t -> "\\</ol\\>" <> T.drop 5 t
                _ | T.isPrefixOf "[" t -> "[" <> T.drop 1 t
                _ | T.isPrefixOf "]" t -> "]" <> T.drop 1 t
                _ -> case T.uncons t of
                       Just (c, t') -> T.singleton c <> protect t'
                       Nothing -> T.empty

protectHtml :: Text -> Markup
protectHtml = Html . T.foldr escape mempty
    where
      escape :: Char -> Text -> Text
      escape '<'  b = "&lt;"   `mappend` b
      escape '>'  b = "&gt;"   `mappend` b
      escape '&'  b = "&amp;"  `mappend` b
      escape '"'  b = "&quot;" `mappend` b
      escape '\'' b = "&#39;"  `mappend` b
      escape x    b = singleton x `mappend` b

protectLaTeX :: Text -> Markup
protectLaTeX = LaTeX . TeXRaw . protectText

lines :: Markup -> [Markup]
lines (Markdown t) = List.map rawMarkdown . T.lines $ t
lines (Html t) = List.map rawHtml . T.lines $ t
lines x = [x]

blocks :: Markup -> [[Markup]]
blocks m = groups (/= mempty) (lines m)

groups :: (a -> Bool) -> [a] -> [[a]]
groups p xs =
    case List.foldr g [[]] xs of
      ([] : rs) -> rs
      rs -> rs
    where
      g _ [] = error "groups"             -- this can't happen
      g x (r : rs) | p x = ((x : r) : rs) -- Add to current group
      g _ ([] : rs) = ([] : rs)           -- Already started a new group
      g _ (r : rs) = ([] : r : rs)        -- Start a new group

mapChars :: (Char -> Char) -> Markup -> Markup
mapChars f (Markup xs) = Markup (List.map (mapChars f) xs)
mapChars f (Markdown t) = Markdown (T.map f t)
mapChars f (Html t) = Html (T.map f t)
mapChars _ x = x

lens_CIString_Text :: Lens' CIString Text
lens_CIString_Text = iso (pack . unCIString) fromText

-- | If a Markup value has constructor Markdown, convert it to Html.
htmlify :: Markup -> Markup
htmlify (Markdown s) = Html $ pack $ P.writeHtmlString P.def $ pandocFromMarkdown $ s
htmlify x = x

{-
$(deriveSafeCopy 3 'base ''Markup)
-}
