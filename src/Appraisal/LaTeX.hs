{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             OverloadedStrings, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TupleSections, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind -fno-warn-orphans #-}
module Appraisal.LaTeX
    ( texenv
    , cooked
    -- * Former Wiki class methods

    , Appraisal.LaTeX.paragraph

    , nowiki
    , lines
    , bold
    , footnote
    , center
    , heading
    , orderedList
    , orderedList'
    , bulletList
    , bulletList'
    , bulletItem
    , description

    , setcounter
    , setlength
    , newcommand
    , renewcommand
    , oldCentering
    , centering
    , nl
    , comm2
    , comm3
    , tt
    , cjk
    , larger
    , largest
    , huger
    , nocomment
    , nbsp
    , texty
    ) where

import Appraisal.Utils.List (terminate)
import Data.Generics (Data, Typeable)
import qualified Data.List as L (intersperse)
--import Data.SafeCopy (base, deriveSafeCopy)
import qualified Data.Text as T (pack, Text)
import Prelude hiding (lines)
import Text.LaTeX (raw, lnbk)
import qualified Text.LaTeX as LaTeX (center, enumerate, footnote, item)
import Text.LaTeX.Base.Class (comm1, commS, fromLaTeX, LaTeXC, liftL, liftL2, liftL3)
import Text.LaTeX.Base.Commands as LaTeX (huge2, large2, large3, textbf, texttt, usepackage, section, subsection, subsubsection, paragraph, comment)
import Text.LaTeX.Base.Syntax (LaTeX(TeXEnv))
import Text.LaTeX.Base.Syntax as LaTeX (LaTeX(..), Measure(..), TeXArg(..), MathType(..))
import Text.LaTeX.Base.Texy (texy)
import Text.LaTeX.Base.Writer (LaTeXT)

deriving instance Data LaTeX
deriving instance Data MathType
deriving instance Data Measure
deriving instance Data TeXArg
deriving instance Ord LaTeX
deriving instance Ord MathType
deriving instance Ord Measure
deriving instance Ord TeXArg
deriving instance Read LaTeX
deriving instance Read MathType
deriving instance Read Measure
deriving instance Read TeXArg
-- deriving instance Typeable LaTeX
deriving instance Typeable MathType
deriving instance Typeable Measure
deriving instance Typeable TeXArg

cooked :: Monad m => T.Text -> LaTeXT m ()
cooked = texy

{-
$(deriveSafeCopy 1 'base ''LaTeX)
$(deriveSafeCopy 1 'base ''MathType)
$(deriveSafeCopy 1 'base ''Measure)
$(deriveSafeCopy 1 'base ''TeXArg)
-}

texenv :: Monad m => String -> [LaTeXT m ()] -> LaTeXT m ()
texenv name xs = liftL (TeXEnv name []) (mconcat xs)

nowiki :: Monad m => LaTeXT m ()
nowiki = fromLaTeX TeXEmpty

lines :: Monad m => [LaTeXT m ()] -> LaTeXT m ()
lines l = sequence_ $ L.intersperse nl l

paragraph :: Monad m => LaTeXT m () -> LaTeXT m ()
paragraph p = p >> nl >> nl

nl :: Monad m => LaTeXT m ()
nl = raw "\n" -- nocomment

-- | Renders as "%\n" - an idiom frequently used to start a new line
-- without the effect of a "real" newline.
nocomment :: LaTeXC l => l
nocomment = comment mempty

-- break :: Monad m => LaTeXT m a
-- break = nl

-- newpage :: Monad m => LaTeXT m a
-- newpage = LaTeX.newpage

-- newline :: Monad m => LaTeXT m a
-- newline = LaTeX.lnbk

heading :: Monad m => Int -> LaTeXT m () -> LaTeXT m ()
heading 1 title = LaTeX.section title >> nl
heading 2 title = LaTeX.subsection title >> nl
heading 3 title = LaTeX.subsubsection title >> nl
heading _ title = LaTeX.paragraph title >> nl

orderedList :: Monad m => [LaTeXT m ()] -> LaTeXT m ()
orderedList wiki =
    LaTeX.enumerate . sequence_ $ map (\ x -> LaTeX.item Nothing >> x) wiki

bulletList :: Monad m => [LaTeXT m ()] -> LaTeXT m ()
bulletList [] =
    texenv "compactitem" [LaTeX.item (Just (raw "$\\bullet$")) >> texy ("(no items)" :: T.Text)]
bulletList wiki =
    texenv "compactitem" (map (\ x -> LaTeX.item (Just (raw "$\\bullet$")) >> x) wiki)

orderedList' :: Monad m => [LaTeXT m ()] -> LaTeXT m ()
orderedList' wiki = LaTeX.enumerate . sequence_ $ wiki

bulletList' :: Monad m => [LaTeXT m ()] -> LaTeXT m ()
bulletList' [] =
    texenv "compactitem" [bulletItem (texy ("(no items)" :: T.Text))]
bulletList' wiki =
    texenv "compactitem" [sequence_ wiki]

bulletItem :: Monad m => LaTeXT m () -> LaTeXT m ()
bulletItem l = LaTeX.item (Just (raw "$\\bullet$")) >> l

description :: Monad m => [(LaTeXT m (), LaTeXT m b)] -> LaTeXT m ()
description [] = texenv "description" [LaTeX.item Nothing >> raw "(no items)"]
description wiki = texenv "description" (map (\ (item, desc) -> LaTeX.item (Just item) >> extraSpace >> desc >> raw "\n") wiki)
    where
      -- These is only here so my test output doesn't change
      extraSpace = raw " "

footnote :: Monad m => LaTeXT m () -> LaTeXT m ()
footnote = LaTeX.footnote

bold :: Monad m => LaTeXT m () -> LaTeXT m ()
bold = textbf

center :: Monad m => LaTeXT m () -> LaTeXT m ()
center = LaTeX.center

setcounter :: LaTeXC l => String -> Int -> l
setcounter name arg = fromLaTeX $ TeXComm "setcounter" [FixArg (texy (T.pack name)), FixArg (texy arg)]

setlength :: LaTeXC l => String -> Measure -> l
setlength name len = liftL2 (\ l1 l2 -> TeXComm "setlength" [FixArg l1, FixArg l2]) (commS name) (texy len)

renewcommand :: LaTeXC l => String -> l -> l
renewcommand name def = liftL2 (\ l1 l2 -> TeXComm "renewcommand" [FixArg l1, FixArg l2]) (commS name) def

-- | (<http://www.tex.ac.uk/ctan/info/latex2e-help-texinfo/latex2e.html#g_t_005ccentering LaTeX2e: \\centering>)
centering :: LaTeXC l => l
centering = fromLaTeX (TeXComm "centering" [])

-- Does this environment even exist?
oldCentering :: Monad m => [LaTeXT m ()] -> LaTeXT m ()
oldCentering xs = liftL (TeXEnv "center" []) (mconcat (terminate (lnbk >> nl) xs))
--
comm2 :: LaTeXC l => String -> l -> l -> l
comm2 str = liftL2 $ \ l1 l2 -> TeXComm str [FixArg l1, FixArg l2]

comm3 :: LaTeXC l => String -> l -> l -> l -> l
comm3 str = liftL3 $ \ l1 l2 l3 -> TeXComm str [FixArg l1, FixArg l2, FixArg l3]

newcommand :: Monad m => String -> LaTeXT m () -> LaTeXT m ()
newcommand name def = comm2 "newcommand" (commS name) def

tt :: Monad m => LaTeXT m () -> LaTeXT m ()
tt = texttt

cjk :: Monad m => LaTeXT m ()
cjk =
    do usepackage [] "fontspec"
       comm1 "setmainfont" "Nimbus Roman No9 L"
       comm1 "setsansfont" "Nimbus Sans L"
       comm1 "setmonofont" "Nimbus Mono L"
       usepackage [] "xeCJK" >> nl
       comm1 "setCJKmainfont"
                 -- "AR PL KaitiM Big5" -- Missing 冲
                 -- "AR PL Mingti2L Big5" -- There is no 冲 in font AR PL Mingti2L Big5/ICU!
                 -- "AR PL UKai CN"
                 -- "AR PL UKai HK"
                 "AR PL UKai TW MBE"
                 -- "AR PL UKai TW"
                 -- "AR PL UMing CN"
                 -- "AR PL UMing HK"
                 -- "AR PL UMing TW MBE"
                 -- "AR PL UMing TW"
       -- comm2 "setCJKfamilyfont" (raw "tt") (raw "AR PL KaitiM Big5")
       -- Using this font seems to require fonts-arphic-bkai00mp.  The
       -- fonts available on your system are shown by the fc-list
       -- command

larger :: LaTeXC l => l -> l
larger = large2

largest :: LaTeXC l => l -> l
largest = large3

huger :: LaTeXC l => l -> l
huger = huge2

-- | Non-breakable space character.
nbsp :: LaTeXC l => l
nbsp = raw "~"

texty :: LaTeXC l => T.Text -> l
texty = texy
