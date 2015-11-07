{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Appraisal.LaTeX.Margins
    ( marginSetup
    , textWidth
    , textWidthInInches
    , textHeight
    , textHeightInInches
    , titleImage
    ) where

import Appraisal.Config (imagesDir, Paths)
import Appraisal.File (File(fileChksum))
import Appraisal.ImageFile (ImageFile(imageFile))
import Appraisal.LaTeX (nl, setlength)
import System.FilePath ((</>))
import Text.LaTeX (LaTeXT, Measure(In))
import Text.LaTeX.Base.Syntax (Measure(..), LaTeX(TeXRaw))
import Text.LaTeX.Packages.Graphicx

marginSetup :: Monad m => LaTeXT m ()
marginSetup =
    do nl >> setlength "hoffset" (In hoffset) >> nl
       setlength "oddsidemargin" (In 0.0) >> nl
       setlength "evensidemargin" (In 0.0) >> nl
       setlength "textwidth" textWidth >> nl >> nl

       setlength "voffset" (In voffset) >> nl
       setlength "topmargin" (In 0.0) >> nl
       setlength "headheight" (In headerHeight) >> nl
       setlength "headsep" (In 0.0) >> nl
       setlength "textheight" textHeight >> nl
       setlength "footskip" (In footSkip) >> nl >> nl -- LaTeX said it need 0.4 pt.  HaTeX bug: (In 0.01) -> {1.0e-2in}

paperHeight :: Double
paperHeight = 11.0
paperWidth :: Double
paperWidth = 8.5
hoffset :: Double
hoffset = 0.0
voffset :: Double
voffset = - 0.5
footSkip :: Double
footSkip = 0.2

-- The left margin is one inch plus hoffset, so create an equal right margin
textWidthInInches :: Double
textWidthInInches = paperWidth - 2.0 * (hoffset + 1.0)

textWidth :: Measure
textWidth = In textWidthInInches

-- | We should compute this size from the actual report
headerHeight :: Double
headerHeight = 1.0
footerHeight :: Double
footerHeight = 1.0

-- The top margin is one inch plus voffset
textHeightInInches :: Double
textHeightInInches = paperHeight - 2.0 * (voffset + 1.0) - headerHeight - footerHeight - footSkip

textHeight :: Measure
textHeight = In textHeightInInches

titleImage :: (Monad m, Paths p) => p -> ImageFile -> LaTeXT m ()
titleImage ver img =
    includegraphics
      [IGHeight (Cm 2.8), IGWidth (CustomMeasure (TeXRaw "\\textwidth")), KeepAspectRatio True]
      (imagesDir ver </> fileChksum (imageFile img))
--    picture img (I.ImageSize {I.dim = I.TheHeight, I.size = 2.8, I.units = I.Cm}) (PicName (Text.pack (imagesDir ver </> fileChksum (imageFile img))))
