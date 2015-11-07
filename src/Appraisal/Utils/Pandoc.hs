{-# LANGUAGE CPP, DeriveDataTypeable, DataKinds, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             OverloadedStrings, ScopedTypeVariables, TupleSections, TypeSynonymInstances #-}
{-# OPTIONS -Wall -fno-warn-orphans -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}
module Appraisal.Utils.Pandoc
    ( pandocFromMarkdown
    ) where

import Data.Set as Set (difference, fromList)
import qualified Data.Text as T
import Text.Pandoc

pandocFromMarkdown :: T.Text -> Pandoc
pandocFromMarkdown t = either (error $ "Invalid markdown? " ++ T.unpack t) id . readMarkdown markdownOpts . T.unpack {- . fixMarkdown -} {- . T.strip -} $ t
    where
      markdownOpts =
          def { -- Setting this to True makes the following transformations, at least:
                --   Turns the space after things like Mr. to unicode 0xa0 (nbsp)
                --   Turns ... into …
                --   Turns ' into ’ (unicode 8217.)  Sometimes you will get a pair of these.  XeLaTeX doesn't like them.
                --   Turns " into `` or '' depending on position
                -- This is useful as long as you understand that it is happening.
                -- For LaTeX we need to turn some quotes back into regular ascii.
                readerSmart = True,
                readerParseRaw = True,
                readerExtensions = Set.difference (readerExtensions def)
                                                  (Set.fromList [Ext_backtick_code_blocks, Ext_tex_math_dollars]) }
