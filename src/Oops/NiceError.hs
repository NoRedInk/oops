{-# LANGUAGE QuasiQuotes #-}

-- |
-- Description : A module for printing helpful error messages.
module Oops.NiceError
  ( putNiceError,
    Extra,
    extra,
  )
where

import "base" Data.List (intersperse)
import "base" Data.Monoid ((<>))
import "text" Data.Text (Text)
import qualified "text" Data.Text as T
import "prettyprinter" Data.Text.Prettyprint.Doc
  ( Doc,
    LayoutOptions (LayoutOptions),
    PageWidth (AvailablePerLine, Unbounded),
    align,
    annotate,
    indent,
    layoutPretty,
    line,
    pageWidth,
    pretty,
    vsep,
    (<+>),
  )
import "prettyprinter-ansi-terminal" Data.Text.Prettyprint.Doc.Render.Terminal
  ( AnsiStyle,
    bold,
    renderIO,
  )
import "prettyprinter" Data.Text.Prettyprint.Doc.Util (reflow)
import "this" Oops.Types (Help (Help))
import qualified "terminal-size" System.Console.Terminal.Size as Terminal
import "base" System.IO (stdout)
import "heredoc" Text.Heredoc (str)

-- | Print a nice error to the console.
--
--   Nice errors consist of a Help value which can be generated from a help file
--   using the 'help' QuasiQuoter. You can pass extra pieces of information that
--   will be printed as well.
putNiceError :: Help -> [Extra] -> IO ()
putNiceError help' e = do
  w <- toPageWidth . maybe 80 Terminal.width <$> Terminal.size
  let layout = LayoutOptions w
  renderIO stdout . layoutPretty layout $ doc help' e
  where
    toPageWidth :: Int -> PageWidth
    toPageWidth w = AvailablePerLine w 1

doc :: Help -> [Extra] -> Doc AnsiStyle
doc (Help desc) e =
  vsep [line, title, center puffin, line, desc, line, extras e]

title :: Doc AnsiStyle
title = annotate bold $ center "Oops, that didn't work"

extras :: [Extra] -> Doc AnsiStyle
extras e = vsep . intersperse line $ fmap unExtra e

puffin :: Text
puffin =
  [str|    __
      |   ( ->
      |   / )\
      |  <_/_/
      |   " "     |]

center :: Text -> Doc ann
center txt =
  pageWidth $ \case
    Unbounded -> pretty txt
    AvailablePerLine n frac ->
      let cols = floor $ fromIntegral n * frac
          contentWidth = maximum $ T.length <$> T.lines txt
          margin = max (cols - contentWidth) 0
          leftMargin = floor (fromIntegral margin / 2 :: Double)
       in indent leftMargin $ pretty txt

-- | An extra piece of information to be attached to a nice error.
newtype Extra = Extra
  { unExtra :: Doc AnsiStyle
  }

-- | Create an extra piece of information to attach to a nice error.
--   This consists of a short description and a Show-able value.
extra :: Show a => Text -> a -> Extra
extra descriptionText value = Extra $ description <+> align prettyValue
  where
    description :: Doc AnsiStyle
    description = annotate bold $ pretty descriptionText <> ":"
    prettyValue :: Doc ann
    prettyValue = reflow . T.pack $ show value
