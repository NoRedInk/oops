{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Description : Types used by the Oops modules
module Oops.Types where

import "base" Control.Monad (fail)
import "base" Data.String (String)
import "prettyprinter" Data.Text.Prettyprint.Doc
import "this" Data.Text.Prettyprint.Doc.Markdown (fromMarkdown)
import "prettyprinter-ansi-terminal" Data.Text.Prettyprint.Doc.Render.Terminal
  ( AnsiStyle,
  )
import qualified "template-haskell" Language.Haskell.TH as TH
import qualified "template-haskell" Language.Haskell.TH.Quote as QQ
import qualified "heredoc" Text.Heredoc

-- | A help message to go along with an error.
newtype Help
  = Help (Doc AnsiStyle)

-- instance THS.Lift HelpFile where
--   lift (HelpFile t) = [|HelpFile $ T.pack $(THS.lift $ T.unpack t)|]

-- | A QuasiQuoter for help messages that can be logged as nice errors.
--    Takes the path to a help file that will be processed.
help :: QQ.QuasiQuoter
help =
  QQ.QuasiQuoter
    { QQ.quoteExp = parseHelp,
      QQ.quoteType = const $ fail "help not supported in types",
      QQ.quotePat = const $ fail "help not supported in patterns",
      QQ.quoteDec = const $ fail "help not supported in top level declarations"
    }

parseHelp :: String -> TH.Q TH.Exp
parseHelp helpText =
  [|Help (fromMarkdown $(QQ.quoteExp Text.Heredoc.str helpText))|]
