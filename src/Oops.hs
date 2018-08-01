{-|
Description : A module for printing helpful error messages.

-}
module Oops
  ( putNiceError
  , extra
  , help
  , Extra
  , Help
  ) where

import "this" Oops.NiceError (Extra, extra, putNiceError)
import "this" Oops.Types (Help, help)
