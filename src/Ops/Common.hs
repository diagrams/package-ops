{-# LANGUAGE OverloadedStrings #-}

-- |

module Ops.Common where

import           Data.Monoid
import qualified Data.Text   as T
import           Prelude     hiding (FilePath)
import           Shelly

data FileMatchError = NoMatches | MultipleMatches [FilePath]

sed_ :: [T.Text] -> FilePath -> Sh ()
sed_ parts fn = cmd "sed" ("-i" :: T.Text) (mconcat parts) fn
