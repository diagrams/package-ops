{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | Commonly used operations that change the local git repository.

module Ops.Git.Commit where

import           Data.Text (Text)
import           Shelly

gitCommit :: Text -> Sh ()
gitCommit msg  = cmd "git" "commit" "-am" msg
