{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}

-- | Functions to modify prepend common Changelog entries.

module Ops.Changelog where

import           Ops.Common

import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Filesystem.Path.CurrentOS as FP
import           Shelly

import           Prelude                   hiding (FilePath)

-- | Test whether a given filename looks like a changelog.
changelogNames :: FilePath -> Bool
changelogNames fp = e (FP.extension fp) && b (toTextIgnore . FP.basename $ fp)
  where
    e ext = ext `elem` [Nothing, Just "md", Just "markdown"]
    b name = name `elem` ["CHANGES", "CHANGELOG"]

-- | Looks in the current directory for a changelog file.  Returns the
-- filename if available, or an error.
findChangelog :: Sh (Either FileMatchError FilePath)
findChangelog = do
    dir <- pwd
    fns <- ls dir
    case filter changelogNames fns of
     [] -> return $ Left NoMatches
     [one] -> return $ Right one
     multiple -> return . Left . MultipleMatches $ multiple

-- | remove the top header
trimChangelog :: Text -> Text
trimChangelog oldCL = case T.lines oldCL of
    ("# Change Log":rest) -> T.unlines $ dropWhile (\l -> T.strip l == "") rest
    _ -> oldCL
