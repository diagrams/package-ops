{-# LANGUAGE OverloadedStrings #-}

-- | Functions for building Haskell packages.

module Ops.Cabal.Build where

import           Ops.Cabal.Common

import           Data.Text        (Text)
import qualified Data.Text        as T
import           Shelly

import           Prelude          hiding (FilePath)

cabalInstall :: [FilePath] -> Sh ()
cabalInstall repos =  do
    cabal_ "install" $ map (flip T.snoc '/' . toTextIgnore) repos
