{-# LANGUAGE OverloadedStrings #-}

-- |

module Ops.Cabal.Common where

import           Data.Text (Text)
import           Shelly

cabal_ :: Text -> [Text] -> Sh ()
cabal_ = command1_ "cabal" []

cabal :: Text -> [Text] -> Sh Text
cabal = command1 "cabal" []
