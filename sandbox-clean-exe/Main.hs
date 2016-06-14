-- | Delete a cabal sandbox in the current working directory, and
-- restore, preserving add-source dependencies

module Main where

import           Ops.Sandbox

import           Shelly

main :: IO ()
main = shelly cleanSandbox
