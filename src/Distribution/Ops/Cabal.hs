{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Functions for working with .cabal files

module Distribution.Ops.Cabal where

import           Ops.Common

import qualified Filesystem.Path.CurrentOS             as FP
import           Shelly

import           Control.Lens
import           Data.List
import           Data.Monoid
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import           Data.Version
import qualified Data.Version                          as C
import qualified Distribution.Package                  as C
import qualified Distribution.PackageDescription.Parse as C
import qualified Distribution.Verbosity                as C

import           Prelude                               hiding (FilePath)

findCabalFile :: FilePath -> Sh (Either FileMatchError FilePath)
findCabalFile repo = do
    files <- ls repo
    return $ case filter (`FP.hasExtension` "cabal") files of
      [fn] -> Right fn
      [] -> Left NoMatches
      fns -> Left $ MultipleMatches fns

-- | Return a .cabal filename or exit with an error.
findCabalOrErr :: FilePath -> Sh FilePath
findCabalOrErr repo = do
    mayFn <- findCabalFile repo
    case mayFn of
      Right fn -> return fn
      Left NoMatches -> errorExit $ "Could not find .cabal file in: " <> toTextIgnore repo
      Left (MultipleMatches []) -> errorExit "Impossible 0 > 1 bug"
      Left (MultipleMatches (fn:_)) -> do
        echo_err $ "Found multiple .cabal files in: " <> toTextIgnore repo
        echo_err $ "Continuing using: " <> toTextIgnore fn
        return fn

-- | Test whether a given repo needs updating for the given package
-- and version.  Only handles @build-depends@ with one package per
-- line, and upper bounds expressed with @<@, not @==@.  Violating the
-- first precondition can result in false positives; the second can
-- result in false negatives.
needsUpdate :: C.PackageIdentifier -> FilePath -> Sh Bool
needsUpdate pkg repo = errExit False $ do
    fn <- findCabalOrErr repo
    match <- cmd "grep" (packageName pkg <> ".*<") fn
    ex <- lastExitCode
    case ex of
      1 -> return False
    -- pkg is used, but maybe all the lines are up to date
      0 -> return . not $ all (T.isInfixOf . packageVersion $ pkg) (T.lines match)
      _ -> errorExit $ "grep exited with unknown code " <> (T.pack . show $ ex)

-- | Modify a cabal file in place to allow newer versions of the given
-- dependency.  The 'PackageIdentifier' is used as the new upper
-- bound, typically a version which does not yet exist.
updateBounds :: C.PackageIdentifier -> FilePath -> Sh ()
updateBounds pkg repo = do
    fn <- findCabalOrErr repo
    -- TODO Don't use regexen
    sed_ ["s/\\(", pname, ".*\\)< [0-9\\.]*/\\1< ", v, "/"] fn where
       pname = packageName pkg
       v = packageVersion pkg

-- | Extract the package name from a PackageIdentifier
packageName :: C.PackageIdentifier -> Text
packageName p = T.pack name where
  (C.PackageName name) = C.pkgName p
-- unPackageName only added in Cabal-1.22

packageVersion :: C.PackageIdentifier -> Text
packageVersion = T.pack . showVersion . C.pkgVersion

makeLensesFor [("versionBranch", "versionBranch_")] ''Version
makeLensesFor [("pkgVersion", "pkgVersion_")] ''C.PackageIdentifier

-- Not a law-abiding Lens, due to extend.
modVersion :: (Int -> Int) -> Int -> Version -> Version
modVersion f n = versionBranch_ %~ ((element n %~ f) . extend)
  where
    extend = take (n+1) . (++repeat 0)

-- | Increment a version in the nth position.  Suitable both for point
-- releases (4th position) and typical dependency increases (2nd
-- position).
incVersion :: Int -> Version -> Version
incVersion = modVersion (+1)

-- | Decrement a version in the nth position.  See @incVersion@.
decVersion :: Int -> Version -> Version
decVersion = modVersion pred

-- | Calculate the highest matching version by decrementing the least
-- significant non-zero digit.
highestMatchingVersion :: Version -> Version
highestMatchingVersion = versionBranch_ %~ dec . dropWhileEnd (== 0) where
  dec = _last -~ 1

getPackageVersion :: FilePath -> Sh C.Version
getPackageVersion repo = do
  fn <- findCabalOrErr repo
  description <- liftIO $ C.readPackageDescription C.normal . FP.encodeString $ fn
  return $ C.packageVersion description

showVersion :: Version -> Text
showVersion = T.pack . C.showVersion
