{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | This executable makes edits & commits following a format suitable
-- for the Diagrams packages.

module Main where

import           Distribution.Ops.Cabal
import           Ops.Cabal.Build
import           Ops.Cabal.Sandbox
import           Ops.Changelog
import           Ops.Common
import           Ops.Git.Commit
import           Ops.Git.Config

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.Reader
import           Data.Foldable
import qualified Data.Map.Strict            as M
import           Data.Maybe
import           Data.Semigroup
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Time
import qualified Data.Version               as C
import qualified Distribution.Package       as C
import           Distribution.Text          (simpleParse)
import qualified Filesystem.Path.CurrentOS  as FP
import qualified Options.Applicative        as O
import qualified Options.Applicative.Types  as O
import           Shelly
import           Text.Trifecta

import           Prelude                    hiding (FilePath)

main :: IO ()
main = do
    config <- O.execParser helpParser
    shelly $ runActions config

runActions :: Config -> Sh ()
runActions config = do
    goals <- if doBounds config
               -- only modify / commit repos with this dependency
               then filterM (needsUpdate . package $ config) (repos config)
               -- assume user has already restricted to the modified repos
               else return $ repos config
    when (doBounds config) $ traverse_ (updateBoundsCommit (package config)) goals
    when (doClean config) $ cleanSandbox
    when (doBuild config) $ cabalInstall goals
    when (doChangelog config) $ traverse_ commitChangelog goals
    when (doVersion config) $ traverse_ commitVersionBump goals
    echo "The following packages were modified:"
    echo $ T.unwords $ map toTextIgnore goals

data Config = Config
              { package     :: C.PackageIdentifier -- -p
              , repos       :: [FilePath]
              , doBounds    :: Bool -- -b
              , doClean     :: Bool -- -l
              , doBuild     :: Bool -- -i
              , doChangelog :: Bool -- c
              , doVersion   :: Bool -- V
              }

cliParser :: O.Parser Config
cliParser = Config
            <$> O.option parsePackage (O.long "package" <> O.short 'p' <> O.help "the lowest excluded package version, like `lens-4.11`")
            <*> O.many (O.argument parseFilepath (O.help "a directory with a .cabal file" <> O.metavar "DIR"))
            <*> O.switch (O.long "bounds" <> O.short 'b' <> O.help "update .cabal files with the new upper bounds")
            <*> O.switch (O.long "clean" <> O.short 'l' <> O.help "delete a cabal sandbox in the current working directory, and restore, preserving add-source dependencies")
            <*> O.switch (O.long "build" <> O.long "install" <> O.short 'i' <> O.help "cabal install the specified directories")
            <*> O.switch (O.long  "changelog" <> O.short 'c' <> O.help "update Changelog")
            <*> O.switch (O.long "bump" <> O.short 'V' <> O.help "increment package version")

helpParser :: O.ParserInfo Config
helpParser = O.info (O.helper <*> cliParser)
             (O.fullDesc
              <> O.progDesc "upper-bounds automates several repetitive parts of point releases for new versions of dependencies.  It is especially intended for a large set of packages which depend on eachother or on the same upstream packages.  It is opinionated by design - the commit message format is hardcoded, for example.  Typical usage is to call with --cabal, verify that everything builds, then call with --git.  It is also possible to run multiple actions in a single invocation.  In this case, the actions are always run in the same order, regardless of the order in which they appear on the commandline."
             <> O.header "upper-bounds - point release automation for Haskell")

-- trivial Monoid => Semigroup instances
instance Semigroup (O.Mod O.OptionFields C.PackageIdentifier) where
    (<>) = mappend

instance Semigroup (O.Mod O.ArgumentFields FilePath) where
    (<>) = mappend

instance Semigroup (O.Mod O.FlagFields Bool) where
    (<>) = mappend

instance Semigroup (O.InfoMod Config) where
    (<>) = mappend

-- TODO more validation that this looks like a package constraint
parsePackage :: O.ReadM C.PackageIdentifier
parsePackage = O.ReadM  $ do
  input <- ask
  case simpleParse input of
    Just pkg -> return pkg
    Nothing -> mzero

parseFilepath :: O.ReadM FP.FilePath
parseFilepath = O.ReadM . asks $ FP.fromText . T.pack

textOption :: O.ReadM Text
textOption = T.pack <$> O.str

formatPkg :: C.PackageIdentifier -> Text
formatPkg (C.PackageIdentifier (C.PackageName n) v) =
  mconcat [T.pack n, "-", showVersion v]

updateBoundsCommit :: C.PackageIdentifier -> FilePath -> Sh ()
updateBoundsCommit pkg repo = do
    updateBounds (pkg & pkgVersion_ %~ highestMatchingVersion) repo
    chdir repo $ gitCommit $ "cabal: allow " <> formatPkg pkg

commitChangelog :: FilePath -> Sh ()
commitChangelog repo' = chdir repo' $ do
    repo <- pwd
    oldVer <- getPackageVersion repo
    let newVer = incVersion 4 oldVer
    (clFN, oldCL) <- readChangelog
    date <- today
    mayGitConfig <- parseGitConfig repo
    case mayGitConfig of
     Nothing -> errorExit $ "Could not find git config in " <> toTextIgnore repo
     Just gc -> case M.lookup (Remote "origin") gc >>= M.lookup "url" of
         Nothing -> errorExit $ "Could not find origin URL in git config"
         Just url -> case preview _Success . parseString githubUrl mempty . T.unpack $ url of
             Nothing -> errorExit $ "Could not parse as github URL: " <> url
             Just ghUrl -> do
                 writefile clFN $
                     changelog newVer oldVer date ghUrl oldCL
                 gitCommit $ "CHANGELOG for " <> showVersion newVer

today :: Sh Text
today = T.pack . formatTime defaultTimeLocale (iso8601DateFormat Nothing) <$>
  liftIO getCurrentTime

-- commit Changelog before version bump so we can identify oldVer
-- TODO make this more robust, looking more places (git history? tags?) for oldVer
commitVersionBump :: FilePath -> Sh ()
commitVersionBump repo' = chdir repo' $ do
    repo <- pwd
    fn <- findCabalOrErr repo
    oldVer <- getPackageVersion repo
    let newVer = incVersion 4 oldVer
    sed_ ["s/\\(^[Vv]ersion: *\\)[0-9\\.]*/\\1", showVersion newVer, "/"] fn
    gitCommit $ "bump version to " <> showVersion newVer

changelog :: C.Version -> C.Version -> Text -> GithubUrl -> Text -> Text
changelog newVer oldVer date (GithubUrl _ user repo) oldCL =
    mconcat ["## [v", showVersion newVer,
             "](https://github.com/", user, "/", repo, "/tree/v", showVersion newVer,
             ") (", date, ")\n\n",
             "[Full Changelog](https://github.com/", user, "/", repo, "/compare/v",
             showVersion oldVer, "...v", showVersion newVer, ")\n\n",
             trimChangelog oldCL
             ]

readChangelog :: Sh (FilePath, Text)
readChangelog = do
  tryCl <- findChangelog
  dir <- pwd
  case tryCl of
    Right fn -> do
      contents <- readfile fn
      return (fn, contents)
    Left NoMatches -> errorExit $ "Could not find Changelog in " <> toTextIgnore dir
    Left (MultipleMatches fns) -> errorExit $ "Could not choose among: " <>
                                T.unwords (map toTextIgnore fns)
