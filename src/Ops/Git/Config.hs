{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}

-- | parse .git/config file for origin URL

module Ops.Git.Config where

import           Control.Applicative
import           Control.Lens              hiding (noneOf)
import qualified Data.CharSet.Common       as Chars
import           Data.Map.Strict           as M
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Filesystem.Path.CurrentOS as FP
import           Shelly
import           Text.Trifecta

import           Data.Functor.Compat
import           Prelude                   ()
import           Prelude.Compat

data SectionHead = Core | Remote Text | Branch Text
                 deriving (Show, Ord, Eq)

type GitConfig = Map SectionHead (Map Text Text)

gitConfig :: Parser GitConfig
gitConfig = fromList <$> many section

sectionHead :: Parser SectionHead
sectionHead = between (char '[') (char ']' *> newline)
              (core <|> remote <|> branch) where
  core = string "core" *> return Core
  remote = string "remote" *> spaces *> (Remote <$> quoted)
  branch = string "branch" *> spaces  *> (Branch <$> quoted)

quoted :: Parser Text
quoted = between (char '"') (char '"') $ T.pack <$> many (noneOf "\"\n\r")

section :: Parser (SectionHead, Map Text Text)
section = (,) <$> sectionHead <*> body where
  body = fromList <$> many definition

definition :: Parser (Text, Text)
definition = do
    space >> spaces
    key <- many letter
    spaces >> char '=' >> spaces
    value <- many $ oneOfSet Chars.print
    void newline
    return (T.pack key, T.pack value)

parseGitConfig :: FP.FilePath -> Sh (Maybe GitConfig)
parseGitConfig dir = parseFromFile gitConfig =<< fp where
  fp = fmap T.unpack . toTextWarn . (FP.</> ".git/config") $ dir

data GithubUrl = GithubUrl
                 Text -- ^ protocol
                 Text -- ^ User or Group
                 Text -- ^ Repo name

parseGithubUrl :: GitConfig -> Maybe GithubUrl
parseGithubUrl gc = M.lookup (Remote "origin") gc >>= M.lookup "url" >>=
                    preview _Success . parseString githubUrl mempty . T.unpack

githubUrl :: Parser GithubUrl
githubUrl = GithubUrl <$> gitProtocol <*> segment <*> (char '/' *> segment)

gitProtocol :: Parser Text
gitProtocol = choice
              [ string "git@github.com:" $> "git"
              , string "https://github.com/" $> "http"
              ]

-- don't accept ".", so we can ignore .git suffix
segment :: Parser Text
segment = T.pack <$> many urlChar

urlChar :: Parser Char
urlChar = alphaNum <|> oneOf "_-"
