{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Types where

import Control.Lens
import Data.Aeson
import Data.Generics.Labels ()
import Data.Text (Text)
import Data.Text qualified as T
import Development.Shake.Classes
import Dhall (FromDhall)
import GHC.Generics (Generic)

class Content a where
  getUrl :: SiteMeta -> a -> Text
  getTitle :: a -> Text
  getDescription :: a -> Text
  getType :: a -> Text
  getCreatedAt :: a -> Maybe Text
  getModifiedAt :: a -> Maybe Text

data SiteMeta = SiteMeta
  { siteTitle :: Text,
    siteDescription :: Text,
    siteAuthor :: Text,
    baseUrl :: Text,
    cssUrl :: Text,
    themeUrl :: Text
  }
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass (ToJSON, FromJSON, Binary)

data Sitemap = Sitemap
  { baseUrl :: Text,
    buildTime :: Text,
    pages :: [Page]
  }
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass (ToJSON, FromJSON, Binary)

siteMeta :: Text -> Text -> SiteMeta
siteMeta style theme =
  SiteMeta
    { siteTitle = "Eons :: IO ()",
      siteDescription = "The online home for Sondre Aasemoen",
      siteAuthor = "Sondre Aasemoen",
      baseUrl = "https://www.eons.io",
      cssUrl = "/" <> style,
      themeUrl = "/" <> theme
    }

data Page = Page
  { title :: Text,
    description :: Text,
    content :: Text,
    slug :: Text,
    kind :: Text,
    createdAt :: Maybe Text,
    modifiedAt :: Maybe Text
  }
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass (ToJSON, FromJSON, Binary, FromDhall)

data Project = Project
  { name :: Text,
    description :: Text,
    technology :: [Text],
    gitHub :: Text
  }
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass (ToJSON, Binary, FromDhall)

data Content' = Page' Page | Project' Project

instance Content Page where
  getUrl meta page = (meta ^. #baseUrl) <> normalizeSlug (Page' page)
  getTitle page = page ^. #title
  getDescription page = page ^. #description
  getType page = page ^. #kind
  getCreatedAt page = page ^. #createdAt
  getModifiedAt page = page ^. #modifiedAt

normalizeSlug :: Content' -> Text
normalizeSlug (Page' p) = stripSlash $ p ^. #slug
normalizeSlug (Project' p) = undefined

stripSlash :: Text -> Text
stripSlash t = if T.null t then "/" else "/" <> T.dropAround (== '/') t <> "/"
