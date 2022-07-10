{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Types where

import Control.Lens
import Data.Generics.Labels ()
import Data.Text (Text)
import Data.Text qualified as T
import Development.Shake.Classes
import Dhall (FromDhall)
import GHC.Generics (Generic)
import Web.Sitemap.Gen

class Content a where
  getUrl :: SiteMeta -> a -> Text
  getTitle :: a -> Text
  getDescription :: a -> Text
  getType :: a -> Text
  getContent :: a -> Text
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
  deriving anyclass (Binary)

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
  deriving anyclass (Binary, FromDhall)

pageToSitemap :: SiteMeta -> Page -> SitemapUrl
pageToSitemap meta page =
  SitemapUrl
    { sitemapLocation = getUrl meta page,
      sitemapLastModified = Nothing,
      sitemapChangeFrequency = Just Monthly,
      sitemapPriority = Just 0.5
    }

data Project = Project
  { name :: Text,
    description :: Text,
    technology :: [Text],
    gitHub :: Text
  }
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass (Binary, FromDhall)

data Content' = Page' Page | Project' Project

instance Content Page where
  getUrl meta page = (meta ^. #baseUrl) <> normalizeSlug (Page' page)
  getTitle page = page ^. #title
  getDescription page = page ^. #description
  getType page = page ^. #kind
  getContent page = page ^. #content
  getCreatedAt page = page ^. #createdAt
  getModifiedAt page = page ^. #modifiedAt

normalizeSlug :: Content' -> Text
normalizeSlug (Page' p) = stripSlash $ p ^. #slug
normalizeSlug (Project' p) = undefined

stripSlash :: Text -> Text
stripSlash t = if T.null t then "/" else "/" <> T.dropAround (== '/') t <> "/"
