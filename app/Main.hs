{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Monad (void)
import qualified Data.Aeson as A
import Data.Aeson.Types
import Data.Text (Text)
import qualified Data.Text as T
import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Forward
import GHC.Generics (Generic)
import Slick (compileTemplate', substitute)

data SiteMeta = SiteMeta
  { siteTitle :: Text,
    siteDescription :: Text,
    siteAuthor :: Text,
    baseUrl :: Text,
    cssUrl :: Text,
    themeUrl :: Text
  }
  deriving stock (Generic, Eq, Ord, Show)

instance ToJSON SiteMeta where
  toJSON SiteMeta {..} =
    object
      [ "title" A..= siteTitle,
        "description" A..= siteDescription,
        "author" A..= siteAuthor,
        "baseUrl" A..= baseUrl,
        "cssUrl" A..= cssUrl,
        "themeUrl" A..= themeUrl
      ]

siteMeta :: SiteMeta
siteMeta =
  SiteMeta
    { siteTitle = "Eons :: IO ()",
      siteDescription = "The online home for Sondre Nilsen",
      siteAuthor = "Sondre Nilsen",
      baseUrl = "https://www.eons.io/",
      cssUrl = "style.css",
      themeUrl = "/js/theme.js"
    }

-- withSiteMeta :: Value -> Value
-- withSiteMeta (Object obj) = Object $ HML.union obj meta
--   where
--     Object meta = toJSON siteMeta
-- withSiteMeta _ = error "Not allowed"

outputFolder :: FilePath
outputFolder = "./build/"

siteFolder :: FilePath
siteFolder = "./site/"

buildIndex :: Action ()
buildIndex = do
  indexT <- compileTemplate' (siteFolder <> "templates/index.html")
  writeFile' (outputFolder </> "index.html") (T.unpack $ substitute indexT (toJSON siteMeta))

copyStaticFiles :: Action ()
copyStaticFiles = do
  statics <- getDirectoryFiles siteFolder ["static//*"]
  files <- getDirectoryFiles siteFolder ["js//*", "fonts//*"]
  void $ forP statics $ \path -> copyFileChanged (siteFolder </> path) (outputFolder </> takeFileName path)
  void $ forP files $ \path -> copyFileChanged (siteFolder </> path) (outputFolder </> path)

buildRules :: Action ()
buildRules = do
  buildIndex
  copyStaticFiles

main :: IO ()
main = do
  let shOpts = forwardOptions $ shakeOptions {shakeVerbosity = Chatty, shakeLintInside = [siteFolder]}
  shakeArgsForward shOpts buildRules
