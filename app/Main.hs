{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main (main) where

import Control.Applicative (Applicative (liftA2))
import Control.Lens
import Control.Monad (void)
import Data.Aeson
import Data.Aeson.Lens
import Data.Digest.Pure.MD5 (md5)
import Data.Generics.Labels ()
import qualified Data.HashMap.Lazy as HML
import qualified Data.HashMap.Strict as HM
import Data.Maybe (isJust)
import qualified Data.String as BLU
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime, iso8601DateFormat)
import Data.Time.Clock (UTCTime)
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import Development.Shake.Forward
import Dhall (FromDhall, auto, defaultInputSettings, inputWithSettings, rootDirectory)
import GHC.Generics (Generic)
import Slick
import System.Environment (lookupEnv)

data SiteMeta = SiteMeta
  { siteTitle :: String,
    siteDescription :: String,
    siteAuthor :: String,
    baseUrl :: String,
    cssUrl :: String,
    themeUrl :: String
  }
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass (ToJSON, FromJSON, Binary)

siteMeta :: String -> String -> SiteMeta
siteMeta style theme =
  SiteMeta
    { siteTitle = "Eons :: IO ()",
      siteDescription = "The online home for Sondre Aasemoen",
      siteAuthor = "Sondre Aasemoen",
      baseUrl = "https://www.eons.io/",
      cssUrl = style,
      themeUrl = theme
    }

mergeJson :: Value -> Value -> Value
mergeJson (Object r) (Object l) = Object $ HML.union r l
mergeJson _ _ = error "can only merge two objects"

data Page = Page
  { title :: String,
    description :: String,
    content :: String,
    slug :: String,
    kind :: String,
    createdAt :: Maybe String,
    modifiedAt :: Maybe String
  }
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass (ToJSON, FromJSON, Binary)

data Sitemap = Sitemap
  { baseUrl :: String,
    buildTime :: String,
    pages :: [Page]
  }
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass (ToJSON, FromJSON, Binary)

data Project = Project
  { name :: String,
    description :: String,
    technology :: [String],
    gitHub :: String
  }
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass (ToJSON, Binary, FromDhall)

outputFolder :: FilePath
outputFolder = "./build/"

siteFolder :: FilePath
siteFolder = "./site/"

commonPage :: Value -> Value -> Value
commonPage meta page = mergeJson meta (mergeJson page $ toJSON (HM.fromList [("kind", "website")]))

rfc3339 :: Maybe String
rfc3339 = Just "%H:%M:SZ"

parseDhall :: FromDhall a => T.Text -> FilePath -> IO a
parseDhall input root = inputWithSettings (defaultInputSettings & rootDirectory .~ (siteFolder </> root)) auto input

toIsoDate :: UTCTime -> String
toIsoDate = formatTime defaultTimeLocale (iso8601DateFormat Nothing)

sitemap :: SiteMeta -> [Page] -> [Project] -> Action ()
sitemap meta ps _ = do
  now <- liftIO getCurrentTime
  let sm = Sitemap {baseUrl = meta ^. #baseUrl, buildTime = toIsoDate now, pages = ps}
  indexT <- compileTemplate' (siteFolder <> "templates" </> "sitemap.xml")
  writeFile' (outputFolder </> "sitemap.xml") . T.unpack $ substitute indexT (toJSON sm)

buildIndex :: SiteMeta -> [Page] -> [Project] -> Action ()
buildIndex meta _ _ = do
  let page = Page {title = "Home", description = "The online home for Sondre Aasemoen", kind = "website", content = "", slug = "", createdAt = Nothing, modifiedAt = Nothing}
      indexData = mergeJson (toJSON page) (toJSON meta)
  indexT <- compileTemplate' (siteFolder <> "templates" </> "index.html")
  writeFile' (outputFolder </> "index.html") . T.unpack $ substitute indexT indexData

buildPages :: SiteMeta -> Action [Page]
buildPages meta = do
  pages <- getDirectoryFiles "" [siteFolder </> "pages" </> "*.md"]
  forP pages (buildPage meta)

buildPage :: SiteMeta -> FilePath -> Action Page
buildPage meta path = cacheAction ("pages", path) $ do
  liftIO . putStrLn $ "Rebuilding page " <> path
  content <- readFile' path
  page <- commonPage (toJSON meta) <$> markdownToHTML (T.pack content)
  let slug = case page ^? key (T.pack "slug") . _String of
        Just v -> v
        Nothing -> error "could not get page slug"
  template <- compileTemplate' (siteFolder </> "templates" </> "page.html")
  writeFile' (outputFolder </> T.unpack slug </> "index.html") . T.unpack $ substitute template page
  convert page

buildProjects :: SiteMeta -> Action [Project]
buildProjects meta = do
  projects <- getDirectoryFiles "" [siteFolder </> "projects" </> "*.md"]
  forP projects (buildProject meta)

buildProject :: SiteMeta -> FilePath -> Action Project
buildProject meta path = cacheAction ("projects", path) $ do
  liftIO . putStrLn $ "Rebuilding project " <> path
  content <- readFile' path
  dhall <- T.pack <$> readFile' (replaceExtension path ".dhall")
  info <- liftIO $ parseDhall dhall "projects"
  _ <- commonPage (toJSON meta) <$> markdownToHTML (T.pack content)
  pure info

copyStaticFiles :: Action ()
copyStaticFiles = do
  statics <- getDirectoryFiles siteFolder ["static//*"]
  files <- getDirectoryFiles siteFolder ["fonts//*"]
  void $ forP statics $ \path -> copyFileChanged (siteFolder </> path) (outputFolder </> takeFileName path)
  void $ forP files $ \path -> copyFileChanged (siteFolder </> path) (outputFolder </> path)

hashFile :: FilePath -> String -> String -> Action String
hashFile path name ext = do
  content <- readFile' path
  let cssHash = take 8 $ show $ md5 (BLU.fromString content)
      file = name <> cssHash <> ext
  pure file

compileScss :: Action String
compileScss = do
  cache $ cmd "pnpx sass" [siteFolder </> "scss" </> "style.scss", siteFolder </> "scss" </> "style.css"]
  file <- hashFile (siteFolder </> "scss" </> "style.css") "style." ".css"
  cache $ cmd "pnpx parcel-css" [siteFolder </> "scss" </> "style.css", "-o", outputFolder </> file, "-m"]
  pure file

compileJs :: Action String
compileJs = do
  file <- hashFile (siteFolder </> "js" </> "theme.js") "theme." ".js"
  cache $ cmd "pnpx esbuild" [siteFolder </> "js" </> "theme.js", "--minify", "--format=iife", "--outfile=" <> outputFolder </> file, "--log-level=warning"]
  pure file

optimizeHTML :: Bool -> Action ()
optimizeHTML prod =
  if prod
    then do
      files <- getDirectoryFiles "" [outputFolder </> "**/*.html"]
      void $ forP files $ \f -> cmd_ ("pnpx minify-html --minify-css --minify-js" <> " --output " <> f) [f]
    else pure ()

compress :: Bool -> Action ()
compress prod =
  if prod
    then do
      files <- filterFiles <$> getDirectoryFiles "" [outputFolder </> "**/*"]
      cmd_ "gzip -k -9 -f" files
      cmd_ "brotli -k -Z -f" files
    else pure ()
  where
    filterFiles = filter (\f -> takeExtension f `notElem` [".gz", ".br", ".png", ".jpg"])

buildRules :: Action ()
buildRules = do
  copyStaticFiles
  css <- compileScss
  js <- compileJs
  prod <- liftIO isProd
  let meta = siteMeta css js
  pages <- buildPages meta
  projects <- buildProjects meta
  buildIndex meta pages projects
  sitemap meta pages projects
  optimizeHTML prod
  compress prod

isProd :: IO Bool
isProd = liftA2 (||) (isJust <$> lookupEnv "CI") (isJust <$> lookupEnv "PROD")

main :: IO ()
main = do
  let shOpts = forwardOptions $ shakeOptions {shakeVerbosity = Normal, shakeLintInside = [siteFolder]}
  shakeArgsForward shOpts buildRules
