{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main (main) where

import Commonmark (Html, defaultSyntaxSpec, parseCommonmarkWith, renderHtml, tokenize)
import Commonmark.Extensions (attributesSpec, autoIdentifiersAsciiSpec, fencedDivSpec, footnoteSpec, smartPunctuationSpec)
import Control.Applicative (Applicative (liftA2))
import Control.Lens
import Control.Monad (void)
import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Digest.Pure.MD5 (md5)
import Data.Generics.Labels ()
import Data.Maybe (isJust)
import Data.String qualified as BLU
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as L
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format.ISO8601
import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Forward
import Dhall (FromDhall, auto, defaultInputSettings, inputWithSettings, rootDirectory)
import Lucid (renderText)
import System.Environment (lookupEnv)
import Templates (indexTemplate, pageTemplate, sitemapTemplate)
import Types

outputFolder :: FilePath
outputFolder = "./build/"

siteFolder :: FilePath
siteFolder = "./site/"

mergeJson :: Value -> Value -> Value
mergeJson (Object r) (Object l) = Object $ KM.union r l
mergeJson _ _ = error "can only merge two objects"

commonPage :: ToJSON a => SiteMeta -> a -> Value
commonPage meta page = mergeJson (toJSON meta) (toJSON page)

rfc3339 :: Maybe String
rfc3339 = Just "%H:%M:SZ"

parseDhall :: FromDhall a => Text -> FilePath -> IO a
parseDhall input root = inputWithSettings (defaultInputSettings & rootDirectory .~ (siteFolder </> root)) auto input

toIsoDate :: UTCTime -> Text
toIsoDate date = case formatShowM iso8601Format date of
  Just v -> T.pack v
  Nothing -> ""

markdownToHTML :: Text -> Action L.Text
markdownToHTML val = do
  let exts = (smartPunctuationSpec <> footnoteSpec <> attributesSpec <> autoIdentifiersAsciiSpec <> fencedDivSpec <> defaultSyntaxSpec)
      parser = runIdentity . parseCommonmarkWith exts
  case parser (tokenize "file" val) of
    Right (html :: Html ()) -> pure $ renderHtml html
    Left e -> error (show e)

sitemap :: SiteMeta -> [Page] -> [Project] -> Action ()
sitemap meta ps _ = do
  now <- liftIO getCurrentTime
  let sm = Sitemap {baseUrl = meta ^. #baseUrl, buildTime = toIsoDate now, pages = ps}
  writeFile' (outputFolder </> "sitemap.xml") . L.unpack $ renderText $ sitemapTemplate sm

buildIndex :: SiteMeta -> [Page] -> [Project] -> Action ()
buildIndex meta _ _ = do
  let page = Page {title = "Home", description = "The online home for Sondre Aasemoen", kind = "website", content = "", slug = "", createdAt = Nothing, modifiedAt = Nothing}
      template = indexTemplate meta page
  writeFile' (outputFolder </> "index.html") (L.unpack $ renderText template)

buildPages :: SiteMeta -> Action [Page]
buildPages meta = do
  pages <- getDirectoryFiles "" [siteFolder </> "pages" </> "*.md"]
  forP pages (buildPage meta)

buildPage :: SiteMeta -> FilePath -> Action Page
buildPage meta path = cacheAction ("pages" :: String, path) $ do
  liftIO . putStrLn $ "Rebuilding page " <> path
  content <- readFile' path
  page <- L.toStrict <$> markdownToHTML (T.pack content)
  dhall <- T.pack <$> readFile' (replaceExtension path ".dhall")
  info <- liftIO $ parseDhall dhall "pages"
  let pageMeta = (info & #content .~ page)
      template = pageTemplate meta pageMeta
  writeFile' (outputFolder </> T.unpack (info ^. #slug) </> "index.html") . L.unpack $ renderText template
  pure info

buildProjects :: SiteMeta -> Action [Project]
buildProjects meta = do
  projects <- getDirectoryFiles "" [siteFolder </> "projects" </> "*.md"]
  forP projects (buildProject meta)

buildProject :: SiteMeta -> FilePath -> Action Project
buildProject _ path = cacheAction ("projects" :: String, path) $ do
  liftIO . putStrLn $ "Rebuilding project " <> path
  content <- readFile' path
  dhall <- T.pack <$> readFile' (replaceExtension path ".dhall")
  info <- liftIO $ parseDhall dhall "projects"
  _ <- L.toStrict <$> markdownToHTML (T.pack content)
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

compileScss :: Action Text
compileScss = do
  cache $ cmd ("node_modules/.bin/sass" :: String) [siteFolder </> "scss" </> "style.scss", siteFolder </> "scss" </> "style.css"]
  file <- hashFile (siteFolder </> "scss" </> "style.css") "style." ".css"
  cache $ cmd ("node_modules/.bin/parcel-css" :: String) [siteFolder </> "scss" </> "style.css", "-o", outputFolder </> file, "-m"]
  pure $ T.pack file

compileJs :: Action Text
compileJs = do
  file <- hashFile (siteFolder </> "js" </> "theme.js") "theme." ".js"
  cache $ cmd ("node_modules/.bin/esbuild" :: String) [siteFolder </> "js" </> "theme.js", "--minify", "--format=iife", "--outfile=" <> outputFolder </> file, "--log-level=warning"]
  pure $ T.pack file

optimizeHTML :: Bool -> Action ()
optimizeHTML prod =
  if prod
    then do
      files <- getDirectoryFiles "" [outputFolder </> "**/*.html"]
      void $ forP files $ \f -> cmd_ ("node_modules/.bin/minify-html --minify-css --minify-js" <> " --output " <> f) [f]
    else pure ()

compress :: Bool -> Action ()
compress prod =
  if prod
    then do
      files <- filterFiles <$> getDirectoryFiles "" [outputFolder </> "**/*"]
      cmd_ ("gzip -k -9 -f" :: String) files
      cmd_ ("brotli -k -Z -f" :: String) files
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
