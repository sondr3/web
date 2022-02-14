{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Main (main) where

import Control.Applicative (Applicative (liftA2))
import Control.Lens
import Control.Monad (void)
import Data.Aeson
import Data.Aeson.Lens
import Data.Digest.Pure.MD5 (md5)
import qualified Data.HashMap.Lazy as HML
import Data.Maybe (isJust)
import qualified Data.String as BLU
import qualified Data.Text as T
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import Development.Shake.Forward
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
    { siteTitle = "=> Eons :: IO ()",
      siteDescription = "The online home for Sondre Nilsen",
      siteAuthor = "Sondre Nilsen",
      baseUrl = "https://www.eons.io/",
      cssUrl = style,
      themeUrl = theme
    }

withSiteMeta :: Value -> Value -> Value
withSiteMeta (Object obj) (Object meta) = Object $ HML.union obj meta
withSiteMeta _ _ = error "only add site meta to objects"

data Page = Page
  { title :: String,
    description :: String,
    content :: String,
    slug :: String,
    createdAt :: Maybe String,
    modifiedAt :: Maybe String
  }
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass (ToJSON, FromJSON, Binary)

outputFolder :: FilePath
outputFolder = "./build/"

siteFolder :: FilePath
siteFolder = "./site/"

buildIndex :: SiteMeta -> Action ()
buildIndex meta = do
  let page = Page {title = "Home", description = "The online home for Sondre Nilsen", content = "", slug = "", createdAt = Nothing, modifiedAt = Nothing}
      indexData = withSiteMeta (toJSON page) (toJSON meta)
  indexT <- compileTemplate' (siteFolder <> "templates/index.html")
  writeFile' (outputFolder </> "index.html") . T.unpack $ substitute indexT indexData

buildPages :: SiteMeta -> Action [Page]
buildPages meta = do
  pages <- getDirectoryFiles "" [siteFolder </> "pages" </> "*.md"]
  forP pages (buildPage meta)

buildPage :: SiteMeta -> FilePath -> Action Page
buildPage meta path = cacheAction ("pages", path) $ do
  liftIO . putStrLn $ "Rebuilding page " <> path
  content <- readFile' path
  page <- markdownToHTML $ T.pack content
  let pageData = withSiteMeta page (toJSON meta)
      slug = case pageData ^? key (T.pack "slug") . _String of
        Just v -> v
        Nothing -> error "could not get page slug"
  template <- compileTemplate' (siteFolder </> "templates" </> "page.html")
  writeFile' (outputFolder </> T.unpack slug </> "index.html") (T.unpack $ substitute template pageData)
  convert pageData

copyStaticFiles :: Action ()
copyStaticFiles = do
  statics <- getDirectoryFiles siteFolder ["static//*"]
  files <- getDirectoryFiles siteFolder ["fonts//*"]
  void $ forP statics $ \path -> copyFileChanged (siteFolder </> path) (outputFolder </> takeFileName path)
  void $ forP files $ \path -> copyFileChanged (siteFolder </> path) (outputFolder </> path)

compileScss :: Action String
compileScss = do
  cache $ cmd "pnpx sass" [siteFolder </> "scss" </> "style.scss", siteFolder </> "scss" </> "style.css"]
  css <- readFile' (siteFolder </> "scss" </> "style.css")
  let cssHash = take 8 $ show $ md5 (BLU.fromString css)
      file = "style." <> cssHash <> ".css"
  Stdout compressed <- cmd "node scripts/css.mjs" [file]
  writeFile' (outputFolder </> file) compressed
  pure file

compileJs :: Action String
compileJs = do
  js <- readFile' (siteFolder </> "js" </> "theme.js")
  cache $ cmd "pnpx terser" [siteFolder </> "js" </> "theme.js", "-c", "-m toplevel", "-o", outputFolder </> "theme.js"]
  let jsHash = take 8 $ show $ md5 (BLU.fromString js)
      file = "theme." <> jsHash <> ".js"
  writeFile' (outputFolder </> file) js
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
      files <- getDirectoryFiles "" (map (\f -> outputFolder </> "**" </> f) ["*.html", "*.css", "*.js", "*.woff2"])
      cache $ cmd "gzip -k -9 -f" files
      cache $ cmd "brotli -k -Z -f" files
      pure ()
    else pure ()

buildRules :: Action ()
buildRules = do
  css <- compileScss
  js <- compileJs
  copyStaticFiles
  prod <- liftIO isProd
  let meta = siteMeta css js
  pages <- buildPages meta
  buildIndex meta
  optimizeHTML prod
  compress prod

isProd :: IO Bool
isProd = liftA2 (||) (isJust <$> lookupEnv "CI") (isJust <$> lookupEnv "PROD")

main :: IO ()
main = do
  let shOpts = forwardOptions $ shakeOptions {shakeVerbosity = Normal, shakeLintInside = [siteFolder]}
  shakeArgsForward shOpts buildRules
