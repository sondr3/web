{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Applicative (Applicative (liftA2))
import Control.Monad (void)
import Data.Aeson (ToJSON (toJSON), object)
import qualified Data.Aeson as A
import Data.Digest.Pure.MD5 (md5)
import Data.Maybe (isJust)
import qualified Data.String as BLU
import Data.Text (Text)
import qualified Data.Text as T
import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Forward
import GHC.Generics (Generic)
import Slick (compileTemplate', substitute)
import System.Environment (lookupEnv)

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

siteMeta :: Text -> Text -> SiteMeta
siteMeta style theme =
  SiteMeta
    { siteTitle = "Eons :: IO ()",
      siteDescription = "The online home for Sondre Nilsen",
      siteAuthor = "Sondre Nilsen",
      baseUrl = "https://www.eons.io/",
      cssUrl = style,
      themeUrl = theme
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

buildIndex :: SiteMeta -> Action ()
buildIndex meta = do
  indexT <- compileTemplate' (siteFolder <> "templates/index.html")
  writeFile' (outputFolder </> "index.html") (T.unpack $ substitute indexT (toJSON meta))

copyStaticFiles :: Action ()
copyStaticFiles = do
  statics <- getDirectoryFiles siteFolder ["static//*"]
  files <- getDirectoryFiles siteFolder ["fonts//*"]
  void $ forP statics $ \path -> copyFileChanged (siteFolder </> path) (outputFolder </> takeFileName path)
  void $ forP files $ \path -> copyFileChanged (siteFolder </> path) (outputFolder </> path)

compileScss :: Action Text
compileScss = do
  cache $ cmd ("pnpx sass" :: String) ([siteFolder </> "scss" </> "style.scss", siteFolder </> "scss" </> "style.css"] :: [String])
  css <- readFile' (siteFolder </> "scss" </> "style.css")
  let hash = T.pack $ take 8 $ show $ md5 (BLU.fromString css)
      file = "style." <> hash <> ".css"
  Stdout compressed <- cmd ("node scripts/css.mjs" :: String) ([T.unpack file] :: [String])
  writeFile' (outputFolder </> T.unpack file) compressed
  pure file

compileJs :: Action Text
compileJs = do
  js <- readFile' (siteFolder </> "js" </> "theme.js")
  cache $ cmd ("pnpx terser" :: String) ([siteFolder </> "js" </> "theme.js", "-c", "-m toplevel", "-o", outputFolder </> "theme.js"] :: [String])
  let hash = T.pack $ take 8 $ show $ md5 (BLU.fromString js)
      file = "theme." <> hash <> ".js"
  writeFile' (outputFolder </> T.unpack file) js
  pure file

optimizeHTML :: Bool -> Action ()
optimizeHTML prod =
  if prod
    then do
      files <- getDirectoryFiles "" [outputFolder </> "**/*.html"]
      void $ forP files $ \f -> cmd_ ("pnpx minify-html --minify-css --minify-js" <> " --output " <> f :: String) ([f] :: [String])
    else pure ()

compress :: Bool -> Action ()
compress prod =
  if prod
    then do
      files <- getDirectoryFiles "" (map (\f -> outputFolder </> "**" </> f) ["*.html", "*.css", "*.js", "*.woff2"])
      cache $ cmd ("gzip -9 -f" :: String) files
      cache $ cmd ("brotli -Z -f" :: String) files
      pure ()
    else pure ()

buildRules :: Action ()
buildRules = do
  css <- compileScss
  js <- compileJs
  copyStaticFiles
  prod <- liftIO isProd
  let meta = siteMeta css js
  buildIndex meta
  optimizeHTML prod
  compress prod

isProd :: IO Bool
isProd = liftA2 (||) (isJust <$> lookupEnv "CI") (isJust <$> lookupEnv "PROD")

main :: IO ()
main = do
  let shOpts = forwardOptions $ shakeOptions {shakeVerbosity = Normal, shakeLintInside = [siteFolder]}
  shakeArgsForward shOpts buildRules
