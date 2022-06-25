module Main where

import Prelude

import Data.Array (filter)
import Data.String (take)
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Console (log)
import Effect.Uncurried (runEffectFn1, runEffectFn2, runEffectFn3)
import FFI (copyFileSyncImpl, cpSyncImpl, createHash, optimizeCSS, optimizeJS, sassCompileImpl)
import Node.Encoding (Encoding(..))
import Node.FS.Perms (all, mkPerms)
import Node.FS.Sync (mkdir', readTextFile, readdir, writeTextFile)
import Node.Path (FilePath, basename, dirname, extname, normalize)

combine :: FilePath -> FilePath -> FilePath
combine x y = normalize $ x <> "/" <> y

infixr 5 combine as </>

outputFolder :: FilePath
outputFolder = "./build/"

siteFolder :: FilePath
siteFolder = "./site/"

createDir :: FilePath -> Effect Unit
createDir dir = mkdir' dir { recursive: true, mode: mkPerms all all all }

getDirectoryFiles :: FilePath -> (FilePath -> Boolean) -> Effect (Array String)
getDirectoryFiles dir f = filter f <$> readdir dir

getDirectoryFilesExt :: FilePath -> String -> Effect (Array String)
getDirectoryFilesExt dir ext = getDirectoryFiles dir (\e -> extname e == ext)

copyFile :: FilePath -> FilePath -> Effect Unit
copyFile src dest = do
  createDir $ dirname dest
  runEffectFn2 copyFileSyncImpl src dest

copyDir :: FilePath -> FilePath -> Effect Unit
copyDir src dest = runEffectFn3 cpSyncImpl src dest { recursive: true }

hashString :: String -> String
hashString input = take 8 $ createHash input

copyStaticFiles :: Effect Unit
copyStaticFiles = do
  let staticDir = siteFolder </> "static"
  statics <- getDirectoryFiles staticDir (\_ -> true)
  for_ statics $ \p -> copyFile (staticDir </> p) (outputFolder </> basename p)

compileSass :: Effect String
compileSass = do
  createDir (outputFolder </> "css")
  result <- runEffectFn1 sassCompileImpl (siteFolder </> "scss" </> "style.scss")
  let name = "style." <> hashString result.css <> ".css"
  optimized <- optimizeCSS name result.css false
  writeTextFile UTF8 (outputFolder </> "css" </> name) optimized.code
  writeTextFile UTF8 (outputFolder </> "css" </> name <> ".map") optimized.map
  pure name

compileJs :: Effect String
compileJs = do
  createDir (outputFolder </> "js")
  file <- readTextFile UTF8 $ siteFolder </> "js" </> "theme.js"
  let name = "theme." <> hashString file <> ".js"
  res <- optimizeJS file
  writeTextFile UTF8 (outputFolder </> "js" </> name) res
  pure name

main :: Effect Unit
main = do
  log "Building site..."
  copyStaticFiles
  _ <- compileSass
  _ <- compileJs
  pure unit
