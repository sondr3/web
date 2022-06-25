module Main where

import Prelude

import Data.Array (filter)
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Console (log)
import Effect.Uncurried (runEffectFn2, runEffectFn3)
import FFI (CpSyncOptions(..), copyFileSyncImpl, cpSyncImpl)
import Node.FS.Perms (all, mkPerms)
import Node.FS.Sync (mkdir', readdir)
import Node.Path (FilePath, basename, dirname, extname)

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
copyDir src dest = runEffectFn3 cpSyncImpl src dest $ CpSyncOptions { recursive: true }

copyStaticFiles :: Effect Unit
copyStaticFiles = do
  let staticDir = siteFolder <> "static/"
  statics <- getDirectoryFiles staticDir (\_ -> true)
  for_ statics $ \p -> copyFile (staticDir <> p) (outputFolder <> basename p)

main :: Effect Unit
main = do
  log "Building site..."
  copyStaticFiles
