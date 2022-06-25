module Main where

import Prelude

import Data.Array (filter)
import Effect (Effect)
import Effect.Console (log)
import Node.FS.Sync (readdir)
import Node.Path (FilePath, extname)

outputFolder :: FilePath
outputFolder = "./build/"

siteFolder :: FilePath
siteFolder = "./site/"

getDirectoryFiles :: FilePath -> String -> Effect (Array String)
getDirectoryFiles dir ext = filter (\e -> extname e == ext) <$> readdir dir

copyStaticFiles :: Effect Unit
copyStaticFiles = do
  log "copying assets"

main :: Effect Unit
main = do
  log "🍝"
