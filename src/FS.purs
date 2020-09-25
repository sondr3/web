module FS where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Array as A
import Effect (Effect)
import Effect.Aff (Aff)
import Node.FS.Aff (readdir)
import Node.Path (FilePath, extname)

filterFiles :: String -> Array FilePath -> Array FilePath
filterFiles ext xs = A.filter (\f -> extname f == "." <> ext) xs

dirFiles :: FilePath -> String -> Aff (Array FilePath)
dirFiles dir ext = do
   files <- readdir dir
   let files' = filterFiles ext files
   pure $ files'

dirWalk :: FilePath -> String -> Aff (Array FilePath)
dirWalk dir ext = dirFiles dir ext

foreign import _mkdir :: FilePath -> Effect (Promise Unit)

createDirectory :: FilePath -> Aff Unit
createDirectory = toAffE <<< _mkdir 