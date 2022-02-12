module Main (main) where

import Control.Monad (void)
import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Forward

outputFolder :: FilePath
outputFolder = "build/"

copyStaticFiles :: Action ()
copyStaticFiles = do
  files <- getDirectoryFiles "./site" ["static//*"]
  void $ forP files $ \path -> copyFileChanged ("./site" </> path) (outputFolder </> takeFileName path)

buildRules :: Action ()
buildRules = do
  copyStaticFiles

main :: IO ()
main = do
  let shOpts = forwardOptions $ shakeOptions {shakeVerbosity = Chatty, shakeLintInside = ["./site/"]}
  shakeArgsForward shOpts buildRules
