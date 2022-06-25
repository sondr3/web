module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Main (getDirectoryFiles, getDirectoryFilesExt)
import Node.Path (extname)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldContain)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "getDirectoryFiles" do
    it "finds dhall files" $ liftEffect do
      res <- getDirectoryFiles "." (\f -> extname f == ".dhall")
      res `shouldContain` "packages.dhall"
      res `shouldContain` "spago.dhall"
  describe "getDirectoryFilesExt" do
    it "finds json files" $ liftEffect do
      res <- getDirectoryFilesExt "." ".json"
      res `shouldContain` "package.json"
