module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Main (getDirectoryFiles)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldContain)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "getDirectoryFiles" do
    it "finds dhall files" $ liftEffect do
      res <- getDirectoryFiles "." ".dhall"
      res `shouldContain` "packages.dhall"
      res `shouldContain` "spago.dhall"
