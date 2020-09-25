module Test.FS where

import Prelude

import FS (dirFiles)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldContain)

fsSpec :: Spec Unit
fsSpec = 
  describe "dirFiles" do
    it "finds JSON in project root" do
      files <- dirFiles "." "json"
      files `shouldContain` "package.json"
      files `shouldContain` "renovate.json"