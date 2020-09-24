module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Main (ColorType(..), FormatType(..), color, format, formatEnd, formatStart)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "color codes" do
          it "gives corrent normal color code" do
            color Log false `shouldEqual` 39
            color Error false `shouldEqual` 31
            color Debug false `shouldEqual` 33
            color Info false `shouldEqual` 36
            color Success false `shouldEqual` 32
          it "gives corrent bright color code" do
            color Log true `shouldEqual` 97
            color Error true `shouldEqual` 91
            color Debug true `shouldEqual` 93
            color Info true `shouldEqual` 96
            color Success true `shouldEqual` 92
        describe "format codes" do
          it "gives correct start code" do
            formatStart Reset `shouldEqual` 0
            formatStart Bold `shouldEqual` 1
            formatStart Underline `shouldEqual` 4
            formatStart Strike `shouldEqual` 9
          it "gives correct end code" do
            formatEnd Reset `shouldEqual` 0
            formatEnd Bold `shouldEqual` 22
            formatEnd Underline `shouldEqual` 24
            formatEnd Strike `shouldEqual` 29
        describe "format input string" do
          it "correctly formats string" do
            format "Hello, world!" Log `shouldEqual` "\x1b[39mHello, world!\x1b[0m"