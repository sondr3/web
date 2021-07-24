module Test.Utils where

import Prelude
import Utils (slugify)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Spec (describe, it, parallel)
import Test.Spec.Assertions (shouldEqual)

utilsSpec :: Effect Unit
utilsSpec = 
  launchAff_
    $ runSpec [ consoleReporter ] do
      describe "slugify" $ parallel do
        it "replaces simple strings" do
          slugify "foo bar baz" `shouldEqual` "foo-bar-baz"
        it "removes trailing whitespace" do
          slugify "   foo bar   baz  " `shouldEqual` "foo-bar-baz"
        it "removes illegal characters" do
          slugify "foo, bar. baz!" `shouldEqual` "foo-bar-baz"
          slugify "fo''o b%$!ar baz,  " `shouldEqual` "foo-bar-baz"
        it "strips dashes correctly" do
          slugify "-foo-- bar, baz" `shouldEqual` "foo-bar-baz"
        it "strips" do
          slugify "Hello, world! It's a glorious" `shouldEqual` "hello-world-its-a-glorious"
          slugify "this_ IS a % of $dollars" `shouldEqual` "this-is-a-of-dollars"
          slugify "1 is equal == to ---3" `shouldEqual` "1-is-equal-to-3"
