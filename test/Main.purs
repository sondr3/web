module Test.Main where

import Prelude

import Effect (Effect)
import Test.Utils (utilsSpec)

main :: Effect Unit
main = do
  utilsSpec