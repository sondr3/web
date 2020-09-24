module Test.Main where

import Prelude

import Effect (Effect)
import Test.Colors (colorSpec)

main :: Effect Unit
main = do
  colorSpec