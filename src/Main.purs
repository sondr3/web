module Main where

import Effect

import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import FS (dirWalk)
import Prelude (Unit, bind, discard, pure, unit, ($))

main :: Effect Unit
main = launchAff_ do
  files <- dirWalk "." "json" 
  liftEffect $ logShow $ files
  pure unit