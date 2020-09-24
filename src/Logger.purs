module Logger where

import Prelude

import Colors (ColorType(..), format)
import Effect (Effect)
import Effect.Console (log)

logMessage :: String -> Effect Unit
logMessage message = log $ format message Log

logError :: String -> Effect Unit
logError message = log $ format message Error

logDebug :: String -> Effect Unit
logDebug message = log $ format message Debug

logInfo :: String -> Effect Unit
logInfo message = log $ format message Info

logSuccess :: String -> Effect Unit
logSuccess message = log $ format message Success