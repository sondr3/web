module Logger where

import Prelude

import Colors (ColorType(..), format)
import Effect (Effect)
import Effect.Console (log)

logMessage :: String -> Effect Unit
logMessage message = log $ format message Log

logError :: String -> Effect Unit
logError message = log $ format "[ERROR] " Error <> message

logDebug :: String -> Effect Unit
logDebug message = log $ format "[DEBUG] " Debug <> message

logInfo :: String -> Effect Unit
logInfo message = log $ format "[INFO] " Info <> message

logSuccess :: String -> Effect Unit
logSuccess message = log $ format "[SUCCESS] " Success <> message