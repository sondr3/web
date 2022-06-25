module FFI where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3)
import Node.Path (FilePath)

foreign import cpSyncImpl :: EffectFn3 FilePath FilePath { recursive :: Boolean } Unit
foreign import copyFileSyncImpl :: EffectFn2 FilePath FilePath Unit
foreign import sassCompileImpl :: EffectFn1 FilePath { css :: String }
foreign import createHash :: String -> String
foreign import optimizeCSS :: FilePath -> String -> Boolean -> Effect { code :: String, map :: String }
foreign import optimizeJS :: String -> Effect String
