module FFI where

import Prelude

import Effect.Uncurried (EffectFn2, EffectFn3)
import Node.Path (FilePath)

data CpSyncOptions = CpSyncOptions
  { recursive :: Boolean
  }

foreign import cpSyncImpl :: EffectFn3 FilePath FilePath CpSyncOptions Unit
foreign import copyFileSyncImpl :: EffectFn2 FilePath FilePath Unit

