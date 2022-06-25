{ name = "web"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "effect"
  , "foldable-traversable"
  , "maybe"
  , "node-buffer"
  , "node-fs"
  , "node-path"
  , "node-process"
  , "prelude"
  , "record"
  , "spec"
  , "strings"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
