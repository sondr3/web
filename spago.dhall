{ name = "web"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "effect"
  , "foldable-traversable"
  , "functions"
  , "node-buffer"
  , "node-fs"
  , "node-path"
  , "prelude"
  , "spec"
  , "strings"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
