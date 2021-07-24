{ name = "web"
, dependencies =
  [ "aff", "console", "effect", "prelude", "psci-support", "spec", "strings" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
