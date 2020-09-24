{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "web"
, dependencies = [ "aff", "console", "effect", "psci-support", "spec" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
