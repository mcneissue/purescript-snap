{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "snap-examples"
, dependencies = [ "console", "effect", "psci-support", "snap" ]
, packages = ../packages.dhall
, sources = [ "./**/*.purs" ]
}
