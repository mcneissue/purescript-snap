{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ sources = [ "src/**/*.purs", "test/**/*.purs" ]
, name = "snap"
, dependencies =
  [ "effect"
  , "console"
  , "profunctor-lenses"
  , "react"
  , "react-basic"
  , "react-basic-dom"
  , "avar"
  , "variant"
  , "typelevel-prelude"
  , "heterogeneous"
  , "const"
  , "record"
  , "record-optics-extra"
  , "filterable"
  , "debug"
  , "type-equality"
  , "affjax"
  , "argonaut"
  , "routing-duplex"
  , "routing"
  , "profunctor-traverse"
  , "profunctor-extra"
  , "web-storage"
  , "simple-json"
  , "kishimen"
  ]
, packages = ../packages.dhall
}
