{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "snap-examples"
, dependencies =
    [ "snap"
    , "kishimen"
    , "simple-json"
    , "record-optics-extra"
    , "argonaut"
    , "affjax"
    ]
, packages = ./packages.dhall
, sources = [ "./**/*.purs" ]
}
