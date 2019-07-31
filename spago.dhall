{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "my-project"
, dependencies =
    [ "effect"
    , "console"
    , "profunctor-lenses"
    , "react"
    , "react-basic"
    , "avar"
    , "variant"
    , "typelevel-prelude"
    , "heterogeneous"
    , "const"
    , "record"
    , "record-optics-extra"
    , "filterable"
    , "debug"
    ]
, packages =
    ./packages.dhall
}
