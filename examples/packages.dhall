let upstream = ../packages.dhall

let snap = ../spago.dhall

let additions = 
      { snap = 
          { dependencies = snap.dependencies
          , version = snap.version
          , repo    = snap.repository
          }
      } 

in  upstream // additions
