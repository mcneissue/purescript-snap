let upstream = ../packages.dhall

let additions =
      { snap = ../spago.dhall as Location
      }

in  upstream // additions
