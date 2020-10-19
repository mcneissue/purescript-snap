// This file was generated by purescript-docs-search
window.DocsSearchTypeIndex["1477867367"] = [{"values":[{"sourceSpan":{"start":[297,1],"name":".spago/routing-duplex/v0.4.1/src/Routing/Duplex.purs","end":[297,53]},"score":0,"packageInfo":{"values":["routing-duplex"],"tag":"Package"},"name":"string","moduleName":"Routing.Duplex","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Routing","Duplex"],"RouteDuplex'"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Routing","Duplex"],"RouteDuplex'"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"This does nothing (internally it's defined as identity).\nIt can be used to restrict a type parameter of a polymorphic `RouteDuplex' a` to `String`.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[292,1],"name":".spago/routing-duplex/v0.4.1/src/Routing/Duplex.purs","end":[292,55]},"score":0,"packageInfo":{"values":["routing-duplex"],"tag":"Package"},"name":"boolean","moduleName":"Routing.Duplex","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Routing","Duplex"],"RouteDuplex'"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Routing","Duplex"],"RouteDuplex'"]},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Refines a codec of Strings to Booleans, where `true` and `false` are the\nstrings `\"true\"` and `\"false\"`, and other strings are rejected.\n\n```purescript\nparse (boolean segment) \"true\"  == Right true\nparse (boolean segment) \"x\"     == Left (Expected \"Boolean\" \"x\")\n\nprint (boolean segment) true    == \"true\"\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[280,1],"name":".spago/routing-duplex/v0.4.1/src/Routing/Duplex.purs","end":[280,47]},"score":0,"packageInfo":{"values":["routing-duplex"],"tag":"Package"},"name":"int","moduleName":"Routing.Duplex","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Routing","Duplex"],"RouteDuplex'"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Routing","Duplex"],"RouteDuplex'"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Refines a codec of Strings to Ints.\n\n```purescript\nparse (int segment) \"1\"  == Right 1\nparse (int segment) \"x\"  == Left (Expected \"Int\" \"x\")\n\nprint (int segment) 1    == \"1\"\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[181,1],"name":".spago/routing-duplex/v0.4.1/src/Routing/Duplex.purs","end":[181,52]},"score":0,"packageInfo":{"values":["routing-duplex"],"tag":"Package"},"name":"flag","moduleName":"Routing.Duplex","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Routing","Duplex"],"RouteDuplex'"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Routing","Duplex"],"RouteDuplex'"]},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Consumes or prints a query flag (i.e. parameter without value).\n**Note:** that this combinator ignores the value of the parameter. It only cares about its presence/absence.\nPresence is interpreted as `true`, absence as `false`.\n\n```purescript\nparse (flag (param \"x\")) \"?x\"        == Right true\nparse (flag (param \"x\")) \"?x=true\",  == Right true\nparse (flag (param \"x\")) \"?x=false\", == Right true -- value is ignored, what matters is presence of the parameter x\nparse (flag (param \"x\")) \"?y\",       == Right false\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[31,1],"name":"./routing/src/Main.purs","end":[31,42]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"subscribeHash","moduleName":"Examples.Routing.Main","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Effect","AVar"],"AVar"]},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Effect"],"Effect"]},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[51,1],"name":".spago/effect/semigroup/src/Effect.purs","end":[51,55]},"score":21,"packageInfo":{"values":["effect"],"tag":"Package"},"name":"untilE","moduleName":"Effect","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Effect"],"Effect"]},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Effect"],"Effect"]},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Loop until a condition becomes `true`.\n\n`untilE b` is an effectful computation which repeatedly runs the effectful\ncomputation `b`, until its return value is `true`.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[58,1],"name":".spago/strings/v4.0.2/src/Data/String/NonEmpty/CodeUnits.purs","end":[58,52]},"score":8,"packageInfo":{"values":["strings"],"tag":"Package"},"name":"fromCharArray","moduleName":"Data.String.NonEmpty.CodeUnits","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeConstructor","contents":[["Prim"],"Char"]}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Data","String","NonEmpty","Internal"],"NonEmptyString"]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Creates a `NonEmptyString` from a character array `String`, returning\n`Nothing` if the input is empty.\n\n```purescript\nfromCharArray [] = Nothing\nfromCharArray ['a', 'b', 'c'] = Just (NonEmptyString \"abc\")\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[50,1],"name":".spago/strings/v4.0.2/src/Data/String/NonEmpty/CodePoints.purs","end":[50,62]},"score":8,"packageInfo":{"values":["strings"],"tag":"Package"},"name":"fromCodePointArray","moduleName":"Data.String.NonEmpty.CodePoints","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeConstructor","contents":[["Data","String","CodePoints"],"CodePoint"]}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Data","String","NonEmpty","Internal"],"NonEmptyString"]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]