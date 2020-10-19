// This file was generated by purescript-docs-search
window.DocsSearchTypeIndex["1829853192"] = [{"values":[{"sourceSpan":{"start":[194,1],"name":".spago/prelude/v4.1.1/src/Data/Ord.purs","end":[194,53]},"score":63,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"between","moduleName":"Data.Ord","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Ord"],"Ord"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Test whether a value is between a minimum and a maximum (inclusive).\nFor example:\n\n``` purescript\nlet f = between 0 10\nf 0    == true\nf (-5) == false\nf 5    == true\nf 10   == true\nf 15   == false\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[180,1],"name":".spago/prelude/v4.1.1/src/Data/Ord.purs","end":[180,45]},"score":63,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"clamp","moduleName":"Data.Ord","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Ord"],"Ord"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"a"}]}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Clamp a value between a minimum and a maximum. For example:\n\n``` purescript\nlet f = clamp 0 10\nf (-5) == 0\nf 5    == 5\nf 15   == 10\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[157,1],"name":".spago/enums/v4.0.1/src/Data/Enum.purs","end":[157,68]},"score":5,"packageInfo":{"values":["enums"],"tag":"Package"},"name":"toEnumWithDefaults","moduleName":"Data.Enum","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Enum"],"BoundedEnum"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeVar","contents":"a"}]}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Like `toEnum` but returns the first argument if `x` is less than\n`fromEnum bottom` and the second argument if `x` is greater than\n`fromEnum top`.\n\n``` purescript\ntoEnumWithDefaults False True (-1) -- False\ntoEnumWithDefaults False True 0    -- False\ntoEnumWithDefaults False True 1    -- True\ntoEnumWithDefaults False True 2    -- True\n```\n"}],"tag":"SearchResult"}]