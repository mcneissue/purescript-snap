// This file was generated by purescript-docs-search
window.DocsSearchTypeIndex["1069326838"] = [{"values":[{"sourceSpan":{"start":[13,1],"name":".spago/routing-duplex/v0.4.1/src/Routing/Duplex/Generic.purs","end":[17,17]},"score":0,"packageInfo":{"values":["routing-duplex"],"tag":"Package"},"name":"sum","moduleName":"Routing.Duplex.Generic","info":{"values":[{"type":{"tag":"ForAll","contents":["r",{"tag":"ForAll","contents":["rep",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Generic","Rep"],"Generic"],"constraintArgs":[{"tag":"TypeVar","contents":"a"},{"tag":"TypeVar","contents":"rep"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Routing","Duplex","Generic"],"GRouteDuplex"],"constraintArgs":[{"tag":"TypeVar","contents":"rep"},{"tag":"TypeVar","contents":"r"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Record"]},{"tag":"TypeVar","contents":"r"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Routing","Duplex"],"RouteDuplex'"]},{"tag":"TypeVar","contents":"a"}]}]}]}]},null]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[6,1],"name":".spago/record-optics-extra/v0.1.0/src/Internal.purs","end":[6,69]},"score":0,"packageInfo":{"values":["record-optics-extra"],"tag":"Package"},"name":"rp2rlp","moduleName":"Internal","info":{"values":[{"type":{"tag":"ForAll","contents":["proxy",{"tag":"ForAll","contents":["rl",{"tag":"ForAll","contents":["r",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Prim","RowList"],"RowToList"],"constraintArgs":[{"tag":"TypeVar","contents":"r"},{"tag":"TypeVar","contents":"rl"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"proxy"},{"tag":"TypeVar","contents":"r"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Type","Data","RowList"],"RLProxy"]},{"tag":"TypeVar","contents":"rl"}]}]}]},null]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[177,1],"name":".spago/variant/v6.0.1/src/Data/Variant.purs","end":[181,15]},"score":3,"packageInfo":{"values":["variant"],"tag":"Package"},"name":"expand","moduleName":"Data.Variant","info":{"values":[{"type":{"tag":"ForAll","contents":["gt",{"tag":"ForAll","contents":["a",{"tag":"ForAll","contents":["lt",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Prim","Row"],"Union"],"constraintArgs":[{"tag":"TypeVar","contents":"lt"},{"tag":"TypeVar","contents":"a"},{"tag":"TypeVar","contents":"gt"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Variant"],"Variant"]},{"tag":"TypeVar","contents":"lt"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Variant"],"Variant"]},{"tag":"TypeVar","contents":"gt"}]}]}]},null]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Every `Variant lt` can be cast to some `Variant gt` as long as `lt` is a\nsubset of `gt`.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[80,3],"name":".spago/prelude/v4.1.1/src/Data/Monoid.purs","end":[80,51]},"score":63,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"memptyRecord","moduleName":"Data.Monoid","info":{"values":[{"typeClassArguments":[["rowlist",null],["row",null],["subrow",null]],"typeClass":[["Data","Monoid"],"MonoidRecord"],"type":{"tag":"ForAll","contents":["rowlist",{"tag":"ForAll","contents":["row",{"tag":"ForAll","contents":["subrow",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Monoid"],"MonoidRecord"],"constraintArgs":[{"tag":"TypeVar","contents":"rowlist"},{"tag":"TypeVar","contents":"row"},{"tag":"TypeVar","contents":"subrow"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Type","Data","RowList"],"RLProxy"]},{"tag":"TypeVar","contents":"rowlist"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Record"]},{"tag":"TypeVar","contents":"subrow"}]}]}]},null]},null]},null]}}],"tag":"TypeClassMemberResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[10,3],"name":".spago/functors/v3.1.1/src/Data/Functor/Coproduct/Inject.purs","end":[10,30]},"score":5,"packageInfo":{"values":["functors"],"tag":"Package"},"name":"inj","moduleName":"Data.Functor.Coproduct.Inject","info":{"values":[{"typeClassArguments":[["f",null],["g",null]],"typeClass":[["Data","Functor","Coproduct","Inject"],"Inject"],"type":{"tag":"ForAll","contents":["f",{"tag":"ForAll","contents":["g",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Functor","Coproduct","Inject"],"Inject"],"constraintArgs":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeVar","contents":"g"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"g"},{"tag":"TypeVar","contents":"a"}]}]}]},null]},null]},null]}}],"tag":"TypeClassMemberResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[25,1],"name":".spago/contravariant/v4.0.1/src/Data/Functor/Contravariant.purs","end":[25,67]},"score":2,"packageInfo":{"values":["contravariant"],"tag":"Package"},"name":"coerce","moduleName":"Data.Functor.Contravariant","info":{"values":[{"type":{"tag":"ForAll","contents":["b",{"tag":"ForAll","contents":["a",{"tag":"ForAll","contents":["f",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Functor","Contravariant"],"Contravariant"],"constraintArgs":[{"tag":"TypeVar","contents":"f"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Functor"],"Functor"],"constraintArgs":[{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeVar","contents":"b"}]}]}]}]},null]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[143,1],"name":".spago/filterable/v3.0.2/src/Data/Filterable.purs","end":[144,13]},"score":0,"packageInfo":{"values":["filterable"],"tag":"Package"},"name":"cleared","moduleName":"Data.Filterable","info":{"values":[{"type":{"tag":"ForAll","contents":["b",{"tag":"ForAll","contents":["a",{"tag":"ForAll","contents":["f",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Filterable"],"Filterable"],"constraintArgs":[{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeVar","contents":"b"}]}]}]},null]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Filter out all values.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[142,1],"name":".spago/tailrec/v4.1.1/src/Control/Monad/Rec/Class.purs","end":[142,50]},"score":11,"packageInfo":{"values":["tailrec"],"tag":"Package"},"name":"forever","moduleName":"Control.Monad.Rec.Class","info":{"values":[{"type":{"tag":"ForAll","contents":["b",{"tag":"ForAll","contents":["a",{"tag":"ForAll","contents":["m",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Monad","Rec","Class"],"MonadRec"],"constraintArgs":[{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"m"},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"m"},{"tag":"TypeVar","contents":"b"}]}]}]},null]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"`forever` runs an action indefinitely, using the `MonadRec` instance to\nensure constant stack usage.\n\nFor example:\n\n```purescript\nmain = forever $ trace \"Hello, World!\"\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[73,1],"name":".spago/gen/v2.1.1/src/Control/Monad/Gen.purs","end":[73,66]},"score":6,"packageInfo":{"values":["gen"],"tag":"Package"},"name":"elements","moduleName":"Control.Monad.Gen","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ForAll","contents":["f",{"tag":"ForAll","contents":["m",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Monad","Gen","Class"],"MonadGen"],"constraintArgs":[{"tag":"TypeVar","contents":"m"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Semigroup","Foldable"],"Foldable1"],"constraintArgs":[{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"m"},{"tag":"TypeVar","contents":"a"}]}]}]}]},null]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Creates a generator that outputs a value chosen from a selection with\nuniform probability.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[46,3],"name":".spago/fork/v4.0.0/src/Control/Monad/Fork/Class.purs","end":[46,24]},"score":1,"packageInfo":{"values":["fork"],"tag":"Package"},"name":"join","moduleName":"Control.Monad.Fork.Class","info":{"values":[{"typeClassArguments":[["f",null],["m",null]],"typeClass":[["Control","Monad","Fork","Class"],"MonadFork"],"type":{"tag":"ForAll","contents":["f",{"tag":"ForAll","contents":["m",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Monad","Fork","Class"],"MonadFork"],"constraintArgs":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"m"},{"tag":"TypeVar","contents":"a"}]}]}]},null]},null]},null]}}],"tag":"TypeClassMemberResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]