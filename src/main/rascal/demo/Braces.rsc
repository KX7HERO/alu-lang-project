module demo::Braces

lexical Id = [a-z]+;

syntax Foo = foo: "(" {Id ","}+ items ")";
