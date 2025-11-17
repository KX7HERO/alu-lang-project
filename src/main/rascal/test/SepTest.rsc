module test::SepTest

lexical Id = [a-z]+;

syntax Foo = foo: {Id ","}+ items;
