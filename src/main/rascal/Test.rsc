module Test

layout Layout = [\t \r\n]*;
lexical Id = [a-z]+;

start syntax Program = "data" {Id ","}+ ops;
