module lang::alu::Syntax

extend lang::alu::CommonLex;

syntax Foo
  = assign: Id target "=" Id value
  ;
