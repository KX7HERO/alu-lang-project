module lang::alu::CommonLex

layout Layout = [\t \r\n]*;

// Integer literals (no sign, handled at expression level)
lexical Integer = [0-9]+;

// Boolean literals
lexical Boolean = "true" | "false";

// Simple identifiers: starting with a letter, followed by letters, digits or '-'
lexical Id = [a-z][a-z0-9\-]*;

// Keywords taken from the ALU specification
keyword Reserved
  = "cond" | "do" | "data" | "elseif" | "end"
  | "for" | "from" | "then" | "function" | "else"
  | "if" | "in" | "iterator" | "sequence" | "struct"
  | "to" | "tuple" | "type" | "with" | "yielding"
  ;
