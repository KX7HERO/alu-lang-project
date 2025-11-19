module lang::alu::CommonLex

layout Layout = WhitespaceAndComment* !>> [\ \t\n\r] !>> "//" !>> "/*";

lexical WhitespaceAndComment
  = [\ \t\n\r]
  | Comment
  ;

lexical Comment
  = "//" ![\n]* ("\n" | "\r" | "\r\n")?
  | "/*" CommentChar* "*/"
  ;

lexical CommentChar
  = ![*]
  | "*" ![/]
  ;

// Integer literals (no sign, handled at expression level)
lexical Integer = [0-9]+;

// Floating point literals with a single decimal point
lexical Float = [0-9]+ "." [0-9]+;

// Character literals (simplified, escapes handled in semantic phase)
lexical Char = [\'] ![\n\r\'] [\'];

// Double quoted strings without embedded newlines
lexical String = "\"" ![\n\r\"]* "\"";

// Boolean literals
lexical Boolean = "true" | "false";

// Simple identifiers: starting with a letter, followed by letters, digits or '-'
lexical Id = [a-z][a-z0-9\-]* \ Reserved;

// Keywords taken from the ALU specification
keyword Reserved
  = "cond" | "do" | "data" | "elseif" | "end"
  | "for" | "from" | "then" | "function" | "else"
  | "if" | "in" | "iterator" | "sequence" | "struct"
  | "to" | "tuple" | "type" | "with" | "yielding"
  | "var" | "Int" | "Bool" | "Char" | "String" | "Float"
  | "Sequence" | "Tuple"
  ;
