module lang::alu::Syntax

// extend lang::alu::CommonLex;
// layout Layout = [\t\n\r\ ]*;
lexical Identifier = [a-z][a-z0-9\-]*;
lexical Integer = [0-9]+;
lexical Boolean = "true" | "false";
lexical String = "\"" ![\n\r\"]* "\"";
lexical Float = [0-9]+ "." [0-9]+;
lexical Char = [\'] ![\n\r\'] [\'];

syntax Name = Identifier;

start syntax Program
  = program: Decl+
  ;

syntax Decl
  = dataDecl: DataDecl
  | funDecl: FunDecl
  | stmtDecl: Stmt
  ;

syntax DataDecl
  = dDecl: Identifier "=" "data" "with" {Identifier ","}+ "end" Identifier? ";"?
  ;

syntax FunDecl
  = fDecl: n:Name "=" "function" "(" {Param ","}* ")" "do" Block "end" endName:Name? ";"?
  ;

syntax Param
  = paramTyped: n:Identifier ":" typeAnn:Type
  | paramBare: n:Identifier
  ;

syntax VarDecl
  = varDecl: "var" {VarBinding ","}+ ";"?
  ;

syntax VarBinding
  = bindingTypedInit: n:Identifier ":" typeAnn:Type "=" init:Expr
  | bindingTyped: n:Identifier ":" typeAnn:Type
  | bindingInit: n:Identifier "=" init:Expr
  | bindingBare: n:Identifier
  ;

syntax Block
  = block: Stmt*
  ;

syntax Stmt
  = stmtVar: VarDecl
  | stmtAssign: LValue "=" Expr ";"?
  | stmtExpr: Expr ";"?
  ;

syntax LValue
  = lvName: Identifier
  ;

syntax Expr
  = ifExpr: IfExpr
  | condExpr: CondExpr
  | forExpr: ForExpr
  | logicOr: OrExpr
  ;

syntax IfExpr
  = ifExpr: "if" cond:Expr "then" thenBlock:Block elseifParts:ElseIfPart* "else" elseBlock:Block "end"
  ;

syntax ElseIfPart
  = elseifPart: "elseif" cond:Expr "then" Block
  ;

syntax CondExpr
  = condExpr: "cond" subject:Expr "do" clauses:CondClause+ "end"
  ;

syntax CondClause
  = condClause: guard:Expr "->" Block
  ;

syntax ForExpr
  = forRange: "for" var:Identifier "from" start:Expr "to" stop:Expr "do" Block "end"
  | forIter: "for" var:Identifier "in" source:Expr "do" Block "end"
  ;

syntax OrExpr
  = orExpr: OrExpr "or" AndExpr
  | andExpr: AndExpr
  ;

syntax AndExpr
  = andExpr: AndExpr "and" EqualityExpr
  | equalityExpr: EqualityExpr
  ;

syntax EqualityExpr
  = eqExpr: EqualityExpr "=" RelExpr
  | neqExpr: EqualityExpr "<>" RelExpr
  | relExpr: RelExpr
  ;

syntax RelExpr
  = ltExpr: RelExpr "<" AddExpr
  | leExpr: RelExpr "<=" AddExpr
  | gtExpr: RelExpr ">" AddExpr
  | geExpr: RelExpr ">=" AddExpr
  | addExpr: AddExpr
  ;

syntax AddExpr
  = plusExpr: AddExpr "+" MulExpr
  | minusExpr: AddExpr "-" MulExpr
  | mulExpr: MulExpr
  ;

syntax MulExpr
  = timesExpr: MulExpr "*" PowExpr
  | divExpr: MulExpr "/" PowExpr
  | modExpr: MulExpr "%" PowExpr
  | powExpr: PowExpr
  ;

syntax PowExpr
  = powExpr: UnaryExpr "**" PowExpr
  | unaryExpr: UnaryExpr
  ;

syntax UnaryExpr
  = unaryNeg: "-" UnaryExpr
  | unaryPlus: "+" UnaryExpr
  | unaryNot: "neg" UnaryExpr
  | postfixExpr: PostfixExpr
  ;

syntax PostfixExpr
  = callExpr: PostfixExpr "(" {Expr ","}* ")"
  | memberExpr: PostfixExpr "." field:Identifier
  | primaryExpr: PrimaryExpr
  ;

syntax PrimaryExpr
  = parenExpr: "(" Expr ")"
  | dataCallExpr: dataName:Identifier "$" opName:Identifier "(" {Expr ","}* ")"
  | structBuildExpr: structName:Identifier "$" "(" {FieldInit ","}+ ")"
  | sequenceExpr: "sequence" "(" {Expr ","}* ")"
  | tupleExpr: "tuple" "(" first:Expr "," second:Expr ")"
  | structExpr: "struct" "(" {Identifier ","}+ ")"
  | boolLiteral: Boolean
  | intLiteral: Integer
  | floatLiteral: Float
  | charLiteral: Char
  | stringLiteral: String
  | idExpr: Identifier
  ;

syntax FieldInit
  = fieldInit: n:Identifier ":" value:Expr
  ;

syntax Type
  = intType: "Int"
  | boolType: "Bool"
  | floatType: "Float"
  | charType: "Char"
  | stringType: "String"
  | sequenceType: "Sequence" "[" elem:Type "]"
  | tupleType: "Tuple" "[" fst:Type "," snd:Type "]"
  | dataType: Identifier
  ;
