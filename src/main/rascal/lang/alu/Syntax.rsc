module lang::alu::Syntax

extend lang::alu::CommonLex;

start syntax Program
  = program: Decl+
  ;

syntax Decl
  = dataDecl: DataDecl
  | funDecl: FunDecl
  | stmtDecl: Stmt
  ;

syntax DataDecl
  = dataDecl: name:Id "=" "data" "with" {Id ","}+ "end" endName:Id? ";"?
  ;

syntax FunDecl
  = funDecl: name:Id "=" "function" "(" {Param ","}* ")" "do" body:Block "end" endName:Id? ";"?
  ;

syntax Param
  = paramTyped: name:Id ":" typeAnn:Type
  | paramBare: name:Id
  ;

syntax VarDecl
  = varDecl: "var" {VarBinding ","}+ ";"?
  ;

syntax VarBinding
  = bindingTypedInit: name:Id ":" typeAnn:Type "=" init:Expr
  | bindingTyped: name:Id ":" typeAnn:Type
  | bindingInit: name:Id "=" init:Expr
  | bindingBare: name:Id
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
  = lvName: Id
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
  = forRange: "for" var:Id "from" start:Expr "to" stop:Expr "do" Block "end"
  | forIter: "for" var:Id "in" source:Expr "do" Block "end"
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
  | memberExpr: PostfixExpr "." field:Id
  | primaryExpr: PrimaryExpr
  ;

syntax PrimaryExpr
  = parenExpr: "(" Expr ")"
  | dataCallExpr: dataName:Id "$" opName:Id "(" {Expr ","}* ")"
  | structBuildExpr: structName:Id "$" "(" {FieldInit ","}+ ")"
  | sequenceExpr: "sequence" "(" {Expr ","}* ")"
  | tupleExpr: "tuple" "(" first:Expr "," second:Expr ")"
  | structExpr: "struct" "(" {Id ","}+ ")"
  | boolLiteral: Boolean
  | intLiteral: Integer
  | floatLiteral: Float
  | charLiteral: Char
  | stringLiteral: String
  | idExpr: Id
  ;

syntax FieldInit
  = fieldInit: name:Id ":" value:Expr
  ;

syntax Type
  = intType: "Int"
  | boolType: "Bool"
  | floatType: "Float"
  | charType: "Char"
  | stringType: "String"
  | sequenceType: "Sequence" "[" elem:Type "]"
  | tupleType: "Tuple" "[" fst:Type "," snd:Type "]"
  | dataType: Id
  ;
