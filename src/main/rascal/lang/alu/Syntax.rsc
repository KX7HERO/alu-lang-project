module lang::alu::Syntax

extend lang::alu::CommonLex;

syntax Name = Id;

start syntax Program
  = program: Decl+
  ;

syntax Decl
  = dataDecl: DataDecl
  | funDecl: FunDecl
  | stmtDecl: Stmt
  ;

syntax DataDecl
  = dDecl: Id name "=" "data" "with" {Id ","}+ ops "end" Id? endName ";"?
  ;

syntax FunDecl
  = fDecl: Name n "=" "function" "(" {Param ","}* params ")" "do" Block body "end" Name? endName ";"?
  ;

syntax Param
  = paramTyped: Id n ":" Type typeAnn
  | paramBare: Id n
  ;

syntax VarDecl
  = varDecl: "var" {VarBinding ","}+ bindings ";"?
  ;

syntax VarBinding
  = bindingTypedInit: Id n ":" Type typeAnn "=" Expr init
  | bindingTyped: Id n ":" Type typeAnn
  | bindingInit: Id n "=" Expr init
  | bindingBare: Id n
  ;

syntax Block
  = block: Stmt* stmts
  ;

syntax Stmt
  = stmtVar: VarDecl decl
  | stmtAssign: LValue target "=" Expr expr ";"?
  | stmtExpr: Expr expr ";"?
  ;

syntax LValue
  = lvName: Id name
  ;

syntax Expr
  = ifExpr: IfExpr
  | condExpr: CondExpr
  | forExpr: ForExpr
  | logicOr: OrExpr
  ;

syntax IfExpr
  = ifExpr: "if" Expr cond "then" Block thenBlock ElseIfPart* elseifParts "else" Block elseBlock "end"
  ;

syntax ElseIfPart
  = elseifPart: "elseif" Expr cond "then" Block block
  ;

syntax CondExpr
  = condExpr: "cond" Expr subject "do" CondClause+ clauses "end"
  ;

syntax CondClause
  = condClause: Expr guard "->" Block block
  ;

syntax ForExpr
  = forRange: "for" Id var "from" Expr start "to" Expr stop "do" Block block "end"
  | forIter: "for" Id var "in" Expr source "do" Block block "end"
  ;

syntax OrExpr
  = orExpr: OrExpr left "or" AndExpr right
  | andExpr: AndExpr
  ;

syntax AndExpr
  = andExpr: AndExpr left "and" EqualityExpr right
  | equalityExpr: EqualityExpr
  ;

syntax EqualityExpr
  = eqExpr: EqualityExpr left "=" RelExpr right
  | neqExpr: EqualityExpr left "<>" RelExpr right
  | relExpr: RelExpr
  ;

syntax RelExpr
  = ltExpr: RelExpr left "<" AddExpr right
  | leExpr: RelExpr left "<=" AddExpr right
  | gtExpr: RelExpr left ">" AddExpr right
  | geExpr: RelExpr left ">=" AddExpr right
  | addExpr: AddExpr
  ;

syntax AddExpr
  = plusExpr: AddExpr left "+" MulExpr right
  | minusExpr: AddExpr left "-" MulExpr right
  | mulExpr: MulExpr
  ;

syntax MulExpr
  = timesExpr: MulExpr left "*" PowExpr right
  | divExpr: MulExpr left "/" PowExpr right
  | modExpr: MulExpr left "%" PowExpr right
  | powExpr: PowExpr
  ;

syntax PowExpr
  = powExpr: UnaryExpr base "**" PowExpr exponent
  | unaryExpr: UnaryExpr
  ;

syntax UnaryExpr
  = unaryNeg: "-" UnaryExpr operand
  | unaryPlus: "+" UnaryExpr operand
  | unaryNot: "neg" UnaryExpr operand
  | postfixExpr: PostfixExpr
  ;

syntax PostfixExpr
  = callExpr: PostfixExpr callee "(" {Expr ","}* args ")"
  | memberExpr: PostfixExpr target "." Id field
  | primaryExpr: PrimaryExpr
  ;

syntax PrimaryExpr
  = parenExpr: "(" Expr inner ")"
  | dataCallExpr: Id dataName "$" Id opName "(" {Expr ","}* args ")"
  | structBuildExpr: Id structName "$" "(" {FieldInit ","}+ inits ")"
  | sequenceExpr: "sequence" "(" {Expr ","}* elements ")"
  | tupleExpr: "tuple" "(" Expr first "," Expr second ")"
  | structExpr: "struct" "(" {Id ","}+ fields ")"
  | boolLiteral: Boolean
  | intLiteral: Integer
  | floatLiteral: Float
  | charLiteral: Char
  | stringLiteral: String
  | idExpr: Id name
  ;

syntax FieldInit
  = fieldInit: Id n ":" Expr value
  ;

syntax Type
  = intType: "Int"
  | boolType: "Bool"
  | floatType: "Float"
  | charType: "Char"
  | stringType: "String"
  | sequenceType: "Sequence" "[" Type elem "]"
  | tupleType: "Tuple" "[" Type fst "," Type snd "]"
  | dataType: Id name
  ;
