module lang::alu::Syntax

extend lang::alu::CommonLex;

start syntax Program
  = program: Stmt*
  ;

syntax Semi = ";"?;

syntax Stmt
  = varStmt: "var" VarDecls Semi
  | funStmt: FunDecl
  | exprStmt: Expr Semi
  ;

syntax VarDecls = varDecls: VarDecl ("," VarDecl)*;

syntax VarDecl
  = varTypedInit: Id ":" Type "=" Expr
  | varTyped: Id ":" Type
  | varInit: Id "=" Expr
  | varBare: Id
  ;

syntax FunDecl
  = funDecl: Id name "=" "function" "(" Params? ")" "do" Block "end" Id? Semi
  ;

syntax Params = params: Param ("," Param)*;

syntax Param
  = paramTyped: Id ":" Type
  | paramBare: Id
  ;

syntax Block
  = block: Stmt* Expr?
  ;

syntax LValue
  = lvId: Id
  ;

syntax Expr
  = assignExpr: LValue "=" Expr
  | orExpr: OrExpr
  ;

syntax OrExpr
  = orExpr: OrExpr "or" AndExpr
  | andExpr: AndExpr
  ;

syntax AndExpr
  = andExpr: AndExpr "and" RelExpr
  | relExpr: RelExpr
  ;

syntax RelExpr
  = relLess: RelExpr "<" AddExpr
  | relLessEq: RelExpr "<=" AddExpr
  | relGreater: RelExpr ">" AddExpr
  | relGreaterEq: RelExpr ">=" AddExpr
  | relEqual: RelExpr "==" AddExpr
  | relNotEqual: RelExpr "<>" AddExpr
  | addOnly: AddExpr
  ;

syntax AddExpr
  = plusExpr: AddExpr "+" MulExpr
  | minusExpr: AddExpr "-" MulExpr
  | mulOnly: MulExpr
  ;

syntax MulExpr
  = mulExpr: MulExpr "*" Atom
  | divExpr: MulExpr "/" Atom
  | atomOnly: Atom
  ;

syntax ArgList = args: Expr ("," Expr)*;

syntax Atom
  = parenExpr: "(" Expr ")"
  | callExpr: Id "(" ArgList? ")"
  | intLiteral: Integer
  | boolLiteral: Boolean
  | idExpr: Id
  ;

syntax Type
  = intType: "Int"
  | boolType: "Bool"
  | dataType: Id
  ;
