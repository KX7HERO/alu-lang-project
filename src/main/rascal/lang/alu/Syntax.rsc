module lang::alu::Syntax

extend lang::alu::CommonLex;

/**
 * A complete ALU program is a non-empty sequence of declarations.
 */
start syntax Program
  = decls: Decl+
  ;

/**
 * Top-level declarations in ALU.
 */
syntax Decl
  = dataDecl: DataDecl
  | funDecl: FunDecl
  | varDecl: VarDecl
  ;

/**
 * Data abstraction: 
 *   complex = data with create, add, get_theta, equal end complex
 */
syntax DataDecl
  = name: Id "=" "data" "with" ops: OpList "end" Id?
  ;

syntax OpList
  = head: Id tail: {"," Id}*
  ;

/**
 * Function definition:
 *   sqrt = function(x, g) do
 *     ...
 *   end sqrt
 *
 * The last expression in the body is the returned value (by convention).
 */
syntax FunDecl
  = name: Id "=" "function" "(" params: [ParamList]? ")" "do"
      body: StmtBlock
    "end" Id?
  ;

syntax StmtBlock
  = head: Stmt tail: {";" Stmt}* [";"]?
  ;

syntax ParamList
  = head: Param tail: {"," Param}*
  ;

syntax Param
  = id: Id [":" Type]?
  ;

/**
 * Variable declaration, optionally with type annotation and initializer:
 *   var x : Int = 1, y = 2;
 */
syntax VarDecl
  = "var" bindings: VarBindingList ";"
  ;

syntax VarBindingList
  = head: VarBinding tail: {"," VarBinding}*
  ;

syntax VarBinding
  = id: Id [":" Type]? ["=" init: Expr]?
  ;

/**
 * Statements: either assignments or bare expressions.
 */
syntax Stmt
  = assign: Assign
  | exprStmt: Expr
  ;

syntax Assign
  = target: Id "=" value: Expr
  ;

/**
 * Expressions: a subset of ALU used in the projects.
 */
syntax Expr
  = id: Id
  | int: Integer
  | bool: Boolean
  | bracket "(" Expr ")"
  > left Expr "*" Expr
  > left Expr "/" Expr
  > left Expr "+" Expr
  > left Expr "-" Expr
  > left Expr "and" Expr
  > left Expr "or" Expr
  > "if" Expr "then" Expr "else" Expr
  | call: CallExpr
  | seqLit: SequenceLiteral
  | tupleLit: TupleLiteral
  ;

syntax CallExpr
  = callee: Id "(" args: [ArgList]? ")"
  ;

syntax ArgList
  = head: Expr tail: {"," Expr}*
  ;

/**
 * sequence(x, y, z)
 */
syntax SequenceLiteral
  = "sequence" "(" elements: ExprList ")"
  ;

syntax ExprList
  = head: Expr tail: {"," Expr}*
  ;

/**
 * tuple(x, y)
 */
syntax TupleLiteral
  = "tuple" "(" fst: Expr "," snd: Expr ")"
  ;

/**
 * Simple type language for Project 3.
 * You can refine/extend this if you add more features.
 */
syntax Type
  = "Int"                // integers
  | "Bool"               // booleans
  | seqType: "Sequence" "[" elem: Type "]"
  | tupleType: "Tuple" "[" fst: Type "," snd: Type "]"
  | dataType: Id         // user-defined data abstractions
  ;
