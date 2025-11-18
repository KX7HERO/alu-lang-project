module lang::alu::Checker

import List;
import Map;
import Message;
import ParseTree;
import Set;
import String;

import lang::alu::Syntax;

extend analysis::typepal::TypePal;

data StructField = structField(str name, AType fieldType);

data AType
  = intType()
  | boolType()
  | floatType()
  | charType()
  | stringType()
  | voidType()
  | sequenceType(AType elementType)
  | tupleType(AType first, AType second)
  | functionType(list[AType] params, AType result)
  | dataType(str name)
  | structDescriptorType(list[StructField] fields)
  | structInstanceType(list[StructField] fields)
  | builtinType(str name)
  ;

data IdRole
  = variableId()
  | functionId()
  | dataId()
  ;

str joinWith(str sep, list[str] parts) {
  str result = "";
  bool first = true;
  for (str part <- parts) {
    if (!first) {
      result += sep;
    }
    result += part;
    first = false;
  }
  return result;
}

bool sameStructLayout(list[StructField] left, list[StructField] right) {
  if (size(left) != size(right)) {
    return false;
  }
  if (isEmpty(left)) {
    return true;
  }
  for (int idx <- [0 .. size(left) - 1]) {
    StructField l = left[idx];
    StructField r = right[idx];
    if (l.name != r.name || !aluIsSubType(l.fieldType, r.fieldType)) {
      return false;
    }
  }
  return true;
}

str prettyAType(intType()) = "Int";
str prettyAType(boolType()) = "Bool";
str prettyAType(floatType()) = "Float";
str prettyAType(charType()) = "Char";
str prettyAType(stringType()) = "String";
str prettyAType(voidType()) = "Void";
str prettyAType(sequenceType(AType elementType)) = "Sequence(<prettyAType(elementType)>)";
str prettyAType(tupleType(AType first, AType second)) = "Tuple(<prettyAType(first)>, <prettyAType(second)>)";
str prettyAType(functionType(list[AType] params, AType result)) =
  "Function(" + joinWith(", ", [prettyAType(param) | param <- params]) + " -> " + prettyAType(result) + ")";
str prettyAType(dataType(str name)) = "Data(<name>)";
str prettyAType(structDescriptorType(list[StructField] fields)) =
  "Struct<" + joinWith(", ", [field.name | field <- fields]) + ">";
str prettyAType(structInstanceType(list[StructField] fields)) =
  "StructInstance<" + joinWith(", ", [field.name | field <- fields]) + ">";
str prettyAType(builtinType(str name)) = "Builtin(<name>)";
default str prettyAType(AType atype) = "<atype>";

bool isNumeric(intType()) = true;
bool isNumeric(floatType()) = true;
default bool isNumeric(AType _) = false;

bool isIterable(sequenceType(_)) = true;
bool isIterable(stringType()) = true;
default bool isIterable(AType _) = false;

tuple[list[str], set[IdRole]] aluGetTypeNamesAndRole(dataType(str name)) = <[name], {dataId()}>;
default tuple[list[str], set[IdRole]] aluGetTypeNamesAndRole(AType _) = <[], {}>;

bool aluIsSubType(AType left, AType right) {
  if (left == right) {
    return true;
  }
  switch (<left, right>) {
    case <intType(), floatType()>:
      return true;
    case <sequenceType(AType lElem), sequenceType(AType rElem)>:
      return aluIsSubType(lElem, rElem);
    case <tupleType(AType lf, AType ls), tupleType(AType rf, AType rs)>:
      return aluIsSubType(lf, rf) && aluIsSubType(ls, rs);
    case <functionType(list[AType] lp, AType lr), functionType(list[AType] rp, AType rr)>:
      return size(lp) == size(rp) &&
             all(idx <- [0 .. size(lp) - 1], aluIsSubType(lp[idx], rp[idx])) &&
             aluIsSubType(lr, rr);
    case <structDescriptorType(list[StructField] leftFields), structDescriptorType(list[StructField] rightFields)>:
      return sameStructLayout(leftFields, rightFields);
    case <structInstanceType(list[StructField] leftFields), structInstanceType(list[StructField] rightFields)>:
      return sameStructLayout(leftFields, rightFields);
    case <structInstanceType(list[StructField] leftFields), structDescriptorType(list[StructField] rightFields)>:
      return sameStructLayout(leftFields, rightFields);
    default:
      return false;
  }
}

TypePalConfig aluConfig() =
  tconfig(
    isSubType = aluIsSubType,
    getTypeNamesAndRole = aluGetTypeNamesAndRole
  );

map[str, set[str]] dataOperations = ();

public void checkProgram(str code) {
  resetState();
  Tree parsed = parse(#start[Program], code);
  Program program = parsed;
  TModel model = aluTModel(program);
  list[Message] messages = model.messages;
  if (!isEmpty(messages)) {
    throw IllegalArgument(formatMessages(messages));
  }
}

TModel aluTModel(Program program) {
  Tree root = program;
  Collector collector = newCollector("alu", root, aluConfig());
  aluPreCollectInitialization(root, collector);
  collect(program, collector);
  return newSolver(root, collector.run()).run();
}

void resetState() {
  dataOperations = ();
}

str formatMessages(list[Message] messages) {
  list[str] lines = ["Type errors detected:"];
  lines += [" - <msg>" | msg <- messages];
  return joinWith("\n", lines);
}

void aluPreCollectInitialization(Tree tree, Collector c) {
  loc anchor = getLoc(tree);
  c.predefine("print", functionId(), anchor, defType(builtinType("print")));
  c.predefine("abs", functionId(), anchor, defType(builtinType("abs")));
  c.predefine("arctan2", functionId(), anchor, defType(builtinType("arctan2")));
  c.predefine("len", functionId(), anchor, defType(builtinType("len")));
}

void ensureUniqueIdentifiers(list[str] names, Tree current, Collector c, str kind) {
  set[str] seen = {};
  for (str name <- names) {
    if (name in seen) {
      c.report(error(current, "Duplicate <kind> name %q", name));
    }
    seen += {name};
  }
}

void ensureUniqueFieldInits(list[FieldInit] inits, Collector c) {
  set[str] seen = {};
  for (FieldInit init <- inits) {
    switch (init) {
      case fieldInit(str name, Expr _):
        if (name in seen) {
          c.report(error(init, "Field %q initialized multiple times", name));
        }
        seen += {name};
    }
  }
}

map[str, Expr] fieldInitValues(list[FieldInit] inits) {
  map[str, Expr] values = ();
  for (FieldInit init <- inits) {
    switch (init) {
      case fieldInit(str name, Expr value):
        values = values[name := value];
    }
  }
  return values;
}

void ensureOperationBelongs(str dataName, str opName, Tree current, Collector c) {
  if (dataName notin dataOperations) {
    c.report(error(current, "Unknown data abstraction %q", dataName));
    return;
  }
  if (opName notin dataOperations[dataName]) {
    c.report(error(current, "Operation %q is not declared on data %q", opName, dataName));
  }
}

bool areComparable(AType left, AType right) {
  if (left == right) {
    return true;
  }
  if (isNumeric(left) && isNumeric(right)) {
    return true;
  }
  return false;
}

void ensureComparable(value leftTree, value rightTree, Tree current, str op, Solver s) {
  AType leftType = s.getType(leftTree);
  AType rightType = s.getType(rightTree);
  if (!areComparable(leftType, rightType)) {
    s.report(error(current, "Operator %q is not defined for %t and %t", op, leftTree, rightTree));
  }
}

AType arithmeticResult(Tree current, value leftTree, value rightTree, str op, Solver s) {
  AType leftType = s.getType(leftTree);
  AType rightType = s.getType(rightTree);
  if (!isNumeric(leftType)) {
    s.report(error(leftTree, "Operator %q expects numeric left operand, found %t", op, leftTree));
  }
  if (!isNumeric(rightType)) {
    s.report(error(rightTree, "Operator %q expects numeric right operand, found %t", op, rightTree));
  }
  if (leftType == floatType() || rightType == floatType()) {
    return floatType();
  }
  return intType();
}

AType divisionResult(Tree current, value leftTree, value rightTree, Solver s) {
  AType leftType = s.getType(leftTree);
  AType rightType = s.getType(rightTree);
  if (!isNumeric(leftType)) {
    s.report(error(leftTree, "Division expects numeric left operand, found %t", leftTree));
  }
  if (!isNumeric(rightType)) {
    s.report(error(rightTree, "Division expects numeric right operand, found %t", rightTree));
  }
  return floatType();
}

AType modulusResult(value leftTree, value rightTree, Solver s) {
  AType leftType = s.getType(leftTree);
  AType rightType = s.getType(rightTree);
  if (leftType != intType()) {
    s.report(error(leftTree, "`%` expects Int left operand, found %t", leftTree));
  }
  if (rightType != intType()) {
    s.report(error(rightTree, "`%` expects Int right operand, found %t", rightTree));
  }
  return intType();
}

AType powResult(Tree current, value baseTree, value expTree, Solver s) {
  AType baseType = s.getType(baseTree);
  AType expType = s.getType(expTree);
  if (!isNumeric(baseType)) {
    s.report(error(baseTree, "`**` expects numeric base, found %t", baseTree));
  }
  if (!isNumeric(expType)) {
    s.report(error(expTree, "`**` expects numeric exponent, found %t", expTree));
  }
  return floatType();
}

void ensureArgCount(str name, list[Expr] args, int expected, Tree callSite, Solver s) {
  if (size(args) != expected) {
    s.report(error(callSite, "Builtin %q expects %q arguments, found %q", name, expected, size(args)));
  }
}

AType inferBuiltinType(str name, list[Expr] args, Tree callSite, Solver s) {
  switch (name) {
    case "print":
      return voidType();
    case "abs": {
      ensureArgCount(name, args, 1, callSite, s);
      if (isEmpty(args)) {
        return intType();
      }
      AType argType = s.getType(args[0]);
      if (!isNumeric(argType)) {
        s.report(error(args[0], "`abs` expects numeric argument, found %t", args[0]));
      }
      return argType == floatType() ? floatType() : intType();
    }
    case "arctan2": {
      ensureArgCount(name, args, 2, callSite, s);
      if (size(args) < 2) {
        return floatType();
      }
      for (Expr arg <- args[0..1]) {
        if (!isNumeric(s.getType(arg))) {
          s.report(error(arg, "`arctan2` expects numeric arguments, found %t", arg));
        }
      }
      return floatType();
    }
    case "len": {
      ensureArgCount(name, args, 1, callSite, s);
      if (isEmpty(args)) {
        return intType();
      }
      AType argType = s.getType(args[0]);
      if (!isIterable(argType)) {
        s.report(error(args[0], "`len` expects Sequence or String, found %t", args[0]));
      }
      return intType();
    }
    default:
      s.report(error(callSite, "Unknown builtin %q", name));
      return voidType();
  }
}

AType inferCallResult(AType calleeType, list[Expr] args, Tree callSite, Tree calleeTree, Solver s) {
  switch (calleeType) {
    case functionType(list[AType] params, AType result):
      if (size(params) != size(args)) {
        s.report(error(callSite, "Function expects %q arguments, found %q", size(params), size(args)));
      }
      int limit = min(size(params), size(args));
      for (int idx <- [0 .. limit - 1]) {
        s.requireSubType(args[idx], params[idx], error(args[idx], "Argument mismatch, expected %t, found %t", params[idx], args[idx]));
      }
      return result;
    case builtinType(str name):
      return inferBuiltinType(name, args, callSite, s);
    default:
      s.report(error(callSite, "Value %t is not callable", calleeTree));
      return voidType();
  }
}

AType elementTypeFromIterable(AType sourceType, Tree source, Solver s) {
  switch (sourceType) {
    case sequenceType(AType elem):
      return elem;
    case stringType():
      return charType();
    default:
      s.report(error(source, "Iterable expression expects Sequence or String, found %t", source));
      return voidType();
  }
}

list[Block] elseIfBlocks(list[ElseIfPart] parts) {
  list[Block] result = [];
  for (ElseIfPart part <- parts) {
    switch (part) {
      case elseifPart(_, Block block):
        result += [block];
    }
  }
  return result;
}

AType buildType(Type type, Collector c) {
  switch (type) {
    case intType():
      return intType();
    case boolType():
      return boolType();
    case floatType():
      return floatType();
    case charType():
      return charType();
    case stringType():
      return stringType();
    case sequenceType(Type elem):
      return sequenceType(buildType(elem, c));
    case tupleType(Type fst, Type snd):
      return tupleType(buildType(fst, c), buildType(snd, c));
    case dataType(str name):
      c.use(name, {dataId()});
      return dataType(name);
  }
  return voidType();
}

str extractParamName(Param p) {
  switch (p) {
    case paramTyped(str name, _): return name;
    case paramBare(str name): return name;
  }
  return "";
}

str extractDataName(DataDecl d) {
  switch (d) {
    case (DataDecl)`<Id name> = data with <{Id ","}+ _> end <Id? _> <";"? _>`:
      return "<name>";
  }
  return "";
}

str extractFunName(FunDecl f) {
  switch (f) {
    case (FunDecl)`<Id name> = function(<{Param ","}* _>) do <Block _> end <Id? _> <";"? _>`:
      return "<name>";
  }
  return "";
}

list[str] extractVarNames(VarDecl vd) {
  list[str] names = [];
  switch (vd) {
    case varDecl(list[VarBinding] bindings):
      for (VarBinding vb <- bindings) {
        switch (vb) {
          case bindingTypedInit(str name, _, _): names += [name];
          case bindingTyped(str name, _): names += [name];
          case bindingInit(str name, _): names += [name];
          case bindingBare(str name): names += [name];
        }
      }
  }
  return names;
}

void collect(Program program, Collector c) {
  switch (program) {
    case program(list[Decl] decls): {
      c.enterScope(program);
      
      // Global uniqueness check
      list[str] globalNames = [];
      for (Decl d <- decls) {
        switch (d) {
          case dataDecl(DataDecl data): globalNames += [extractDataName(data)];
          case funDecl(FunDecl fun): globalNames += [extractFunName(fun)];
          case stmtDecl(stmtVar(VarDecl vd)): globalNames += extractVarNames(vd);
        }
      }
      ensureUniqueIdentifiers(globalNames, program, c, "global identifier");

      for (Decl decl <- decls) {
        preCollect(decl, c);
      }
      for (Decl decl <- decls) {
        collect(decl, c);
      }
      c.leaveScope(program);
      c.fact(program, voidType());
    }
  }
}

void preCollect(Decl decl, Collector c) {
  switch (decl) {
    case dataDecl(DataDecl data):
      registerData(data, c);
    case funDecl(FunDecl fun):
      registerFunction(fun, c);
    default:
      ()
  }
}

void registerData(DataDecl decl, Collector c) {
  switch (decl) {
    case (DataDecl)`<Id name> = data with <{Id ","}+ ops> end <Id? _> <";"? _>`: {
      c.define("<name>", dataId(), decl, defType(dataType("<name>")));
      list[str] opNames = ["<op>" | Id op <- ops];
      ensureUniqueIdentifiers(opNames, decl, c, "operation");
      dataOperations = dataOperations["<name>" := { opName | str opName <- opNames }];
      for (Id op <- ops) {
        c.use(op, {functionId()});
      }
    }
  }
}

void registerFunction(FunDecl decl, Collector c) {
  switch (decl) {
    case funDecl(str name, list[Param] params, Block body, _): {
      list[loc] deps = [getLoc(body)] + [getLoc(param) | param <- params];
      c.define(name, functionId(), decl, defTypeCall(deps, AType(Solver s) {
        list[AType] paramTypes = [s.getType(param) | param <- params];
        AType resultType = s.getType(body);
        return functionType(paramTypes, resultType);
      }));
    }
  }
}

void collect(Decl decl, Collector c) {
  switch (decl) {
    case dataDecl(DataDecl data):
      collect(data, c);
    case funDecl(FunDecl fun):
      collect(fun, c);
    case stmtDecl(Stmt stmt):
      collect(stmt, c);
  }
}

void collect(DataDecl decl, Collector c) {
  switch (decl) {
    case dataDecl(str name, list[str] _, _):
      c.fact(decl, dataType(name));
  }
}

void collect(FunDecl decl, Collector c) {
  switch (decl) {
    case funDecl(str _, list[Param] params, Block body, _): {
      c.enterScope(decl);
      
      list[str] paramNames = [extractParamName(p) | Param p <- params];
      ensureUniqueIdentifiers(paramNames, decl, c, "parameter");

      for (Param param <- params) {
        collect(param, c);
      }
      collect(body, c);
      c.leaveScope(decl);
      c.fact(decl, body);
    }
  }
}

void collect(Param param, Collector c) {
  switch (param) {
    case paramTyped(str name, Type typeAnn): {
      AType annotated = buildType(typeAnn, c);
      c.define(name, variableId(), param, defType(annotated));
      c.fact(param, annotated);
    }
    case paramBare(str name): {
      AType inferred = c.newTypeVar(param);
      c.define(name, variableId(), param, defType(inferred));
      c.fact(param, inferred);
    }
  }
}

void collect(VarDecl decl, Collector c) {
  switch (decl) {
    case varDecl(list[VarBinding] bindings): {
      for (VarBinding binding <- bindings) {
        collect(binding, c);
      }
      c.fact(decl, voidType());
    }
  }
}

void collect(VarBinding binding, Collector c) {
  switch (binding) {
    case bindingTypedInit(str name, Type typeAnn, Expr init): {
      AType annotated = buildType(typeAnn, c);
      c.define(name, variableId(), binding, defType(annotated));
      c.requireSubType(init, annotated, error(init, "Initializer for %q expects %t, found %t", name, annotated, init));
      c.fact(binding, annotated);
      collect(init, c);
    }
    case bindingTyped(str name, Type typeAnn): {
      AType annotated = buildType(typeAnn, c);
      c.define(name, variableId(), binding, defType(annotated));
      c.fact(binding, annotated);
    }
    case bindingInit(str name, Expr init): {
      AType inferred = c.newTypeVar(binding);
      c.define(name, variableId(), binding, defType(inferred));
      c.requireEqual(init, inferred, error(init, "Initializer for %q incompatible with inferred type %t", name, inferred));
      c.fact(binding, inferred);
      collect(init, c);
    }
    case bindingBare(str name): {
      AType inferred = c.newTypeVar(binding);
      c.define(name, variableId(), binding, defType(inferred));
      c.fact(binding, inferred);
    }
  }
}

void collect(Block block, Collector c) {
  switch (block) {
    case block(list[Stmt] stmts): {
      c.enterScope(block);
      for (Stmt stmt <- stmts) {
        collect(stmt, c);
      }
      c.leaveScope(block);
      if (isEmpty(stmts)) {
        c.fact(block, voidType());
      } else {
        c.fact(block, stmts[size(stmts) - 1]);
      }
    }
  }
}

void collect(Stmt stmt, Collector c) {
  switch (stmt) {
    case stmtVar(VarDecl decl):
      collect(decl, c);
    case stmtAssign(LValue target, Expr expr): {
      collect(target, c);
      collect(expr, c);
      c.requireSubType(expr, target, error(stmt, "Cannot assign %t to %t", expr, target));
      c.fact(stmt, expr);
    }
    case stmtExpr(Expr expr):
      collect(expr, c);
  }
}

void collect(LValue lvalue, Collector c) {
  switch (lvalue) {
    case (LValue)`<Id name>`:
      c.use(name, {variableId()});
      c.fact(lvalue, name);
  }
}

void collect(Expr expr, Collector c) {
  switch (expr) {
    case ifExpr(IfExpr branch):
      collect(branch, c);
    case condExpr(CondExpr cond):
      collect(cond, c);
    case forExpr(ForExpr loop):
      collect(loop, c);
    case logicOr(OrExpr logic):
      collect(logic, c);
  }
}

void collect(IfExpr expr, Collector c) {
  switch (expr) {
    case ifExpr(Expr cond, Block thenBlock, list[ElseIfPart] elseifs, Block elseBlock): {
      c.requireEqual(cond, boolType(), error(cond, "If condition expects Bool, found %t", cond));
      list[Block] branchBlocks = [thenBlock] + elseIfBlocks(elseifs) + [elseBlock];
      c.calculate("if expression", expr, branchBlocks, AType(Solver s) {
        return s.lubList([s.getType(block) | block <- branchBlocks]);
      });
      collect(cond, c);
      collect(thenBlock, c);
      for (ElseIfPart part <- elseifs) {
        collect(part, c);
      }
      collect(elseBlock, c);
    }
  }
}

void collect(ElseIfPart part, Collector c) {
  switch (part) {
    case elseifPart(Expr cond, Block block): {
      c.requireEqual(cond, boolType(), error(cond, "Else-if guard expects Bool, found %t", cond));
      collect(cond, c);
      collect(block, c);
      c.fact(part, block);
    }
  }
}

void collect(CondExpr expr, Collector c) {
  switch (expr) {
    case condExpr(Expr subject, list[CondClause] clauses): {
      collect(subject, c);
      for (CondClause clause <- clauses) {
        collect(clause, c);
      }
      list[Block] clauseBlocks = [];
      for (CondClause clause <- clauses) {
        switch (clause) {
          case condClause(_, Block block):
            clauseBlocks += [block];
        }
      }
      c.calculate("cond expression", expr, clauseBlocks, AType(Solver s) {
        return s.lubList([s.getType(block) | block <- clauseBlocks]);
      });
    }
  }
}

void collect(CondClause clause, Collector c) {
  switch (clause) {
    case condClause(Expr guard, Block block): {
      c.requireEqual(guard, boolType(), error(guard, "Cond guard expects Bool, found %t", guard));
      collect(guard, c);
      collect(block, c);
      c.fact(clause, block);
    }
  }
}

void collect(ForExpr expr, Collector c) {
  switch (expr) {
    case forRange(Id var, Expr start, Expr stop, Block block): {
      str name = "<var>";
      c.enterScope(expr);
      c.define(name, variableId(), expr, defType(intType()));
      c.requireEqual(start, intType(), error(start, "Loop start expects Int, found %t", start));
      c.requireEqual(stop, intType(), error(stop, "Loop end expects Int, found %t", stop));
      collect(start, c);
      collect(stop, c);
      collect(block, c);
      c.leaveScope(expr);
      c.fact(expr, block);
    }
    case forIter(Id var, Expr source, Block block): {
      str name = "<var>";
      c.enterScope(expr);
      c.define(name, variableId(), expr, defType([source], AType(Solver s) {
        return elementTypeFromIterable(s.getType(source), source, s);
      }));
      collect(source, c);
      collect(block, c);
      c.leaveScope(expr);
      c.fact(expr, block);
    }
  }
}

void collect(OrExpr expr, Collector c) {
  switch (expr) {
    case orExpr(OrExpr left, AndExpr right): {
      c.calculate("logical or", expr, [left, right], AType(Solver s) {
        s.requireEqual(left, boolType(), error(left, "`or` expects Bool operands, found %t", left));
        s.requireEqual(right, boolType(), error(right, "`or` expects Bool operands, found %t", right));
        return boolType();
      });
      collect(left, c);
      collect(right, c);
    }
    case andExpr(AndExpr operand):
      collect(operand, c);
  }
}

void collect(AndExpr expr, Collector c) {
  switch (expr) {
    case andExpr(AndExpr left, EqualityExpr right): {
      c.calculate("logical and", expr, [left, right], AType(Solver s) {
        s.requireEqual(left, boolType(), error(left, "`and` expects Bool operands, found %t", left));
        s.requireEqual(right, boolType(), error(right, "`and` expects Bool operands, found %t", right));
        return boolType();
      });
      collect(left, c);
      collect(right, c);
    }
    case equalityExpr(EqualityExpr operand):
      collect(operand, c);
  }
}

void collect(EqualityExpr expr, Collector c) {
  switch (expr) {
    case eqExpr(EqualityExpr left, RelExpr right): {
      c.calculate("equals", expr, [left, right], AType(Solver s) {
        ensureComparable(left, right, expr, "=", s);
        return boolType();
      });
      collect(left, c);
      collect(right, c);
    }
    case neqExpr(EqualityExpr left, RelExpr right): {
      c.calculate("not equals", expr, [left, right], AType(Solver s) {
        ensureComparable(left, right, expr, "<>", s);
        return boolType();
      });
      collect(left, c);
      collect(right, c);
    }
    case relExpr(RelExpr operand):
      collect(operand, c);
  }
}

void collect(RelExpr expr, Collector c) {
  switch (expr) {
    case ltExpr(RelExpr left, AddExpr right):
      collectRelational(expr, "<", left, right, c);
    case leExpr(RelExpr left, AddExpr right):
      collectRelational(expr, "<=", left, right, c);
    case gtExpr(RelExpr left, AddExpr right):
      collectRelational(expr, ">", left, right, c);
    case geExpr(RelExpr left, AddExpr right):
      collectRelational(expr, ">=", left, right, c);
    case addExpr(AddExpr operand):
      collect(operand, c);
  }
}

void collectRelational(RelExpr current, str op, value left, value right, Collector c) {
  c.calculate("relational <op>", current, [left, right], AType(Solver s) {
    ensureComparable(left, right, current, op, s);
    return boolType();
  });
  collect(left, c);
  collect(right, c);
}

void collect(AddExpr expr, Collector c) {
  switch (expr) {
    case plusExpr(AddExpr left, MulExpr right): {
      c.calculate("addition", expr, [left, right], AType(Solver s) {
        return arithmeticResult(expr, left, right, "+", s);
      });
      collect(left, c);
      collect(right, c);
    }
    case minusExpr(AddExpr left, MulExpr right): {
      c.calculate("subtraction", expr, [left, right], AType(Solver s) {
        return arithmeticResult(expr, left, right, "-", s);
      });
      collect(left, c);
      collect(right, c);
    }
    case mulExpr(MulExpr operand):
      collect(operand, c);
  }
}

void collect(MulExpr expr, Collector c) {
  switch (expr) {
    case timesExpr(MulExpr left, PowExpr right): {
      c.calculate("multiplication", expr, [left, right], AType(Solver s) {
        return arithmeticResult(expr, left, right, "*", s);
      });
      collect(left, c);
      collect(right, c);
    }
    case divExpr(MulExpr left, PowExpr right): {
      c.calculate("division", expr, [left, right], AType(Solver s) {
        return divisionResult(expr, left, right, s);
      });
      collect(left, c);
      collect(right, c);
    }
    case modExpr(MulExpr left, PowExpr right): {
      c.calculate("modulus", expr, [left, right], AType(Solver s) {
        return modulusResult(left, right, s);
      });
      collect(left, c);
      collect(right, c);
    }
    case powExpr(PowExpr operand):
      collect(operand, c);
  }
}

void collect(PowExpr expr, Collector c) {
  switch (expr) {
    case powExpr(UnaryExpr base, PowExpr exponent): {
      c.calculate("power", expr, [base, exponent], AType(Solver s) {
        return powResult(expr, base, exponent, s);
      });
      collect(base, c);
      collect(exponent, c);
    }
    case unaryExpr(UnaryExpr operand):
      collect(operand, c);
  }
}

void collect(UnaryExpr expr, Collector c) {
  switch (expr) {
    case unaryNeg(UnaryExpr operand): {
      c.calculate("unary minus", expr, [operand], AType(Solver s) {
        AType operandType = s.getType(operand);
        if (!isNumeric(operandType)) {
          s.report(error(operand, "Unary `-` expects numeric operand, found %t", operand));
        }
        return operandType;
      });
      collect(operand, c);
    }
    case unaryPlus(UnaryExpr operand): {
      c.fact(expr, operand);
      collect(operand, c);
    }
    case unaryNot(UnaryExpr operand): {
      c.requireEqual(operand, boolType(), error(operand, "`neg` expects Bool operand, found %t", operand));
      collect(operand, c);
      c.fact(expr, boolType());
    }
    case postfixExpr(PostfixExpr operand):
      collect(operand, c);
  }
}

void collect(PostfixExpr expr, Collector c) {
  switch (expr) {
    case callExpr(PostfixExpr callee, list[Expr] args): {
      c.calculate("call", expr, [callee] + args, AType(Solver s) {
        AType calleeType = s.getType(callee);
        return inferCallResult(calleeType, args, expr, callee, s);
      });
      collect(callee, c);
      for (Expr arg <- args) {
        collect(arg, c);
      }
    }
    case memberExpr(PostfixExpr target, Id field): {
      str fieldName = "<field>";
      c.calculate("member access", expr, [target], AType(Solver s) {
        AType targetType = s.getType(target);
        switch (targetType) {
          case structInstanceType(list[StructField] fields): {
            for (StructField structField <- fields) {
              if (structField.name == fieldName) {
                return structField.fieldType;
              }
            }
            s.report(error(expr, "Field %q not present on struct", fieldName));
            return voidType();
          }
          case tupleType(AType first, AType second): {
            if (fieldName == "first") return first;
            if (fieldName == "second") return second;
            s.report(error(expr, "Tuple only has fields 'first' and 'second', found %q", fieldName));
            return voidType();
          }
          default:
            s.report(error(expr, "Member access expects struct instance or tuple, found %t", target));
            return voidType();
        }
      });
      collect(target, c);
    }
    case primaryExpr(PrimaryExpr operand):
      collect(operand, c);
  }
}

void collect(PrimaryExpr expr, Collector c) {
  switch (expr) {
    case parenExpr(Expr inner):
      c.fact(expr, inner); collect(inner, c);
    case dataCallExpr(Id dataName, Id opName, list[Expr] args): {
      str dataId = "<dataName>";
      str operation = "<opName>";
      c.use(dataName, {dataId()});
      c.use(opName, {functionId()});
      ensureOperationBelongs(dataId, operation, expr, c);
      c.calculate("data operation", expr, [opName] + args, AType(Solver s) {
        return inferCallResult(s.getType(opName), args, expr, opName, s);
      });
      for (Expr arg <- args) {
        collect(arg, c);
      }
    }
    case structBuildExpr(Id structName, list[FieldInit] inits): {
      str name = "<structName>";
      c.use(structName, {variableId()});
      ensureUniqueFieldInits(inits, c);
      map[str, Expr] provided = fieldInitValues(inits);
      c.calculate("struct build", expr, [structName] + [value | FieldInit fieldInit(str _, Expr value) <- inits], AType(Solver s) {
        AType descriptor = s.getType(structName);
        switch (descriptor) {
          case structDescriptorType(list[StructField] fields): {
            for (StructField field <- fields) {
              if (field.name notin provided) {
                s.report(error(expr, "Missing field %q in struct literal", field.name));
              } else {
                Expr value = provided[field.name];
                s.requireSubType(value, field.fieldType, error(value, "Field %q expects %t, found %t", field.name, field.fieldType, value));
              }
            }
            for (str supplied <- provided) {
              if (supplied notin { field.name | field <- fields }) {
                s.report(error(expr, "Field %q is not part of struct descriptor", supplied));
              }
            }
            return structInstanceType(fields);
          }
          default:
            s.report(error(structName, "Struct literal expects descriptor value, found %t", structName));
            return voidType();
        }
      });
      for (FieldInit init <- inits) {
        collect(init, c);
      }
    }
    case sequenceExpr(list[Expr] elements): {
      if (isEmpty(elements)) {
        AType placeholder = c.newTypeVar(expr);
        c.fact(expr, sequenceType(placeholder));
      } else {
        c.calculate("sequence literal", expr, elements, AType(Solver s) {
          list[AType] elementTypes = [s.getType(el) | el <- elements];
          return sequenceType(s.lubList(elementTypes));
        });
      }
      for (Expr el <- elements) {
        collect(el, c);
      }
    }
    case tupleExpr(Expr first, Expr second): {
      c.calculate("tuple literal", expr, [first, second], AType(Solver s) {
        return tupleType(s.getType(first), s.getType(second));
      });
      collect(first, c);
      collect(second, c);
    }
    case structExpr(list[Id] fields): {
      list[str] names = ["<field>" | Id field <- fields];
      ensureUniqueIdentifiers(names, expr, c, "struct field");
      list[StructField] layout = [structField(name, c.newTypeVar(expr)) | str name <- names];
      c.fact(expr, structDescriptorType(layout));
    }
    case boolLiteral(Boolean _):
      c.fact(expr, boolType());
    case intLiteral(Integer _):
      c.fact(expr, intType());
    case floatLiteral(Float _):
      c.fact(expr, floatType());
    case charLiteral(Char _):
      c.fact(expr, charType());
    case stringLiteral(String _):
      c.fact(expr, stringType());
    case idExpr(Id name):
      c.use(name, {variableId(), functionId()});
  }
}

void collect(FieldInit init, Collector c) {
  switch (init) {
    case fieldInit(str _, Expr value):
      collect(value, c);
  }
}

void collect(Type type, Collector c) {
  AType atype = buildType(type, c);
  c.fact(type, atype);
}
