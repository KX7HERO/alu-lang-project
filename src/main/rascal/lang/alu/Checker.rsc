module lang::alu::Checker

import ParseTree;
import lang::alu::Syntax;
extend analysis::typepal::TypePal;

// Basic ALU types for Project 3
data AType
  = intType()
  | boolType()
  | sequenceType(AType elem)
  | tupleType(AType fst, AType snd)
  | dataType(str name)
  | unknownType(str label)
  | functionType(list[AType] params, AType ret)
  ;

// Pretty-printing for error messages
str prettyAType(intType())               = "Int";
str prettyAType(boolType())              = "Bool";
str prettyAType(sequenceType(elem))      = "Sequence[<prettyAType(elem)>]";
str prettyAType(tupleType(fst, snd))     = "Tuple[<prettyAType(fst)>,<prettyAType(snd)>]";
str prettyAType(dataType(name))          = "<name>";
str prettyAType(unknownType(label))      = "'<label>'";
str prettyAType(functionType(params, ret)) = "(<strJoin([prettyAType(p) | p <- params], ", ")>) -> <prettyAType(ret)>";

/**
 * Map a syntactic 'Type' node into an 'AType' value.
 */
public AType typeOfTree(Type t, set[str] dataTypes = {}) {
  switch (t) {
    case (Type) `Int`:
      return intType();
    case (Type) `Bool`:
      return boolType();
    case (Type) `Sequence[<Type elem>]`:
      return sequenceType(typeOfTree(elem, dataTypes));
    case (Type) `Tuple[<Type fst>,<Type snd>]`:
      return tupleType(typeOfTree(fst, dataTypes), typeOfTree(snd, dataTypes));
    case (Type) `<Id name>`:
      if (!("<name>" in dataTypes)) {
        typeError("Unknown data type <name>");
      }
      return dataType("<name>");
  }
}

alias TypeEnv = map[str, AType];
alias FunEnv = map[str, functionType];

private void typeError(str message) {
  throw IllegalArgument(message);
}

public AType inferProgram(str code) {
  Program program = parse(#start[Program], code);
  return inferProgram(program);
}

public AType inferProgram(Program program) {
  set[str] dataTypes = {};
  FunEnv funEnv = ();
  TypeEnv varEnv = ();
  AType lastType = unknownType("program");

  // Collect data declarations and initial function signatures
  for (decl <- program.decls) {
    switch (decl) {
      case dataDecl(DataDecl)`<Id name> = data with <{Id ","}+ ops> end <Id?>`:
        dataTypes += {"<name>"};
      case funDecl(FunDecl) fd:
        funEnv["<fd.name>"] = initialFunSignature(fd, dataTypes);
    }
  }

  // Second pass performs full checks
  for (decl <- program.decls) {
    switch (decl) {
      case dataDecl(DataDecl) dd:
        verifyDataDecl(dd, funEnv);
      case funDecl(FunDecl) fd:
        <functionType signature, FunEnv updatedFunEnv> = inferFunction(fd, varEnv, funEnv, dataTypes);
        funEnv = updatedFunEnv;
      case varDecl(VarDecl)`var <VarBindingList bindings>;`:
        <TypeEnv newEnv, AType bindingType> = inferBindings(bindings, varEnv, funEnv, dataTypes);
        varEnv = newEnv;
        lastType = bindingType;
    }
  }

  return lastType;
}

public void checkProgram(str code) {
  inferProgram(code);
}

private functionType initialFunSignature(FunDecl decl, set[str] dataTypes) {
  list[AType] params = [];
  if (ParamList) ps := decl.params {
    params += paramType(ps.head, dataTypes);
    for (p <- ps.tail) {
      params += paramType((Param) p, dataTypes);
    }
  }
  return functionType(params, unknownType("return-<decl.name>"));
}

private AType paramType(Param p, set[str] dataTypes) {
  switch (p) {
    case Param)`<Id _> : <Type t>`:
      return typeOfTree(t, dataTypes);
    default:
      return unknownType("param-<p.id>");
  }
}

private void verifyDataDecl(DataDecl decl, FunEnv funEnv) {
  if (decl.ops == []) return;
  for (id <- decl.ops) {
    str op = "<id>";
    if (!(op in funEnv)) {
      typeError("Data abstraction <decl.name> requires operation <op> but it is not defined");
    }
  }
}

private tuple[TypeEnv, AType] inferBindings(VarBindingList bindings, TypeEnv env,
    FunEnv funEnv, set[str] dataTypes) {
  tuple[TypeEnv, AType] head = inferBinding(bindings.head, env, funEnv, dataTypes);
  env = head[0];
  AType last = head[1];
  for (b <- bindings.tail) {
    tuple[TypeEnv, AType] res = inferBinding((VarBinding) b, env, funEnv, dataTypes);
    env = res[0];
    last = res[1];
  }
  return <env, last>;
}

private tuple[TypeEnv, AType] inferBinding(VarBinding binding, TypeEnv env, FunEnv funEnv,
    set[str] dataTypes) {
  switch (binding) {
    case VarBinding)`<Id name> : <Type t> = <Expr init>`:
      <AType initType, TypeEnv updated> = inferExpr(init, env, funEnv, dataTypes);
      AType declared = typeOfTree(t, dataTypes);
      <AType final, TypeEnv merged> = ensureType(initType, declared, updated, name);
      merged["<name>"] = final;
      return <merged, final>;
    case VarBinding)`<Id name> = <Expr init>`:
      <AType initType, TypeEnv updated> = inferExpr(init, env, funEnv, dataTypes);
      updated["<name>"] = initType;
      return <updated, initType>;
    case VarBinding)`<Id name> : <Type t>`:
      AType declared = typeOfTree(t, dataTypes);
      env["<name>"] = declared;
      return <env, declared>;
    case VarBinding)`<Id name>`:
      typeError("Variable <name> must have a type annotation or initializer");
  }
  return <env, unknownType("binding")>;
}

private tuple[functionType, FunEnv] inferFunction(FunDecl decl, TypeEnv globals, FunEnv funEnv,
    set[str] dataTypes) {
  list[str] paramNames = [];
  list[AType] paramTypes = [];
  if (ParamList) ps := decl.params {
    paramNames += ps.head.id;
    paramTypes += paramType(ps.head, dataTypes);
    for (p <- ps.tail) {
      Param param = (Param) p;
      paramNames += param.id;
      paramTypes += paramType(param, dataTypes);
    }
  }

  TypeEnv local = globals + ();
  for (int i <- [0 .. size(paramNames) - 1]) {
    local[paramNames[i]] = paramTypes[i];
  }

  AType last = unknownType("return-<decl.name>");
  for (stmt <- decl.body) {
    <AType stmtType, TypeEnv updatedLocals> = inferStmt((Stmt) stmt, local, globals, funEnv, dataTypes);
    local = updatedLocals;
    last = stmtType;
  }

  functionType signature = functionType([local[n] | n <- paramNames], last);
  funEnv["<decl.name>"] = signature;
  return <signature, funEnv>;
}

private tuple[AType, TypeEnv] inferStmt(Stmt stmt, TypeEnv locals, TypeEnv globals, FunEnv funs,
    set[str] dataTypes) {
  switch (stmt) {
    case assign(Assign)`<Id name> = <Expr value>`:
      <AType exprType, TypeEnv env> = inferExpr(value, globals + locals, funs, dataTypes);
      if (name in locals) {
        <AType unified, TypeEnv updated> = ensureType(locals[name], exprType, env, name);
        locals[name] = unified;
        return <unified, locals>;
      }
      if (name in globals) {
        <AType unified, TypeEnv updated> = ensureType(globals[name], exprType, env, name);
        globals[name] = unified;
        return <unified, globals + locals>;
      }
      typeError("Assigning to undefined variable <name>");
    case exprStmt(Expr) e:
      return inferExpr(e, globals + locals, funs, dataTypes);
  }
  return <unknownType("stmt"), locals>;
}

private tuple[AType, TypeEnv] inferExpr(Expr expr, TypeEnv env, FunEnv funs, set[str] dataTypes) {
  switch (expr) {
    case Expr) `<Id name>`:
      if (name in env) return <env[name], env>;
      typeError("Undefined variable <name>");
    case Expr) `<Integer _>`:
      return <intType(), env>;
    case Expr) `<Boolean _>`:
      return <boolType(), env>;
    case Expr) `(<Expr inner>)`:
      return inferExpr(inner, env, funs, dataTypes);
    case Expr) `<Expr l> + <Expr r>`:
      return numericOp(l, r, env, funs, dataTypes);
    case Expr) `<Expr l> - <Expr r>`:
      return numericOp(l, r, env, funs, dataTypes);
    case Expr) `<Expr l> * <Expr r>`:
      return numericOp(l, r, env, funs, dataTypes);
    case Expr) `<Expr l> / <Expr r>`:
      return numericOp(l, r, env, funs, dataTypes);
    case Expr) `<Expr l> and <Expr r>`:
      return booleanOp(l, r, env, funs, dataTypes);
    case Expr) `<Expr l> or <Expr r>`:
      return booleanOp(l, r, env, funs, dataTypes);
    case Expr) `if <Expr cond> then <Expr thenE> else <Expr elseE>`:
      <AType condType, TypeEnv env1> = inferExpr(cond, env, funs, dataTypes);
      <AType condOk, TypeEnv env2> = ensureType(condType, boolType(), env1, "if-condition");
      <AType thenType, TypeEnv envThen> = inferExpr(thenE, env2, funs, dataTypes);
      <AType elseType, TypeEnv envElse> = inferExpr(elseE, envThen, funs, dataTypes);
      <AType unified, TypeEnv merged> = ensureType(thenType, elseType, envElse, "if-branches");
      return <unified, merged>;
    case call(CallExpr)`<Id name>(<ArgList? args>)`:
      if (!(name in funs)) typeError("Undefined function <name>");
      functionType sig = funs[name];
      list[Expr] argExprs = [];
      if (ArgList) argList := args {
        argExprs += argList.head;
        for (e <- argList.tail) argExprs += (Expr) e;
      }
      if (size(argExprs) != size(sig.params)) {
        typeError("Function <name> expects <size(sig.params)> arguments but got <size(argExprs)>");
      }
      TypeEnv updated = env;
      for (int i <- [0 .. size(argExprs) - 1]) {
        <AType argType, TypeEnv envArg> = inferExpr(argExprs[i], updated, funs, dataTypes);
        <AType unified, TypeEnv merged> = ensureType(argType, sig.params[i], envArg, "arg-<i>");
        updated = merged;
      }
      return <sig.ret, updated>;
    case seqLit(SequenceLiteral)`sequence(<{Expr ","}+ elems>)`:
      <AType headType, TypeEnv envHead> = inferExpr((Expr) elems[0], env, funs, dataTypes);
      TypeEnv currentEnv = envHead;
      AType elementType = headType;
      if (size(elems) > 1) {
        for (e <- elems[1 .. ]) {
          <AType nextType, TypeEnv envNext> = inferExpr((Expr) e, currentEnv, funs, dataTypes);
          <AType unified, TypeEnv merged> = ensureType(elementType, nextType, envNext, "sequence element");
          elementType = unified;
          currentEnv = merged;
        }
      }
      return <sequenceType(elementType), currentEnv>;
    case tupleLit(TupleLiteral)`tuple(<Expr fst>,<Expr snd>)`:
      <AType t1, TypeEnv env1> = inferExpr(fst, env, funs, dataTypes);
      <AType t2, TypeEnv env2> = inferExpr(snd, env1, funs, dataTypes);
      return <tupleType(t1, t2), env2>;
  }
  return <unknownType("expr"), env>;
}

private tuple[AType, TypeEnv] numericOp(Expr l, Expr r, TypeEnv env, FunEnv funs, set[str] dataTypes) {
  <AType lt, TypeEnv env1> = inferExpr(l, env, funs, dataTypes);
  <AType mergedL, TypeEnv env2> = ensureType(lt, intType(), env1, "arith-left");
  <AType rt, TypeEnv env3> = inferExpr(r, env2, funs, dataTypes);
  <AType mergedR, TypeEnv env4> = ensureType(rt, intType(), env3, "arith-right");
  return <intType(), env4>;
}

private tuple[AType, TypeEnv] booleanOp(Expr l, Expr r, TypeEnv env, FunEnv funs, set[str] dataTypes) {
  <AType lt, TypeEnv env1> = inferExpr(l, env, funs, dataTypes);
  <AType mergedL, TypeEnv env2> = ensureType(lt, boolType(), env1, "bool-left");
  <AType rt, TypeEnv env3> = inferExpr(r, env2, funs, dataTypes);
  <AType mergedR, TypeEnv env4> = ensureType(rt, boolType(), env3, "bool-right");
  return <boolType(), env4>;
}

private tuple[AType, TypeEnv] ensureType(AType found, AType expected, TypeEnv env, str context) {
  if (equals(found, expected)) return <expected, env>;
  if (unknownType(label) := found) {
    return <expected, env>;
  }
  if (unknownType(label) := expected) {
    return <found, env>;
  }
  if (sequenceType(f) := found, sequenceType(e) := expected) {
    <AType inner, TypeEnv updated> = ensureType(f, e, env, context);
    return <sequenceType(inner), updated>;
  }
  if (tupleType(ff, fs) := found, tupleType(ef, es) := expected) {
    <AType f1, TypeEnv env1> = ensureType(ff, ef, env, context);
    <AType f2, TypeEnv env2> = ensureType(fs, es, env1, context);
    return <tupleType(f1, f2), env2>;
  }
  typeError("Type mismatch in <context>: expected <prettyAType(expected)> but found <prettyAType(found)>");
  return <expected, env>;
}

