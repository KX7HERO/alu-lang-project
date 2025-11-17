module lang::alu::Interpreter

import IO;
import ParseTree;
import lang::alu::Syntax;

data Value
  = intValue(int i)
  | boolValue(bool b)
  | sequenceValue(list[Value] elems)
  | tupleValue(Value fst, Value snd)
  | functionValue(FunDecl decl)
  | dataValue(str name)
  ;

private void runtimeError(str message) {
  throw IllegalArgument(message);
}

public value evalProgram(str code) {
  Tree pt = parse(#start[Program], code).top;
  println("Parsed ALU program:");
  println(pt);

  Program program = parse(#start[Program], code);
  set[str] dataTypes = {};
  map[str, Value] functions = ();
  map[str, Value] variables = ();
  Value lastValue = intValue(0);

  // First collect data declarations and functions so they can be referenced
  // before their textual appearance.
  for (decl <- program.decls) {
    switch (decl) {
      case (Decl)`<DataDecl dd>`:
        if ((DataDecl)`<Id name> = data with <OpList ops> end` := dd || 
            (DataDecl)`<Id name> = data with <OpList ops> end <Id _>` := dd) {
          dataTypes += {"<name>"};
        }
      case (Decl)`<FunDecl fd>`:
        functions["<fd.name>"] = functionValue(fd);
    }
  }

  // Second pass evaluates variable declarations (which may invoke functions)
  // and keeps the latest bindings for variables and functions.
  for (decl <- program.decls) {
    switch (decl) {
      case (Decl)`<DataDecl _>`:
        // already recorded
        continue;
      case (Decl)`<FunDecl fd>`:
        functions["<fd.name>"] = functionValue(fd);
      case (Decl)`<VarDecl vd>`:
        if ((VarDecl)`var <VarBindingList bindings>;` := vd) {
          tuple[map[str, Value], Value] result = evalBindings(bindings, variables, functions, dataTypes);
          variables = result[0];
          lastValue = result[1];
        }
    }
  }

  // By convention, the result of the program is the value of the last
  // evaluated variable or the result of the final expression statement. If
  // there are no declarations we return void.
  return lastValue;
}

private tuple[map[str, Value], Value] evalBindings(VarBindingList bindings, map[str, Value] vars,
    map[str, Value] funs, set[str] dataTypes) {
  // Evaluate head
  tuple[map[str, Value], Value] result = evalBinding(bindings.head, vars, funs, dataTypes);
  vars = result[0];
  Value last = result[1];
  // Evaluate tail
  for (v <- bindings.tail) {
    result = evalBinding(v, vars, funs, dataTypes);
    vars = result[0];
    last = result[1];
  }
  return <vars, last>;
}

private tuple[map[str, Value], Value] evalBinding(VarBinding binding, map[str, Value] vars,
    map[str, Value] funs, set[str] dataTypes) {
  switch (binding) {
    case (VarBinding)`<Id name> : <Type _> = <Expr init>`: {
      Value v = evalExpr(init, vars, funs, dataTypes);
      str nameStr = "<name>";
      vars[nameStr] = v;
      return <vars, v>;
    }
    case (VarBinding)`<Id name> = <Expr init>`: {
      Value v = evalExpr(init, vars, funs, dataTypes);
      str nameStr = "<name>";
      vars[nameStr] = v;
      return <vars, v>;
    }
    case (VarBinding)`<Id name> : <Type _>`:
      runtimeError("Uninitialized variable <name>");
    case (VarBinding)`<Id name>`:
      runtimeError("Uninitialized variable <name>");
  }
  return <vars, intValue(0)>;
}

private Value evalExpr(Expr expr, map[str, Value] vars, map[str, Value] funs,
    set[str] dataTypes) {
  switch (expr) {
    case (Expr) `<Id name>`: {
      str nameStr = "<name>";
      if (nameStr in vars) {
        return vars[nameStr];
      }
      runtimeError("Undefined variable <name>");
    }
    case (Expr) `<Integer n>`:
      return intValue(toInt("<n>"));
    case (Expr) `<Boolean b>`:
      return boolValue(b == "true");
    case (Expr) `(<Expr inner>)`:
      return evalExpr(inner, vars, funs, dataTypes);
    case (Expr) `<Expr l> + <Expr r>`:
      return arithOp(l, r, vars, funs, dataTypes, int (int a, int b) { return a + b; });
    case (Expr) `<Expr l> - <Expr r>`:
      return arithOp(l, r, vars, funs, dataTypes, int (int a, int b) { return a - b; });
    case (Expr) `<Expr l> * <Expr r>`:
      return arithOp(l, r, vars, funs, dataTypes, int (int a, int b) { return a * b; });
    case (Expr) `<Expr l> / <Expr r>`: {
      int right = asInt(evalExpr(r, vars, funs, dataTypes));
      if (right == 0) runtimeError("Division by zero");
      int left = asInt(evalExpr(l, vars, funs, dataTypes));
      return intValue(left / right);
    }
    case (Expr) `<Expr l> and <Expr r>`: {
      bool lb = asBool(evalExpr(l, vars, funs, dataTypes));
      bool rb = asBool(evalExpr(r, vars, funs, dataTypes));
      return boolValue(lb && rb);
    }
    case (Expr) `<Expr l> or <Expr r>`: {
      bool lb = asBool(evalExpr(l, vars, funs, dataTypes));
      bool rb = asBool(evalExpr(r, vars, funs, dataTypes));
      return boolValue(lb || rb);
    }
    case (Expr) `if <Expr cond> then <Expr thenE> else <Expr elseE>`: {
      bool c = asBool(evalExpr(cond, vars, funs, dataTypes));
      return c ? evalExpr(thenE, vars, funs, dataTypes) : evalExpr(elseE, vars, funs, dataTypes);
    }
    case (Expr)`<CallExpr ce>`:
      if ((CallExpr)`<Id name>(<ArgList? args>)` := ce) {
        str nameStr = "<name>";
        if (!(nameStr in funs)) runtimeError("Undefined function <name>");
        functionValue(FunDecl) fd = funs[nameStr];
        list[Expr] argExprs = [];
        if ((ArgList) argsTree := args) {
          argExprs += argsTree.head;
          for (e <- argsTree.tail) {
            argExprs += e;
          }
        }
        list[Value] evaluatedArgs = [evalExpr(a, vars, funs, dataTypes) | a <- argExprs];
        return evalFunction(fd, evaluatedArgs, vars, funs, dataTypes);
      }
    case (Expr)`<SequenceLiteral seq>`: {
      list[Expr] elems = exprListToList(seq.elements);
      list[Value] values = [evalExpr(e, vars, funs, dataTypes) | e <- elems];
      return sequenceValue(values);
    }
    case (Expr)`<TupleLiteral tl>`:
      if ((TupleLiteral)`tuple(<Expr fst>,<Expr snd>)` := tl) {
        return tupleValue(evalExpr(fst, vars, funs, dataTypes), evalExpr(snd, vars, funs, dataTypes));
      }
  }
  runtimeError("Unknown expression type");
}

private Value evalFunction(FunDecl decl, list[Value] args, map[str, Value] globals,
    map[str, Value] funs, set[str] dataTypes) {
  list[str] paramNames = [];
  if ((ParamList) params := decl.params) {
    paramNames += "<params.head.id>";
    for (p <- params.tail) {
      paramNames += "<p.id>";
    }
  }

  if (size(paramNames) != size(args)) {
    runtimeError("Function <decl.name> expects <size(paramNames)> args but got <size(args)>");
  }

  map[str, Value] localEnv = ();
  for (int i <- [0 .. size(paramNames) - 1]) {
    localEnv[paramNames[i]] = args[i];
  }

  // Evaluate statements; the last expression is the return value.
  Value last = intValue(0);
  list[Stmt] stmts = stmtBlockToList(decl.body);
  for (Stmt stmt <- stmts) {
    last = evalStmt(stmt, localEnv, globals, funs, dataTypes);
  }
  return last;
}

private Value evalStmt(Stmt stmt, map[str, Value] locals, map[str, Value] globals,
    map[str, Value] funs, set[str] dataTypes) {
  switch (stmt) {
    case (Stmt)`<Assign a>`: {
      if ((Assign)`<Id name> = <Expr value>` := a) {
        str nameStr = "<name>";
        Value v = evalExpr(value, globals + locals, funs, dataTypes);
        if (nameStr in locals) {
          locals[nameStr] = v;
        } else if (nameStr in globals) {
          globals[nameStr] = v;
        } else {
          runtimeError("Assigning to undefined variable <name>");
        }
        return v;
      }
    }
    case (Stmt)`<Expr e>`:
      return evalExpr(e, globals + locals, funs, dataTypes);
  }
  return intValue(0);
}

private int asInt(Value v) {
  switch (v) {
    case intValue(int i): return i;
    default: runtimeError("Expected Int but got <v>");
  }
}

private bool asBool(Value v) {
  switch (v) {
    case boolValue(bool b): return b;
    default: runtimeError("Expected Bool but got <v>");
  }
}

private Value arithOp(Expr l, Expr r, map[str, Value] vars, map[str, Value] funs,
    set[str] dataTypes, int (int,int) op) {
  int left = asInt(evalExpr(l, vars, funs, dataTypes));
  int right = asInt(evalExpr(r, vars, funs, dataTypes));
  return intValue(op(left, right));
}

private list[Stmt] stmtBlockToList(StmtBlock block) {
  list[Stmt] stmts = [block.head];
  for (Stmt stmt <- block.tail) {
    stmts += stmt;
  }
  return stmts;
}

private list[Expr] exprListToList(ExprList exprList) {
  list[Expr] exprs = [exprList.head];
  for (Expr expr <- exprList.tail) {
    exprs += expr;
  }
  return exprs;
}

