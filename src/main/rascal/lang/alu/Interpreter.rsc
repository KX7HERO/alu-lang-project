module lang::alu::Interpreter

import IO;
import List;
import Map;
import Set;
import String;
import util::Math;
import lang::alu::Syntax;

data Value
  = voidValue()
  | intValue(int n)
  | floatValue(real n)
  | boolValue(bool b)
  | charValue(str ch)
  | stringValue(str s)
  | sequenceValue(list[Value] elements)
  | tupleValue(Value first, Value second)
  | structTypeValue(list[str] fields)
  | structValue(str name, map[str, Value] fields)
  | functionValue(FunValue fun)
  | builtinValue(str name)
  ;

data FunValue = funValue(
  str name,
  list[str] params,
  Block body
);

data DataInfo = dataInfo(
  str name,
  set[str] operations
);

data EvalEnv = evalEnv(
  map[str, Value] vars,
  map[str, FunValue] funs,
  map[str, DataInfo] datas
);

alias ScopeStack = list[map[str, Value]];

data EvalState = evalState(ScopeStack scopes, EvalEnv env);

data EvalError = evalError(str message);

private void fail(str message) {
  throw evalError(message);
}

public value evalProgram(str code) {
  Program program = parse(#Program, code);
  <EvalState finalState, Value last> = interpret(program);
  return toRuntimeValue(last);
}

private tuple[EvalState, Value] interpret(Program program) {
  EvalState state = evalState([], evalEnv(initialBindings(), (), ()));
  Value last = voidValue();
  switch (program) {
    case program(list[Decl] decls):
      for (Decl decl <- decls) {
        <state, last> = evalDecl(decl, state);
      }
  }
  return <state, last>;
}

private map[str, Value] initialBindings() = (
  "true" : boolValue(true),
  "false" : boolValue(false),
  "print" : builtinValue("print"),
  "abs" : builtinValue("abs"),
  "arctan2" : builtinValue("arctan2"),
  "len" : builtinValue("len")
);

private tuple[EvalState, Value] evalDecl(Decl decl, EvalState state) {
  switch (decl) {
    case dataDecl(DataDecl data): {
      EvalEnv updated = registerData(data, state.env);
      return <state[env = updated], voidValue()>;
    }
    case funDecl(FunDecl fun): {
      EvalEnv updated = registerFunction(fun, state.env);
      FunValue stored = updated.funs[funName(fun)];
      return <state[env = updated], functionValue(stored)>;
    }
    case stmtDecl(Stmt stmt):
      return evalStmt(stmt, state);
  }
}

private str funName(FunDecl decl) {
  switch (decl) {
    case funDecl(str name, _, _, _):
      return name;
  }
  return "";
}

private EvalEnv registerData(DataDecl decl, EvalEnv env) {
  switch (decl) {
    case dataDecl(str name, list[str] ops, _): {
      set[str] opNames = { op | op <- ops };
      DataInfo info = dataInfo(name, opNames);
      return evalEnv(env.vars, env.funs, env.datas[name := info]);
    }
  }
  return env;
}

private EvalEnv registerFunction(FunDecl decl, EvalEnv env) {
  switch (decl) {
    case funDecl(str name, list[Param] params, Block body, _): {
      list[str] names = [paramName(p) | p <- params];
      FunValue fun = funValue(name, names, body);
      return evalEnv(env.vars[name := functionValue(fun)], env.funs[name := fun], env.datas);
    }
  }
  return env;
}

private str paramName(Param p) {
  switch (p) {
    case paramBare(str name): return name;
    case paramTyped(str name, _): return name;
  }
  return "";
}

private tuple[EvalState, Value] evalStmt(Stmt stmt, EvalState state) {
  switch (stmt) {
    case stmtVar(VarDecl decl): {
      EvalState updated = evalVarDecl(decl, state);
      return <updated, voidValue()>;
    }
    case stmtAssign(LValue target, Expr expr): {
      <EvalState next, Value value> = evalExpr(expr, state);
      EvalState assigned = assignTarget(target, value, next);
      return <assigned, value>;
    }
    case stmtExpr(Expr expr):
      return evalExpr(expr, state);
  }
}

private EvalState evalVarDecl(VarDecl decl, EvalState state) {
  switch (decl) {
    case varDecl(list[VarBinding] bindings):
      for (VarBinding binding <- bindings) {
        state = bindVar(binding, state);
      }
  }
  return state;
}

private EvalState bindVar(VarBinding binding, EvalState state) {
  str name = bindingName(binding);
  <EvalState next, Value value> = bindingValue(binding, state);
  return declareInScope(name, value, next);
}

private str bindingName(VarBinding binding) {
  switch (binding) {
    case bindingTypedInit(str name, Type _, _): return name;
    case bindingTyped(str name, Type _): return name;
    case bindingInit(str name, _): return name;
    case bindingBare(str name): return name;
  }
  return "";
}

private tuple[EvalState, Value] bindingValue(VarBinding binding, EvalState state) {
  switch (binding) {
    case bindingTypedInit(_, Type _, Expr expr):
      return evalExpr(expr, state);
    case bindingInit(_, Expr expr):
      return evalExpr(expr, state);
  }
  return <state, voidValue()>;
}

private EvalState declareInScope(str name, Value value, EvalState state) {
  if (isEmpty(state.scopes)) {
    EvalEnv env = state.env;
    env = env[vars = env.vars[name := value]];
    return state[env = env];
  }
  map[str, Value] head = state.scopes[0];
  head = head[name := value];
  return evalState([head] + state.scopes[1..], state.env);
}

private EvalState assignTarget(LValue target, Value value, EvalState state) {
  str name;
  switch (target) {
    case lvName(str n): name = n;
  }
  <ScopeStack locals, bool assigned> = assignLocal(name, value, state.scopes);
  if (assigned) {
    return evalState(locals, state.env);
  }
  EvalEnv env = state.env;
  env = env[vars = env.vars[name := value]];
  return evalState(state.scopes, env);
}

private tuple[ScopeStack, bool] assignLocal(str name, Value value, ScopeStack locals) {
  if (isEmpty(locals)) {
    return <locals, false>;
  }
  map[str, Value] head = locals[0];
  if (name in head) {
    head = head[name := value];
    return <[head] + locals[1..], true>;
  }
  <ScopeStack tailUpdated, bool found> = assignLocal(name, value, locals[1..]);
  return <[head] + tailUpdated, found>;
}

private tuple[EvalState, Value] evalExpr(Expr expr, EvalState state) {
  switch (expr) {
    case ifExpr(IfExpr branch):
      return evalIf(branch, state);
    case condExpr(CondExpr cond):
      return evalCond(cond, state);
    case forExpr(ForExpr loop):
      return evalFor(loop, state);
    case logicOr(OrExpr orTree):
      return evalOr(orTree, state);
  }
}

private tuple[EvalState, Value] evalIf(IfExpr branch, EvalState state) {
  switch (branch) {
    case ifExpr(Expr cond, Block thenBlock, list[ElseIfPart] elseifs, Block elseBlock): {
      <EvalState condState, Value condVal> = evalExpr(cond, state);
      if (truthy(condVal)) {
        return evalBlock(thenBlock, condState, true);
      }
      EvalState guardState = condState;
      for (ElseIfPart part <- elseifs) {
        switch (part) {
          case elseifPart(Expr guard, Block block): {
            <guardState, Value branchCond> = evalExpr(guard, guardState);
            if (truthy(branchCond)) {
              return evalBlock(block, guardState, true);
            }
          }
        }
      }
      return evalBlock(elseBlock, guardState, true);
    }
  }
}

private tuple[EvalState, Value] evalCond(CondExpr cond, EvalState state) {
  switch (cond) {
    case condExpr(Expr subject, list[CondClause] clauses): {
      <EvalState working, Value _> = evalExpr(subject, state);
      for (CondClause clause <- clauses) {
        switch (clause) {
          case condClause(Expr guard, Block block): {
            <working, Value guardVal> = evalExpr(guard, working);
            if (truthy(guardVal)) {
              return evalBlock(block, working, true);
            }
          }
        }
      }
    }
  }
  fail("No cond clause evaluated to true");
}

private tuple[EvalState, Value] evalFor(ForExpr loop, EvalState state) {
  switch (loop) {
    case forRange(str name, Expr startExpr, Expr endExpr, Block block): {
      <EvalState afterStart, Value startVal> = evalExpr(startExpr, state);
      int start = expectInt(startVal);
      <EvalState afterEnd, Value endVal> = evalExpr(endExpr, afterStart);
      int stop = expectInt(endVal);
      EvalState loopState = pushScope(afterEnd);
      Value last = voidValue();
      for (int current <- inclusiveRange(start, stop)) {
        loopState = declareInCurrent(name, intValue(current), loopState);
        <EvalState afterBody, Value bodyVal> = evalBlock(block, loopState, true);
        loopState = afterBody;
        last = bodyVal;
      }
      return <popScope(loopState), last>;
    }
    case forIter(str name, Expr source, Block block): {
      <EvalState afterSource, Value iterable> = evalExpr(source, state);
      list[Value] items = asList(iterable);
      EvalState loopState = pushScope(afterSource);
      Value last = voidValue();
      for (Value item <- items) {
        loopState = declareInCurrent(name, item, loopState);
        <EvalState afterBody, Value bodyVal> = evalBlock(block, loopState, true);
        loopState = afterBody;
        last = bodyVal;
      }
      return <popScope(loopState), last>;
    }
  }
}

private EvalState declareInCurrent(str name, Value value, EvalState state) {
  if (isEmpty(state.scopes)) {
    return declareInScope(name, value, state);
  }
  map[str, Value] head = state.scopes[0];
  head = head[name := value];
  return evalState([head] + state.scopes[1..], state.env);
}

private tuple[EvalState, Value] evalOr(OrExpr expr, EvalState state) {
  switch (expr) {
    case orExpr(OrExpr left, AndExpr right): {
      <EvalState leftState, Value leftVal> = evalOr(left, state);
      if (truthy(leftVal)) {
        return <leftState, boolValue(true)>;
      }
      return evalAnd(right, leftState);
    }
    case andExpr(AndExpr operand):
      return evalAnd(operand, state);
  }
}

private tuple[EvalState, Value] evalAnd(AndExpr expr, EvalState state) {
  switch (expr) {
    case andExpr(AndExpr left, EqualityExpr right): {
      <EvalState leftState, Value leftVal> = evalAnd(left, state);
      if (!truthy(leftVal)) {
        return <leftState, boolValue(false)>;
      }
      return evalEquality(right, leftState);
    }
    case equalityExpr(EqualityExpr operand):
      return evalEquality(operand, state);
  }
}

private tuple[EvalState, Value] evalEquality(EqualityExpr expr, EvalState state) {
  switch (expr) {
    case eqExpr(EqualityExpr left, RelExpr right): {
      <EvalState ls, Value lv> = evalEquality(left, state);
      <EvalState rs, Value rv> = evalRel(right, ls);
      return <rs, boolValue(valueEquals(lv, rv))>;
    }
    case neqExpr(EqualityExpr left, RelExpr right): {
      <EvalState ls, Value lv> = evalEquality(left, state);
      <EvalState rs, Value rv> = evalRel(right, ls);
      return <rs, boolValue(!valueEquals(lv, rv))>;
    }
    case relExpr(RelExpr operand):
      return evalRel(operand, state);
  }
}

private tuple[EvalState, Value] evalRel(RelExpr expr, EvalState state) {
  switch (expr) {
    case ltExpr(RelExpr left, AddExpr right): {
      <EvalState ls, Value lv> = evalRel(left, state);
      <EvalState rs, Value rv> = evalAdd(right, ls);
      return <rs, boolValue(compare(lv, rv) < 0)>;
    }
    case leExpr(RelExpr left, AddExpr right): {
      <EvalState ls, Value lv> = evalRel(left, state);
      <EvalState rs, Value rv> = evalAdd(right, ls);
      return <rs, boolValue(compare(lv, rv) <= 0)>;
    }
    case gtExpr(RelExpr left, AddExpr right): {
      <EvalState ls, Value lv> = evalRel(left, state);
      <EvalState rs, Value rv> = evalAdd(right, ls);
      return <rs, boolValue(compare(lv, rv) > 0)>;
    }
    case geExpr(RelExpr left, AddExpr right): {
      <EvalState ls, Value lv> = evalRel(left, state);
      <EvalState rs, Value rv> = evalAdd(right, ls);
      return <rs, boolValue(compare(lv, rv) >= 0)>;
    }
    case addExpr(AddExpr operand):
      return evalAdd(operand, state);
  }
}

private tuple[EvalState, Value] evalAdd(AddExpr expr, EvalState state) {
  switch (expr) {
    case plusExpr(AddExpr left, MulExpr right): {
      <EvalState ls, Value lv> = evalAdd(left, state);
      <EvalState rs, Value rv> = evalMul(right, ls);
      return <rs, addValues(lv, rv)>;
    }
    case minusExpr(AddExpr left, MulExpr right): {
      <EvalState ls, Value lv> = evalAdd(left, state);
      <EvalState rs, Value rv> = evalMul(right, ls);
      return <rs, subtractValues(lv, rv)>;
    }
    case mulExpr(MulExpr operand):
      return evalMul(operand, state);
  }
}

private tuple[EvalState, Value] evalMul(MulExpr expr, EvalState state) {
  switch (expr) {
    case timesExpr(MulExpr left, PowExpr right): {
      <EvalState ls, Value lv> = evalMul(left, state);
      <EvalState rs, Value rv> = evalPow(right, ls);
      return <rs, multiplyValues(lv, rv)>;
    }
    case divExpr(MulExpr left, PowExpr right): {
      <EvalState ls, Value lv> = evalMul(left, state);
      <EvalState rs, Value rv> = evalPow(right, ls);
      return <rs, divideValues(lv, rv)>;
    }
    case modExpr(MulExpr left, PowExpr right): {
      <EvalState ls, Value lv> = evalMul(left, state);
      <EvalState rs, Value rv> = evalPow(right, ls);
      return <rs, moduloValues(lv, rv)>;
    }
    case powExpr(PowExpr operand):
      return evalPow(operand, state);
  }
}

private tuple[EvalState, Value] evalPow(PowExpr expr, EvalState state) {
  switch (expr) {
    case powExpr(UnaryExpr base, PowExpr exponent): {
      <EvalState bs, Value bv> = evalUnary(base, state);
      <EvalState es, Value ev> = evalPow(exponent, bs);
      return <es, powerValues(bv, ev)>;
    }
    case unaryExpr(UnaryExpr operand):
      return evalUnary(operand, state);
  }
}

private tuple[EvalState, Value] evalUnary(UnaryExpr expr, EvalState state) {
  switch (expr) {
    case unaryNeg(UnaryExpr operand): {
      <EvalState next, Value value> = evalUnary(operand, state);
      return <next, negateValue(value)>;
    }
    case unaryPlus(UnaryExpr operand):
      return evalUnary(operand, state);
    case unaryNot(UnaryExpr operand): {
      <EvalState next, Value value> = evalUnary(operand, state);
      return <next, boolValue(!truthy(value))>;
    }
    case postfixExpr(PostfixExpr operand):
      return evalPostfix(operand, state);
  }
}

private tuple[EvalState, Value] evalPostfix(PostfixExpr expr, EvalState state) {
  switch (expr) {
    case callExpr(PostfixExpr callee, list[Expr] args): {
      <EvalState afterCallee, Value target> = evalPostfix(callee, state);
      <EvalState afterArgs, list[Value] evaluated> = evalExprList(args, afterCallee);
      return invoke(target, evaluated, afterArgs);
    }
    case memberExpr(PostfixExpr receiver, str field): {
      <EvalState next, Value value> = evalPostfix(receiver, state);
      return <next, accessMember(value, field)>;
    }
    case primaryExpr(PrimaryExpr primary):
      return evalPrimary(primary, state);
  }
}

private tuple[EvalState, list[Value]] evalExprList(list[Expr] exprs, EvalState state) {
  list[Value] values = [];
  EvalState current = state;
  for (Expr expr <- exprs) {
    <current, Value value> = evalExpr(expr, current);
    values += [value];
  }
  return <current, values>;
}

private tuple[EvalState, Value] evalPrimary(PrimaryExpr expr, EvalState state) {
  switch (expr) {
    case parenExpr(Expr inner):
      return evalExpr(inner, state);
    case dataCallExpr(str dataName, str opName, list[Expr] args): {
      <EvalState next, list[Value] values> = evalExprList(args, state);
      return invokeDataOperation(dataName, opName, values, next);
    }
    case structBuildExpr(str structName, list[FieldInit] fields):
      return instantiateStruct(structName, fields, state);
    case sequenceExpr(list[Expr] items): {
      <EvalState next, list[Value] values> = evalExprList(items, state);
      return <next, sequenceValue(values)>;
    }
    case tupleExpr(Expr first, Expr second): {
      <EvalState sf, Value fv> = evalExpr(first, state);
      <EvalState ss, Value sv> = evalExpr(second, sf);
      return <ss, tupleValue(fv, sv)>;
    }
    case structExpr(list[str] fields):
      return <state, structTypeValue(fields)>;
    case boolLiteral(str lit):
      return <state, boolValue(lit == "true")>;
    case intLiteral(str digits):
      return <state, intValue(toInt(digits))>;
    case floatLiteral(str digits):
      return <state, floatValue(toReal(digits))>;
    case charLiteral(str literal):
      return <state, charValue(decodeChar(literal))>;
    case stringLiteral(str literal):
      return <state, stringValue(decodeString(literal))>;
    case idExpr(str name):
      return <state, lookupValue(name, state)>;
  }
}

private tuple[EvalState, Value] invoke(Value callee, list[Value] args, EvalState state) {
  switch (callee) {
    case functionValue(FunValue fun):
      return callFunction(fun, args, state);
    case builtinValue(str name):
      return callBuiltin(name, args, state);
    default:
      fail("Attempted to call a non-function value");
  }
}

private tuple[EvalState, Value] callFunction(FunValue fun, list[Value] args, EvalState state) {
  if (size(args) != size(fun.params)) {
    fail("Function <fun.name> expected <size(fun.params)> arguments but received <size(args)>");
  }
  map[str, Value] frame = ();
  for (int idx <- zeroTo(size(fun.params))) {
    frame = frame[fun.params[idx] := args[idx]];
  }
  EvalState nested = evalState([frame] + state.scopes, state.env);
  <EvalState afterBody, Value result> = evalBlock(fun.body, nested, false);
  return <evalState(state.scopes, afterBody.env), result>;
}

private tuple[EvalState, Value] callBuiltin(str name, list[Value] args, EvalState state) {
  switch (name) {
    case "print": {
      for (Value v <- args) {
        println(showValue(v));
      }
      return <state, voidValue()>;
    }
    case "abs": {
      ensureArgs(name, args, 1);
      switch (args[0]) {
        case intValue(int n): return <state, intValue(abs(n))>;
        case floatValue(real n): return <state, floatValue(abs(n))>;
      }
      fail("abs expects a numeric argument");
    }
    case "arctan2": {
      ensureArgs(name, args, 2);
      real y = toRealNumber(args[0]);
      real x = toRealNumber(args[1]);
      return <state, floatValue(atan2(y, x))>;
    }
    case "len": {
      ensureArgs(name, args, 1);
      switch (args[0]) {
        case sequenceValue(list[Value] items): return <state, intValue(size(items))>;
        case stringValue(str s): return <state, intValue(size(s))>;
      }
      fail("len expects a sequence or string");
    }
  }
  fail("Unknown builtin <name>");
}

private void ensureArgs(str name, list[Value] args, int expected) {
  if (size(args) != expected) {
    fail("Builtin <name> expected <expected> arguments");
  }
}

private tuple[EvalState, Value] invokeDataOperation(str dataName, str opName, list[Value] args, EvalState state) {
  if (!(dataName in state.env.datas)) {
    fail("Unknown data abstraction <dataName>");
  }
  DataInfo info = state.env.datas[dataName];
  if (!(opName in info.operations)) {
    fail("Operation <opName> is not part of data abstraction <dataName>");
  }
  if (!(opName in state.env.funs)) {
    fail("Operation <opName> has no implementation");
  }
  return callFunction(state.env.funs[opName], args, state);
}

private tuple[EvalState, Value] instantiateStruct(str structName, list[FieldInit] fields, EvalState state) {
  Value descriptor = lookupValue(structName, state);
  switch (descriptor) {
    case structTypeValue(list[str] expected): {
      map[str, Value] result = ();
      EvalState current = state;
      for (FieldInit field <- fields) {
        switch (field) {
          case fieldInit(str name, Expr expr): {
            <current, Value value> = evalExpr(expr, current);
            result = result[name := value];
          }
        }
      }
      for (str field <- expected) {
        if (!(field in result)) {
          fail("Missing field <field> for struct <structName>");
        }
      }
      return <current, structValue(structName, result)>;
    }
    default:
      fail("<structName> is not a struct definition");
  }
}

private list[Value] asList(Value value) {
  switch (value) {
    case sequenceValue(list[Value] items): return items;
    case structValue(_, map[str, Value] fields): {
      list[str] keys = sort([field | field <- domain(fields)]);
      return [fields[key] | str key <- keys];
    }
    case tupleValue(Value first, Value second): return [first, second];
    case stringValue(str s): {
      list[Value] chars = [];
      for (str ch <- explodeChars(s)) {
        chars += [charValue(ch)];
      }
      return chars;
    }
  }
  fail("Value is not iterable");
}

private Value accessMember(Value target, str field) {
  switch (target) {
    case structValue(_, map[str, Value] fields):
      if (field in fields) return fields[field];
      break;
    case tupleValue(Value first, Value second):
      if (field == "first") return first;
      if (field == "second") return second;
      break;
  }
  fail("Field <field> not found");
}

private Value lookupValue(str name, EvalState state) {
  for (map[str, Value] scope <- state.scopes) {
    if (name in scope) {
      return scope[name];
    }
  }
  if (name in state.env.vars) {
    return state.env.vars[name];
  }
  fail("Unknown identifier <name>");
}

private EvalState pushScope(EvalState state) = evalState([()] + state.scopes, state.env);

private EvalState popScope(EvalState state) {
  if (isEmpty(state.scopes)) {
    return state;
  }
  return evalState(state.scopes[1..], state.env);
}

private tuple[EvalState, Value] evalBlock(Block block, EvalState state, bool scoped) {
  EvalState working = scoped ? pushScope(state) : state;
  Value last = voidValue();
  switch (block) {
    case block(list[Stmt] stmts):
      for (Stmt stmt <- stmts) {
        <working, last> = evalStmt(stmt, working);
      }
  }
  EvalState result = scoped ? popScope(working) : working;
  return <result, last>;
}

private bool truthy(Value value) {
  switch (value) {
    case boolValue(bool b): return b;
    case voidValue(): return false;
    default: return true;
  }
}

private bool valueEquals(Value left, Value right) {
  if (left == right) return true;
  switch (<left, right>) {
    case <intValue(a), intValue(b)>: return a == b;
    case <floatValue(a), floatValue(b)>: return a == b;
    case <boolValue(a), boolValue(b)>: return a == b;
    case <charValue(a), charValue(b)>: return a == b;
    case <stringValue(a), stringValue(b)>: return a == b;
    case <sequenceValue(list[Value] as), sequenceValue(list[Value] bs)>:
      if (size(as) != size(bs)) return false;
      for (int idx <- zeroTo(size(as))) {
        if (!valueEquals(as[idx], bs[idx])) {
          return false;
        }
      }
      return true;
    case <tupleValue(a1, a2), tupleValue(b1, b2)>:
      return valueEquals(a1, b1) && valueEquals(a2, b2);
    case <structValue(_, map[str, Value] af), structValue(_, map[str, Value] bf)>:
      return af == bf;
  }
  return false;
}

private int compare(Value left, Value right) {
  <real a, bool _> = toNumber(left);
  <real b, bool _> = toNumber(right);
  if (a < b) return -1;
  if (a > b) return 1;
  return 0;
}

private Value addValues(Value left, Value right) {
  switch (<left, right>) {
    case <stringValue(str a), stringValue(str b)>:
      return stringValue(a + b);
  }
  <real a, bool fa> = toNumber(left);
  <real b, bool fb> = toNumber(right);
  real res = a + b;
  return fa || fb ? floatValue(res) : intValue(toInt(res));
}

private Value subtractValues(Value left, Value right) {
  <real a, bool fa> = toNumber(left);
  <real b, bool fb> = toNumber(right);
  real res = a - b;
  return fa || fb ? floatValue(res) : intValue(toInt(res));
}

private Value multiplyValues(Value left, Value right) {
  <real a, bool fa> = toNumber(left);
  <real b, bool fb> = toNumber(right);
  real res = a * b;
  return fa || fb ? floatValue(res) : intValue(toInt(res));
}

private Value divideValues(Value left, Value right) {
  <real a, bool _> = toNumber(left);
  <real b, bool _> = toNumber(right);
  if (b == 0) fail("Division by zero");
  return floatValue(a / b);
}

private Value moduloValues(Value left, Value right) {
  int a = expectInt(left);
  int b = expectInt(right);
  if (b == 0) fail("Modulo by zero");
  return intValue(a % b);
}

private Value powerValues(Value left, Value right) {
  <real a, bool _> = toNumber(left);
  <real b, bool _> = toNumber(right);
  return floatValue(pow(a, b));
}

private Value negateValue(Value value) {
  switch (value) {
    case intValue(int n): return intValue(-n);
    case floatValue(real n): return floatValue(-n);
  }
  fail("Unary minus expects a numeric value");
}

private tuple[real, bool] toNumber(Value value) {
  switch (value) {
    case intValue(int n): return <n, false>;
    case floatValue(real n): return <n, true>;
  }
  fail("Value is not numeric");
}

private real toRealNumber(Value value) {
  <real v, bool _> = toNumber(value);
  return v;
}

private int expectInt(Value value) {
  switch (value) {
    case intValue(int n): return n;
  }
  fail("Expected an integer value");
}

private str decodeChar(str literal) {
  if (size(literal) <= 2) {
    return "";
  }
  str inner = substring(literal, 1, size(literal) - 1);
  return decodeEscapes(inner);
}

private str decodeString(str literal) {
  if (size(literal) <= 2) {
    return "";
  }
  str inner = substring(literal, 1, size(literal) - 1);
  return decodeEscapes(inner);
}

private str decodeEscapes(str raw) {
  str result = "";
  int idx = 0;
  while (idx < size(raw)) {
    str ch = substring(raw, idx, idx + 1);
    if (ch == "\\" && idx + 1 < size(raw)) {
      str next = substring(raw, idx + 1, idx + 2);
      switch (next) {
        case "n": result += "\n";
        case "t": result += "\t";
        case "r": result += "\r";
        case "\"": result += "\"";
        case "'": result += "'";
        case "\\": result += "\\";
        default: result += next;
      }
      idx += 2;
    }
    else {
      result += ch;
      idx += 1;
    }
  }
  return result;
}

private list[str] explodeChars(str text) {
  list[str] chars = [];
  int idx = 0;
  while (idx < size(text)) {
    chars += [substring(text, idx, idx + 1)];
    idx += 1;
  }
  return chars;
}

private list[int] zeroTo(int n) {
  list[int] values = [];
  int idx = 0;
  while (idx < n) {
    values += [idx];
    idx += 1;
  }
  return values;
}

private list[int] inclusiveRange(int start, int stop) {
  list[int] values = [];
  if (start <= stop) {
    int current = start;
    while (current <= stop) {
      values += [current];
      current += 1;
    }
  }
  else {
    int current = start;
    while (current >= stop) {
      values += [current];
      current -= 1;
    }
  }
  return values;
}

private str showValue(Value value) {
  switch (value) {
    case voidValue(): return "void";
    case intValue(int n): return "<n>";
    case floatValue(real n): return "<n>";
    case boolValue(bool b): return b ? "true" : "false";
    case charValue(str ch): return "'" + ch + "'";
    case stringValue(str s): return "\"" + s + "\"";
    case sequenceValue(list[Value] items): return "[" + join([showValue(v) | v <- items], ", ") + "]";
    case tupleValue(Value first, Value second): return "(" + showValue(first) + ", " + showValue(second) + ")";
    case structValue(str name, map[str, Value] fields): {
      list[str] keys = sort([f | f <- domain(fields)]);
      list[str] parts = ["<f> = <showValue(fields[f])>" | str f <- keys];
      return "<name>{" + join(parts, ", ") + "}";
    }
    case structTypeValue(list[str] fields):
      return "struct(" + join(fields, ", ") + ")";
    case functionValue(FunValue fun):
      return "<function <fun.name>>";
    case builtinValue(str name):
      return "<builtin <name>>";
  }
}

private value toRuntimeValue(Value value) {
  switch (value) {
    case intValue(int n): return n;
    case floatValue(real n): return n;
    case boolValue(bool b): return b;
    case charValue(str ch): return ch;
    case stringValue(str s): return s;
    case sequenceValue(list[Value] items): return [toRuntimeValue(v) | v <- items];
    case tupleValue(Value first, Value second): return <toRuntimeValue(first), toRuntimeValue(second)>;
    case structValue(str _, map[str, Value] fields): {
      map[str, value] result = ();
      for (str field <- domain(fields)) {
        result = result[field := toRuntimeValue(fields[field])];
      }
      return result;
    }
    case functionValue(FunValue fun): return "<function <fun.name>>";
    case builtinValue(str name): return "<builtin <name>>";
    default: return "void";
  }
}
