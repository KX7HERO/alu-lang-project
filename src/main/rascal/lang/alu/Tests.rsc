module lang::alu::Tests

import lang::alu::Syntax;
import lang::alu::Checker;
import ParseTree;

// Helper to parse expression
Expr parseExpr(str code) = parse(#Expr, code);
Program parseProg(str code) = parse(#Program, code);

// Parser Tests
test bool testIntLiteral() = parseExpr("123") is intLiteral;
test bool testStringLiteral() = parseExpr("\"hello\"") is stringLiteral;
test bool testStructExpr() = parseExpr("struct(x, y)") is structExpr;
test bool testMemberExpr() = parseExpr("obj.field") is memberExpr;

// Type Checker Tests (Mocking behavior by checking if it throws or not)
// Note: Real type checking tests would inspect the TModel, but here we just check for exceptions.

bool checks(str code) {
  try {
    checkProgram(code);
    return true;
  } catch: {
    return false;
  }
}

test bool testValidVarDecl() = checks("var x: Int = 5;");
test bool testInvalidVarDecl() = !checks("var x: Int = \"string\";");

test bool testValidFunction() = checks("f = function(x: Int) do x + 1 end");
test bool testDuplicateParam() = !checks("f = function(x, x) do x end");

test bool testValidStruct() = checks("Point = data with create end; Point = struct(x, y); var p = Point$(x:1, y:2);");
test bool testMissingField() = !checks("Point = data with create end; Point = struct(x, y); var p = Point$(x:1);");

test bool testTupleAccess() = checks("var t = tuple(1, 2); var x = t.first;");
test bool testInvalidTupleAccess() = !checks("var t = tuple(1, 2); var x = t.third;");

test bool testGlobalUniqueness() = !checks("var x = 1; x = function() do end");
