module Main

import IO;
import ParseTree;
import lang::alu::Syntax;
import lang::alu::Interpreter;
import lang::alu::Checker;

/**
 * Simple CLI-style entry point.
 * In Rascal's REPL you can also import Main and call evalProgram directly.
 */
public int main(list[str] args = []) {
  if (size(args) == 0) {
    println("Usage: alu <file.alu>");
    return 1;
  }

  str path = args[0];
  str code = readFile(path);
  checkProgram(code);
  value result = evalProgram(code);
  println("Result: <result>");
  return 0;
}
