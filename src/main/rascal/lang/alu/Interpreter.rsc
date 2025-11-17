module lang::alu::Interpreter

import IO;
import ParseTree;
import lang::alu::Syntax;

/**
 * Toy interpreter used as a scaffold for Project 2.
 * It simply parses the program, prints the parse tree and returns
 * the integer value of the last literal found (or 0 if none exist).
 */
public int evalProgram(str code) {
  Tree parseTree = parse(#start[Program], code).top;
  println("Parsed ALU program:");
  println(parseTree);

  return findLastInteger(parseTree);
}

private int findLastInteger(Tree tree) {
  bool seen = false;
  int last = 0;

  visit(tree) {
    case (Integer)`<Integer n>`: {
      last = toInt("<n>");
      seen = true;
    }
  }

  return seen ? last : 0;
}
