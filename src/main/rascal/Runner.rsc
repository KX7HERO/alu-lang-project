module Runner

import IO;
import lang::alu::Interpreter;

public int main(list[str] args = []) {
  str path = "examples/simple.alu";
  str code = readFile(|file:///workspaces/alu-lang-project/examples/simple.alu|);
  println("Running via Runner: parsing and evaluating program...");
  int result = evalProgram(code);
  println("Result: <result>");
  return 0;
}
