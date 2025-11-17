module lang::alu::Checker

import ParseTree;
import lang::alu::Syntax;

/**
 * Minimal checker scaffold that simply ensures that every data abstraction
 * is declared at most once. It keeps the Maven build green until the
 * TypePal-based solution is implemented.
 */
public void checkProgram(str code) {
  Program program = parse(#start[Program], code);
  ensureUniqueDataDecls(program);
}

private void ensureUniqueDataDecls(Program program) {
  set[str] seen = {};
  for (Decl decl <- program.decls) {
    switch (decl) {
      case (Decl)`<DataDecl dd>`: {
        str name = dataName(dd);
        if (name in seen) {
          throw IllegalArgument("Duplicate data declaration <name>");
        }
        seen += {name};
      }
      default:
        continue;
    }
  }
}

private str dataName(DataDecl decl) {
  switch (decl) {
    case (DataDecl)`<Id name> = data with <OpList _> end <Id _>`:
      return "<name>";
    case (DataDecl)`<Id name> = data with <OpList _> end`:
      return "<name>";
  }
  return "";
}
