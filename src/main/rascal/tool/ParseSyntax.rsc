module tool::ParseSyntax

import lang::rascal::grammar::definition::Modules;

public void main(list[str] args) {
  loc target = |file:///workspaces/alu-lang-project/src/main/rascal/lang/alu/Syntax.rsc|;
  lang::rascal::grammar::definition::Modules::parseModule(target);
}
