# ALU (Alpes Language Universe) – Rascal + TypePal + Codespaces Skeleton

This repository is a **starting point** for the three PLE projects over the Alpes Language Universe (ALU):

1. **Project 1** – Grammar (captured here in `lang::alu::Syntax`).
2. **Project 2** – Rascal implementation (syntax, parser and a minimal interpreter scaffold).
3. **Project 3** – Type system with **TypePal** (skeleton in `lang::alu::Checker`).

The goal is that you can open this project directly in **GitHub Codespaces** (or any VS Code Dev Container) and have:

- Java + Maven preinstalled.
- Rascal + TypePal available via Maven.
- The Rascal VS Code extension pre-installed for syntax highlighting, navigation and REPL.
- A place to implement the ALU interpreter and the full TypePal type checker.

> ⚠️ **Important:** the interpreter and type checker here are **only skeletons**.  
> They compile as Rascal modules, but you still need to complete:
> - Semantic evaluation of ALU programs in `lang::alu::Interpreter`.
> - All `collect` rules and constraints for the type system in `lang::alu::Checker`.

---

## 1. Dev Container / Codespaces

- Dev container configuration: `.devcontainer/devcontainer.json`
- Image: `mcr.microsoft.com/devcontainers/java:1-17-bullseye`
- Tools inside the container:
  - Java 17
  - Maven
  - VS Code Rascal extension (`usethesource.rascalmpl`)

When the container is created, this command is run once:

```bash
mvn -q -e dependency:go-offline || true
```

This warms up Maven's local cache (Rascal + TypePal) without blocking you if something transient fails.

If you want to run this manually later, you can use:

```bash
./scripts/bootstrap.sh
```

(from inside the container).

---

## 2. Rascal Project Layout

- `pom.xml` – Maven configuration based on the official Rascal template, extended with **TypePal**.
- `META-INF/RASCAL.MF` – points Rascal to `src/main/rascal` and requires the `typepal` library.
- `src/main/rascal/Main.rsc` – simple entry point that reads a `.alu` file and calls the interpreter.
- `src/main/rascal/lang/alu/CommonLex.rsc` – basic lexical definitions (integers, booleans, identifiers, keywords).
- `src/main/rascal/lang/alu/Syntax.rsc` – concrete syntax of ALU in Rascal (Project 1 → Project 2).
- `src/main/rascal/lang/alu/Interpreter.rsc` – **placeholder** interpreter (Project 2 scaffold).
- `src/main/rascal/lang/alu/Checker.rsc` – **placeholder** TypePal-based type system (Project 3 scaffold).
- `examples/simple.alu` – minimal ALU program for parser tests.
- `scripts/bootstrap.sh` – helper script to pre-download Maven deps.

You can import modules in the Rascal REPL like:

```rascal
import lang::alu::Syntax;
import lang::alu::Interpreter;
```

---

## 3. Grammar (Project 1 → `lang::alu::Syntax`)

The module `lang::alu::Syntax` includes:

- `Program` as a sequence of `Decl`.
- `DataDecl` for data abstractions (`name = data with op1, op2, ... end name`).
- `FunDecl` for functions (`name = function(params) do ... end name`).
- `VarDecl` with optional type annotations (`var x : Int = 1, y = 2;`).
- An expression language with:
  - identifiers, integer literals
  - arithmetic (`+`, `-`, `*`, `/`)
  - logical (`and`, `or`)
  - conditional expression (`if e1 then e2 else e3`)
  - function calls
  - sequence and tuple constructors
- A simple `Type` grammar with `Int`, `Bool`, `Sequence[T]`, `Tuple[T1,T2]` and user-defined data types (`Id`).

This is intentionally conservative but follows the spirit of the ALU spec and is designed to be a good starting point to refine/extend.

---

## 4. Minimal Interpreter Scaffold (Project 2)

`lang::alu::Interpreter` currently:

- Parses a program using `parse(#start[Program], code)`.
- Prints the resulting parse tree.
- As a toy behaviour, scans for the last `Integer` literal and returns its numeric value.

This is **not** the full semantics of ALU. To complete Project 2 you should:

1. Define an evaluation model (environments for variables, functions, and data abstractions).
2. Traverse the `Program` and `Decl` trees, building environments.
3. Implement evaluation for all `Expr` cases (arithmetic, logical, conditionals, calls, sequences, tuples, etc.).
4. Decide how to handle modules and data abstractions operationally.

The `Main` module is already wired to call `evalProgram(code)` so you can run ALU programs once the interpreter is in place.

---

## 5. TypePal Skeleton (Project 3)

`lang::alu::Checker` does the following:

- Extends `analysis::typepal::TypePal`.
- Introduces `AType` constructors for:
  - `intType()`
  - `boolType()`
  - `sequenceType(elem)`
  - `tupleType(fst, snd)`
  - `dataType(name)`
- Defines `prettyAType` for nicer error messages.
- Provides a helper `typeOfTree(Type t)` that maps the concrete `Type` syntax into `AType`.

You still need to:

1. Add `collect` rules for:
   - variable declarations and uses
   - function declarations and calls
   - expressions (arithmetic, logical, conditionals, sequences, tuples, etc.)
   - data abstractions and their operations
2. Use TypePal primitives (`define`, `use`, `fact`, `requireEqual`, `calculate`, etc.) to enforce:
   - Correct types for literals and expressions.
   - Consistency of user-provided type annotations.
   - Correct typing of data structures and their elements.
   - The extra rule that all elements used in a data structure must exist (per the project statement).

The structure closely follows the TypePal calculator example, so you can mirror its patterns here.

---

## 6. Quick test in the Rascal REPL

Inside the dev container / Codespaces terminal:

```bash
mvn -q -e compile
```

Then in the Rascal REPL (started from VS Code or the terminal):

```rascal
import lang::alu::Syntax;
import lang::alu::Interpreter;
str code = readFile("examples/simple.alu");
value result = evalProgram(code);
println(result);
```

You should see the parsed tree printed and the last integer literal returned as a value (with the current toy semantics).

---

## 7. Next steps

- Refine the grammar if TAs provide an official solution for Project 1.
- Replace the toy interpreter with the full ALU semantics.
- Complete the TypePal-based type checker in `Checker.rsc`.
- Add tests in Rascal (and optionally TypePal TTL tests) to validate the language.

This repository is meant to give you a **robust, Codespaces-friendly starting point** so that you can focus your energy on the language design and type system, not on all the plumbing.
