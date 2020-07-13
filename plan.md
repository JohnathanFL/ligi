# New Plan for Parsing and Evaluation

- Stricter AST
  - Store binds/defers in dedicated lists in each block
- Stack-based evaluation
  - Massive, global arraylist. Allocate space for variables just as you would for
    a normal stack in ASM
- libtcc for code generation
  - For debug builds, we'll rely on TCC. For release, we'll output the full C source
    and pass it through a normal compiler (of the user's choosing?)
- 2 stage compiler:
  - Evaluator: Evaluates all compiletime things and leaves a runtime-ready AST in its wake
  - Translator: Translates a purely runtime AST into C
