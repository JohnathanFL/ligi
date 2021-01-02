# Plano por Ligi

- Parser based around table redirects to allow extensibility
- S-Expression styled AST
  - Even the simple operations like evaluating a block just call a function/word (`@block`)
  - Atoms can contain "native" things like Nim procs or other data types
- Evaluator
  - Tracks "contexts" for each scope of variables
    - Each Atom can have its own context, inside of which field/method accesses are resolved.
      - This may be replaced eventually
  - Evaluation is composed of 
    - "reductions" (ensuring things are in their simplest state)
    - "applications" (evaluating functions).
      - Arguments aren't evaluated before being passed to the controlling function, allowing
        for short-circuit eval
- Compiler
  - Is split into "modes"
    - The difference between "modes" is entirely down to what's bound in the root context.
      - e.g the evaluator's context's `@block` implementation only evaluates `comptime` and leaves
        other things unchanged other than annotating with types.
    - Interpreter mode: Straight tree-walking interpeter to evaluate the entire program
    - Evaluator mode: Interpreter that evaluates comptime things
    - Compiler mode: Calls the evaluator, then walks the tree to generate code
  - Optimizations will be implemented as either plugins to the evaluator or separate modes


The basic workflow for Ligi is then:

- Write code
  - Ligujo works in concert with the editor via the LSP to give information on types/etc.
- Compile code
  - Invokes the evaluator
    - Resolves types, performs optimizations
    - Stores information about types and such into Ligujo
    - AST is left in a state suitable for codegen
    - Store this new AST into a more condensed on-disk cache to speed things up
  - Invokes the compiler proper to generate code
    - If needed, the compiler makes a pass through to change any tag values the user wishes to hardcode.
- Either run the output directly or use the REPL with the interpreter to test things.
