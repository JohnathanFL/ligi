# Eval(uation) Plans

- Eval-values should be trivially copyable (if at all possible)
- TypeInfo structures should be trivially copyable and immutable (always)
- Bindings should be passed by pointer and have interior mutability


- We will work on a 3-stack system:
  - Exec stack: Works like the normal stack. Holds temps
  - Bind stack: Holds bindings
  - Heap "stack": Used to allocate dynamic stuff, along with statics
    - Stuff can be allocated here at comptime and it will be available in the data segment at 
      runtime

- Pointers will store offsets/lens into the stacks. These offsets can be resolved once eval is 
  over to get real pointers.


- Ligi should be designed such that everything can be evaluated in order. In other words, you can
  fully typecheck any statement given the statements which preceded it.
  - "That which has passed tells you that which is to come" was one of the original tenets of Ligi,
    back when it was still Zag.
  - This means we won't ever have out-of-order types/funcs like Rust or Zig, but it makes things
    more predictable.
  - This lets us make evaluating a file work exactly like a REPL (and vice versa)
  - This *should* let us make optimizations for which line is changed in compilation
    - If I know that <=line 10 is unchanged, then I only have to recompile things after that.
  - This would allow us to e.g modify the lexer/parser as we go. Thus, we could add new operators
    and the like based on user code, or even directly plug a parser into the `foo {}` styled
    macros, rather than requiring a separate ArrayList to store them.

- I may try folding the `bool` type into an enum. We could then simplify logic by saying that
  any ADT enum's `#true` state can be captured. `true` and `false` would still be builtin
  synonyms for `#true` and `#false`, to reduce typing and flatten the learning curve.
