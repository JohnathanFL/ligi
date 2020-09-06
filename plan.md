# Latest hare-brained scheme for Ligi's compiler

The latest idea is essentially:

- Lexer written in Zig as a C library
- Parser and all the rest written in LuaJIT
  - Converting things to Zig as needed for performance once things settle. The Lua is for
    speed of development, so I don't have to think about things quite as much until I've settled
    on semantics.
