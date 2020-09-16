# Latest hare-brained scheme for Ligi's compiler

The latest idea is essentially:

- Lexer written in Zig as a C-compatible library
- Parser written modularly
  - It should be written such that you could conceivably swap things out on the fly while parsing
    - Use case: You could have an `externC:` macro that swaps the parser to something compatible with C
    - To support the above, it should be stack-based
  - Parser should generate a hash of the tree as it parses
- Semantic analysis written as a set of modules in much the same way as the parser
- Code gen written - you guessed it - as a set of modules

The hash from the Parser's stage will be cached with the set of loaded modules.
Since Ligi is parsed statement by statement, we could then cache the subsequent steps. You could then,
conceivably, have a program like
```
let x = 1
printf "Hello, world!\n"
do_some_io
```
where a change to a given line only recompiles that line, with the others being taken from cache 
(assuming `printf` and `do_some_io` don't depend on each other). In other words, `ccache` at the
statement (or tree) level instead of the file level. This is a very long-term goal, though.
It may just end up being another pipe dream.


The general end game is that the core (lexer and framework around the parser) should be simple enough that
you could implement it in a stripped-down form of Ligi, which would then compile other modules written
in Ligi that implement the rest of the language. Thus, bootstrapping a new program is as simple as
compiling that stripped-down form in, say, `C` and then having it compile its siblings.
