import system, tables, sugar, strformat

import lazy

import lexing, parser, ast, pretty, parse_hooks, comptime_eval

proc evalPrint(self: var Atom, context: Context) =
  for i, child in self.children.mpairs:
    if i in 0..0: continue
    reduce child, context
    echo child
  self = VoidAtom

when isMainModule:
  stdout.write ">>"
  while not stdin.endOfFile:
    var input = stdin.readAll()
    var main = input.lex.parse
    echo "\nCode:"
    pretty main
    # echo dumpCache()
    reduce(main, comptimeEvalCtx)
    stdout.write ">>>"
    # echo main
    pretty main
    stdout.write ">>"
