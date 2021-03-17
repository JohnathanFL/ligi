import system, tables, sugar, strformat

import lazy

import lexing, parser, ast, pretty, parse_hooks, comptime_eval

when isMainModule:
  stdout.write ">>"
  while not stdin.endOfFile:
    var input = stdin.readAll()
    var main = input.lex.parse
    echo "\nCode:"
    pretty main
    echo ""
    # echo dumpCache()
    reduce(main, comptimeEvalCtx)
    echo ""
    stdout.write ">>>"
    # echo main
    pretty main
