import system
import json

import lexing, parser, ast, pretty, parse_hooks


when isMainModule:
  var exText = readFile "example.li"
  # let p = exText.lex.parse
  # echo pretty jsonifyAll p.parseStmtSeq
  let main = exText.lex.parse
  pretty main
