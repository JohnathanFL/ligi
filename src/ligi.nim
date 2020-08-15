import system
import json

import ligipkg/[lexing, parser, ast, pretty]


when isMainModule:
  var exText = readFile "example.li"
  let p = exText.lex.parse
  echo pretty jsonifyAll p.parseStmtSeq
  # var lexer = exText.lex
  # var (pos, tok)= lexer.scan()
  # while tok.tag != tEOF:
  #   echo tok
  #   (pos, tok) = lexer.scan()
