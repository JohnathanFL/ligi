import system
import os
import json
import macros

import compiler/[lexing, parser, ast, pretty]


when isMainModule:
  var exText = readFile "example.li"
  let o = exText.lex.parse
  for i in o:
    echo pretty jsonify i
  # var lexer = exText.lex
  # var (pos, tok)= lexer.scan()
  # while tok.tag != tEOF:
  #   echo tok
  #   (pos, tok) = lexer.scan()
