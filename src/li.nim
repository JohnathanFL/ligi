import streams

import tags
import tokens
import lexer
import nodes
import parser
import evaluator
import pretty

when isMainModule:
  var file = openFileStream "example.ligi"
  var lex = file.newLexer()
  #var tok = lex.scan()
  #while tok.tag != Tag.EOF:
  #  echo $tok
  #  tok = lex.scan
  var blocky = lex.parse()
  blocky.prettyPrint()

