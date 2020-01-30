import streams

import tokens
import lexer
import nodes
import parser
import evaluator

when isMainModule:
  var file = openFileStream "example.zag"
  var lex = file.newLexer()
  var tok = lex.scan()
  #while tok.tag != Tag.EOF:
  #  echo $tok
  #  tok = lex.scan
  var blocky = lex.parse()
  #blocky.prettyPrint()
  
