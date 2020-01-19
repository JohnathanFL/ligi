import streams

import tokens
import lexer
import nodes
import parser

when isMainModule:
  var file = openFileStream "grammar.zag"
  var blocky = file.newLexer.parse()
  blocky.prettyPrint()
  
