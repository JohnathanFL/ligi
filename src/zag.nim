import streams

import tokens
import lexer
import nodes
import parser

when isMainModule:
  var file = openFileStream "grammar.zag"
  var lex = file.newLexer()
  var cur = lex.scan()
  while cur.what.tag != Tag.EOF:
    echo $cur
    cur = lex.scan()
