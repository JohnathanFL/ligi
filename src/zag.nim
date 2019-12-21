import streams

import lexer
import node

when isMainModule:
  var file = openFileStream "grammar.zag"
  var lex = file.newLexer()
  var cur = lex.scan()
  while cur.what.tag != Tag.EOF:
    echo $cur
    cur = lex.scan()
