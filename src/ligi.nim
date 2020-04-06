import streams

import tags
import tokens
import lexer
import nodes
import parser
import evaluator
import comptime_eval
import pretty

when isMainModule:
  var file = openFileStream "example.li"
  var lex = file.newLexer()
  #var tok = lex.scan()
  #while tok.tag != Tag.EOF:
  #  echo $tok
  #  tok = lex.scan
  var blocky = lex.parse()
  blocky.prettyPrint()
  echo "\n"
  for n in blocky.children:
    if n of Bind:
      # TODO: Now send this to Ligujo
      echo $n.Bind.loc.BindSym.ty.evalAsType

