import streams

import strformat

import options
import tags
import tokens
import lexer
import nodes
import parser
import evaluator
import comptime_eval
import pretty
import json
import httpclient

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

  var client = newHttpClient()
  client.headers = newHttpHeaders {"Content-Type": "application/json"}

  var nextID: uint = 3000
  for n in blocky.children:
    if n of Bind:
      var ty = n.Bind.init.evalAsType()
      if ty.isSome: # We were able to eval the init expr as a type
        let id = nextID
        inc nextID
        var newType = ty.get
        newType.name = n.Bind.loc.BindSym.loc
        newType.pos = fmt"{n.Bind.pos.line}:{n.Bind.pos.col}"

        let json = %* {$id: newType}
        
        echo fmt"Found type {id}({newType.name}). About to send: "
        echo (json.pretty(2))

        let resp = client.putContent("http://localhost:8080/mktype", $json)
        
        
