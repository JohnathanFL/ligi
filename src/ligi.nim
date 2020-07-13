import system
import os

import ligipkg/[lexing]

when isMainModule:
  var exText = readFile "example.li"
  var i = 0
  iterator iter(): char {.closure.}=
    while i < exText.len:
      yield exText[i]
      inc i
  var lexer = Lexer(
    pos: (1, 0),
    data: iter
  )
  lexer.consume 3 # Kick the next buffer
  var cur = lexer.scan
  while cur.tok.tag != EOF:
    echo cur
    cur = lexer.scan
