import unittest
import json

import ligipkg/[lexing, parser, ast, pretty]


test "atoms":
  let node = "hello".lex.parse.parseAtom
  check node.Word.word == "hello"
  # TODO: More of these AND more automation

