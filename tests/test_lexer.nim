import unittest
import tables

import ligipkg/lexing
proc `=>`(s: string, t: Tag): bool =
  var lexer = s.lex
  if lexer.scan.tok.tag != t: return false
  if lexer.scan.tok.tag != tEOF: return false
  return true
proc testSet(s: set[Tag]) =
  for tag in s:
    check $tag => tag

test "empty file":
  check "" => tEOF

# All verifiable tags have their string associated with them in source
test "keyword recognition":
  testSet KeywordSet
# DOES check to make sure that subset tags (like `..` vs `..=`) are treated properly
# (the "last is tEOF" check)
test "sigil recognition":
  testSet SigilSet





# TODO: Add more random words
const words = @[
  "hey", "@hey", "hey@",
  "_hey", "hey_",
  "1hey", "hey1",
  "1", "_", "@"
]
test "word recognition":
  # TODO: Can we make this test any better?
  for word in words:
    check word => tWord

# Does lexing a word spit back that same word in .str?
test "word spitback":
  for word in words:
    var lexer = word.lex
    check lexer.scan.tok.str == word

# TODO: More random strings to test
let strings = @[
  """
  "Hey there!"
  """,
]
test "quoted strings":
  for str in strings:
    var lexer = str.lex
    var tok = lexer.scan.tok
    check tok.tag != tEOF
    check tok.str == "Hey there!"

let lineStrings = toTable[string, string] {
  # Remember that the lexer doesn't actually interpret `\` sequences
  """
  \\ Test
  """
  : "Test",
  """
  \\ line1
  \\ line2
  """
  : "line1\\nline2",
  """
  \\ line1
  \\ line2
  \\ 
  """ # note the space after the final `\\`
  : "line1\\nline2\\n",
  """
  \\ line1
  \\ line2
  \\
  """ # note the lack of a space after the final `\\`
  : "line1\\nline2\\n"
}
test "Line strings":
  for str, expected in lineStrings:
    var lexer = str.lex
    var tok = lexer.scan.tok
    check tok.tag != tEOF
    check lexer.scan.tok.tag == tEOF
    check tok.str == expected

