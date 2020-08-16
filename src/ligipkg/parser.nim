import ast
import lexing
import lexast


import options
import strformat
import tables
import sets
import json



# Everything in Ligi is either blocking or non-blocking (indentation stuff).
# By convention, explicitly indicate the blockiness of every parser when i matters.

# Because of this, we also must be careful not to return from inside any functions, lest
# we skip preserveLevel/withBlocking/etc

# I try to keep a record of the syntax for each proc. Some conventions for those:
# - I mostly ignore whitespace in these. The comments are for the big picture
# - I use a mixture of EBNF, regexes, and custom syntax
# - Actual words refer to something else
# - Things in quotes refer to the literal text
# - {} is 0+
# - [] is 0-1
# - ?word predicates an optional [] or {} on word's value
# - ~word means find but do not match word


template err(msg: string) = quit fmt"{getStackTrace()}{self.pos.line}:{self.pos.col}: " & msg  # TODO

type Parser = ref object
  lexer: Lexer
  cur: Token
  pos: Pos
  blocking: bool
  curLevel: int # The col of the first token of this line
  ourLevel: int # The col of the first token of this sequence
  newlined: bool


# Commands for working with indentation
template newlined() : bool {.dirty.} = self.blocking and self.newlined
#template samelined(): bool {.dirty.} = not newlined 
template indented() : bool {.dirty.} = self.blocking and self.curLevel > self.ourLevel
template dedented() : bool {.dirty.} = self.blocking and self.curLevel < self.ourLevel
template moveline() {.dirty.} = self.newlined = false
# Move to the current line's indent
template movedent() {.dirty.} = self.ourLevel = self.curLevel
template movedentTo(col: int) = self.ourLevel = col

# We'll make commas optional inside tups/compounds, but we'll also let them

