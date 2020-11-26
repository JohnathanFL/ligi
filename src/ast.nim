import macros
import tables
import strformat
import sequtils

import lexing

type
  ID* = uint64
  FnID* = ID
  TypeID* = ID
  StaticID* = ID
  StrID* = ID
  StringCache* = Table[string, StrID]
  StringLookup* = Table[StrID, string]
  CompoundKind* = enum ckArray, ckTup, ckBlock
  AtomKind* = enum
    akWord, akStr, akChar, akTag, akList,
    # Posteval:
    akFn, akType, akStatic,
    akVal

  Atom* = object
    case kind*: AtomKind
      # Stuff that can exist directly in a text file to parse
      of akWord, akTag:
        id*: StrID
      of akStr:
        str*: string
      of akChar:
        chr*: char
      of akList: # wush that you jush shaid? I have a lishp?
        children*: seq[Atom]

      # Evaluated stuff
      # I may eventually replace these with fully qualified words.
      # e.g: A function "add" could be the single word %"root.foo.add"
      of akFn:
        fnID*: FnID
      of akType:
        typeID*: TypeID
      of akStatic:
        staticID*: StaticID
      of akVal: discard # TODO

var nextID: StrID = 0.StrID
const INITIAL_CACHE_SIZE = 1024 # Because programs gots lots of idents
var stringCache: StringCache = initTable[string, StrID](INITIAL_CACHE_SIZE)
var stringLookup: StringLookup = initTable[StrID, string](INITIAL_CACHE_SIZE)


proc toID*(s: string): StrID =
  if stringCache.contains s:
    return stringCache[s]
  else:
    inc nextID
    stringCache[s] = nextID
    stringLookup[nextID] = s
    return nextID

proc lookup*(s: StrID): string =
  return stringLookup[s]

macro makeTags*(body: untyped): untyped =
  expectKind body, nnkTableConstr
  result = newStmtList()

  for node in body.children:
    expectKind node, nnkExprColonExpr
    result.add newLetStmt(postfix(node[0], "*"), newCall("toID", node[1]))

# Ast helpers

proc toAtom*(word: StrID): Atom = Atom(
  kind: akWord,
  id: word,
)
proc toAtom*(atom: Atom): Atom = atom

proc list*(cmd: StrID, items: varargs[Atom, toAtom]): Atom =
  result = Atom( kind: akList, children: @[ Atom(kind: akWord, id: cmd) ] )
  for item in items:
    result.children.add item
proc list*(cmd: StrID, items: seq[Atom]): Atom =
  result = Atom( kind: akList, children: @[ Atom(kind: akWord, id: cmd) ] )
  for item in items:
    result.children.add item
proc list*(cmd: Atom, items: seq[Atom]): Atom =
  result = Atom( kind: akList, children: @[ cmd ] )
  for item in items:
    result.children.add item
proc list*(cmd: Atom, items: varargs[Atom, toAtom]): Atom =
  result = Atom( kind: akList, children: @[ cmd ] )
  for item in items:
    result.children.add item

proc add*(a: var Atom, items: varargs[Atom, toAtom]) = 
  a.children.add items

makeTags {
  iColon: ":",
  iComma: ",",
  iLBrace: "{",
  iRBrace: "}",
  iLParen: "(",
  iRParen: ")",
  iLBracket: "[",
  iRBracket: "]",
  iStoreIn: "->",

  iAssg: "=",
  iAddAssg: "+=",
  iSubAssg: "-=",
  iMulAssg: "*=",
  iDivAssg: "/=",

  iLambda: "=>",
  
  iSpaceship: "<=>",

  iAnd: "and",
  iOr: "or",
  iXor: "xor",

  iEq: "==",
  iNeq: "!=",
  iLt: "<",
  iGt: ">",
  iLtEq: "<=",
  iGtEq: ">=",

  iAdd: "+",
  iSub: "-",
  iMul: "*",
  iPtr: "*", # One benefit of the StrID setup is it's wonderfully easy to alias keywords/sigils
  iDiv: "/",
  iMod: "mod",
  iExpand: "...",

  iAccess: ".",
  iAccessPipe: ".>",
  iOptAccess: ".?",
  iOptAccessPipe: ".?>",

  iFn: "fn",
  iMacro: "macro",

  iLet: "let",
  iVar: "var",
  iCVar: "cvar",
  iField: "field",
  iCase: "case",

  iIf: "if",
  iWhen: "when",
  iWhile: "while",
  iLoop: "loop",
  iFor: "for",

  iElse: "else",
  iFinally: "finally",
  iIs: "is",

  iAssert: "assert",
  iExpect: "expect",
  iBreak: "break",
  iReturn: "return",
  iDelete: "delete",
  iContinue: "continue",

  iSink: "_",


  # Purely AST ids - ([i]d of [b]uiltin)
  # Because these are just words, this also means Ligi can be homoiconic
  #
  # ###################
  # # Core statements #
  # ###################
  #
  # For these three, children[1] is always the typedesc. If no desc was present, it's `_`
  ibBlock: "@block",     # a series of statements, with the last one yielding the value
  ibTuple: "@tuple",     # a set of values
  ibArray: "@array",     # a series of values
  # ibCall: "@()",       # No such thing. Lists are always function calls
  ibAt: "@at",           # an indexing, with [1] being what to index and [2.._] being the arguments
  ibArm: "@arm",         # a normal arm of a control statement. Exact meaning dependant upon which.
  ibElse: "@else",       # the else arm of a statement
  ibFinally: "@finally", # the finally arm of a statement
  ibIf: "@if",           # an if statement. Each arm is a sequential if/elif. Last two may be @else/@finally
  ibWhen: "@when",       # a when statement.
  ibWhile: "@while",     # a while loop
  ibFor: "@for",         # a for loop
  ibLoop: "@loop",       # a loop-de-loop
  ibBind: "@bind",       # a bind statement. [1] is the spec, [2.._] are the expressions to bind
  #
  # ##################
  # # Ext statements #
  # ##################
  #
  ibFunc: "@func",       # a result-location function or macro definition. [1] is either fn or macro
}

proc `$`*(self: seq[Atom]): string
proc `$`*(self: Atom): string

proc `$`*(self: seq[Atom]): string =
  # result = "["
  for (n, el) in self.pairs:
    if n == 0:
      result &= $el
    else:
      result &= " " & $el

  # result &= "]"
proc `$`*(self: Atom): string =
  case self.kind:
    of akWord: fmt"{self.id.lookup}"
    of akStr: fmt""""{self.str}""""
    of akTag: fmt"#{self.id.lookup}"
    of akList: fmt"({self.children})"
    else:
      quit fmt"{self.kind} is TODO"
