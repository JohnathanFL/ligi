import macros
import tables

import lexing

type
  StringID* = uint64
  StringCache* = Table[string, StringID]
  StringLookup* = Table[StringID, string]
  AtomKind* = enum
    akWord, akList, akStr, akChar, akLabel
  Atom* = object
    # pos*: Pos
    case kind*: AtomKind
      of akWord, akLabel: # Includes operators
        word*: StringID
      of akStr:
        str*: string # We'll only keep words/labels in the string cache
      of akChar:
        chr*: char
      of akList:
        list*: seq[Atom]


var nextID: StringID = 0.StringID
const INITIAL_CACHE_SIZE = 1024 # Because programs gots lots of idents
var stringCache: StringCache = initTable[string, StringID](INITIAL_CACHE_SIZE)
var stringLookup: StringLookup = initTable[StringID, string](INITIAL_CACHE_SIZE)


proc toID*(s: string): StringID =
  if stringCache.contains s:
    return stringCache[s]
  else:
    inc nextID
    stringCache[s] = nextID
    stringLookup[nextID] = s
    return nextID

proc lookup*(s: StringID): string =
  return stringLookup[s]

macro makeTags*(body: untyped): untyped =
  expectKind body, nnkTableConstr
  result = newStmtList()

  for node in body.children:
    expectKind node, nnkExprColonExpr
    result.add newLetStmt(postfix(node[0], "*"), newCall("toID", node[1]))

makeTags {
  iSpaceship: "<=>",

  iAnd: "and",
  iOr: "or",

  iEq: "==",
  iNeq: "!=",
  iLt: "<",
  iGt: ">",
  iLtEq: "<=",
  iGtEq: ">=",

  iAdd: "+",
  iSub: "-",
  iMul: "*",
  iPtr: "*",
  iDiv: "/",
  iMod: "mod",

  iFn: "fn",
  iMacro: "macro",

  iLet: "let",
  iVar: "var",
  iCVar: "cvar",
  iField: "field",
  iEnum: "enum",
}
let MaxBuiltin* = nextID - 1

let OpPrecs = @[
  @[iSpaceship],
  @[iOr],
  @[iAnd],
  @[iEq, iNeq],
  @[iLt, iGt, iLtEq, iGtEq],
  @[iAdd, iSub],
  @[iMul, iDiv, iMod],

]
proc opPrec*(id: StringID): int =
  result = -1
  # First handle the known cases, then deduce(TODO)
  for i, precs in OpPrecs:
    for op in precs:
      if id == op:
        return i
