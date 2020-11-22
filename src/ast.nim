import macros
import tables
import strformat
import sequtils

import lexing

type
  ID* = uint64
  StrID* = ID
  StringCache* = Table[string, StrID]
  StringLookup* = Table[StrID, string]
  CompoundKind* = enum ckArray, ckTup, ckBlock
  AtomKind* = enum
    akWord, akStr, akTag, akCompound, akCmd,
    akUnit, akSink, akNone,
    # Posteval:
    akFn, akType, akStatic,
    akVal

  FnID* = ID
  TypeID* = ID
  StaticID* = ID
  Arm* = object
    kind*: StrID
    expr*: AtomRef #?
    body*: AtomRef #?
  AtomRef* = ref Atom
  Atom* = object
    case kind*: AtomKind
      of akWord, akTag:
        id*: StrID
      of akStr:
        str*: string
      of akCompound:
        compoundKind*: CompoundKind
        typeSpec*: AtomRef #?
        children*: seq[Atom]
      of akCmd:
        # Cmd is *never* a variable
        # It's only keywords/sigils.
        cmd*: StrID
        args*: seq[Atom]
      of akUnit, akSink, akNone: discard
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

  iCall: "()",
  iIndex: "[]",
}

proc `$`*(self: AtomRef): string
proc `$`*(self: seq[Atom]): string
proc `$`*(self: Atom): string

proc `$`*(self: AtomRef): string =
  if self.isNil:
    return "nil"
  else:
    return $(self[])
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
    of akWord: fmt"[Word {self.id.lookup}]"
    of akStr: fmt"[Str `{self.str}`]"
    of akTag: fmt"[Tag #{self.id.lookup}]"
    of akCompound: fmt"[{self.compoundKind} :{self.typeSpec}: {self.children}]"
    of akCmd: fmt"[do `{self.cmd.lookup}` {self.args}]"
    of akUnit: fmt"Unit"
    of akSink: fmt"Sink"
    of akNone: fmt"None"
    else:
      quit fmt"{self.kind} is TODO"
