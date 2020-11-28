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
    akWord, akTag, akList,
    akNative,

  NativeKind* = enum
    nkProc, nkInt, nkUInt, nkFloat, nkString, nkChar, nkVoid, nkSink
  Native* = object
    case kind*: NativeKind
      of nkProc:
        procedure*: (proc(self: var Atom, context: Context)) # Takes the atom and the atom's current context
      of nkInt:
        integer*: int
      of nkUInt:
        uinteger*: uint
      of nkFloat:
        floating*: float
      of nkString:
        str*: string
      of nkChar:
        ch*: char
      of nkVoid: discard
      of nkSink: discard
      else: discard
  Context* = ref object
    parent*: Context #?
    values*: Table[StrID, Atom]
  Atom* = object
    innerCtx*: Context #? # For doing field/method accesses.
    case kind*: AtomKind
      # Stuff that can exist directly in a text file to parse
      # (+native stuff for strings)
      of akWord, akTag:
        id*: StrID
      of akList: # wush that you jush shaid? I have a lishp?
        children*: seq[Atom]
      of akNative:
        native*: Native

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

template add*(a: Atom, items: untyped) =
  a.children.add items
template len*(a: Atom): untyped =
  if a.kind != akList: 1
  else: a.children.len
template `[]`*(a: Atom, i: untyped): var Atom = a.children[i]
proc `==`*(a: Atom, s: StrID): bool = a.kind == akWord and a.id == s
proc `==`*(s: StrID, a: Atom): bool = a == s
proc `==`*(l, r: Atom): bool =
  if not (l.kind == r.kind): return false
  if l.kind != akWord: raise newException(ValueError, "Currently only support == for words")
  return l.id == r.id

proc list*(args: varargs[Atom, toAtom]): Atom =
  result = Atom(
    kind: akList,
    children: @[]
  )
  result.add args
proc procAtom*(p: proc(self:var Atom, context:Context)): Atom = Atom(
  kind: akNative,
  native: Native(
    kind: nkProc,
    procedure: p
  )
)


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

  iIn: "in",
  iNotIn: "notin",

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
  iElIf: "elif",
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
  ibBlock: "@block", # a series of statements, with the last one yielding the value
  ibTuple: "@tuple", # a set of values
  ibArray: "@array", # a series of values
  # ibCall: "@()",       # No such thing. Lists are always function calls
  ibAt: "@at", # an indexing, with [1] being what to index and [2.._] being the arguments
  ibArm: "@arm", # a normal arm of a control statement. Exact meaning dependant upon which.
  ibElse: "@else", # the else arm of a statement
  ibFinally: "@finally", # the finally arm of a statement
  ibIf: "@if", # an if statement. Each arm is a sequential if/elif. Last two may be @else/@finally
  ibWhen: "@when", # a when statement.
  ibWhile: "@while", # a while loop
  ibFor: "@for", # a for loop
  ibLoop: "@loop", # a loop-de-loop
  ibBind: "@bind", # a bind statement. [1] is the spec, [2.._] are the expressions to bind
  #
  # ##################
  # # Ext statements #
  # ##################
  #
  ibFunc: "@func", # a result-location function or macro definition. [1] is either fn or macro
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
    of akNative: $self.native
    of akTag: fmt"#{self.id.lookup}"
    of akList: fmt"({self.children})"
    else:
      quit fmt"{self.kind} is TODO"

proc lookup*(c: Context, a: StrID): var Atom # Get the value of a word
proc reduce*(a: var Atom, context: Context) # Ensure an atom is in its minimal state (i.e a native or similar)
proc apply*(a: var Atom, context: Context) # Call a list. Assumes a[0] is pre-reduced

let VoidAtom* = Atom(kind: akNative, native: Native(kind: nkVoid))
proc lookup*(c: Context, a: StrID): var Atom =
  if c.values.contains a:
    return c.values.mgetOrPut(a, VoidAtom)
  elif c.parent != nil:
    return c.parent.lookup(a)
  else:
    raise newException(ValueError, fmt"Word {a}({a.lookup}) not found.")

# Currently somewhat redundant. May stay that way, in fact.
proc apply*(a: var Atom, context: Context) =
  if a[0].kind != akNative or a[0].native.kind != nkProc:
    raise newException(
      ValueError,
      fmt"Can only apply a [0] of akNative (currently)"
    )
  a[0].native.procedure(a, context)

proc reduce*(a: var Atom, context: Context) =
  case a.kind:
    of akNative: discard # Already reduced
    of akList:
      # Don't reduce the entire list at once.
      # This allows for short-circuit eval, if required.
      reduce(a[0], context)
      apply(a, context)
    of akWord:
      a = context.lookup(a.id)
    of akTag:
      quit "TODO: Tag reduction"
