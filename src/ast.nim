import macros
import tables
import strformat
import sequtils

import types

import lexing

type
  ID* = uint64
  FnID* = ID
  StaticID* = ID
  StrID* = ID
  TypeID* = ID
  StringCache* = Table[string, StrID]
  StringLookup* = Table[StrID, string]
  CompoundKind* = enum ckArray, ckTup, ckBlock
  AtomKind* = enum
    akWord, akTag, akList,
    # Native kinds
    nkProc, nkInt, nkFloat, nkDecimal, nkString, nkChar, nkVoid, nkSink,
    nkObj, nkPattern, nkType, nkBool, nkFn, nkRef,
  Context* = ref object
    parent*: Context #?
    values*: Table[StrID, Atom]
    items*: seq[Atom] # For tuples and arrays
  Atom* = object
    # innerCtx.values contains fields, innerCtx.parent points to the type's Ctx for statics (methods)
    innerCtx*: Context #?
    case kind*: AtomKind
      # Stuff that can exist directly in a text file to parse
      # (+native stuff for strings)
      of akWord, akTag:
        id*: StrID
      of akList: # wush that you jush shaid? I have a lishp?
        children*: seq[Atom]
      # Native stuff
      of nkProc: # A nim proc
        procedure*: EvalProc # Takes the atom and the atom's current context
      of nkInt:
        integer*: uint
      of nkFloat:
        floating*: float
      of nkString:
        str*: string
      of nkChar:
        ch*: char
      of nkBool:
        boolean*: bool
      of nkVoid: discard
      of nkSink: discard
  EvalProc* = proc(self:var Atom, context:Context)

var nextID: StrID = 0.StrID
const INITIAL_CACHE_SIZE = 1024 # Because programs gots lots of idents
var stringCache: StringCache = initTable[string, StrID](INITIAL_CACHE_SIZE)
var stringLookup: StringLookup = initTable[StrID, string](INITIAL_CACHE_SIZE)

proc dumpCache*(): string = $stringLookup
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

proc add*[T](a: var Atom, items: T) =
  a.children.add items
proc len*(a: Atom): Natural =
  if a.kind != akList: 1
  else: a.children.len
proc `[]`*(a: var Atom, i: Natural): var Atom = a.children[i]
proc `[]=`*(a: var Atom, i: Natural, new: Atom) = a.children[i] = new
proc `[]`*(a: Atom, i: Natural): Atom = a.children[i]
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

proc procAtom*(p: EvalProc): Atom = Atom(
  kind: nkProc,
  procedure: p
)

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
    of akTag: fmt"#{self.id.lookup}"
    of akList: fmt"({self.children})"
    of nkString: '"' & self.str & '"'
    of nkChar: fmt"'{self.ch}'"
    else: "!@#"

proc lookup*(c: Context, a: StrID): var Atom # Get the value of a word
proc reduce*(a: var Atom, context: Context) # Ensure an atom is in its minimal state (i.e a native or similar)
proc apply*(a: var Atom, context: Context) # Call a list. Assumes a[0] is pre-reduced

let VoidAtom* = Atom(kind: nkVoid)
let SinkAtom* = Atom(kind: nkSink)

proc lookup*(c: Context, a: StrID): var Atom =
  if c.values.contains a:
    return c.values.mgetOrPut(a, VoidAtom)
  elif c.parent != nil:
    return c.parent.lookup(a)
  else:
    raise newException(ValueError, fmt"{a.lookup} not found in current context")
proc bindName*(c: Context, name: StrID, ty: Atom): var Atom =
  if c.values.contains name:
    raise newException(ValueError, fmt"Name {name.lookup} already exists in the current context!")
  # TODO: Putting types in the AST
  c.values[name] = SinkAtom
  return c.values[name]

# Currently somewhat redundant. May stay that way, in fact.
proc apply*(a: var Atom, context: Context) =
  case a[0].kind:
    of nkProc:
      a[0].procedure(a, context)
    else:
      raise newException(
        ValueError,
        fmt"Can currently only apply a [0] of akNative. Found {a}"
      )

proc reduce*(a: var Atom, context: Context) =
  case a.kind:
    of akList:
      # Don't reduce the entire list at once, only enough to know what to call.
      # This allows for short-circuit eval in the proc, if required.
      reduce(a[0], context)
      apply(a, context)
    of akWord:
      a = context.lookup(a.id)
    of akTag:
      quit "TODO: Tag reduction"
    else: discard # A native. already reduced

# We'll have the most common modifiers be baked into the TypeID itself.
# These will only be modifiers that can only logically be applied once.
# Thus, `const const T` is the same as `const T`, but `* * T` and `slice slice T` are not.
#
# This also means that the evaluators can simply do a `t.isRuntime = true` when typechecking,
# and don't need to do any sort of lookups in the type tables.
#
# For determining which way to interpret them (bit 63 is const vs runtime), use whatever
# would make sense for the most default type of `void` (0) (`const comptime void`, basically.)
func bit(n: int): ID = 1.ID shl n.ID
func bits(ns: varargs[int]): ID =
  result = 0
  for n in ns:
    result = result or (bit n)
const
  RuntimeMask*: ID = bit 63
  MutableMask* = bit 62
  RefMask* = bit 61
  # A 3 bit number: (4:decimal, 3:float, 2:int, 1:uint, 0:not a number)
  # They are purposefully ranked in order of ascending complexity (none, unsigned, 2's comp, etc)
  # This makes the ID get interpreted as the number of bits if not 0.
  # Decimal numbers are a future TODO/reserved bit
  NumMask* = bits(60, 59, 58)
  FlagShift* = 48 # shl required to turn low 16 bits into the high flag mask
  FlagMask* = uint16.high.ID shl FlagShift # Reserving the top 16 bits for common modifiers
  IDMask* = not FlagMask

func checkMask*(t: TypeID, n: ID | int | uint): bool = (t and n.ID) != 0
func isRuntime*(t: TypeID): bool = t.checkMask RuntimeMask
func `isRuntime=`*(t: var TypeID, itIs: bool) =
  if itIs:
    t = t and RuntimeMask.ID
  else:
    t = t and not RuntimeMask.ID

func isComptime*(t: TypeID): bool = not t.isRuntime
func `isComptime=`*(t: var TypeID, itIs: bool): bool =
  t.isRuntime = not itIs


func isMutable*(t: TypeID): bool = t.checkMask MutableMask
func `isMutable=`*(t: var TypeID, itIs: bool) =
  if itIs:
    t = t and MutableMask.ID
  else:
    t = t and not MutableMask.ID

func isConst*(t: TypeID): bool = not t.isMutable
func `isConst=`*(t: var TypeID, itIs: bool): bool =
  t.isMutable = not itIs


func isNum*(t: TypeID): bool = t.checkMask NumMask


func flags*(t: TypeID): ID = t and FlagMask

func id*(t: TypeID): TypeID = t and IDMask
func `id=`*(t: var TypeID, id: TypeID) =
  t = (t and FlagMask) or (id and IDMask)


type
  Field* = object
    name*: string
    typeOf*: TypeID
    offset*: uint64 # Offset from the beginning of the parent object in bits

  Type* = object
    id*: TypeID
    size*: uint64 # In bits
    fields*: seq[Field]
    statics*: Context
var TypeCache*: Table[TypeID, Type] = initTable[TypeID, Type](0)
