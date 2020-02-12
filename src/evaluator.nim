import tables
import sets

import cmds
import nodes

type FilePos = tuple[line: uint, col: uint]

# Well ain't this ironic. I set out to make a language without standard OOP stuff, only to end up
# using OOP at every turn in its compiler.
type
  MemSlice = ref seq[Val]
  ValType = enum
    # Can be applied to any type. Simply means it cannot be read from yet
    NoVal,
    # For optionals
    NullVal
    TypeVal,
    IntVal, BoolVal, StringVal
    SliceVal, FuncVal, ArrayVal
    RangeVal

    # Holding an actual instance of a struct
    StructVal
    # Holding an actual instance of an enum
    EnumVal
    TupleVal

  # Doing it like this allows both passing refs around arbitrarily *and*
  # mutating the inner value without convoluted stuff.
  Val = ref object
    ty: Type
    case kind: ValType
      of NoVal, NullVal: discard
      of TypeVal:
        inner: AnyType
      of IntVal:
        integer: int
      of StringVal:
        str: string
      of BoolVal:
        boolean: bool
      of SliceVal:
        start: uint
        stop: uint
        mem: MemSlice
      of ArrayVal:
        backing: MemSlice
      of RangeVal:
        # Both inclusive here
        min: int
        max: int
      of StructVal:
        fields: Table[string, Val]
      of EnumVal:
        tag: string
        contains: Val
      of TupleVal:
        children: seq[Val]
      of FuncVal:
        # Map 1-1 with the argTypes in FuncType
        argNames: seq[string]
        retName: string
        body: Expr


  # Also used for 'undef' type
  # Can be instantiated, but just means that type has yet to be set
  AnyType = ref object of RootObj
  # Writes to it do nothing
  # Reading from it is an error
  # All things return to the void, but none return from the void
  VoidType = ref object of AnyType
  # Outside
  TypeType = ref object of AnyType
  # Any const/comptime mods applied to a TupleType will just go through to its members
  TupleType = ref object of AnyType
    children: seq[AnyType]
  # Any type that was defined by the user with struct or enum (i.e holds records)
  # Should never be instantiated.
  RecordType = ref object of Type
    name: string
    statics: Table[string, tuple[ty: Type, val: Val]]
    props: Table[string, tuple[get: Val, set: Val]]
  StructType = ref object of AnyType
    # Order will matter when we end up making this a compiler (memory layout)
    fields: OrderedTable[string, tuple[ty: Type, val: Val]]
  EnumType = ref object of AnyType
    # Order doesn't matter for tags
    # discrim is a Val since technically @tagType could be set to anything that's comparable.
    # Note you can't default a union member's inner value
    tags: Table[string, tuple[holds: AnyType, discrim: Val]]
  SliceType = ref object of AnyType
    target: AnyType
  ArrayType = ref object of SliceType
    len: Val
  OptionalType = ref object of AnyType
    target: AnyType
  FuncType = ref object of AnyType
    # Note that we don't compare based on arg/ret names
    argTypes: seq[AnyType]
    retType: AnyType
  ConstType = ref object of AnyType
    target: AnyType
  ComptimeType = ref object of AnyType
    target: AnyType
  IntType = ref object of AnyType
    signed: bool
    bits: uint
  BoolType = ref object of AnyType

method typeName(t: AnyType): string {.base.} = "any"
proc `$`(t: AnyType): string = t.typeName

# Using Zig-style notation here since it's more compact
method typeName(u: TypeType): string = "type"
method typeName(v: VoidType): string = "void"
method typeName(b: BoolType): string = "bool"
method typeName(b: IntType): string = (if b.signed: "i" else: "u") & $b.bits
method typeName(t: ComptimeType): string = "comptime " & $t.target
method typeName(t: ConstType): string = "const " & $t.target
method typeName(t: SliceType): string = "[]" & $t.target
method typeName(t: ArrayType): string = "[" & $t.len & "]" & $t.target
method typeName(t: OptionalType): string = "?" & $t.target
method typeName(t: RecordType): string = t.name # No need for a separate one for Struct vs Enum
# Using rust notation here since it's more precise
method typeName(f: FuncType): string =
  result = "fn("
  for arg, i in f.argTypes: result &= (if i != 0: "," else: "") & $arg
  result &= ") -> "
  result &= $f.retType
method typeName(t: TupleType): string =
  result = "("
  for child, i in t.children: result &= (if i != 0: "," else: "") & $child
  result &= ")"
  
# Think of it a little like a stack frame
type Context = ref object
  # For resolving variables from above this scope
  parent: Context #?
  vars: Table[string, Val]
  # The name of the current return 
  ret: string

type Flow = enum
  Return, Break,
  Value

type Result = object
  case kind*: Flow
    of Return: discard
    of Break:
      breakWith: Value #NonNull. May be NoVal
      breakFrom: string #?
    of Value:
      val: Val
template expectValue(res: Result, pos: FilePos): Val =
  if res.kind != Value:
    quit "Expected a value at " & $pos
  res.val

template voidResult(): Result = Result(
  kind: Value,
  val: Val(
    ty: VoidType(),
    kind: NoVal
  )
)

method eval(e: Stmt, parent: Context): Result {.base.} =
  quit "HIT BASE EVAL AT " & $e.pos
method eval(b: Block, parent: Context): Result =
  var context = Context(
    parent: parent,
    vars: new[Table[string, Val]]
  )
  for child in b.children:
    let res = child.eval(context)
    case res.kind:
      # We ain't a function. This is above our paygrade
      of Return: return res
      of Break:
        if res.breakFrom == "" or res.breakFrom == b.label:
          return Result(kind: Value, val: res.breakWith)
        # Meant for a block above us
        else:
          return res
      of Value:
        # The first non-void expression breaks the entire block
        if res.val.ty is VoidType: continue
        else: return res
method eval(a: Assert, context: Context): Result =
  let res = child.expr.eval.expectValue(a.pos)
  return voidResult()
method eval(b: Break, context: Context): Result =
  result = Result(kind: Break)
  if b.val != nil:
    let res = b.val.eval.expectValue(b.val.pos)
    result.breakWith = res
  result.breakFrom = b.label
method eval(r: Return, context: Context): Result =
  result = Result(kind: Return)
  if r.val != nil

# This hides the Flow ugliness
proc evaluate*(b: Block):Val {.base.} =
  let res = b.eval()
  if res.kind != Flow.Value:
    quit "Expected root block to return a value!"
  else:
    return res.val
