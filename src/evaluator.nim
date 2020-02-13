# This will be stage 1 of the interpreter - comptime evaluation

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
        inner: Type
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

  TypeKind = enum
    AnyType, VoidType, TypeType
    TupleType
    StructType, EnumType
    SliceType, ArrayType
    OptionalType
    FuncType
    ConstType, ComptimeType
    IntType, BoolType, 
  Type = ref object
    name: string
    statics: Table[string, tuple[ty: Type, val: Val]]
    props: Table[string, tuple[get: Val, set: Val]]
    case kind: TypeKind
      # Used for 'undef' type
      # Technically means that type has yet to be set
      of AnyType: discard
      # Writes to it do nothing
      # Reading from it is an error
      # All things return to the void, but none return from the void
      of VoidType: discard
      of TypeType: discard
      # Any const/comptime mods applied to a TupleType will just go through to its members
      of TupleType:
        children: seq[Type]
      # Any type that was defined by the user with struct or enum (i.e holds records)
      # Should never be instantiated.
      of StructType:
        # Order will matter when we end up making this a compiler (memory layout)
        fields: OrderedTable[string, tuple[ty: Type, val: Val]]
      of EnumType:
        # Order doesn't matter for tags
        # discrim is a Val since technically @tagType could be set to anything that's comparable.
        # Note you can't default a union member's inner value
        tags: Table[string, tuple[holds: Type, discrim: Val]]
      of SliceType:
        sliceOf: Type
      of ArrayType:
        arrayOf: Type
        len: Val
      of FuncType:
        # Note that we don't compare based on arg/ret names
        argTypes: seq[Type]
        retType: Type
      of ConstType, OptionalType, ComptimeType:
        target: Type
      of IntType:
        signed: bool
        bits: uint
      of BoolType: discard

method typeName(t: Type): string {.base.} = "any"
proc `$`(t: Type): string = t.typeName

 
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
      breakWith: Val #NonNull. May be NoVal
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
    ty: Type(kind: VoidType),
    kind: NoVal
  )
)

method eval(e: Stmt, parent: Context): Result {.base.} =
  quit "HIT BASE EVAL AT " & $e.pos
method eval(e: Expr, parent: Context): Result =
  quit "HIT EXPR EVAL AT " & $e.pos
method eval(b: Block, parent: Context): Result =
  var context = Context(
    parent: parent,
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
        if res.val.ty.kind == VoidType: continue
        else: return res
method eval(a: Assert, context: Context): Result =
  let res = a.expr.eval(context).expectValue(a.pos)
  return voidResult()
method eval(b: Break, context: Context): Result =
  result = Result(kind: Break)
  if b.val != nil:
    let res = b.val.eval(context).expectValue(b.val.pos)
    result.breakWith = res
  result.breakFrom = b.label
method eval(r: Return, context: Context): Result =
  result = Result(kind: Return)
  if r.val != nil: discard

# This hides the Flow ugliness
proc evaluate*(b: Block):Val =
  let res = b.eval(Context())
  if res.kind != Flow.Value:
    quit "Expected root block to return a value!"
  else:
    return res.val
