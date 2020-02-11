import tables
import sets

import cmds
import nodes

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
  # A concrete type that can be named or otherwise mutated with mods
  Type = ref object of AnyType
    name: string
  # Any type that was defined by the user with struct or enum (i.e holds records)
  # Should never be instantiated.
  RecordType = ref object of Type
    statics: Table[string, tuple[ty: Type, val: Val]]
    props: Table[string, tuple[get: Val, set: Val]]
  StructType = ref object of Type
    # Order will matter when we end up making this a compiler (memory layout)
    fields: OrderedTable[string, tuple[ty: Type, val: Val]]
  EnumType = ref object of Type
    # Order doesn't matter for tags
    # discrim is a Val since technically @tagType could be set to anything that's comparable.
    # Note you can't default a union member's inner value
    tags: Table[string, tuple[holds: AnyType, discrim: Val]]
  SliceType = ref object of Type
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

type Stack = seq[Val]

# Think of it like a stack frame
type Context = ref object
  # For resolving variables from above this scope
  parent: Context #?
                  # For mutually recursive things
                  # If the evaluator encounters something which references something that's in scope
                  # but not initialized yet, it pushes that expression tree here.
                  # Whenever a value changes from NoVal to some value, this needs to be checked for a hook.
  lazy: Table[string, Expr]
  # Note 'var's is a misnomer here. They could be constants.
  # Note that while evaluating, this only has the vars that have been bound up to that point
  vars: Table[string, Val]
