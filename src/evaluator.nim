import tables
import sets

import cmds
import nodes

# Well ain't this ironic. I set out to make a language without standard OOP stuff, only to end up
# using OOP at every turn in its compiler.
type
  
  Val = ref object of RootObj # TODO
    ty: Type
  # The Val counterpart to VoidType
  NoVal = ref object of Val
  # The null literal
  NullVal = ref object of Val

  # An actual type
  TypeVal = ref object of Val
  IntVal = ref object of Val
  StringVal = ref object of Val
  BoolVal = ref object of Val
  SliceVal = ref object of Val
  FuncVal = ref object of Val
  ArrayVal = ref object of Val
  SliceVal = ref object of Val
  RangeVal = ref object of Val
  StructVal = ref object of Val
  EnumVal = ref object of Val
  TupleVal = ref object of Val
  FuncVal = ref object of Val
  
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
    comptime: bool
    constant: bool
  # Any type that was defined by the user with struct or enum (i.e holds records)
  # Should never be instantiated.
  RecordType = ref object of Type
    statics: Table[string, tuple[ty: Type, default: Val]]
    funcs: Table[string, Func] # Note these are stored separately from statics
    props: Table[string, tuple[get: Func, set: Func]]
  StructType = ref object of Type
    # Order will matter when we end up making this a compiler
    fields: OrderedTable[string, tuple[ty: Type, default: Val]]
  EnumType = ref object of Type
    # Order doesn't matter for tags
    # discrim is a Val since technically @tagType could be set to anything that's comparable.
    # Note you can't default a union member's inner value
    tags: Table[string, tuple[holds: AnyType, discrim: Val]]
  SliceType = ref object of Type
    target: AnyType
  ArrayType = ref object of SliceType
    len: Val
  OptionalType = ref object of SliceType
    target: AnyType
    
# The 
type TypeDB = object
  types: HashSet[Type]
