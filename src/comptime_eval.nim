# This is to explore making a simplified comptime-only evaluator

import tables
import hashes
import strutils
import strformat

import nodes
import cmds


type
  FilePos* = tuple[line: uint, col: uint]
  TypeType* = enum
    AnyType, VoidType, SinkType, TupleType,
    BoolType, IntType, ArrayType, SliceType,
    ConstType, ComptimeType, PtrType, RefType,
    EnumType
  Type* = ref object
    case kind*:TypeType
      # This type may be transmuted into anything
      # The way this will work is you make a new Type with kind AnyType,
      # then use a transmute function to make a new type while ensuring it's a unique instance
      of AnyType: discard
      # Absolutely nothing. Can't write to it, can't read from it
      of VoidType: discard
      # Writes to it do nothing, comparing against it is always true
      of SinkType: discard
      of TupleType:
        children*: seq[Type]
      of IntType:
        signed*: bool
        bits*: uint
      of ArrayType:
        arrayOf*: Type
        arraySize*: uint
      of SliceType:
        sliceOf*: Type
      of ConstType, ComptimeType, PtrType, RefType:
        inner*: Type
      of EnumType:
        pos*: FilePos
        tags*: seq[string]
      else: discard
proc hash(t: Type): Hash =
  result = 0
  result = result !& t.kind.int

proc `$`*(t: Type): string =
  case t.kind:
    of AnyType: result = "undef"
    of VoidType: result = "void"
    of SinkType: result = "_"
    of BoolType: result = "bool"
    of TupleType:
      result = "TUPLE("
      for child in t.children:
        result &= $child & " "
      result &= "\b)"
    of IntType:
      result = if t.signed: "i" else: "u"
      result &= $t.bits
    of ArrayType:
      result = fmt"ARRAY({$t.arraySize} {$t.arrayOf})"
    of SliceType: result = fmt"SLICE({$t.sliceOf})"
    of ConstType: result = fmt"CONST({$t.inner})"
    of ComptimeType: result = fmt"COMPTIME({$t.inner})"
    of PtrType: result = fmt"PTR({$t.inner})"
    of RefType: result = fmt"REF({$t.inner})"
    of EnumType:
      result = fmt"ENUM@{$t.pos}OF( "
      for tag in t.tags: result &= fmt"#{tag} "
      result &= ")"

# Static storage for any int types we ever need, to avoid duplicates
var intTypes: Table[(bool, uint), Type] = initTable[(bool, uint), Type]()
proc getIntType(signed: bool, bits: uint): Type =
  return intTypes.getOrDefault((signed, bits), Type(kind: IntType, signed: signed, bits: bits))

var constTypes: Table[Type, Type] = initTable[Type, Type]()
proc getConstType(target: Type): Type =
  return constTypes.getOrDefault(target, Type(kind: ConstType, inner: target))

var sliceTypes: Table[Type, Type] = initTable[Type, Type]()
proc getSliceType(target: Type): Type =
  return sliceTypes.getOrDefault(target, Type(kind: SliceType, sliceOf: target))

var arrayTypes: Table[(uint, Type), Type] = initTable[(uint, Type), Type]()
proc getArrayType(size: uint, inner: Type): Type =
  return arrayTypes.getOrDefault((size, inner), Type(kind: ArrayType, arrayOf: inner, arraySize: size))

var comptimeTypes: Table[Type, Type] = initTable[Type, Type]()
proc getComptimeType(t: Type): Type =
  return comptimeTypes.getOrDefault(t, Type(kind: ComptimeType, inner: t))

# Other static common types for simple instantiations
let voidType = Type(kind: VoidType)
let sinkType = Type(kind: SinkType)
let boolType = Type(kind: BoolType)
# TODO: Make sure this is unique
let strType = getSliceType( getConstType( getIntType(false, 8) ) )


method evalAsType*(e: Expr): Type {.base.} = quit "HIT BASE EXPR"
method evalAsType*(b: Block): Type = quit "EVAL BLOCK NOT YET SUPPORTED"
method evalAsType*(s: Symbol): Type =
  case s.sym:
    of "_": return sinkType
    of "void": return voidType
    of "undef": return Type(kind: AnyType)
    of "bool": return boolType
    of "char": return getIntType(false, 32) # Chars are simply u32
    of "str": return strType
    else: discard
  # Not an easy one, time to get serious
  if s.sym.len in 2..4 and s.sym[0] in {'u', 'i'}:
    let signed = s.sym[0] == 'i'
    try:
      let bits = parseInt(s.sym[1..<s.sym.len])
      return getIntType(signed, bits.uint)
    except ValueError: quit fmt"We don't support parsing {s.sym} as a type!"
method evalAsType*(u: Undef): Type = Type(kind: AnyType)
method evalAsType*(u: UnaryExpr): Type =
  case u.cmd:
    of UnaryCmd.Enum:
      var tags = newSeqOfCap[string](2)
      for child in u.target.Tuple.children: tags.add child.Symbol.sym
      # For now, we'll simplify things and assume they'll only do enum(tag1, tag2...)
      return Type(
        kind: EnumType,
        tags: tags
      )
    of Array:
      # For now, assume that we only do things like array(0, type)
      return getArrayType(
        u.target.Tuple.children[0].Int.val.uint,
        u.target.Tuple.children[1].evalAsType()
      )
    of UnaryCmd.Slice: return getSliceType( u.target.evalAsType() )
    of Comptime: return getComptimeType( u.target.evalAsType() )
    of Const: return getConstType( u.target.evalAsType() )
    else: discard
