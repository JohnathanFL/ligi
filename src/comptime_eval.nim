# This is to explore making a simplified comptime-only evaluator
# Currently just directly sends stuff to Ligujo
# For now, only supporting things like `let TName = enum(...)`


import options
import tables
import hashes
import strutils
import strformat

# For communication with Ligujo
import httpclient
import json

import nodes
import cmds


type
  FilePos* = tuple[line: uint, col: uint]
  # ids 0..=2056 are reserved for builtins, and match the typeOfType
  TypeID* = uint
  Field* = object
    access*: int32
    name*: string
    typeid*: TypeID
  Type* = object
    name*: string
    statics*: seq[Field]
    pos*: string
    ty*: TypeID
    backing*: TypeID
    len*: uint
    fields*: seq[Field]

proc `$`*(t: Type): string = $ %* t
# Static storage for any int types we ever need, to avoid duplicates

let emptySeq = newSeq[Field](0)

let voidType = Type(name: "void", pos: "builtin", ty: 0)

proc intType(size: uint): Type = Type(name: "i" & $size, pos: "builtin", ty: size + 1024)
proc uintType(size: uint): Type = Type(name: "u" & $size, pos: "builtin", ty: size)
let usizeType = Type(name: "usize", pos: "builtin", ty: 2049)
let isizeType = Type(name: "isize", pos: "builtin", ty: 2050)

let boolType = Type(name: "bool", pos: "builtin", ty: 2051)

let f16Type = Type(name: "f16", pos: "builtin", ty: 2052)
let f32Type = Type(name: "f32", pos: "builtin", ty: 2053)
let f64Type  = Type(name: "f64", pos: "builtin", ty: 2054)
let f128Type = Type(name: "f128", pos: "builtin", ty: 2055)

let typeType = Type(name: "type", pos: "builtin", ty: 2056)


method evalAsType*(e: Expr): Option[Type] {.base.} = none(Type)
method evalAsType*(b: Block): Option[Type] = quit "EVAL BLOCK NOT YET SUPPORTED"
method evalAsType*(s: Symbol): Option[Type] =
  case s.sym:
    of "void": return some voidType
    of "bool": return some boolType
    of "char": return some uintType(32) # Chars are simply u32
    else: # Not an easy one, time to get serious
      if s.sym.len in 2..4 and s.sym[0] in {'u', 'i'}:
        let signed = s.sym[0] == 'i'
        try:
          let bits = parseInt(s.sym[1..<s.sym.len])
          return if signed: some intType(bits.uint) else: some uintType(bits.uint)
        except ValueError: quit fmt"We don't support parsing {s.sym} as a type!"
      return none[Type]()
method evalAsType*(u: UnaryExpr): Option[Type] =
  case u.cmd:
    of UnaryCmd.Enum:
      # 2059 === an enum
      # Enums backed by usize by default
      var res = Type(statics: emptySeq, ty: 2059, backing: usizeType.ty, fields: newSeq[Field](0))
      for child in u.target.Tuple.children:
        # Assume it's a Symbol
        # Discriminators in a shorthand enum are always public
        # Discriminators in a shorthand enum always hold void (.typeid = 0)
        res.fields.add Field(access: 2, name: child.Symbol.sym, typeid: 0)
      result = some(res)
    else: result = none[Type]()
