# This file is for providing a subset of tags that map onto different types of commands

# The middleground between not directly relying on the lexer and having some degree of comptime safety
# with commands
import tags
type # Note that tags is kept sorted in alphabetical order. Thus that (Tag) order is needed here too.
  BinCmd*{.pure.} = enum
    Add = Tag.Add
    AddAssign = Tag.AddAssign
    And = Tag.And
    AShr = Tag.AShr
    Assign = Tag.Assign
    BitAnd = Tag.BitAnd
    BitAndAssign = Tag.BitAndAssign
    BitNotAssign = Tag.BitNotAssign
    BitOr = Tag.BitOr
    BitOrAssign = Tag.BitOrAssign
    BitXor = Tag.BitXor
    BitXorAssign = Tag.BitXorAssign
    ClosedRange = Tag.ClosedRange
    Div = Tag.Div
    DivAssign = Tag.DivAssign
    Equal = Tag.Equal
    Greater = Tag.Greater
    GreaterEq = Tag.GreaterEq
    In = Tag.In
    Less = Tag.Less
    LessEq = Tag.LessEq
    Mod = Tag.Mod
    Mul = Tag.Mul
    MulAssign = Tag.MulAssign
    NotEqual = Tag.NotEqual
    NotIn = Tag.NotIn
    OpenRange = Tag.OpenRange
    Or = Tag.Or
    Shl = Tag.Shl
    ShlAssign = Tag.ShlAssign
    Shr = Tag.Shr
    ShrAssign = Tag.ShrAssign
    Spaceship = Tag.Spaceship
    Sub = Tag.Sub
    SubAssign = Tag.SubAssign
    Xor = Tag.Xor

  UnaryCmd*{.pure.} = enum
    Array = Tag.Array
    BitNot = Tag.BitNot
    Comptime = Tag.Comptime
    Const = Tag.Const
    Enum = Tag.Enum
    Inline = Tag.Inline
    Not = Tag.Not
    Optional = Tag.Optional
    Overload = Tag.Overload
    Pure = Tag.Pure
    Slice = Tag.Slice
    Struct = Tag.Struct
    Negative = Tag.Sub
  BindCmd*{.pure.} = enum
    CVar = Tag.CVar
    Enum = Tag.Enum
    Field = Tag.Field
    Let = Tag.Let
    Property = Tag.Property
    Var = Tag.Var
