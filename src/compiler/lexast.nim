# This is to be a translation layer between tags and AST constructs

import options
import tables

import lexing
import ast

const AssgOps*: set[Tag] = {
  tAssg, tAddAssg, tSubAssg, tMulAssg, tDivAssg
}

const BinOps* = [
  { tEq, tNotEq, tSpaceship },
  { tLt, tGt, tLtEq, tGtEq },
  { tAdd, tSub },
  { tMul, tDiv, tMod },
]

const UnaOps*: set[Tag] = {
  tSub,
  tMul,
  tBitNot,
  tNot,
  tStruct,
  tRef,
  tSlice,
  tArray,
  tConst,
  tComptime,
  tOpt,
  tPure,
  tInline,
  tOverload,
  tProperty,
  tConcept,
  tBlock,
  tEnum,
}

const BindSpecs*: set[Tag] = {
  tLet,
  tVar,
  tCVar,
  tEnum,
  tField,
}


const WhenOps*: set[Tag] = {
  tEq, tNotEq, tIn, tNotIn
}
