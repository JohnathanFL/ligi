import ids
# Predefined TypeIDs:
  # 0: void
  # 1-256: unsigned int of n bits
  # 257-512: signed int of n bits
  # 513-516: real number of {16,32,64,128} bits
# High TypeID bits are flags: {isRuntime,isMutable,isRef,isPtr}

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
