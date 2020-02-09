# Plans for the evaluator


## Types
Types are essentially stored as a tree. An effort must be made to at least keep
user types unique because...


Types store the value of their own statics. This way they act like the globals they are, 
and can be accessed from any context.

In this document, I'll refer to types with Type->(Type1, Type2) notation.


## Values
Each value shall store a ref to its own type.

Values must be done in such a way that a ref to them can be passed around, yet still
have their interior values mutated. I am currently doing this with a `ref object` type
on the outside and a `case kind` on the interior.


Values can be of the following kinds:
* NoVal: Nothing has been written to this val yet.
* NullVal: A `null` literal. Note that it still has a type attached to it.
* TypeVal: A simple reference to a type. Its own type is always TypeType.
* IntVal: An integer. What did you expect? Type is always some IntType. Duh.
  * Note that the IntType can constrain the possible values, depending on if its signed
  	and how many bits it's supposed to be.
* StringVal: Not actually a value. Instead, use a SliceVal/ArrayVal
* SliceVal: A series of Vals from a MemSlice, marked by a start and stop inside that slice.
* ArrayVal: A series of Vals, all of the same type.
* RangeVal: A range that starts somewhere and stops somewhere. What'd you think this would be?
* StructVal: A bunch of strings mapped to Vals.
* EnumVal: A tag and an inner val. If the enum tag's type has no inner value, the val is NoVal.
* TupleVal: A series of Vals, possibly all of different types.
* FuncVal: A function to be called, what else? Has a list of arg names and a ret name.
  * Note that a retname of `_` is perfectly valid.


## Contexts
A "Context" shall be our stackframe. A new context is created for each scope and is deleted when that scope is over.
Each shall store a ref to its parent for looking up variables not defined in that scope.

## Functions
FuncVals shall store the name of the retval. When a function is called, 
a new context is pushed to the stack with that retval in it and no parent set.
This way, functions don't have to worry about touching external variables.
Statics should still work like this, as they're stored inside the types themselves.


