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


### Mutual recursion
Contexts shall store a list of values that can be lazily evaluated (`let`s, usually). 
This is to allow mutually recursive things. In the following example:
```
let Foo: type, Bar: type
Foo = struct {field b: *Bar}
Bar = struct {field f: *Foo}
```
The evaluator does the following:
* Parses line 1, which introduces 2 new binds
  * `Foo: const undef = undef`
  * `Bar: const undef = undef`
  * Although their types are marked const, they can still be mutated once because their value is undef.
* Begins parsing the assignment for Foo
  * Sees a `struct` operator, enters struct parsing mode and enters its block
  * Begins parsing field `b`
  * Sees that field `b` depends on `Bar`, ~~and that `Bar` must be a type,~~ but since `b` is a pointer, we don't
  	need to know all details inside `Bar` yet.
  	* Registers a hook for `Bar`'s assignment and sets `Bar`'s type to TypeType
	* Since `b` is a pointer, it simply leaves that field's type in a Pointer->LazyType("Bar") state.
  	* Had it not been a pointer, it would have pushed the entire expression into a lazy hook for later
* Begins parsing the assignment for `Bar`
  * Sees a `struct` operator, enters struct parsing mode and parses the struct out normally, since
  	`Foo`'s type is technically known and `f` is a pointer.
* After exiting `Bar`'s assignment, it sees there was a hook waiting for it, and runs it.
  * Had the hook been for a non-pointer `Bar`, it would've gone back to `Foo`'s expression and evaluated
  	the rest so it would have size information.
 	
## Functions
### Representation

### Calling
FuncVals shall store the name of the retval. When a function is called, 
a new context is pushed to the stack with that retval in it and no parent set.
This way, functions don't have to worry about touching external variables.
Statics should still work like this, as they're stored inside the types themselves.
