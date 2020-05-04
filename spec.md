# Ligi Spec
This document is to specify Ligi's semantics.

Note that this is to be the official specification of Ligi. The reference interpreter
is *not* currently up to spec.


## Modes
Ligi programs may be compiled in one of the following 'modes':
- debug
- release
- release_fast

## Assertions
Any `assert` statement which can be checked at compiletime *will* be, and compilation will
produce an error for each invalid assertion.

For runtime, the `assert` statement is defined as follows:
- In debug and release modes, the condition will be checked.
  - If the condition is false, the program will error out.
- In release and release_fast modes, the compiler may assume that the condition is true
  and may make optimizations off of that.
- In release_fast mode, the condition will be neither checked not evaluated.
  - To avoid confusion over this, an assertion's condition's subtree is implicitly marked
    as `pure`. Thus, it is not allowed to mutate outside values.

## Primitive Types
Ligi contains the following primitive types. All other types are based upon these.
- `void`: The absence of any value.
- `uN` and `iN`, where `N` is from `1` to `1024`, inclusive: This is defined to be a
  signed (if `i`) or unsigned (if `u`) integer `N` bits in size.
  - Except in cases of packing structs, values of `N` which do not align with a byte
    boundary are stored in a `iN` or `uN` widened to fit a byte boundary.
    - Further widening to fit other alignment may also occur.
- `usize` and `isize`: Defined to be `uN` and `iN`, respectively, where `N` is the
  number of bits for the target platform. (e.g 32 or 64)
- `char`: Defined to be `u32`. Intended to hold UTF-32 values.
- `f16`, `f32`, `f64`, and `f128`: These are the appropriate IEEE specifications for
  floating point numbers of 16, 32, 64, and 128 bits, respectively.
- `bool`: Holds either `true` or `false`.
- `type`: Holds a Ligi type. `type` is implicitly `comptime`.
- `_`: The sink. May not be instantiated, but is used for pattern matching.

## Value and Reference
With one exception, Ligi is always pass by value.

The sole execption is a `ref T`. A `ref` type is initialized by a pointer
with its first write (making it pass by value of pointer), but thereafter acts
like a value, even though it still points to whatever the pointer pointed to.
Thus it acts like C++'s `&`, if `&` was initialized by pointers.


**Maybe remove the below?**

Function arguments are always immutable. This is to allow Ligi to decide whether they
should internally be pass-by-value or pass-by-reference as needed. Of course, a `ref`
argument's pointed location may still be manipulated according to its own rules,
and a `ref` may only every have one location in its lifetime.


## Scope and Lifetime
Values are always destroyed when they exit scope. Ligi (currently) has no destructors.



## Tuples
There are two types of tuples: primitive tuples and compound tuples.

The members of any tuple may be accessed through `.<number>`, such as `.0`, `.1`, and so on.


### Tuple Operators
Unary operators pass though tuples. Hence, the following holds:
```
assert unaryop (x, y, z) == (unaryop x, unaryop y, unaryop z)
```

With exceptions, binary operators are defined only for N-1/1-N operations and N-N operations, where
N is the number of items in a tuple. 
- When N-1/1-N, the operator is applied to produce a cartesian product of the two values.
- When N-N, the operator is applied pairwise to produce a tuple of N values.

Thus, using `+` as a standin for any binary operator:
```
assert (x, y, z) + w == (x + w, y + w, z + w)
assert (x, y, z) + (p, q, r) == (x + p, y + q, z + r)
```

Binary operations with tuples are expected to use SIMD operations where possible.

Exceptions:
- `in` and `notin`: 1-N for these operators is defined such that it produces only a single boolean.

### Primitive Tuples
With the exception of `.<number>`, primitive tuple field access (`.`) is 'auto-swizzled'.
This means that the following holds true:
```
assert (x, y, z).foo == (x.foo, y.foo, z.foo)
```
Thus, it can be said that swizzles "passe through" primitive tuples to form a tuple of
the result of accessing the swizzle of that name on each argument. Thus it also holds for:
```
assert (x, y).(foo, bar) == ((x.foo, x.bar), (y.foo, y.bar))
```
#### Primitive Tuple Types
The type of a tuple is the tuple that contains the type of each of its arguments. Hence:
```
assert (x, y, z).@type == (x.@type, y.@type, z.@type)
```

### Compound Tuples
Compound tuples are produced by adding a tuple to a struct, as in:
```
let Vec2 = (f32, f32) + struct {...}
```

Compound tuples may be initialized from tuples matching their primitive predeccessor, and may be
implicitly converted back to their primitive predeccessor.

Compound tuples are immune to auto-swizzling, but preserve their builtin operator rules.
Thus the above `Vec2` can be used as such by default:
```
let x = [:Vec2: .x = 1, .y = 1], y: Vec2 = [:Vec2: .x = 2, .y = 2]
assert x + y == (3, 3)
assert x + 1.0 == (3, 3)
```

## Optionals
An optional type is created with the `?` operator, as in `?T`. An optional may be initialized with
either the null literal `null` or an object of the type it contains (`T`), and may be compared
against either. Non-equality operators (anything but `==` and `!=`) are not defined for optionals.

Optionals contain 2 members:
- `.has`: A boolean telling whether the optional value is present.
- `.val`: A direct reference to the inner value.
  - Attempting to access this when `.has` is false in debug mode is an error, 
    and is undefined in release.

### Representation
Optional pointers are guaranteed to be the same size as their underlying pointer.
If an optional pointer is `null`, that means its in-memory value is `0`.
- This means that currently, it is impossible to have an optional pointer with an underlying
  value which itself may be `0`.

All other optional type `?T`s are defined to have the same layout as:
```
let V = struct { field has: bool, val: T }
```

### Truthyness and Capturing
If an optional value is used as the condition of `if`, a `when` without an overridden 
binary operator, or `while`, it is tested
to see if it is `null`. If it is not, then its inner value may be captured in a `->` block.

If the capturing bind for one of these is marked `var`, then it may be mutated with `ref` 
semantics.


## Type Operators

### Unary Type Operators
The following unary operators create new types. `T` is used as a standin for any other type.
- `const T`: Creates a version of `T` which may not be mutated. 
  If `T` is already `const`, it is unchanged.
- `comptime T`: Creates a version of `T` which may only be mutated at compiletime.
  If `T` is already `comptime`, it is unchanged.
- `array T` and `array (N, T)`: Creates an array containing `T`s. If `N` is present, that is
  evaluated and becomes the size of the array. Otherwise, the size is deduced based on its
  initializer, the size of which must be `comptime`.
- `slice T`: Creates a new slice of `T`s.
- `?`: Creates a new optional version of `T`.
- `pure`: When used on a function, ensures that the function must always return the same value
  when given the same input parameters. It is an error if used on any other type.
- `inline`: When used on a function, ensures that the function is inlined in all callsites.
  It is an error when used on any other type.
- `struct`: Used on a block to create a new type. The block may contain 0 or more binds.
  - `let`, `var`, and `cvar` binds are treated as statics of the new struct type.
    - If these are functions, they are considered for `.call()` syntax on instances of that type.
  - `field` binds are treated as the fields of the new structure.
- `enum`: Used on one of:
  - A block. Follows the same rules as `struct`, except that `enum` is used instead of `field`
    in order to produce discriminators, the type of an `enum` bind is the inner type it stores, 
    and its init expression is its tag value.
    
    If @tagType is assigned to within the block, that sets what type the enum will store its tag
    in.
    
    Two or more discriminators having the same value is not an error, and may be used to produce
    C-like unions (where any field of the union is accessible simultaneously).
    
  - A tuple of symbols. This is shorthand for creating an enum with all of those symbols as its
    discriminators. This method does not allow @tagType, inner types, or tag values.
- `overload`: Used with a block. Creates a new `overloaded` object. All exported functions
  found within the block are considered as being part of the overload.
- `* T`: Creates a pointer to a *single* `T`. 
- `ref T`: Creates a reference type that points to a single `T`.

### Binary Type Operators
The following binary operators create new types. `T` and `V` are used as standins for any other
types. `N` is used as a standin for a number.
- `T + V`: 
  - In the case of structs and enums, concatenates them together, preserving order.
  - In the case of `T` being a tuple and `V` being a struct, creates a compound tuple.
- `T * N` and `N * T`: Creates a tuple of `N` `T`s. For example `u32 * 3` is `(u32, u32, u32)`.


## Bind Specs
Aside from `field` and `enum`, all bind specs translate directly to a `var` bind.
- `let x:T` translates to `var x:const T`
- `cvar x:T` translates to `var x: comptime T`

## Enum Literals
Enum literals (`#Symbol` or `#Symbol(expr,...)`) are a shorthand for typing `EnumType.Symbol`.

Enum literals may only be used in places where the enum's type is clearly known, such as
comparisons and direct assignments.


## Access Operator
The access operator (`.`) is used to both access fields and call methods of the type.

## Floating Points
There are no floating point literals. Floating point values are produced through field accesses
on integer literals. Thus the following works:
```
assert = 1 . 1 == 1.1
```

## Swizzling
Swizzling is done by placing a tuple of swizzles and/or symbols after an access operator.

Swizzling produces a tuple of its arguments. Thus:
```
assert x.(y, z) == (x.y, x.z)
```

Swizzling *is* defined for functions. Swizzled function calls are all applied against the same
memory location (i.e functions that take the location by reference may modify it, but it is still
the same location). Thus:
```
assert x.(y(), z()) == (x.y(), x.z())
```

## Sinks
Comparisons against the sink (`_`) are defined to be `true`. Thus:
```
assert (x, _) == (x, z)
```

For type purposes, a sink is its own type. Thus:
```
let x: u32
assert (x, _).@type == (u32, _)
```

## Implicit Casts
The following are the only implicit casts allowed:
- Enum literals to an enum type containing the proper discriminators and inner types
- Numerical casts which widen.
  - Arithmetic between a smaller and larger type casts and produces the larger type.
- Numerical casts which can be known at compiletime to not truncate.
  - Hence a `comptime f32` may be cast to an integer type.
- Anonymous struct literals to structs with matching field names and castable field types.
  - Hence a struct of three floats may be initialized by an anonymous struct of 3 ints.

## `ref` Types
A ref type is produced with `ref T`. A ref type is initialized exactly one time with a pointer
to its underlying `T` type. Thereafter, a `ref T` may be treated as just `T`.

This allows for assigning directly to locations returned by custom indexing operators,
such as is needed for array-list style structures and the like, as well as pass-by-reference
semantics for functions.


## Control Structures

### If

The `if` statement's initial `if` condition, as well as all of its `elif`s are designated
as its "arms". The arms of an `if` statement are always evaluated sequentially. When an arm
evaluates to true, no further arms are evaluated, and the block attached to that arm is
executed end becomes the value for the entire if statement. If no arm evaluates as true, then
the `else` block, if any, is evaluated and becomes the value for the entire `if` statement.
If any arm evaluates as true, then the `finally` block, if any, is evaluated but may not
yield any value, as the arm which evaluated as true already yielded one.


### When

The `when` statement's initial expression (the one directly after the `when` keyword) is evaluated 
exactly once. Each `is` arm is then evaluated sequentially according to the following rules:

- If the `is` arm has an additional binary operator immediately after `is`, then that
  operator is used to compare the initial expression with the `is` arm's expression.
- If the `is` arm has no additional binary operator, then it acts as though `==` had been passed
  as the additional operator.
- If the `is` arm's operator (as determined by the two above conditions) is `==`, then the arm
  is additionally able to capture the inner optional value (if any) in a `->` capture. See
  the section on [Truthyness and Capturing] for more.

The first `is` arm to evaluate as true according to the above rules halts evaluation of any further
`is` arms, and that block becomes the value of the entire `when` expression. if no `is` arm
evaluates as true, then the `else` block, if any, becomes the value of the entire structure.
If any `is` arm evaluates as true, then the `finally` block, if any, is executed but **does not**
become the value of the structure, as the `is` arm which evaluated as true has already done so.

### Loops

#### Breaking
Any loop may be broken, either by a `break` which does not specify a label or by
a `break` which specifies the label of that loop's body or another label above that
loop. All loops except the `loop` loop are eligible for a `finally` block. This block
is to be executed if and only if the loop executes "naturally". What constitutes
"naturally" shall be specified by each loop type.

#### Loop Counter
All loops are capable of having at least one `->` capture, and this capture must always
appear as the first capture if any captures are provided, although it is perfectly valid to
bind the capture to `_` in order to discard it. This first capture is designated as the
"loop counter". On the first iteration of the loop, this location is initialized to the
`usize` value `0`, and is incremented by `1` on each subsequent iteration.

#### Loop
The `loop` loop will loop infinitely until a `break` statement halts it, or program execution
is otherwise halted. As the only way out of a `loop` loop is a `break`, the `loop` loop is not
eligible for a `finally` block.

#### For
The `for` loop is used for iteration over an eligible collection (see the section below). The
for loop is eligible for an second capture -- which must come after the loop counter if at all --
which is used to capture the value of the element currently iterated over.

A `for` loop is considered as having exited "naturally" only if the collection it is iterating
over has no more elements it wishes to expose. For example, reaching the end of an array.

##### For Loop Eligibility
In this initial specification, only arrays and slices are eligible for `for` loop looping.
In both cases, the loop will execute once for every item in the array or slice. Thus, for
an array or slice named `a`, the loop will execute `a.len` times in total, barring an unnatural
exit via `break`.

#### While and Until
`while` and `until` will loop until their condition evaluates as either true or false,
respectively. If the condition is an optional, then the `while` loop is additionally eligible 
for a second capture -- which must come after the loop counter if at all -- which is used 
to capture the value of the optional being used as the condition.
