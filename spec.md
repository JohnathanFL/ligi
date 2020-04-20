# Ligi Spec
This document is to specify Ligi's semantics.


## Value and Reference
With one exception, Ligi is always pass by value.

The sole execption is `ref`. A `ref` type is initialized by a pointer
with its first write (making it pass by value of pointer), but thereafter acts
like a value, even though it still points to whatever the pointer pointed to.
Thus it acts like C++'s `&`, if `&` was initialized by pointers.

Function arguments are always immutable. This is to allow Ligi to decide whether they
should internally be pass-by-value or pass-by-reference as needed. Of course, a `ref`
argument's pointed location may still be manipulated according to its own rules,
and a `ref` may only every have one location in its lifetime.


## Scope and Lifetime
Objects are always destroyed when they exit scope. Ligi (currently) has no destructors.



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

### Truthyness and Capturing
If an optional value is used as the condition of `if`, `when`, or `while`, it is tested
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

### Binary Type Operators
The following binary operators create new types. `T` and `V` are used as standins for any other
types. `N` is used as a standin for a number.
- `T + V`: 
  - In the case of structs and enums, concatenates them together, preserving order.
  - In the case of `T` being a tuple and `V` being a struct, creates a compound tuple.
- `T * N` and `N * T`: Creates a tuple of `N` `T`s. For example `u32 * 3` is `(u32, u32, u32)`.


## Bind Specs
- `let x:T` translates to `var x:const T`
- `cvar x:T` translates to `var x: comptime T`
