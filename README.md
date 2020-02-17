# Ligi - *Just don't be a dumdum*

This repo is for the ligi interpreters (comptime and runtime) and compiler. (Compiler is TODO)


### Principles
* Code is data
* Flexibility and caution are better than absolute safety and thoughtlessness
* 

### As compared to other languages (WIP)
* C and C++
  * C has incredible power, but requires programmers to either repeat themselves often or use ugly preprocessor macros.
    * Ligi replaces both preprocessor macros with pure Ligi functions act directly on the AST.
  * C++ is a good language, but the template system and standard library have grown convoluted.
    * Ligi replaces templates with pure ligi functions that take and return types.
      * Example: C++'s `vector<int>` could be `Vector(isize)` in Ligi.
  * Both are more difficult to parse, both for humans and machines.
    * See most vexing parse, C's variable declaration syntax, etc.
  * Both use ugly direct textual inclusions through `#include <file>`
    * Ligi `@import`s files as new structs, same as Zig. This means the current namespace isn't needlessly polluted and removes strange compile errors.
* Rust
  * Rust's borrow checking is a stroke of genius, but it leads to slower development times and restricts the programmer.
* Zig
  * Another great language. Zig was actually the direct inspiration for the earliest versions of Ligi (when it was named "Zag" in its honor).
  * Zig's explicit resource management with `defer` and manual allocation is fantastic, given that the programmer is cautious.
      * Ligi follows the same philosophy, and also uses a `defer` keyword for easier resource cleanup.
  * Zig's style of allowing functions to create types is a much more elegant solution than C++/Rust's <template> style.
      * Ligi follows the exact same philosophy.
  * Zig doesn't allow any kind of symbol-shadowing.
      * Neither does Ligi.
  * As with C/C++, the parentheses around Zig's control structure conditions are completely unneeded and ugly, especially given
    that Zig requires `{}` when using more than a single `if`.
    * Ligi follows Rust's example in `<keyword> <expr> <block>` syntax for control structures, rather than `<keyword> ( <expr> ) <block>`
  * Zig doesn't allow function overloading as a part of its "only one obvious way to do things"
      * I believe that overloading *is* the way to have "only one obvious way to do things". Without overloading,
        you end up with either generic functions and extra logic inside, or `longFunctionNamesWithTypesAtTheEnd`.
        This isn't "one obvious way", this is "many different ways with postfixes"
        Another problem is Zig's current need to add a `.{}` when formatting without args, as in `warn("Hello world!", .{})`.
        Ligi solves this by simply allowing overloading, so you don't need an explicit extra parameter.
* Nim
  * Nim has a beautiful syntax and amazingly powerful, pure-nim macros
      * Ligi will also have a fully AST-exposed macros that can be written in pure Ligi.
  * Whitespace sensitivity is also less desirable for some.    
    * Ligi is mostly whitespace insensitive, though that comes at the cost of braces.
  * Nim unions (`case` types) can be convoluted to work with
    * Ligi merges unions and enums Rust-style to make them less of a headache.
* Python
  * Another language with beautiful syntax, but it's very slow and has no (builtin) macros.
  * Its interpreted nature makes for wonderful REPL opportunities.
    * Ligi will be a dual interpreted and compiled language. Programs are normally compiled for speed, but an interpreter for both debugging and scripting will be maintained.
* Java
  * Ugly and bloated. 
    * Ligi seeks a simple, flowing syntax with as little punctuation as needed to get the point across.
  * Java's "value of reference" system is convoluted.
    * Ligi is entirely value based. If you want to copy a full struct to pass in, just do that.
  * No macro support.
    * Ligi has full AST-editing macro support.
  * No support for first-class functions and functions must be inside a class.
    * I don't count Java's anonymous inner class hackery.
    * Ligi can define functions anywhere and pass them willy-nilly.
      * There is, however, no direct support for passing runtime state into them to pass around.
  * Runs on a VM
    * Ligi is compiled down to native code.
* Kotlin
  * Despite my hatred of all things JVM, this is a decent language. However, it still runs on a VM
* C#
  * C# has some pretty cool features, but I feel it suffers from some of the same problems as C++, not to mention the cost of GC and a VM/runtime.
  * Ligi's `property` bind is directly inspired (read: lifted) from C#
  * C#'s interfacecs are all well and good, but they can't be arbitrarily extended by other programmers.
    * Ligi follows Nim's concept of... `concept`s. A concept defines how a type must look (fields, methods) and how it must act (e.g for a Stack concept: call push then pop and get the same value back)


### Language Basics

#### Happy comments
```
(: This is a single line comment

let x (: This is an inline comment. :) = 10
(: Obviously, don't overuse those inside the code. Don't be a dumdum.

(: There are no multiline comments.
```

#### Mostly whitespace insentive
The only exception to this is that (), [], and {} must be on the same line as their antecedents.
Thus things like
```
var x = foo, y = bar
(x, y) = (1, 2)
```
are not ambiguous. The above resolves to a statement that creates two new mutable bindings (x and y), followed by a statement that assigns those same bindings to 1 and 2, respectively.

#### Semicolon optional
Semicolons are only ever needed for removing ambiguity, as with multiple statements on the same line.

#### No more `*` and `&`
To make it easier to tell what you're taking the address of or dereferencing, Ligi uses postfix versions of `*` and `&`.
* To take the address of val: `val.addr`
* To dereference  ptr: `ptr.deref`

Note that these *might* be replaced with `.*` and `.&` in the future.

#### Builtin types
* Integers
  * `.max` and `.min`: (Used on the type itself) Get the max or min value of that type
  * `i[1-256]`: A signed integer of size 1-256 bits
  * `u[1-256]`: An unsigned integer of size 1-256 bits
  * `usize`: An alias for `u{current platform bits}`
    * All integer literals are `usize` by default.
  * `isize`: An alias for `i{current platform bits}`
    * All int literals negated by a unary `-` is `isize` by default.
* `bool`: Always either `true` or `false`
* `char`: A `u32`, intended for representing UTF-32 codepoints.
  * `.isASCII`: Property on all char variables to tell if it fits in a standard ASCII byte.
  * All char literals (`a`, `b`, etc) are `char` by default. A comptime-known `char` may be implicitly castable to
    `u8` (bytes, standard ASCII stuff), so `'a'` can easily implicitly cast to a `u8`.
* `str`: Character string. An alias for `slice const u8`
  * `.len`: The length of the string. All strings in Ligi are null-terminated to preserve compatability with C.
* `undef`: Malleable type. It means that variable takes on the type of the first write that happens to it. All variables are bound with this unless otherwise specified
  ```ligi
  var x: undef
  assert x.@type == usize
  x = 10
  assert x.@type == usize
  ```
  `undef` is used for generic arguments:
  ```
  let add = pure fn a, b: undef -> c:undef = a + b
  _ = add(10, 10)
  _ = add(10.0, 60.0)
  ```
* `untyped`(TODO): Macro type. It stores an **untype**checked subtree of the AST. Can be expanded into the current AST with `$`.
  For example:
  ```
  let subtree: untyped = printf("Hey again", ())
  $subtree
  $subtree
  $subtree
  (: OUTPUT: Hey againHey againHey again
  ```

#### Compound types
* `const <type>`: Create a type that is only ever assigned to once
* `comptime <type>`: Create a type whose value must always be known by compiletime
* `array (<size>, <type>)`: Create an array of a specified type and size
  * `array <type>`: Create an array of a specified type, inferring the size
  * `.len`: Get the length of the array
* `slice <type>`: Create a slice (basically a `struct{ptr,len}` to an arbitrary array)
  * `slice const u8` is the type of a string literal.
  * `.ptr`: Get the pointer to the slice's data
  * `.len`: Get the length of the slice
* `(type1{, type2})`: Tuples. The type of a tuple is simply the tuple of all the member types.
  Thus the type of `(0, 1, 2)` is `(usize, usize, usize)`
  * `.0`, `.1`, etc are used to access the nth element of that tuple.
  * `.@tupLen`, `@tupNth(n)` are used for getting the length of a tuple and the nth element of that tuple, respectively.
    * Useful for generics that take a tuple of arbitrary size.
  * The `.` operator, other than for aformentioned tuple fields, redirects to all members and makes a new tuple of those values.
    ```
    assert ("Hello", "H", "World").len == (5, 1, 5)
    ```
  * Certain operators, such as `in`, are defined to act cartesian-style when going 1:M
    ```
    assert 1 in (4, 2, 0..=3, 10)
    ```
* `struct <block>`: Evaluate `<block>` to create a new struct. More on this later
* `enum <block>`: Evaluate `<block>` to create a new enum/tagged-union.

#### Bind statements are everywhere
All Ligi binds follow the general format `<spec> <location>[: <type_expr>] [= init_expr]`

Ligi uses the following bind specifiers:
  * `let`: Immutable bind. May only ever be assigned to once.
      * C/C++: `const int x = 10;`
      * Ligi: `let x: i32 = 10` or `let x = 10`
      * Ligi lets may be assigned to only once, no matter when that assignment happens, so this also works:
          ```
          let x
          (:...
          x = 10
          ```
          Out of orders like this are to facilitate mutually-recursive types/functions.
  * `var`: Mutable bind. May be reassigned to arbitrarily.
      * C/C++: `int x = 10;`
      * Ligi: `var x: i32 = 10` or `var x = 10`
  * `alias`: Transparent bind. The bind and its target literally *are* the same thing. It can be thought of like a C-preprocessor macro specified within the language itself.
      * C/C++:
          ```
          int x = 10;
          #define y x
          y = 50;
          // x == 50
          ```
      * Ligi:
          ```
          var x = 10
          alias y = x
          y = 50
          assert x == 50
          ```
  * `use`: Syntactic sugar for `alias`.
      * Primarily intended for replacing long this.that.thing paths.
      * Must be used on a pure field access (i.e no operators besides `.` and no init expression)
      * `let x; use x` Is acceptable, but redundant.
      * Normal `alias`: `alias std.HashMap = std.HashMap`
      * With `use`: `use std.HashMap`
      * This also works with struct members:
      ```
      let x: Vec2 (: Assuming Vec2 has 2 members: x and y
      use x.y
      y = 10
      assert x.y == 10
      ```
  * `field`: Declare a memory location inside the current type.
    * C/C++: `struct Foo { int x; };`
    * Ligi: `struct Foo { field x: i32 }`
  * `property`: Declare a C# style property. Reading it invokes its .get, writing it invokes its .set
    * C#: `int Prop { get => this.x; set => this.x = value; }`
    * Ligi: `property prop: i32= [ .get = fn self -> res = self.x, .set = fn self, value {self.x = value} ]`
  * `enum`: Declare a new discriminator in a tagged union. 
    * The type of the bind is what it stores
    * The init expression of the bind is the tag's value (i.e `1`, `2`, etc)

#### Most binds may be shadowed
Thus
```
let x = 10
let x = "Hello"
```
is perfectly valid. `x=10` still lives on the stack, but the symbol `x` now refers to `x="Hello"`.

This is also used to enable function overloading, but that's for later.

`field`, `property`, and `enum` binds, however, may *not* be shadowed.

#### Unary operators
* `not`: Logical not. Only works on `bool`s by default.
* `comptime`: Force the entire subtree to be evaluated at compiletime.
* `pure`: Ensure that the entire subtree depends on nothing outside itself.
  * Used on functions to produce pure functions that give the same result given the same arguments.
  * When used on a block of code, it gives the same result as if `comptime` was used.
* `struct`: Mark the subtree for evaluation as a new struct. Only used on blocks of code.
* `enum`: Mark the subtree for evaluation as a new enum. Only used on blocks of code.
* `const`: Make the target type immutable.
* `slice`: Make a new type that holds a slice of its target, as in `slice const u8` from the `str` before.
* `array`: Make a new type that holds an array
  * For an array of exactly `5` `usize`s: `array (5, usize)`
  * For an array of `usize`s, determining the size from the context: `array usize`
* `inline`: Force the entire subtree to be inlined.
  * Can be used on a function to force that function to always get inlined when called:
    ```
    let add = pure inline fn a, b -> c = a + b
    ```
  * Can be used on a subtree to inline all function calls in that subtree:
    ```
    let x = inline foo(bar)
    (: Or even
    let x = inline {
      let y = 10
      foo(y)
    }
    ```
* `#`: Special unary operator. Turns the symbol it targets into an enum literal, and optionally gets a union initializer immediately after.
    ```
    let Meal = enum {
      (: Each contains a bool to tell whether they were takeout
      enum Breakfast: bool, Lunch: bool, Supper: bool
    }
    let myMeal: Meal = #Supper(true)
    ```
  * If that union tag only holds a struct, you may choose to replace
  ```
  #EnumTag([<struct init>])
  ```
  with
  ```
  #EnumTag[<struct init>]
  ```
* `$`: Special "expansion" operator. It's primarily used for macros
  * When used on a string literal, it turns that string into a symbol. (AKA stropping)
    * To bind `10` to the symbol `pure`: `let $"pure" = 10`
  * When used on a *compiletime-known* *variable*, it can be used to access a field/method with that name
  	```ligi
  	let fieldName = "x"
  	my2DVector.$fieldName = 10
  	assert my2DVector.x == 10
  	```
    * This also works with integers and tuple accesses
    	```ligi
    	let tup = (1, 4, 6)
    	let i = 2
    	assert tup.$i == 6
    	```
  * When used on an `untyped` variable, it expands it into the current scope
  	```ligi
  	let code: untyped = printf("Hello, world!\n", ())
  	$code
  	$code
  	$code
  	(: Printed "Hello, world!" 3 times
  	```
#### Binary operators
Here are all of Ligi's binary operators:
* `+, -, *, /, %, <<, >>, >>>`: All work as expected.
* `==, !=, <, >, <=, >=`: Work as expected.
* `<=>`: Spaceship operator. Returns one of `#Less`, `#Eq`, or `#Greater`. (More on enum literals later)
  * Mainly useful when used in a switch/match-style structure. Unfortunately that's not specified in Ligi yet.
