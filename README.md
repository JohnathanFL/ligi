# Ligi - *Just don't be a dumdum*

This is the home for Ligi: an attempt at a modern C replacement.

It should be noted that Ligi takes **heavy** inspiration from Zig and its ideas.
It wouldn't be wrong to describe Ligi as a language that wants to be Zig, but doesn't
like the explicitness and inflexibility of Zig.

This language is still in a heavy state of flux. Quite a few proposals may conflict with each
other. I do, however, try to keep the samples/ directory up to date.

It should be noted that the comments used to be `(:` to start and `:)` to close, so don't
be surprised if you see a lot of `:)` in the code. :)

## Short overview
- Expression-oriented, semicolon-optional, and (almost) whitespace-insensitive
- Explicit allocators and memory management. No RAII *as such*.
- Heavy focus on compiletime features, including the ability to create new types programmatically.
- Heavy focus on making syntax sugary but readable. Things should be close to an English sentence
  in readability.

## Principoj de Ligi
* Code should flow in both reading *and* writing.
  * Code should read like a novel, where each page foreshadows the next.
* Big honkin' errors are better than tricksy little bugs.
* Make it easy to do the right thing.
  * If you want to shoot yourself in the foot, who am I to stop you?

## Small taste
```
-- I'm a comment

-- Expression oriented
-- Optionally enforce functional purity
-- Statically typed, but inferred types where needed
-- Result location sematics everywhere but void functions
let fib = pure fn n: usize -> f = when n
  -- `in` operator for ranges. Can also work with tuples of ranges to create discontinuous ranges
  is in (0, 1) => n
  -- Control structures can use either => for a simple expression or a full block if desired
  else { fib(n - 1) + fib(n - 2) }

-- Types are expressions. 
-- Struct is an operator, {} is a normal ol' block of code. No special syntax inside.
let Vec2 = struct {
  -- `field` is a bind specifier like `let` or `var`. It binds a new field. Obviously.
  field x: f32, y: f32
  
  -- `let` and `var` binds in a type become statics in the type's "namespace".
  let add = pure fn lhs: @This, rhs: @This -> res = [ .x = lhs.x + rhs.x, .y = lhs.y + rhs.y ]
}
-- Arrays and structs are initialized with []
let a = [:Vec2: .x = 1, .y = 2], b = [:Vec2: .x = 4, .y = 5]

-- Functions which take the current struct as an arg can be called as methods
let c = a.add(b)
-- Built in assertions
-- GLSL-styled "swizzling" support
assert c.(x,y) == (c.x, c.y)
assert c.(x, y) == (5, 7)

-- 2D Vector (alternative distinct-tuple version)
-- Adding a tuple-type to a struct makes it a "compound tuple". (i.e "distinct from normal")
let Vec2 = (f32, f32) + struct {
  -- A property can have getters, setters, and more
  -- `property` with only one argument is just a getter
  pub let len = property pure fn (x, y): @This -> length = @sqrt(x*x + y*y)
}
-- Tuples can have a :type: specifier at the beginning just like arrays and structs
let a = (:Vec2: 1, 2), b = (:Vec2: 4, 5)
-- All tuples have arithmetic operators defined for 1:n and n:n, so we
-- don't need to explicitly make them here.
let c = a + b
assert c.len == @sqrt(5*5 + 7*7)

-- Pipelines (pseudo-UFCS)
-- Void functions can leave off result locations with => instead
let format_vec2 = fn v: Vec2 => printf("<{}, {}>", v.(x, y))
c::format_vec2() -- same as format_vec2(c)
```


## Status

Ligi is still very much a work-in-progress. There's a lexer and parser for the current 
version of the language, along with a parser, but the language's exact spec is still
up in the air at this point.

A lot of the samples/feature proposals are also very rough at the moment. Keep in mind that
many of them were written by the sleep-deprived brain of a college student.

* Lexer: Done*
* Parser: Done*
* Pretty Printer: WIP
* Comptime Interpreter: WIP
* Compiler: TODO
