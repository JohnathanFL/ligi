# Ligi - *Just don't be a dumdum*

This is the home for Ligi: an attempt at a modern C replacement.


## Small taste
```
-- I'm a comment

-- Expression oriented
-- Optionally enforce functional purity
-- Statically typed with inferred types where needed
-- Result location sematics (always)
let fib = pure fn n: usize -> f = when n
  is in (0, 1) => n
  else => fib(n - 1) + fib(n - 2)

let Vec2 = struct {
  field x: f32, y: f32
  
  let add = pure fn lhs: @This, rhs: @This -> res = [ .x = lhs.x + rhs.x, .y = lhs.y + rhs.y ]
}
let a = [:Vec2: .x = 1, .y = 2], b = [:Vec2: .x = 4, .y = 5]

-- Functions which take the current struct as an arg can be called as methods
let c = a.add(b)
-- Built in assertions
-- GLSL-styled "swizzling" support
assert c.(x, y) == (5, 7)

-- 2D Vector (alternative distinct-tuple version)
let Vec2 = (f32, f32) + struct {}
let a = (:Vec2: 1, 2), b = (:Vec2: 4, 5)
-- All tuples have arithmetic operators defined for 1:n and n:n
let c = a + b

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
