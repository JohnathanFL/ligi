# Plans for Parsing Ligi

Note that this document was only started on 01/10/20, which is *after* I finished pretty much all of the parser.

## General
Parsing will be done with a simple recursive-descent parser. I've done my best to design the language such that ll(1) is
always sufficient to disamgiguate which path to take. (`{}` is always a block, `()` is always a list of values, etc)

## Function calls
I'll be parsing function calls as an attribute of certain members of the AST rather than putting them around the arguments. Thus `std.io.printf("Hello")` is represented in the AST as
```
Swizzle{Swizzle{std, io}, printf{call: ("Hello")}}
```
rather than
```
Call{Swizzle{Swizzle{std, io}, printf}, "Hello"}
```
This is to enable parsing expressions like the following:
```
foo.(bar.baz.call1(), call2(), call3())
```
Normally to use that snippet's AST to get to the access path, we'd have to go into 1+ levels of Calls to get to the name of each function (like if we called the result of the call).

I **may** change the language to disallow anything but straight atoms, `()`s, and `.`s inside swizzles. That remains to be seen 

### Experimental addition: Implicit Method Chaining
Possible new change for Ligi is allowing the following:
```
foo.(
  .func1(args),
  .func2(args),
  .func3(args),
)
```
as an alternative to requiring functions to be written such that they return a pointer to their calling object.
Thus method chaining is essentially cemented into the language's syntax, and `.` can essentially be used as a unary operator in the context of swizzling.
