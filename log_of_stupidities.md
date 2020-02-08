# Log of Stupidity

A log of incredibly stupid design decisions I ended up making.
Note that this log only started 2020-02-07


* Moving to an OOP styled AST...while still storing a cmd:Command in *every* class. It was useful for BinExpr/UnaryExpr/etc, but completely redundant in everything else. It was also a mess to get nice conversions between Tags and Commands
  * Moved to separate types of Cmds, each of which directly maps to Tags to conversion is a simple .<X>Cmd call
