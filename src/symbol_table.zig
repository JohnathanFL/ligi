
usingnamespace @import("node.zig");

/// Exist "outside" the ast.
/// For example, a finally block and a while block share a single symbol table ancestor,
/// which then points back to the enclosing block. This allows us to share the loop counter in both.
/// In short, think of it like a pseudo-hierarchical linked list, rather than an absolute tree,
/// so you can have multiple symbol tables on the same "level" of the program, allowing for
/// binding shadowing.
pub const SymbolTable = struct {
    /// The last symbol table to be defined
    prev: ?*SymbolTable,
    /// The symbol table that gets defined after this one
    next: ?*SymbolTable,
    /// Symbol tables that branch out from this one.
    enclosing: *Block,
    /// bind.loc -> bind
    binds: StringHashMap(*Bind),

    pub fn get(self: *const SymbolTable, symbol: []const u8) ?*Bind {
        if (self.binds.get(symbol)) |bind| {
            return bind.value;
        } else {
            if(prev.parent) |parent| {
              return parent.get(symbol);
            } else {
              return null;
            }
        }
    }

    pub fn build(block: *Block) 
};
