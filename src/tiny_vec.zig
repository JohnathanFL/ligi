const std = @import("std");

pub fn TinyVec(comptime T: type, comptime numStack: usize) type {
    return struct {
        pub const NumStack = numStack;
        pub const Elem = T;
        onStack: [NumStack]Elem,
        capacity: usize,
        len: usize,
        spill: std.ArrayList(Elem),
        
        const Errors = error {
            CantAlloc, OutOfBounds,
        };

        pub fn init(alloc: *std.mem.Allocator) @This() {
            return @This() {
                .onStack = undefined,
                .capacity = NumStack,
                .len = 0,
                .spill = std.ArrayList(Elem).init(alloc),
            };
        }

        pub fn at(self: *@This(), index: usize) ?*Elem {
            if (index < self.len) {
                if (index < NumStack) {
                    return &self.onStack[index];
                } else {
                    return &self.spill.toSlice()[index - NumStack];
                }
            } else {
                return null;
            }
        }

        pub fn append(self: *@This(), elem: Elem) !void {
            if (self.len < NumStack) {
                self.onStack[self.len] = elem;
                self.len += 1;
            } else {
                try self.spill.append(elem);
                self.len += 1;
            }
        }

        pub fn pop(self: *@This()) ?Elem {
            if(self.len == 0) {
                return null;
            } else if (self.len <= NumStack) {
                self.len -= 1;
                return self.onStack[self.len];
            } else {
                if (self.spill.len > 0) {
                    self.len -= 1;
                }
                return self.spill.popOrNull();
            }
        }

        /// Note: ar[i >= len] not guarenteed to be initialized
        pub fn stackToSlice(self: *@This()) []Elem {
            return self.onStack;
        }

        pub fn spillToSlice(self: *This()) []Elem {
            return self.spill.toSlice();
        }


    };
}

test "basic" {
    var x = TinyVec(u32, 2).init(std.heap.direct_allocator);
    var vals = [_]u32{42, 43, 44, 45};
    for (vals) |val| {
        try x.append(val);
    }

    {var i: usize = 1;
    while (i <= vals.len) : (i += 1) {
        std.testing.expect(vals[vals.len - i] == x.at(i).?.*);
    }}

    {var i: usize = 1;
    while (i <= vals.len) : (i += 1) {
        var val = x.pop().?;
        // std.debug.warn("val was: {}", val);
        std.testing.expect(vals[vals.len - i] == val);
    }}
}