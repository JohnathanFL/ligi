const std = @import("std");

const ast = @import("ast.zig");

indent: usize,
out: std.fs.File.OutStream,
newline: bool,

pub fn print(node: *ast.Expr, indent: usize) void {
    (@This(){
        .indent = indent,
        .out = std.io.getStdOut().outStream(),
        .newline = true,
    }).pretty(node);
}

fn doIndent(self: @This()) void {
    if (!self.newline) return;
    var i = self.indent;
    _ = self.out.write("\n") catch unreachable;
    while (i > 0) {
        _ = self.out.write("  ") catch unreachable;
        i -= 1;
    }
}

fn write(self: @This(), what: []const u8) void {
    _ = self.out.write(what) catch unreachable;
}
fn iwrite(self: @This(), what: []const u8) void {
    self.doIndent();
    self.write(what);
}

fn fmt(self: @This(), comptime what: []const u8, args: var) void {
    _ = self.out.print(what, args) catch unreachable;
}
fn ifmt(self: @This(), comptime what: []const u8, args: var) void {
    self.doIndent();
    self.fmt(what, args);
}

fn pretty(self: *@This(), node: *ast.Expr) void {
    self.indent += 1;
    defer self.indent -= 1;
    self.doIndent();
    switch (node.*) {
        .NOP => _ = self.write("NOP"),
        .Word => |w| _ = self.fmt("$\"{}\"", .{w}),
        .Str => |s| _ = self.fmt("\"{}\"", .{s}),
        .Assert => |a| {
            self.write("Assert");
            self.pretty(a.expr);
        },
        .EnumLit => |e| {
            self.fmt("{}", .{e.label});
            if (e.inner) |inner| {
                self.write(":");
                self.pretty(inner);
            }
        },
        .Block => |block| {
            if (block.label) |lab| self.write(lab);
            self.write("{");
            for (block.body.items) |stmt| self.pretty(stmt);
            self.iwrite("}");
        },
        .Bind => |bind| {
            if (bind.using) {
                self.write("using ");
            } else {
                self.fmt("{} ", .{@tagName(bind.level)});
            }
            self.fmt("{} ", .{@tagName(bind.op)});
            self.indent += 1;
            defer self.indent -= 1;
            for (bind.locs.items) |loc| {
                self.doIndent();
                self.prettyLoc(loc.loc);
                if (loc.init) |init| {
                    self.write(" =");
                    self.pretty(init);
                }
            }
        },
        .Call => |call| {
            self.write(@tagName(call.op));
            self.indent += 1;
            defer self.indent -= 1;
            for (call.args.items) |item| self.pretty(item);
        },
        .Tuple => |tup| {
            self.write("Tuple: ");
            for (tup.vals.items) |item| self.pretty(item);
        },
        .If => |ifs| {
            self.write("If");
            for (ifs.arms.items) |arm| {
                self.pretty(arm.cond);
                self.indent += 1;
                defer self.indent -= 1;
                if (arm.capt) |loc| {
                    self.iwrite("->");
                    self.prettyLoc(loc);
                }
                self.iwrite("=>");
                self.pretty(arm.then);
                self.prettyDefFin(ifs.default, ifs.finally);
            }
        },
        .When => |when| {
            self.write("When");
            self.pretty(when.expr);
            self.indent += 1;
            defer self.indent -= 1;
            for (when.arms.items) |arm| {
                self.ifmt("Is {}", .{@tagName(arm.op)});
                self.pretty(arm.val);

                if (arm.capt) |loc| {
                    self.iwrite("Capturing ");
                    self.prettyLoc(loc);
                }
                self.iwrite("=>");
                self.pretty(arm.then);
            }
            self.prettyDefFin(when.default, when.finally);
        },
        .Func => |func| {
            self.write("Func:");
            self.indent += 1;
            for (func.args.items) |arg| {
                self.doIndent();
                self.prettyLoc(arg);
            }
            self.iwrite("returns ");
            self.prettyLoc(func.ret);
            if (func.body) |body| {
                self.iwrite("=>");
                self.pretty(body);
            }
            self.indent -= 1;
        },
        .Loop => |loop| {
            self.write(if (loop.op == .For) "For:" else if (loop.op == .While) "While:" else "Loop:");
            self.indent += 1;
            defer self.indent -= 1;
            if (loop.expr) |expr| self.pretty(expr);
            if (loop.capt) |capt| {
                self.iwrite("Capture: ");
                self.prettyLoc(capt);
            }
            if (loop.counter) |counter| {
                self.iwrite("Counter: ");
                self.prettyLoc(counter);
            }
            self.iwrite("Do:");
            self.pretty(loop.body);
            self.prettyDefFin(null, loop.finally);
        },
        .Struct => |lit| {
            self.write("[");
            defer self.iwrite("]");

            self.indent += 1;
            defer self.indent -= 1;

            for (lit.fields.items) |field| {
                self.doIndent();
                self.prettyLoc(field.loc);
                if (field.val) |val| {
                    self.write(" = ");
                    self.pretty(val);
                }
            }
        },
        .Break => |b| {
            self.write("Break");
            if (b.from) |lab| self.fmt(" {}", .{lab});
            if (b.with) |val| {
                self.pretty(val);
            }
        },
        else => self.fmt("UNIMPLEMENTED: {}", .{@tagName(node.*)}),
    }
}

// For anything that can have else/finally
// If one is not applicable, just pass null
fn prettyDefFin(self: *@This(), default: ?*ast.Expr, finally: ?*ast.Expr) void {
    if (default) |d| {
        self.iwrite("Else");
        self.pretty(d);
    }
    if (finally) |f| {
        self.iwrite("Finally");
        self.pretty(f);
    }
}

fn prettyLoc(self: *@This(), loc: ast.BindLoc) void {
    switch (loc) {
        .Tuple => |tup| {
            self.write("(");
            defer self.iwrite(":)");

            self.indent += 1;
            defer self.indent -= 1;

            for (tup.locs.items) |child_loc| {
                self.doIndent();
                self.prettyLoc(child_loc);
            }
            if (tup.ty) |ty| {
                self.write(":");
                self.pretty(ty);
            }
        },
        .Named => |n| {
            self.fmt("$\"{}\"", .{n.name});
            if (n.ty) |ty| {
                self.write(":");
                self.pretty(ty);
            }
        },
    }
}
