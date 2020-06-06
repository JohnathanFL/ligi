const std = @import("std");

const ast = @import("ast.zig");

indent: usize,
out: std.fs.File.OutStream,
newline: bool,

pub fn print(node: *ast.Expr, indent: usize) void {
  (@This() {
    .indent = indent,
    .out = std.io.getStdOut().outStream(),
    .newline = true,
  }).pretty(node);
}

fn doIndent(self: @This()) void {
  if(!self.newline) return;
  var i = self.indent;
  _ = self.out.write("\n") catch unreachable;
  while(i > 0) {
    _ = self.out.write("  ") catch unreachable;
    i -= 1;
  }
}

fn write(self: @This(), what: []const u8) void {
  _ = self.out.write(what) catch unreachable;
}

fn fmt(self: @This(), comptime what: []const u8, args: var) void {
  _ = self.out.print(what, args) catch unreachable;
}

fn pretty(self: *@This(), node: *ast.Expr) void {
  self.indent += 1;
  defer self.indent -= 1;
  self.doIndent();
  switch(node.*) {
    .NOP => _ = self.write("NOP"),
    .Word => |w| _ = self.fmt("$\"{}\"", .{w}),
    .Str => |s| _ = self.fmt("\"{}\"", .{s}),
    .EnumLit => |e| {
      self.fmt("#{}", .{e.tag});
      if(e.inner) |inner| {
        self.write(":");
        self.pretty(inner);
      }
    },
    .Block => |block| {
      if(block.label) |lab| self.write(lab);
      self.write("{");

      for(block.body.items) |stmt| self.pretty(stmt);
      
      self.doIndent();
      self.write("}");
    },
    .Bind => |bind| {
      if(bind.using) { self.write("using "); }
      else { self.fmt("{} ", .{ @tagName(bind.level) }); }
      self.fmt("{} ", .{@tagName(bind.op)});
      //self.indent += 1; defer self.indent -= 1;
      for(bind.locs.items) |loc| {
        self.prettyLoc(loc.loc);
        if(loc.init) |init| {
          self.write(" =");
          self.pretty(init);
        }
      }
    },
    .Call => |call| {
      self.write(@tagName(call.op));
      self.indent += 1; defer self.indent -= 1;
      for(call.args.items) |item| self.pretty(item);
    },
    .Tuple => |tup| {
      self.write("Tuple: ");
      for(tup.vals.items) |item| self.pretty(item);
    },
    .Func => |func| {
      self.write("Func:");
      self.indent += 1;
      for(func.args.items) |arg| {
        self.doIndent();
        self.prettyLoc(arg);
      }
      self.doIndent();
      self.write("returns ");
      self.prettyLoc(func.ret);
      if(func.body) |body| {
        self.doIndent();
        self.write("=>");
        self.pretty(body);
      }
      self.indent -= 1;
    },
    else => self.fmt("UNIMPLEMENTED: {}", .{@tagName(node.*)}),
  }
}

fn prettyLoc(self: *@This(), loc: ast.BindLoc) void {
  switch(loc) {
    .Tuple => |tup| {
      self.write("(");
      defer { self.doIndent(); self.write(")"); }
      
      self.indent += 1;
      defer self.indent -= 1;

      for(tup.locs.items) |child_loc| {
        self.doIndent();
        self.prettyLoc(child_loc);
      }
      if(tup.ty) |ty| {
        self.write(":");
        self.pretty(ty);
      }
    },
    .Named => |n| {
      self.fmt("$\"{}\"", .{n.name});
      if(n.ty) |ty| {
        self.write(":");
        self.pretty(ty);
      }
    }
  }
}
