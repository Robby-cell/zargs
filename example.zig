const std = @import("std");
const argify = @import("zargs");

pub fn main() !void {
    const Options = struct {
        foo: bool = false,
        bar: ?[]const u8 = null,

        pub const shorthands = .{
            .f = "foo",
            .b = "bar",
        };
    };

    var opts = try argify.currentProcParse(Options, std.heap.page_allocator);
    defer opts.deinit();

    inline for (@typeInfo(@TypeOf(opts.options)).Struct.fields) |field| {
        std.debug.print("\t{s} = {any}\n", .{ field.name, @field(opts.options, field.name) });
    }
}
