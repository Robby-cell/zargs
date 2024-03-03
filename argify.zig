const std = @import("std");

pub fn Result(comptime T: type) type {
    if (@typeInfo(T) != .Struct) {
        @compileError("Arg template must be a struct.");
    }

    return struct {
        arena: std.heap.ArenaAllocator,
        options: T,

        raw_index: ?usize = null,
        exe_name: ?[:0]const u8,

        pub fn deinit(self: @This()) void {
            self.arena.deinit();
        }
    };
}

pub fn currentProcParse(comptime T: type, allocator: std.mem.Allocator) anyerror!Result(T) {
    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();

    const exe_name = args.next() orelse {
        return error.NoExeName;
    };

    var result = try parseArgs(T, &args, allocator);
    result.exe_name = exe_name;

    return result;
}

pub fn parseArgs(comptime T: type, arg_iterator: anytype, allocator: std.mem.Allocator) anyerror!Result(T) {
    var result: Result(T) = .{
        .arena = std.heap.ArenaAllocator.init(allocator),
        .options = .{},
        .exe_name = null,
    };
    errdefer result.arena.deinit();

    const result_allocator = result.arena.allocator();

    var arg_list = std.ArrayList([:0]const u8).init(allocator);
    defer arg_list.deinit();

    var err: ?anyerror = null;

    const Pair = struct {
        name: []const u8,
        value: ?[]const u8,
    };

    while (arg_iterator.next()) |arg| {
        if (std.mem.startsWith(u8, arg, "--")) {
            const pair: Pair = if (std.mem.indexOf(u8, arg, "=")) |idx|
                .{ .name = arg[2..idx], .value = arg[idx + 1 ..] }
            else
                .{ .name = arg[2..], .value = null };

            var found = false;
            inline for (std.meta.fields(T)) |field| {
                if (std.mem.eql(u8, pair.name, field.name)) {
                    try parseOption(T, result_allocator, &result.options, arg_iterator, &err, field.name, pair.value);
                    found = true;
                    break;
                }
            }

            if (!found) {
                err = error.EncounteredUnknownArgument;
            }
        } else if (@hasDecl(T, "shorthands") and std.mem.startsWith(u8, arg, "-")) {
            const pair: Pair = if (std.mem.indexOf(u8, arg, "=")) |idx|
                .{ .name = arg[1..idx], .value = arg[idx + 1 ..] }
            else
                .{ .name = arg[1..], .value = null };

            var found = false;
            inline for (std.meta.fields(@TypeOf(T.shorthands))) |field| {
                if (std.mem.eql(u8, pair.name, field.name)) {
                    const true_name = @field(T.shorthands, field.name);
                    try parseOption(T, result_allocator, &result.options, arg_iterator, &err, true_name, pair.value);
                    found = true;
                    break;
                }
            }
        } else {
            return error.InvalidArg;
        }
    }
    return result;
}

test "args" {
    const Options = struct {
        run: bool = false,
        dummy_bool: bool = false,
        name: ?[]const u8 = null,
        count: i32 = 0,
        float: f32 = 0.0,
    };
    var iterator = std.mem.splitScalar(u8, "exe_name|--run|--name=name stuff|--count=123|--float=12.34", '|');
    const exe_name = iterator.next().?;

    var result = try parseArgs(Options, &iterator, std.testing.allocator);
    defer result.deinit();

    result.exe_name = @ptrCast(exe_name);
    const opts = result.options;

    const expect = std.testing.expect;

    const name: [:0]const u8 = "name stuff";
    try expect(opts.run);
    try expect(!opts.dummy_bool);
    try expect(std.mem.eql(u8, opts.name.?, name));
    try expect(opts.count == @as(i32, 123));
    try expect(opts.float == (@as(f32, 12.34)));
}

fn parseOption(
    comptime T: type,
    allocator: std.mem.Allocator,
    options: *T,
    args: anytype,
    err: *?anyerror,
    comptime field_name: []const u8,
    value: ?[]const u8,
) anyerror!void {
    const FieldType = @TypeOf(@field(options, field_name));

    const final_value = if (value) |val| blk: {
        break :blk try allocator.dupeZ(u8, val);
    } else if (required(FieldType)) blk: {
        const val = args.next();
        if (val == null or std.mem.eql(u8, val.?, "--")) {
            err.* = error.MissingArg;
            // error handling
            return;
        }

        break :blk try allocator.dupeZ(u8, val.?);
    } else "";

    @field(options, field_name) = convertArgValue(FieldType, allocator, final_value) catch |er| {
        err.* = er;
        return;
    };
}

fn convertArgValue(comptime T: type, allocator: std.mem.Allocator, input: []const u8) anyerror!T {
    const info = @typeInfo(T);
    return switch (info) {
        .Optional => |opt| blk: {
            const val: T = try convertArgValue(opt.child, allocator, input);
            break :blk val;
        },
        .Bool => if (input.len > 0)
            parseBool(input)
        else
            true,
        .Int => parseInt(T, input),
        .Float => std.fmt.parseFloat(T, input),
        .Enum => if (@hasDecl(T, "parse"))
            T.parse(input)
        else
            std.meta.stringToEnum(T, input) orelse return error.InvalidEnumValue,

        .Struct, .Union => if (@hasDecl(T, "parse"))
            T.parse(input)
        else
            @compileError(@typeName(T) ++ " has no available `fn parse([]const u8) !" ++ @typeName(T) ++ "`"),
        .Pointer => |ptr| switch (ptr.size) {
            .Slice => blk: {
                if (ptr.child != u8) {
                    @compileError(@typeName(T) ++ " is not a supported type, only slices of u8 are supported.");
                }

                if (comptime std.meta.sentinel(T)) |sentinel| {
                    const data = try allocator.alloc(u8, input.len + 1);
                    @memcpy(data[0..input.len], input);
                    data[input.len] = sentinel;

                    break :blk data;
                }

                break :blk input;
            },

            else => @compileError(@typeName(T) ++ " is not a supported pointer type."),
        },
        else => @compileError(@typeName(T) ++ " is not a supported type."),
    };
}

test "convert tests" {
    const cases = .{
        "words",

        "true",
        "false",

        "123",
        "-127",
        "12.34",
        "-22.77",

        "variant",
        "parsedFromFunc",

        "a ptr parse",
    };

    const Enum1 = enum { variant, other };

    const Enum2 = enum {
        variant,
        other,

        fn parse(input: []const u8) @This() {
            return if (std.mem.eql(u8, input, "parsedFromFunc"))
                return .other
            else
                @panic("invalid enum");
        }
    };

    const Type: type = struct {
        ?[]const u8,

        bool,
        bool,

        u32,
        i32,
        f32,
        f64,

        Enum1,
        Enum2,

        []const u8,
    };

    const results: Type = .{
        "words",

        true,
        false,

        123,
        -127,
        12.34,
        -22.77,

        Enum1.variant,
        Enum2.other,

        "a ptr parse",
    };

    var answers: Type = undefined;
    inline for (@typeInfo(Type).Struct.fields) |field| {
        @field(answers, field.name) = try convertArgValue(field.type, std.testing.allocator, @field(cases, field.name));
    }

    const deeplyEqual = struct {
        fn callback(lhs: anytype, rhs: @TypeOf(lhs)) bool {
            return switch (@typeInfo(@TypeOf(lhs))) {
                .Struct, .Union => std.meta.eql(lhs, rhs),

                // we wont accept any pointer in parsing that isnt u8
                .Pointer => std.mem.eql(u8, lhs, rhs),

                .Int => lhs == rhs,
                .Float => lhs == rhs,
                .Bool => lhs == rhs,
                .Enum => rhs == rhs,

                .Optional => blk: {
                    if (lhs == null and rhs == null) break :blk true;
                    if (lhs == null or rhs == null) break :blk false;

                    break :blk callback(lhs.?, rhs.?);
                },

                else => |T| @compileError(@typeName(@Type(T)) ++ " can't be compared."),
            };
        }
    }.callback;

    inline for (@typeInfo(@TypeOf(results)).Struct.fields) |field| {
        try std.testing.expect(deeplyEqual(@field(results, field.name), @field(answers, field.name)));
    }
}

fn parseBool(input: []const u8) !bool {
    const eql = std.mem.eql;
    return if (eql(u8, input, "true"))
        true
    else if (eql(u8, input, "false"))
        false
    else
        error.NotValidBoolean;
}

fn parseInt(comptime T: type, input: []const u8) !T {
    var buf = input;
    var multiplier: T = 1;

    if (buf.len != 0) {
        var base1024 = false;
        if (std.ascii.toLower(buf[buf.len - 1]) == 'i') {
            buf.len -= 1;
            base1024 = true;
        }
        if (buf.len != 0) {
            const pow: u3 = switch (buf[buf.len - 1]) {
                'k', 'K' => 1, // kilo
                'm', 'M' => 2, // mega
                'g', 'G' => 3, // giga
                't', 'T' => 4, // tera
                'p', 'P' => 5, // peta
                else => 0,
            };

            if (pow != 0) {
                buf.len -= 1;

                if (comptime std.math.maxInt(T) < 1024) {
                    return error.OverFlow;
                }
                const base: T = if (base1024) 1024 else 1000;
                multiplier = try std.math.powi(T, base, @as(T, @intCast(pow)));
            }
        }
    }

    const ret: T = switch (@typeInfo(T).Int.signedness) {
        .signed => try std.fmt.parseInt(T, buf, 0),
        .unsigned => try std.fmt.parseUnsigned(T, buf, 0),
    };

    return std.math.mul(T, ret, multiplier);
}

fn required(comptime T: type) bool {
    const FN = struct {
        fn callback(comptime Type: type) bool {
            if (Type == []const u8) {
                return true;
            }
            return switch (@as(std.builtin.TypeId, @typeInfo(Type))) {
                .Int, .Float, .Enum => true,
                .Bool => false,
                .Struct, .Union => true,
                .Pointer => true,
                else => @compileError(@typeName(Type) ++ " is not a supported arg type."),
            };
        }
    }.callback;

    const info = @typeInfo(T);
    return if (info == .Optional)
        FN(info.Optional.child)
    else
        FN(T);
}
