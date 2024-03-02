const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const argify_mod = b.addModule("argify", .{
        .optimize = optimize,
        .target = target,
        .root_source_file = .{ .path = "argify.zig" },
        .strip = b.option(bool, "strip", "Strip the module"),
    });

    const exe = b.addExecutable(.{
        .root_source_file = .{ .path = "example.zig" },
        .target = target,
        .optimize = optimize,
        .name = "example",
    });
    exe.root_module.addImport("argify", argify_mod);

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);

    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const lib = b.addStaticLibrary(.{
        .name = "argify",
        .root_source_file = .{ .path = "argify.zig" },
        .target = target,
        .optimize = optimize,
    });

    b.installArtifact(lib);

    const lib_unit_tests = b.addTest(.{
        .root_source_file = .{ .path = "argify.zig" },
        .target = target,
        .optimize = optimize,
    });

    const run_lib_unit_tests = b.addRunArtifact(lib_unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_lib_unit_tests.step);
}