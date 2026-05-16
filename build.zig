const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const raylib_dep = b.dependency("raylib_zig", .{
        .target = target,
        .optimize = optimize,
    });

    const raylib = raylib_dep.module("raylib");
    // const raylib_artifact = raylib_dep.artifact("raylib");
    // const raygui = raylib_dep.module("raygui");

    const search_visualizer_module = b.createModule(.{
        .root_source_file = b.path("src/search_visualizer.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{
            .{
                .name = "raylib",
                .module = raylib,
            },
        },
        // .link_libc = true,
    });

    // search_visualizer_module.linkLibrary(raylib_artifact);

    const aposeij_root_module = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{},
    });

    var exit_code: u8 = undefined;
    const git_commit = b.runAllowFail(&.{ "git", "rev-parse", "--short", "HEAD" }, &exit_code, .ignore) catch "unknown";


    const enable_search_tree_logging = b.option(bool, "enable_search_tree_logging", "Enable search tree logging") orelse false;

    const aposeij_options = b.addOptions();
    // TODO use hash for clean tree, "dev" + hash for tree with changes, version for tagget commit
    aposeij_options.addOption([]const u8, "aposeij_version_string", b.fmt("Aposeij commit {s}", .{git_commit}));
    aposeij_options.addOption(bool, "enable_search_tree_logging", enable_search_tree_logging);

    aposeij_root_module.addOptions("options", aposeij_options);
    search_visualizer_module.addOptions("options", aposeij_options);

    const aposeij = b.addExecutable(.{
        .name = "aposeij",
        .root_module = aposeij_root_module,
    });

    const search_vis = b.addExecutable(.{
        .name = "search_vis",
        .root_module = search_visualizer_module,
        // new glibc version broke zig's linker ((
        .use_llvm = true,
        .use_lld = true,
    });

    b.installArtifact(aposeij);

    const run_cmd = b.addRunArtifact(aposeij);

    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_vis_cmd = b.addRunArtifact(search_vis);

    run_vis_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_vis_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run uci engine");
    run_step.dependOn(&run_cmd.step);

    const run_vis_step = b.step("search_vis", "Run search visualizer");
    run_vis_step.dependOn(&run_vis_cmd.step);

    const exe_unit_tests = b.addTest(.{
        .root_module = aposeij_root_module,
    });

    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_exe_unit_tests.step);
}
