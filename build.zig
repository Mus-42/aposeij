const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    
    const board = b.createModule(.{
        .root_source_file = b.path("src/board.zig"),
        .target = target,
        .optimize = optimize,
    });

    const aposeij_root_module = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{ .{ .name = "board", .module = board } }
    });

    var exit_code: u8 = undefined;
    const git_commit = b.runAllowFail(&.{"git", "rev-parse", "--short", "HEAD"}, &exit_code, .Ignore) catch "unknown";

    const aposeij_options = b.addOptions();
    // TODO use hash for clean tree, "dev" + hash for tree with changes, version for tagget commit
    aposeij_options.addOption([]const u8, "aposeij_version_string", b.fmt("Aposeij commit {s}", .{git_commit}));
    aposeij_root_module.addOptions("options", aposeij_options);

    const aposeij = b.addExecutable(.{
        .name = "aposeij",
        .root_module = aposeij_root_module
    });

    b.installArtifact(aposeij);

    const run_cmd = b.addRunArtifact(aposeij);

    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run uci engine");
    run_step.dependOn(&run_cmd.step);

    const exe_unit_tests = b.addTest(.{
        .root_module = aposeij_root_module,
    });

    const board_unit_tests = b.addTest(.{
        .root_module = board,
    });

    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);
    const run_board_unit_tests = b.addRunArtifact(board_unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_exe_unit_tests.step);
    test_step.dependOn(&run_board_unit_tests.step);

    const perft_test = b.addExecutable(.{
        .name = "perft_test",
        .root_module = b.createModule(.{
            .root_source_file = b.path("test/perft.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{ .{ .name = "board", .module = board } }
        }),
    });

    const run_perft_test_cmd = b.addRunArtifact(perft_test);

    const perft_test_step = b.step("perft_test", "Run perft (movegen) tests");
    perft_test_step.dependOn(&run_perft_test_cmd.step);
}

