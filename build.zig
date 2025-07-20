const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    
    const board = b.createModule(.{
        .root_source_file = b.path("src/board.zig"),
        .target = target,
        .optimize = optimize,
    });

    const perft_root_module = b.createModule(.{
        .root_source_file = b.path("src/perft.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{ .{ .name = "board", .module = board } }
    });

    const perft = b.addExecutable(.{
        .name = "perft",
        .root_module = perft_root_module,
    });

    b.installArtifact(perft);

    const aposeij_root_module = b.createModule(.{
        .root_source_file = b.path("src/uci.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{ .{ .name = "board", .module = board } }
    });

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

    const run_perft_cmd = b.addRunArtifact(perft);
    run_perft_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_perft_cmd.addArgs(args);
    }

    const run_perft_step = b.step("perft", "Run performance test");
    run_perft_step.dependOn(&run_perft_cmd.step);

    const exe_unit_tests = b.addTest(.{
        .root_module = aposeij_root_module,
    });

    const perft_unit_tests = b.addTest(.{
        .root_module = perft_root_module,
    });

    const board_unit_tests = b.addTest(.{
        .root_module = board,
    });

    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);
    const run_perft_unit_tests = b.addRunArtifact(perft_unit_tests);
    const run_board_unit_tests = b.addRunArtifact(board_unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_exe_unit_tests.step);
    test_step.dependOn(&run_perft_unit_tests.step);
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
