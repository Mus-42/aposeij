const std = @import("std");
const rl = @import("raylib");

const log = @import("search_log.zig");

pub fn main(init: std.process.Init) !void {
    const alloc = init.gpa;
    const io = init.io;
    
    var search_record = try log.readSearchRecord(io, alloc, "logs/00129589-000007.slog");
    defer search_record.deinit(alloc);

    var per_ply: [128]u32 = undefined;

    rl.setTraceLogLevel(.warning);
    rl.initWindow(1200, 720, "raylib-zig [core] example - basic window");
    defer rl.closeWindow();

    rl.setTargetFPS(60);

    while (!rl.windowShouldClose()) {
        rl.beginDrawing();
        defer rl.endDrawing();

        rl.clearBackground(.white);

        var ply: u32 = 0;
        per_ply = @splat(0);

        // TODO this is useless. Figure one a better way or abandon it entirely

        for (search_record.entries) |entry| {
            switch (entry) {
                .enter_node => |enter| {
                    per_ply[ply+1] = @max(per_ply[ply+1], per_ply[ply]);

                    const a = per_ply[ply]; 
                    const b = per_ply[ply+1];

                    const v: u8 = @truncate(@abs(enter.a) / 256);
                    const v2: u8 = @truncate(@abs(enter.b) / 256);
                    const col: rl.Color = .{
                        .r = v,
                        .g = v2,
                        .b = 0,
                        .a = 255,
                    };

                    rl.drawLine(
                        @intCast(10 + a),
                        @intCast(ply * 40), 
                        @intCast(10 + b),
                        @intCast((ply+1) * 40),
                        col,
                    );

                    ply += 1;
                },
                .leave_node => |leave| {
                    _ = leave;
                    per_ply[ply] += 1;
                    per_ply[ply] = @max(per_ply[ply], per_ply[ply+1]);
                    ply -= 1;
                },
            }
        }
    }
}
