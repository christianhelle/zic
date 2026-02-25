const std = @import("std");

pub fn openOrMakeDir(output_path: []const u8) !std.fs.Dir {
    var output_folder: std.fs.Dir = undefined;
    const open_result = std.fs.cwd().openDir(output_path, .{});
    if (open_result) |d| {
        output_folder = d;
    } else |err| {
        switch (err) {
            error.FileNotFound => {
                std.debug.print(
                    "Output directory '{s}' does not exist. Attempting to create it...\n",
                    .{output_path},
                );
                try std.fs.cwd().makeDir(output_path);
                output_folder = try std.fs.cwd().openDir(output_path, .{});
            },
            else => return err,
        }
    }
    return output_folder;
}

pub fn isDir(path: []const u8) bool {
    if (std.fs.path.isAbsolute(path)) {
        var dir = std.fs.openDirAbsolute(
            path,
            .{},
        ) catch return false;
        dir.close();
        return true;
    } else {
        var dir = std.fs.cwd().openDir(
            path,
            .{},
        ) catch return false;
        dir.close();
        return true;
    }
}
