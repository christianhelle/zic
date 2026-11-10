const testing = std.testing;

const std = @import("std");

pub fn openOrMakeDir(io: std.Io, output_path: []const u8) !std.Io.Dir {
    var output_folder: std.Io.Dir = undefined;
    const open_result = std.Io.Dir.cwd().openDir(io, output_path, .{});
    if (open_result) |d| {
        output_folder = d;
    } else |err| {
        switch (err) {
            error.FileNotFound => {
                std.debug.print(
                    "Output directory '{s}' does not exist. Attempting to create it...\n",
                    .{output_path},
                );
                try std.Io.Dir.cwd().createDir(io, output_path, .default_dir);
                output_folder = try std.Io.Dir.cwd().openDir(io, output_path, .{});
            },
            else => return err,
        }
    }
    return output_folder;
}

pub fn isDir(io: std.Io, path: []const u8) bool {
    if (std.fs.path.isAbsolute(path)) {
        var dir = std.Io.Dir.openDirAbsolute(
            io,
            path,
            .{},
        ) catch return false;
        dir.close(io);
        return true;
    } else {
        var dir = std.Io.Dir.cwd().openDir(
            io,
            path,
            .{},
        ) catch return false;
        dir.close(io);
        return true;
    }
}

test "isDir returns false for nonexistent path" {
    try testing.expect(!isDir(testing.io, "nonexistent_dir_xyz"));
}

test "isDir returns false for file path" {
    try testing.expect(!isDir(testing.io, "src/main.zig"));
}

test "isDir returns true for existing directory" {
    try testing.expect(isDir(testing.io, "src"));
}
