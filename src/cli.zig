const std = @import("std");
const format = @import("format.zig");

const VERSION = "0.1.0";

pub fn printVersion() void {
    std.debug.print("zic version {s}\n", .{VERSION});
}

pub fn printUsage() void {
    const usage =
        \\Usage: zic [options]
        \\
        \\Convert images from one format to another
        \\
        \\Options:
        \\  -i, --input <path>     Input image file or folder
        \\  -o, --output <path>    Output image file or folder
        \\  -f, --format <format>  Output format (bmp, png, jpg) (default: detected from output file extension)
        \\  -q, --quality <1-100>  JPEG quality (default: 85)
        \\  -h, --help             Show this help
        \\
        \\Supported formats: BMP, PNG, JPEG
        \\Output format is detected from the file extension.
        \\
    ;
    std.debug.print("{s}", .{usage});
}

pub const Options = struct {
    format: format.Format = .bmp_fmt,
    quality: u8 = 85,
    batch_mode: bool,
    version: bool,
    help: bool,
};

fn isAnyOf(
    arg: []const u8,
    short_arg: []const u8,
    long_arg: []const u8,
) bool {
    return std.mem.eql(u8, arg, short_arg) or
        std.mem.eql(u8, arg, long_arg);
}

pub const Args = struct {
    options: Options,
    input_path: []const u8,
    output_path: []const u8,
    errors: std.ArrayList([]const u8),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) !Args {
        var args = try std.process.argsWithAllocator(allocator);
        defer args.deinit();
        _ = args.next();

        var options = Options{
            .help = false,
            .version = false,
            .batch_mode = false,
        };

        var errors: std.ArrayList([]const u8) = .empty;
        var input_path: ?[]const u8 = null;
        var output_path: ?[]const u8 = null;

        while (args.next()) |arg| {
            if (isAnyOf(arg, "-i", "--input")) {
                input_path = args.next();
            } else if (isAnyOf(arg, "-o", "--output")) {
                output_path = args.next();
            } else if (isAnyOf(arg, "-h", "--help")) {
                options.help = true;
            } else if (isAnyOf(arg, "-v", "--version")) {
                options.version = true;
            } else if (isAnyOf(arg, "-q", "--quality")) {
                if (args.next()) |q| {
                    options.quality = std.fmt.parseInt(u8, q, 10) catch 85;
                }
            }
        }

        if (input_path == null) {
            try errors.append(allocator, "No input file or folder specified");
            options.help = true;
        }

        if (output_path == null) {
            try errors.append(allocator, "No output file or folder specified");
            options.help = true;
        }

        if (!options.help) {
            const input_path_is_folder = isFolderPath(input_path orelse "");
            const output_path_is_folder = isFolderPath(output_path orelse "");
            options.batch_mode = input_path_is_folder or output_path_is_folder;
        }

        return Args{
            .options = options,
            .input_path = try allocator.dupe(u8, input_path orelse ""),
            .output_path = try allocator.dupe(u8, output_path orelse ""),
            .errors = errors,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Args) void {
        self.allocator.free(self.input_path);
        self.allocator.free(self.output_path);
        self.errors.deinit(self.allocator);
    }

    pub fn hasPathArgs(self: *Args) bool {
        return !std.mem.eql(u8, self.input_path, "") or
            !std.mem.eql(u8, self.output_path, "");
    }

    pub fn isBatchMode(self: *Args) bool {
        return self.options.batch_mode;
    }

    fn isFolderPath(path: []const u8) bool {
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
};

// ─── Tests ───

const testing = std.testing;

test "isAnyOf matches short arg" {
    try testing.expect(isAnyOf("-i", "-i", "--input"));
}

test "isAnyOf matches long arg" {
    try testing.expect(isAnyOf("--input", "-i", "--input"));
}

test "isAnyOf rejects non-matching" {
    try testing.expect(!isAnyOf("-x", "-i", "--input"));
    try testing.expect(!isAnyOf("", "-i", "--input"));
    try testing.expect(!isAnyOf("--output", "-i", "--input"));
}

test "Options default values" {
    const opts = Options{
        .batch_mode = false,
        .version = false,
        .help = false,
    };
    try testing.expectEqual(format.Format.bmp_fmt, opts.format);
    try testing.expectEqual(@as(u8, 85), opts.quality);
    try testing.expect(!opts.batch_mode);
    try testing.expect(!opts.version);
    try testing.expect(!opts.help);
}

test "printVersion does not crash" {
    printVersion();
}

test "printUsage does not crash" {
    printUsage();
}

test "Args hasPathArgs with empty paths" {
    var args = Args{
        .options = Options{
            .batch_mode = false,
            .version = false,
            .help = true,
        },
        .input_path = "",
        .output_path = "",
        .errors = .empty,
        .allocator = testing.allocator,
    };
    try testing.expect(!args.hasPathArgs());
}

test "Args hasPathArgs with input path" {
    var args = Args{
        .options = Options{
            .batch_mode = false,
            .version = false,
            .help = false,
        },
        .input_path = "input.bmp",
        .output_path = "",
        .errors = .empty,
        .allocator = testing.allocator,
    };
    try testing.expect(args.hasPathArgs());
}

test "Args hasPathArgs with output path" {
    var args = Args{
        .options = Options{
            .batch_mode = false,
            .version = false,
            .help = false,
        },
        .input_path = "",
        .output_path = "output.png",
        .errors = .empty,
        .allocator = testing.allocator,
    };
    try testing.expect(args.hasPathArgs());
}

test "Args hasPathArgs with both paths" {
    var args = Args{
        .options = Options{
            .batch_mode = false,
            .version = false,
            .help = false,
        },
        .input_path = "in.bmp",
        .output_path = "out.png",
        .errors = .empty,
        .allocator = testing.allocator,
    };
    try testing.expect(args.hasPathArgs());
}

test "Args isBatchMode returns batch_mode option" {
    var args = Args{
        .options = Options{
            .batch_mode = true,
            .version = false,
            .help = false,
        },
        .input_path = "",
        .output_path = "",
        .errors = .empty,
        .allocator = testing.allocator,
    };
    try testing.expect(args.isBatchMode());
}

test "Args isBatchMode false by default" {
    var args = Args{
        .options = Options{
            .batch_mode = false,
            .version = false,
            .help = false,
        },
        .input_path = "",
        .output_path = "",
        .errors = .empty,
        .allocator = testing.allocator,
    };
    try testing.expect(!args.isBatchMode());
}

test "isFolderPath returns false for nonexistent path" {
    try testing.expect(!Args.isFolderPath("nonexistent_dir_xyz"));
}

test "isFolderPath returns false for file path" {
    try testing.expect(!Args.isFolderPath("src/main.zig"));
}

test "isFolderPath returns true for existing directory" {
    try testing.expect(Args.isFolderPath("src"));
}

test "isAnyOf with all flags" {
    try testing.expect(isAnyOf("-h", "-h", "--help"));
    try testing.expect(isAnyOf("--help", "-h", "--help"));
    try testing.expect(isAnyOf("-v", "-v", "--version"));
    try testing.expect(isAnyOf("--version", "-v", "--version"));
    try testing.expect(isAnyOf("-q", "-q", "--quality"));
    try testing.expect(isAnyOf("--quality", "-q", "--quality"));
    try testing.expect(isAnyOf("-o", "-o", "--output"));
    try testing.expect(isAnyOf("--output", "-o", "--output"));
}
