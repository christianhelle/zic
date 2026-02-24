const std = @import("std");
const format = @import("format.zig");

const VERSION = "0.1.0";

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
            if (std.mem.eql(u8, arg, "-i") or std.mem.eql(u8, arg, "--input")) {
                input_path = args.next();
            } else if (std.mem.eql(u8, arg, "-o") or std.mem.eql(u8, arg, "--output")) {
                output_path = args.next();
            } else if (std.mem.eql(u8, arg, "-q") or std.mem.eql(u8, arg, "--quality")) {
                if (args.next()) |q| {
                    options.quality = std.fmt.parseInt(u8, q, 10) catch 85;
                }
            } else if (std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "--help")) {
                options.help = true;
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
        const dir = std.fs.cwd();
        const stat = dir.statFile(path) catch |err| {
            if (err == error.FileNotFound) return false;
            return false;
        };
        return stat.kind == .directory;
    }
};
