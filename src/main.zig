const std = @import("std");
const bitmap = @import("bitmap.zig");
const bmp = @import("bmp.zig");
const png = @import("png.zig");
const jpeg = @import("jpeg.zig");

const Format = enum { bmp_fmt, png_fmt, jpg_fmt };

fn detectFormat(path: []const u8) ?Format {
    const lower = struct {
        fn endsWith(haystack: []const u8, needle: []const u8) bool {
            if (haystack.len < needle.len) return false;
            const tail = haystack[haystack.len - needle.len ..];
            for (tail, needle) |a, b| {
                const la = if (a >= 'A' and a <= 'Z') a + 32 else a;
                if (la != b) return false;
            }
            return true;
        }
    }.endsWith;

    if (lower(path, ".bmp")) return .bmp_fmt;
    if (lower(path, ".png")) return .png_fmt;
    if (lower(path, ".jpg") or lower(path, ".jpeg")) return .jpg_fmt;
    return null;
}

fn printUsage() void {
    const usage =
        \\Usage: zic [options]
        \\
        \\Options:
        \\  -i, --input <path>     Input image file
        \\  -o, --output <path>    Output image file
        \\  -q, --quality <1-100>  JPEG quality (default: 85)
        \\  -h, --help             Show this help
        \\
        \\Supported formats: BMP, PNG, JPEG
        \\Output format is detected from the file extension.
        \\
    ;
    std.debug.print("{s}", .{usage});
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();
    _ = args.next(); // skip program name

    var input_path: ?[]const u8 = null;
    var output_path: ?[]const u8 = null;
    var quality: u8 = 85;

    while (args.next()) |arg| {
        if (std.mem.eql(u8, arg, "-i") or std.mem.eql(u8, arg, "--input")) {
            input_path = args.next();
        } else if (std.mem.eql(u8, arg, "-o") or std.mem.eql(u8, arg, "--output")) {
            output_path = args.next();
        } else if (std.mem.eql(u8, arg, "-q") or std.mem.eql(u8, arg, "--quality")) {
            if (args.next()) |q| {
                quality = std.fmt.parseInt(u8, q, 10) catch 85;
            }
        } else if (std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "--help")) {
            printUsage();
            return;
        }
    }

    const in_path = input_path orelse {
        std.debug.print("Error: No input file specified.\n", .{});
        printUsage();
        return;
    };

    const out_path = output_path orelse {
        std.debug.print("Error: No output file specified.\n", .{});
        printUsage();
        return;
    };

    const in_fmt = detectFormat(in_path) orelse {
        std.debug.print("Error: Unknown input format for '{s}'\n", .{in_path});
        return;
    };

    const out_fmt = detectFormat(out_path) orelse {
        std.debug.print("Error: Unknown output format for '{s}'\n", .{out_path});
        return;
    };

    // Read input file
    const cwd = std.fs.cwd();
    const input_data = cwd.readFileAlloc(allocator, in_path, 100_000_000) catch |err| {
        std.debug.print("Error reading '{s}': {}\n", .{ in_path, err });
        return;
    };
    defer allocator.free(input_data);

    // Decode
    var image = switch (in_fmt) {
        .bmp_fmt => bmp.decode(allocator, input_data) catch |err| {
            std.debug.print("Error decoding BMP: {}\n", .{err});
            return;
        },
        .png_fmt => png.decode(allocator, input_data) catch |err| {
            std.debug.print("Error decoding PNG: {}\n", .{err});
            return;
        },
        .jpg_fmt => jpeg.decode(allocator, input_data) catch |err| {
            std.debug.print("Error decoding JPEG: {}\n", .{err});
            return;
        },
    };
    defer image.deinit();

    std.debug.print("Loaded {s}: {}x{}\n", .{ in_path, image.width, image.height });

    // Encode
    const output_data = switch (out_fmt) {
        .bmp_fmt => bmp.encode(allocator, &image) catch |err| {
            std.debug.print("Error encoding BMP: {}\n", .{err});
            return;
        },
        .png_fmt => png.encode(allocator, &image) catch |err| {
            std.debug.print("Error encoding PNG: {}\n", .{err});
            return;
        },
        .jpg_fmt => jpeg.encode(allocator, &image, quality) catch |err| {
            std.debug.print("Error encoding JPEG: {}\n", .{err});
            return;
        },
    };
    defer allocator.free(output_data);

    // Write output
    cwd.writeFile(.{ .sub_path = out_path, .data = output_data }) catch |err| {
        std.debug.print("Error writing '{s}': {}\n", .{ out_path, err });
        return;
    };

    std.debug.print("Written {s} ({} bytes)\n", .{ out_path, output_data.len });
}
