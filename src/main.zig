const std = @import("std");
const bitmap = @import("bitmap.zig");
const bmp = @import("bmp.zig");
const png = @import("png.zig");
const jpeg = @import("jpeg.zig");
const format = @import("format.zig");
const cli = @import("cli.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var args = try cli.Args.init(allocator);
    defer args.deinit();

    if (args.options.help) {
        cli.printUsage();
        return;
    }

    const in_fmt = format.detectFormat(args.input_path) orelse {
        std.debug.print("Error: Unknown input format for '{s}'\n", .{args.input_path});
        return;
    };

    const out_fmt = format.detectFormat(args.output_path) orelse {
        std.debug.print("Error: Unknown output format for '{s}'\n", .{args.output_path});
        return;
    };

    // Read input file
    const cwd = std.fs.cwd();
    const input_data = cwd.readFileAlloc(allocator, args.input_path, 100_000_000) catch |err| {
        std.debug.print("Error reading '{s}': {}\n", .{ args.input_path, err });
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

    std.debug.print("Loaded {s}: {}x{}\n", .{ args.input_path, image.width, image.height });

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
        .jpg_fmt => jpeg.encode(allocator, &image, args.options.quality) catch |err| {
            std.debug.print("Error encoding JPEG: {}\n", .{err});
            return;
        },
    };
    defer allocator.free(output_data);

    // Write output
    cwd.writeFile(.{ .sub_path = args.output_path, .data = output_data }) catch |err| {
        std.debug.print("Error writing '{s}': {}\n", .{ args.output_path, err });
        return;
    };

    std.debug.print("Written {s} ({} bytes)\n", .{ args.output_path, output_data.len });
}
