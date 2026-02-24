const std = @import("std");
const bitmap = @import("bitmap.zig");
const bmp = @import("bmp.zig");
const png = @import("png.zig");
const jpeg = @import("jpeg.zig");
const format = @import("format.zig");
const cli = @import("cli.zig");

pub fn processImages(allocator: std.mem.Allocator, args: cli.Args) !void {
    var dir = try std.fs.cwd().openDir(args.input_path, .{ .iterate = true });
    defer dir.close();

    var iter = dir.iterate();
    while (try iter.next()) |entry| {
        if (entry.kind == .file) {
            const input_path = try std.fs.path.join(
                allocator,
                &.{ args.input_path, entry.name },
            );
            defer allocator.free(input_path);

            const extension = switch (args.options.format) {
                .bmp_fmt => ".bmp",
                .png_fmt => ".png",
                .jpg_fmt => ".jpg",
            };

            const output_file_name = try std.fmt.allocPrint(
                allocator,
                "{s}{s}",
                .{ entry.name[0 .. entry.name.len - 4], extension },
            );
            defer allocator.free(output_file_name);

            const output_path = try std.fs.path.join(
                allocator,
                &.{ args.output_path, output_file_name },
            );
            defer allocator.free(output_path);

            std.debug.print("Processing {s} -> {s}\n", .{ input_path, output_path });
            processImage(
                allocator,
                input_path,
                output_path,
                args.options,
            ) catch |err| {
                std.debug.print("Error processing '{s}': {}\n", .{ input_path, err });
            };
        }
    }
}

pub fn processImage(
    allocator: std.mem.Allocator,
    input_path: []const u8,
    output_path: []const u8,
    options: cli.Options,
) !void {
    const in_fmt = format.detectFormat(input_path) orelse {
        std.debug.print("Error: Unknown input format for '{s}'\n", .{input_path});
        return;
    };

    const out_fmt = format.detectFormat(output_path) orelse {
        std.debug.print("Error: Unknown output format for '{s}'\n", .{output_path});
        return;
    };

    // Read input file
    const cwd = std.fs.cwd();
    const input_data = cwd.readFileAlloc(allocator, input_path, 100_000_000) catch |err| {
        std.debug.print("Error reading '{s}': {}\n", .{ input_path, err });
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

    std.debug.print("Loaded {s}: {}x{}\n", .{ input_path, image.width, image.height });

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
        .jpg_fmt => jpeg.encode(allocator, &image, options.quality) catch |err| {
            std.debug.print("Error encoding JPEG: {}\n", .{err});
            return;
        },
    };
    defer allocator.free(output_data);

    // Write output
    cwd.writeFile(.{ .sub_path = output_path, .data = output_data }) catch |err| {
        std.debug.print("Error writing '{s}': {}\n", .{ output_path, err });
        return;
    };

    std.debug.print("Written {s} ({} bytes)\n", .{ output_path, output_data.len });
}
