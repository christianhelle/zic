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

// ─── Tests ───

const testing = std.testing;
const Color = bitmap.Color;
const Bitmap = bitmap.Bitmap;

fn createTestBitmap(allocator: std.mem.Allocator, w: u32, h: u32) !Bitmap {
    var bm = try Bitmap.init(allocator, w, h);
    var y: u32 = 0;
    while (y < h) : (y += 1) {
        var x: u32 = 0;
        while (x < w) : (x += 1) {
            bm.setPixel(x, y, Color.rgb(
                @truncate(x * 10),
                @truncate(y * 10),
                @truncate((x + y) * 5),
            ));
        }
    }
    return bm;
}

test "BMP to PNG cross-format roundtrip" {
    var bm = try createTestBitmap(testing.allocator, 4, 4);
    defer bm.deinit();

    // Encode as BMP
    const bmp_data = try bmp.encode(testing.allocator, &bm);
    defer testing.allocator.free(bmp_data);

    // Decode BMP
    var decoded_bmp = try bmp.decode(testing.allocator, bmp_data);
    defer decoded_bmp.deinit();

    // Encode as PNG
    const png_data = try png.encode(testing.allocator, &decoded_bmp);
    defer testing.allocator.free(png_data);

    // Decode PNG
    var decoded_png = try png.decode(testing.allocator, png_data);
    defer decoded_png.deinit();

    try testing.expectEqual(@as(u32, 4), decoded_png.width);
    try testing.expectEqual(@as(u32, 4), decoded_png.height);

    // Verify pixel values (BMP is 24-bit so alpha becomes 255)
    const p = decoded_png.getPixel(1, 1);
    try testing.expectEqual(@as(u8, 10), p.r);
    try testing.expectEqual(@as(u8, 10), p.g);
    try testing.expectEqual(@as(u8, 10), p.b);
}

test "PNG to BMP cross-format roundtrip" {
    var bm = try createTestBitmap(testing.allocator, 4, 4);
    defer bm.deinit();

    // Encode as PNG
    const png_data = try png.encode(testing.allocator, &bm);
    defer testing.allocator.free(png_data);

    // Decode PNG
    var decoded_png = try png.decode(testing.allocator, png_data);
    defer decoded_png.deinit();

    // Encode as BMP
    const bmp_data = try bmp.encode(testing.allocator, &decoded_png);
    defer testing.allocator.free(bmp_data);

    // Decode BMP
    var decoded_bmp = try bmp.decode(testing.allocator, bmp_data);
    defer decoded_bmp.deinit();

    try testing.expectEqual(@as(u32, 4), decoded_bmp.width);
    try testing.expectEqual(@as(u32, 4), decoded_bmp.height);
}

test "BMP to JPEG cross-format roundtrip" {
    var bm = try Bitmap.init(testing.allocator, 16, 16);
    defer bm.deinit();

    var y: u32 = 0;
    while (y < 16) : (y += 1) {
        var x: u32 = 0;
        while (x < 16) : (x += 1) {
            bm.setPixel(x, y, Color.rgb(128, 64, 32));
        }
    }

    // Encode as BMP
    const bmp_data = try bmp.encode(testing.allocator, &bm);
    defer testing.allocator.free(bmp_data);

    // Decode BMP
    var decoded_bmp = try bmp.decode(testing.allocator, bmp_data);
    defer decoded_bmp.deinit();

    // Encode as JPEG
    const jpg_data = try jpeg.encode(testing.allocator, &decoded_bmp, 90);
    defer testing.allocator.free(jpg_data);

    // Decode JPEG
    var decoded_jpg = try jpeg.decode(testing.allocator, jpg_data);
    defer decoded_jpg.deinit();

    try testing.expectEqual(@as(u32, 16), decoded_jpg.width);
    try testing.expectEqual(@as(u32, 16), decoded_jpg.height);

    // Lossy - check approximate values
    const p = decoded_jpg.getPixel(8, 8);
    try testing.expect(@abs(@as(i16, p.r) - 128) < 15);
    try testing.expect(@abs(@as(i16, p.g) - 64) < 15);
    try testing.expect(@abs(@as(i16, p.b) - 32) < 15);
}

test "PNG to JPEG cross-format roundtrip" {
    var bm = try Bitmap.init(testing.allocator, 16, 16);
    defer bm.deinit();

    var y: u32 = 0;
    while (y < 16) : (y += 1) {
        var x: u32 = 0;
        while (x < 16) : (x += 1) {
            bm.setPixel(x, y, Color.rgb(200, 100, 50));
        }
    }

    // PNG roundtrip
    const png_data = try png.encode(testing.allocator, &bm);
    defer testing.allocator.free(png_data);
    var decoded_png = try png.decode(testing.allocator, png_data);
    defer decoded_png.deinit();

    // JPEG encode/decode
    const jpg_data = try jpeg.encode(testing.allocator, &decoded_png, 90);
    defer testing.allocator.free(jpg_data);
    var decoded_jpg = try jpeg.decode(testing.allocator, jpg_data);
    defer decoded_jpg.deinit();

    try testing.expectEqual(@as(u32, 16), decoded_jpg.width);
    try testing.expectEqual(@as(u32, 16), decoded_jpg.height);
}

test "JPEG to PNG cross-format roundtrip" {
    var bm = try Bitmap.init(testing.allocator, 16, 16);
    defer bm.deinit();

    var y: u32 = 0;
    while (y < 16) : (y += 1) {
        var x: u32 = 0;
        while (x < 16) : (x += 1) {
            bm.setPixel(x, y, Color.rgb(50, 100, 200));
        }
    }

    const jpg_data = try jpeg.encode(testing.allocator, &bm, 95);
    defer testing.allocator.free(jpg_data);
    var decoded_jpg = try jpeg.decode(testing.allocator, jpg_data);
    defer decoded_jpg.deinit();

    const png_data = try png.encode(testing.allocator, &decoded_jpg);
    defer testing.allocator.free(png_data);
    var decoded_png = try png.decode(testing.allocator, png_data);
    defer decoded_png.deinit();

    try testing.expectEqual(@as(u32, 16), decoded_png.width);
    try testing.expectEqual(@as(u32, 16), decoded_png.height);
}

test "JPEG to BMP cross-format roundtrip" {
    var bm = try Bitmap.init(testing.allocator, 16, 16);
    defer bm.deinit();

    var y: u32 = 0;
    while (y < 16) : (y += 1) {
        var x: u32 = 0;
        while (x < 16) : (x += 1) {
            bm.setPixel(x, y, Color.rgb(80, 160, 240));
        }
    }

    const jpg_data = try jpeg.encode(testing.allocator, &bm, 85);
    defer testing.allocator.free(jpg_data);
    var decoded_jpg = try jpeg.decode(testing.allocator, jpg_data);
    defer decoded_jpg.deinit();

    const bmp_data = try bmp.encode(testing.allocator, &decoded_jpg);
    defer testing.allocator.free(bmp_data);
    var decoded_bmp = try bmp.decode(testing.allocator, bmp_data);
    defer decoded_bmp.deinit();

    try testing.expectEqual(@as(u32, 16), decoded_bmp.width);
    try testing.expectEqual(@as(u32, 16), decoded_bmp.height);
}
