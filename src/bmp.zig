const std = @import("std");
const bitmap = @import("bitmap.zig");
const Color = bitmap.Color;
const Bitmap = bitmap.Bitmap;

fn readU16(data: []const u8, offset: usize) u16 {
    return @as(u16, data[offset]) | (@as(u16, data[offset + 1]) << 8);
}

fn readU32(data: []const u8, offset: usize) u32 {
    return @as(u32, data[offset]) |
        (@as(u32, data[offset + 1]) << 8) |
        (@as(u32, data[offset + 2]) << 16) |
        (@as(u32, data[offset + 3]) << 24);
}

fn readI32(data: []const u8, offset: usize) i32 {
    return @bitCast(readU32(data, offset));
}

fn writeU16(buf: []u8, offset: usize, value: u16) void {
    buf[offset] = @truncate(value);
    buf[offset + 1] = @truncate(value >> 8);
}

fn writeU32(buf: []u8, offset: usize, value: u32) void {
    buf[offset] = @truncate(value);
    buf[offset + 1] = @truncate(value >> 8);
    buf[offset + 2] = @truncate(value >> 16);
    buf[offset + 3] = @truncate(value >> 24);
}

fn writeI32(buf: []u8, offset: usize, value: i32) void {
    writeU32(buf, offset, @bitCast(value));
}

pub fn decode(allocator: std.mem.Allocator, data: []const u8) !Bitmap {
    if (data.len < 54) return error.InvalidBmp;
    if (data[0] != 'B' or data[1] != 'M') return error.InvalidBmp;

    const data_offset = readU32(data, 10);
    const width_raw = readI32(data, 18);
    const height_raw = readI32(data, 22);
    const bpp = readU16(data, 28);
    const compression = readU32(data, 30);

    if (compression != 0) return error.UnsupportedCompression;
    if (bpp != 24 and bpp != 32) return error.UnsupportedBitDepth;
    if (width_raw <= 0) return error.InvalidDimensions;

    const width: u32 = @intCast(width_raw);
    const top_down = height_raw < 0;
    const height: u32 = if (top_down) @intCast(-height_raw) else @intCast(height_raw);

    var bmp = try Bitmap.init(allocator, width, height);
    errdefer bmp.deinit();

    const bytes_per_pixel: u32 = @as(u32, bpp) / 8;
    const row_stride = ((width * bytes_per_pixel + 3) / 4) * 4;

    var y: u32 = 0;
    while (y < height) : (y += 1) {
        const src_row = if (top_down) y else height - 1 - y;
        const row_off = data_offset + src_row * row_stride;
        var x: u32 = 0;
        while (x < width) : (x += 1) {
            const px = row_off + x * bytes_per_pixel;
            if (px + bytes_per_pixel > data.len) return error.UnexpectedEof;
            const b = data[px];
            const g = data[px + 1];
            const r = data[px + 2];
            const a: u8 = if (bpp == 32) data[px + 3] else 255;
            bmp.setPixel(x, y, Color.rgba(r, g, b, a));
        }
    }

    return bmp;
}

pub fn encode(allocator: std.mem.Allocator, bmp: *const Bitmap) ![]u8 {
    const row_stride = ((bmp.width * 3 + 3) / 4) * 4;
    const pixel_data_size = row_stride * bmp.height;
    const file_size = 54 + pixel_data_size;

    var out = try allocator.alloc(u8, file_size);
    @memset(out, 0);

    // File header
    out[0] = 'B';
    out[1] = 'M';
    writeU32(out, 2, file_size);
    writeU32(out, 10, 54);

    // BITMAPINFOHEADER
    writeU32(out, 14, 40);
    writeI32(out, 18, @intCast(bmp.width));
    writeI32(out, 22, @intCast(bmp.height));
    writeU16(out, 26, 1); // planes
    writeU16(out, 28, 24); // bpp
    writeU32(out, 34, pixel_data_size);

    // Pixel data (bottom-up, BGR)
    var y: u32 = 0;
    while (y < bmp.height) : (y += 1) {
        const dst_row = bmp.height - 1 - y;
        const row_off = 54 + dst_row * row_stride;
        var x: u32 = 0;
        while (x < bmp.width) : (x += 1) {
            const color = bmp.getPixel(x, y);
            const px = row_off + x * 3;
            out[px] = color.b;
            out[px + 1] = color.g;
            out[px + 2] = color.r;
        }
    }

    return out;
}
