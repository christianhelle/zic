const testing = std.testing;

const bitmap = @import("bitmap.zig");
const Color = bitmap.Color;
const Bitmap = bitmap.Bitmap;

const std = @import("std");

// BMP file format magic signature: ASCII "BM" identifying a Windows bitmap file
// https://en.wikipedia.org/wiki/BMP_file_format#Bitmap_file_header
const bmp_signature = [2]u8{ 'B', 'M' };

// Offset (in bytes) to the pixel data offset field within the BMP file header
// https://en.wikipedia.org/wiki/BMP_file_format#Bitmap_file_header
const bmp_data_offset_pos = 10;

// Minimum BMP file size: 14-byte file header + 40-byte BITMAPINFOHEADER
// https://en.wikipedia.org/wiki/BMP_file_format#DIB_header_(bitmap_information_header)
const bmp_header_size: u32 = 14;
const bmp_info_header_size: u32 = 40;
const bmp_min_file_size: u32 = bmp_header_size + bmp_info_header_size;

// BITMAPINFOHEADER field offsets (relative to file start)
// https://learn.microsoft.com/en-us/windows/win32/api/wingdi/ns-wingdi-bitmapinfoheader
const bmp_width_offset = 18;
const bmp_height_offset = 22;
const bmp_planes_offset = 26;
const bmp_bpp_offset = 28;
const bmp_compression_offset = 30;
const bmp_image_size_offset = 34;

// BI_RGB: uncompressed pixel format (compression = 0)
// https://learn.microsoft.com/en-us/windows/win32/api/wingdi/ns-wingdi-bitmapinfoheader
const bi_rgb: u32 = 0;

// Supported bits-per-pixel values
const bpp_24: u16 = 24; // 24-bit RGB (no alpha)
const bpp_32: u16 = 32; // 32-bit RGBA

// Number of color planes (always 1 for BMP)
const bmp_color_planes: u16 = 1;

// Row stride alignment: BMP rows are padded to 4-byte boundaries
// https://en.wikipedia.org/wiki/BMP_file_format#Pixel_storage
const bmp_row_alignment: u32 = 4;

// Default alpha value for 24-bit BMP pixels (fully opaque)
const default_alpha: u8 = 255;

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

/// Decodes a BMP (Windows Bitmap) image into a bitmap (raster).
/// Supports 24-bit and 32-bit uncompressed BMP files with bottom-up or top-down row order.
/// https://en.wikipedia.org/wiki/BMP_file_format
/// https://learn.microsoft.com/en-us/windows/win32/gdi/bitmap-storage
pub fn decode(allocator: std.mem.Allocator, data: []const u8) !Bitmap {
    if (data.len < bmp_min_file_size) return error.InvalidBmp;
    if (data[0] != bmp_signature[0] or data[1] != bmp_signature[1]) return error.InvalidBmp;

    const data_offset = readU32(data, bmp_data_offset_pos);
    const width_raw = readI32(data, bmp_width_offset);
    const height_raw = readI32(data, bmp_height_offset);
    const bpp = readU16(data, bmp_bpp_offset);
    const compression = readU32(data, bmp_compression_offset);

    if (compression != bi_rgb) return error.UnsupportedCompression;
    if (bpp != bpp_24 and bpp != bpp_32) return error.UnsupportedBitDepth;
    if (width_raw <= 0) return error.InvalidDimensions;

    const width: u32 = @intCast(width_raw);
    const top_down = height_raw < 0;
    const height: u32 = if (top_down) @intCast(-height_raw) else @intCast(height_raw);

    var bmp = try Bitmap.init(allocator, width, height);
    errdefer bmp.deinit();

    const bytes_per_pixel: u32 = @as(u32, bpp) / 8;
    const row_stride = ((width * bytes_per_pixel + (bmp_row_alignment - 1)) / bmp_row_alignment) * bmp_row_alignment;

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
            const a: u8 = if (bpp == bpp_32) data[px + 3] else default_alpha;
            bmp.setPixel(x, y, Color.rgba(r, g, b, a));
        }
    }

    return bmp;
}

/// Encodes a Bitmap into a 24-bit uncompressed BMP file.
/// Pixels are stored in bottom-up row order with BGR channel ordering.
/// https://en.wikipedia.org/wiki/BMP_file_format
/// https://learn.microsoft.com/en-us/windows/win32/gdi/bitmap-storage
pub fn encode(allocator: std.mem.Allocator, bmp: *const Bitmap) ![]u8 {
    const bytes_per_pixel = bpp_24 / 8;
    const row_stride = ((bmp.width * bytes_per_pixel + (bmp_row_alignment - 1)) / bmp_row_alignment) * bmp_row_alignment;
    const pixel_data_size = row_stride * bmp.height;
    const file_size = bmp_min_file_size + pixel_data_size;

    var out = try allocator.alloc(u8, file_size);
    @memset(out, 0);

    // File header
    out[0] = bmp_signature[0];
    out[1] = bmp_signature[1];
    writeU32(out, 2, file_size);
    writeU32(out, bmp_data_offset_pos, bmp_min_file_size);

    // BITMAPINFOHEADER
    writeU32(out, bmp_header_size, bmp_info_header_size);
    writeI32(out, bmp_width_offset, @intCast(bmp.width));
    writeI32(out, bmp_height_offset, @intCast(bmp.height));
    writeU16(out, bmp_planes_offset, bmp_color_planes);
    writeU16(out, bmp_bpp_offset, bpp_24);
    writeU32(out, bmp_image_size_offset, pixel_data_size);

    // Pixel data (bottom-up, BGR)
    var y: u32 = 0;
    while (y < bmp.height) : (y += 1) {
        const dst_row = bmp.height - 1 - y;
        const row_off = bmp_min_file_size + dst_row * row_stride;
        var x: u32 = 0;
        while (x < bmp.width) : (x += 1) {
            const color = bmp.getPixel(x, y);
            const px = row_off + x * bytes_per_pixel;
            out[px] = color.b;
            out[px + 1] = color.g;
            out[px + 2] = color.r;
        }
    }

    return out;
}

// ─── Tests ───

test "readU16 little endian" {
    const data = [_]u8{ 0x34, 0x12 };
    try testing.expectEqual(@as(u16, 0x1234), readU16(&data, 0));
}

test "readU32 little endian" {
    const data = [_]u8{ 0x78, 0x56, 0x34, 0x12 };
    try testing.expectEqual(@as(u32, 0x12345678), readU32(&data, 0));
}

test "readI32 positive" {
    const data = [_]u8{ 0x01, 0x00, 0x00, 0x00 };
    try testing.expectEqual(@as(i32, 1), readI32(&data, 0));
}

test "readI32 negative" {
    // -1 in little endian = 0xFF 0xFF 0xFF 0xFF
    const data = [_]u8{ 0xFF, 0xFF, 0xFF, 0xFF };
    try testing.expectEqual(@as(i32, -1), readI32(&data, 0));
}

test "writeU16 and readU16 roundtrip" {
    var buf: [2]u8 = undefined;
    writeU16(&buf, 0, 0xABCD);
    try testing.expectEqual(@as(u16, 0xABCD), readU16(&buf, 0));
}

test "writeU32 and readU32 roundtrip" {
    var buf: [4]u8 = undefined;
    writeU32(&buf, 0, 0xDEADBEEF);
    try testing.expectEqual(@as(u32, 0xDEADBEEF), readU32(&buf, 0));
}

test "writeI32 and readI32 roundtrip positive" {
    var buf: [4]u8 = undefined;
    writeI32(&buf, 0, 42);
    try testing.expectEqual(@as(i32, 42), readI32(&buf, 0));
}

test "writeI32 and readI32 roundtrip negative" {
    var buf: [4]u8 = undefined;
    writeI32(&buf, 0, -100);
    try testing.expectEqual(@as(i32, -100), readI32(&buf, 0));
}

test "decode rejects data shorter than minimum" {
    const data = [_]u8{ 'B', 'M' } ++ [_]u8{0} ** 10;
    try testing.expectError(error.InvalidBmp, decode(testing.allocator, &data));
}

test "decode rejects invalid signature" {
    var data = [_]u8{0} ** 60;
    data[0] = 'X';
    data[1] = 'Y';
    try testing.expectError(error.InvalidBmp, decode(testing.allocator, &data));
}

test "decode rejects unsupported compression" {
    // Build a minimal BMP header with compression != 0
    var data = [_]u8{0} ** 70;
    data[0] = 'B';
    data[1] = 'M';
    writeU32(&data, bmp_data_offset_pos, bmp_min_file_size);
    writeI32(&data, bmp_width_offset, 1);
    writeI32(&data, bmp_height_offset, 1);
    writeU16(&data, bmp_bpp_offset, bpp_24);
    writeU32(&data, bmp_compression_offset, 1); // BI_RLE8, unsupported
    try testing.expectError(error.UnsupportedCompression, decode(testing.allocator, &data));
}

test "decode rejects unsupported bit depth" {
    var data = [_]u8{0} ** 70;
    data[0] = 'B';
    data[1] = 'M';
    writeU32(&data, bmp_data_offset_pos, bmp_min_file_size);
    writeI32(&data, bmp_width_offset, 1);
    writeI32(&data, bmp_height_offset, 1);
    writeU16(&data, bmp_bpp_offset, 16); // unsupported
    writeU32(&data, bmp_compression_offset, bi_rgb);
    try testing.expectError(error.UnsupportedBitDepth, decode(testing.allocator, &data));
}

test "decode rejects zero width" {
    var data = [_]u8{0} ** 70;
    data[0] = 'B';
    data[1] = 'M';
    writeU32(&data, bmp_data_offset_pos, bmp_min_file_size);
    writeI32(&data, bmp_width_offset, 0);
    writeI32(&data, bmp_height_offset, 1);
    writeU16(&data, bmp_bpp_offset, bpp_24);
    writeU32(&data, bmp_compression_offset, bi_rgb);
    try testing.expectError(error.InvalidDimensions, decode(testing.allocator, &data));
}

test "decode rejects negative width" {
    var data = [_]u8{0} ** 70;
    data[0] = 'B';
    data[1] = 'M';
    writeU32(&data, bmp_data_offset_pos, bmp_min_file_size);
    writeI32(&data, bmp_width_offset, -1);
    writeI32(&data, bmp_height_offset, 1);
    writeU16(&data, bmp_bpp_offset, bpp_24);
    writeU32(&data, bmp_compression_offset, bi_rgb);
    try testing.expectError(error.InvalidDimensions, decode(testing.allocator, &data));
}

test "encode produces valid BMP header" {
    var bmp = try Bitmap.init(testing.allocator, 2, 2);
    defer bmp.deinit();

    bmp.setPixel(0, 0, Color.rgb(255, 0, 0));
    bmp.setPixel(1, 0, Color.rgb(0, 255, 0));
    bmp.setPixel(0, 1, Color.rgb(0, 0, 255));
    bmp.setPixel(1, 1, Color.rgb(255, 255, 255));

    const data = try encode(testing.allocator, &bmp);
    defer testing.allocator.free(data);

    // Verify signature
    try testing.expectEqual(@as(u8, 'B'), data[0]);
    try testing.expectEqual(@as(u8, 'M'), data[1]);

    // Verify dimensions
    try testing.expectEqual(@as(i32, 2), readI32(data, bmp_width_offset));
    try testing.expectEqual(@as(i32, 2), readI32(data, bmp_height_offset));

    // Verify bpp
    try testing.expectEqual(@as(u16, bpp_24), readU16(data, bmp_bpp_offset));

    // Verify data offset
    try testing.expectEqual(bmp_min_file_size, readU32(data, bmp_data_offset_pos));
}

test "BMP encode-decode roundtrip 2x2" {
    var bmp = try Bitmap.init(testing.allocator, 2, 2);
    defer bmp.deinit();

    bmp.setPixel(0, 0, Color.rgb(255, 0, 0));
    bmp.setPixel(1, 0, Color.rgb(0, 255, 0));
    bmp.setPixel(0, 1, Color.rgb(0, 0, 255));
    bmp.setPixel(1, 1, Color.rgb(128, 64, 32));

    const data = try encode(testing.allocator, &bmp);
    defer testing.allocator.free(data);

    var decoded = try decode(testing.allocator, data);
    defer decoded.deinit();

    try testing.expectEqual(@as(u32, 2), decoded.width);
    try testing.expectEqual(@as(u32, 2), decoded.height);

    // Check pixel values (alpha is 255 from 24-bit decode)
    const p00 = decoded.getPixel(0, 0);
    try testing.expectEqual(@as(u8, 255), p00.r);
    try testing.expectEqual(@as(u8, 0), p00.g);
    try testing.expectEqual(@as(u8, 0), p00.b);

    const p10 = decoded.getPixel(1, 0);
    try testing.expectEqual(@as(u8, 0), p10.r);
    try testing.expectEqual(@as(u8, 255), p10.g);

    const p01 = decoded.getPixel(0, 1);
    try testing.expectEqual(@as(u8, 0), p01.r);
    try testing.expectEqual(@as(u8, 0), p01.g);
    try testing.expectEqual(@as(u8, 255), p01.b);

    const p11 = decoded.getPixel(1, 1);
    try testing.expectEqual(@as(u8, 128), p11.r);
    try testing.expectEqual(@as(u8, 64), p11.g);
    try testing.expectEqual(@as(u8, 32), p11.b);
}

test "BMP encode-decode roundtrip 1x1" {
    var bmp = try Bitmap.init(testing.allocator, 1, 1);
    defer bmp.deinit();

    bmp.setPixel(0, 0, Color.rgb(42, 84, 168));

    const data = try encode(testing.allocator, &bmp);
    defer testing.allocator.free(data);

    var decoded = try decode(testing.allocator, data);
    defer decoded.deinit();

    try testing.expectEqual(@as(u32, 1), decoded.width);
    try testing.expectEqual(@as(u32, 1), decoded.height);
    const p = decoded.getPixel(0, 0);
    try testing.expectEqual(@as(u8, 42), p.r);
    try testing.expectEqual(@as(u8, 84), p.g);
    try testing.expectEqual(@as(u8, 168), p.b);
}

test "BMP encode-decode roundtrip odd width" {
    // Odd width exercises row padding
    var bmp = try Bitmap.init(testing.allocator, 3, 2);
    defer bmp.deinit();

    bmp.setPixel(0, 0, Color.rgb(10, 20, 30));
    bmp.setPixel(1, 0, Color.rgb(40, 50, 60));
    bmp.setPixel(2, 0, Color.rgb(70, 80, 90));
    bmp.setPixel(0, 1, Color.rgb(100, 110, 120));
    bmp.setPixel(1, 1, Color.rgb(130, 140, 150));
    bmp.setPixel(2, 1, Color.rgb(160, 170, 180));

    const data = try encode(testing.allocator, &bmp);
    defer testing.allocator.free(data);

    var decoded = try decode(testing.allocator, data);
    defer decoded.deinit();

    try testing.expectEqual(@as(u32, 3), decoded.width);
    try testing.expectEqual(@as(u32, 2), decoded.height);

    try testing.expectEqual(@as(u8, 10), decoded.getPixel(0, 0).r);
    try testing.expectEqual(@as(u8, 70), decoded.getPixel(2, 0).r);
    try testing.expectEqual(@as(u8, 160), decoded.getPixel(2, 1).r);
}

test "BMP encode-decode roundtrip larger image" {
    const w: u32 = 16;
    const h: u32 = 16;
    var bmp = try Bitmap.init(testing.allocator, w, h);
    defer bmp.deinit();

    // Fill with gradient
    var y: u32 = 0;
    while (y < h) : (y += 1) {
        var x: u32 = 0;
        while (x < w) : (x += 1) {
            bmp.setPixel(x, y, Color.rgb(
                @truncate(x * 16),
                @truncate(y * 16),
                @truncate((x + y) * 8),
            ));
        }
    }

    const data = try encode(testing.allocator, &bmp);
    defer testing.allocator.free(data);

    var decoded = try decode(testing.allocator, data);
    defer decoded.deinit();

    try testing.expectEqual(w, decoded.width);
    try testing.expectEqual(h, decoded.height);

    // Spot check some pixels
    try testing.expectEqual(@as(u8, 0), decoded.getPixel(0, 0).r);
    try testing.expectEqual(@as(u8, 0), decoded.getPixel(0, 0).g);
    try testing.expectEqual(@as(u8, 128), decoded.getPixel(8, 8).r);
    try testing.expectEqual(@as(u8, 128), decoded.getPixel(8, 8).g);
}

test "BMP encode file size" {
    var bmp = try Bitmap.init(testing.allocator, 4, 4);
    defer bmp.deinit();

    const data = try encode(testing.allocator, &bmp);
    defer testing.allocator.free(data);

    // Row stride for width=4, 24bpp: 4*3=12 bytes, already 4-byte aligned
    const expected_pixel_size: u32 = 12 * 4;
    const expected_file_size = bmp_min_file_size + expected_pixel_size;
    try testing.expectEqual(expected_file_size, readU32(data, 2));
    try testing.expectEqual(expected_file_size, @as(u32, @intCast(data.len)));
}

test "BMP decode top-down image" {
    // Manually construct a top-down BMP (negative height)
    const width: u32 = 2;
    const height: u32 = 2;
    const bpp: u16 = 24;
    const bytes_per_pixel: u32 = 3;
    const row_stride = ((width * bytes_per_pixel + 3) / 4) * 4; // 8 bytes
    const pixel_data_size = row_stride * height;
    const file_size = bmp_min_file_size + pixel_data_size;

    var data: [bmp_min_file_size + 16]u8 = undefined;
    @memset(&data, 0);
    data[0] = 'B';
    data[1] = 'M';
    writeU32(&data, 2, file_size);
    writeU32(&data, bmp_data_offset_pos, bmp_min_file_size);
    writeU32(&data, bmp_header_size, bmp_info_header_size);
    writeI32(&data, bmp_width_offset, @intCast(width));
    writeI32(&data, bmp_height_offset, -@as(i32, @intCast(height))); // top-down
    writeU16(&data, bmp_planes_offset, 1);
    writeU16(&data, bmp_bpp_offset, bpp);
    writeU32(&data, bmp_compression_offset, bi_rgb);
    writeU32(&data, bmp_image_size_offset, pixel_data_size);

    // Row 0: Red, Green (BGR order)
    const off0 = bmp_min_file_size;
    data[off0] = 0;
    data[off0 + 1] = 0;
    data[off0 + 2] = 255; // Red pixel (B=0,G=0,R=255)
    data[off0 + 3] = 0;
    data[off0 + 4] = 255;
    data[off0 + 5] = 0; // Green pixel (B=0,G=255,R=0)
    // Row 1: Blue, White
    const off1 = off0 + row_stride;
    data[off1] = 255;
    data[off1 + 1] = 0;
    data[off1 + 2] = 0; // Blue pixel
    data[off1 + 3] = 255;
    data[off1 + 4] = 255;
    data[off1 + 5] = 255; // White pixel

    var decoded = try decode(testing.allocator, &data);
    defer decoded.deinit();

    try testing.expectEqual(@as(u32, 2), decoded.width);
    try testing.expectEqual(@as(u32, 2), decoded.height);

    // Top-down: row 0 is first
    try testing.expectEqual(@as(u8, 255), decoded.getPixel(0, 0).r);
    try testing.expectEqual(@as(u8, 0), decoded.getPixel(0, 0).g);
    try testing.expectEqual(@as(u8, 255), decoded.getPixel(1, 0).g);
    try testing.expectEqual(@as(u8, 255), decoded.getPixel(0, 1).b);
}
