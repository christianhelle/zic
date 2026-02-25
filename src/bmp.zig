const std = @import("std");
const bitmap = @import("bitmap.zig");
const Color = bitmap.Color;
const Bitmap = bitmap.Bitmap;

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
