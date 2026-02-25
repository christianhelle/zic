const std = @import("std");
const bitmap = @import("bitmap.zig");
const Color = bitmap.Color;
const Bitmap = bitmap.Bitmap;
const ByteList = std.array_list.Managed(u8);

// PNG file signature: 8 bytes identifying a valid PNG file
// The signature encodes: high-bit check, "PNG", DOS line ending, EOF, Unix line ending
// https://www.w3.org/TR/png/#5PNG-file-signature
const png_signature = [_]u8{ 0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A };

// PNG IHDR field values
// https://www.w3.org/TR/png/#11IHDR
const png_bit_depth_8: u8 = 8; // 8 bits per channel
const png_color_type_rgb: u8 = 2; // RGB truecolor
const png_color_type_rgba: u8 = 6; // RGBA truecolor + alpha
const png_compression_deflate: u8 = 0; // Deflate/inflate compression
const png_filter_adaptive: u8 = 0; // Adaptive filtering with five basic filter types
const png_interlace_none: u8 = 0; // No interlacing

// IHDR chunk length in bytes: width(4) + height(4) + bitDepth(1) + colorType(1)
//   + compression(1) + filter(1) + interlace(1)
const ihdr_data_length: u32 = 13;

// PNG filter types applied per-row before compression
// https://www.w3.org/TR/png/#9Filter-types
const png_filter_none: u8 = 0;
const png_filter_sub: u8 = 1;
const png_filter_up: u8 = 2;
const png_filter_average: u8 = 3;
const png_filter_paeth: u8 = 4;

// CRC-32/ISO-3309 polynomial in reversed (LSB-first) representation
// https://www.w3.org/TR/png/#D-CRCAppendix
const crc32_polynomial: u32 = 0xEDB88320;

// CRC-32 initial and final XOR value
const crc32_init: u32 = 0xFFFFFFFF;

// Zlib header bytes for deflate with 32K window, compression level 0 (stored)
// CMF = 0x78: CM=8 (deflate), CINFO=7 (32K window)
// FLG = 0x01: FCHECK so CMF*256+FLG is a multiple of 31, FLEVEL=0
// https://www.rfc-editor.org/rfc/rfc1950#section-2.2
const zlib_header = [_]u8{ 0x78, 0x01 };

// Maximum deflate stored block payload size (2^16 - 1 bytes)
// https://www.rfc-editor.org/rfc/rfc1951#section-3.2.4
const deflate_max_stored_block: usize = 65535;

// Adler-32 modulus: largest prime smaller than 2^16
// https://www.rfc-editor.org/rfc/rfc1950#section-8.2
const adler32_mod: u32 = 65521;

// Default alpha value for RGB pixels (fully opaque)
const default_alpha: u8 = 255;

// Number of channels per color type
const channels_rgb: u32 = 3;
const channels_rgba: u32 = 4;

fn readU32BE(data: []const u8, offset: usize) u32 {
    return (@as(u32, data[offset]) << 24) |
        (@as(u32, data[offset + 1]) << 16) |
        (@as(u32, data[offset + 2]) << 8) |
        @as(u32, data[offset + 3]);
}

fn writeU32BE(buf: []u8, offset: usize, value: u32) void {
    buf[offset] = @truncate(value >> 24);
    buf[offset + 1] = @truncate(value >> 16);
    buf[offset + 2] = @truncate(value >> 8);
    buf[offset + 3] = @truncate(value);
}

// CRC32 lookup table (PNG uses CRC-32/ISO-3309)
// https://www.w3.org/TR/png/#D-CRCAppendix
const crc_table: [256]u32 = blk: {
    @setEvalBranchQuota(3000);
    var table: [256]u32 = undefined;
    for (0..256) |n| {
        var c: u32 = @intCast(n);
        for (0..8) |_| {
            if (c & 1 != 0) {
                c = crc32_polynomial ^ (c >> 1);
            } else {
                c = c >> 1;
            }
        }
        table[n] = c;
    }
    break :blk table;
};

fn crc32(init: u32, data: []const u8) u32 {
    var crc = init;
    for (data) |byte| {
        crc = crc_table[(crc ^ byte) & 0xFF] ^ (crc >> 8);
    }
    return crc;
}

fn paeth(a: u8, b: u8, c: u8) u8 {
    const p: i16 = @as(i16, a) + @as(i16, b) - @as(i16, c);
    const pa: u16 = @intCast(@abs(p - @as(i16, a)));
    const pb: u16 = @intCast(@abs(p - @as(i16, b)));
    const pc: u16 = @intCast(@abs(p - @as(i16, c)));
    if (pa <= pb and pa <= pc) return a;
    if (pb <= pc) return b;
    return c;
}

/// Decodes a PNG (Portable Network Graphics) image into a Bitmap.
/// Supports 8-bit RGB and RGBA color types with adaptive filtering (no interlacing).
/// https://www.w3.org/TR/png/
/// https://www.rfc-editor.org/rfc/rfc2083
pub fn decode(allocator: std.mem.Allocator, data: []const u8) !Bitmap {
    if (data.len < 8) return error.InvalidPng;
    if (!std.mem.eql(u8, data[0..8], &png_signature)) return error.InvalidPng;

    var pos: usize = 8;
    var width: u32 = 0;
    var height: u32 = 0;
    var bit_depth: u8 = 0;
    var color_type: u8 = 0;

    var idat_list = ByteList.init(allocator);
    defer idat_list.deinit();

    while (pos + 12 <= data.len) {
        const chunk_len = readU32BE(data, pos);
        const chunk_type = data[pos + 4 .. pos + 8];
        const chunk_data_start = pos + 8;
        const chunk_data_end = chunk_data_start + chunk_len;

        if (chunk_data_end + 4 > data.len) return error.UnexpectedEof;
        const chunk_data = data[chunk_data_start..chunk_data_end];

        if (std.mem.eql(u8, chunk_type[0..4], "IHDR")) {
            if (chunk_len < ihdr_data_length) return error.InvalidPng;
            width = readU32BE(chunk_data, 0);
            height = readU32BE(chunk_data, 4);
            bit_depth = chunk_data[8];
            color_type = chunk_data[9];
            if (bit_depth != png_bit_depth_8) return error.UnsupportedBitDepth;
            if (color_type != png_color_type_rgb and color_type != png_color_type_rgba) return error.UnsupportedColorType;
            if (chunk_data[12] != png_interlace_none) return error.UnsupportedInterlace;
        } else if (std.mem.eql(u8, chunk_type[0..4], "IDAT")) {
            try idat_list.appendSlice(chunk_data);
        } else if (std.mem.eql(u8, chunk_type[0..4], "IEND")) {
            break;
        }

        pos = chunk_data_end + 4;
    }

    if (width == 0 or height == 0) return error.InvalidPng;

    const channels: u32 = if (color_type == png_color_type_rgba) channels_rgba else channels_rgb;
    const raw_row_len = 1 + width * channels;

    // Decompress zlib data using std.compress.flate
    var in_reader: std.Io.Reader = .fixed(idat_list.items);
    var out_aw: std.Io.Writer.Allocating = .init(allocator);
    defer out_aw.deinit();
    var decompress: std.compress.flate.Decompress = .init(&in_reader, .zlib, &.{});
    _ = try decompress.reader.streamRemaining(&out_aw.writer);
    const raw = out_aw.written();

    // Unfilter and build bitmap
    var bmp = try Bitmap.init(allocator, width, height);
    errdefer bmp.deinit();

    var prev_row: ?[]const u8 = null;
    var y: u32 = 0;
    while (y < height) : (y += 1) {
        const row_start = @as(usize, y) * raw_row_len;
        if (row_start + raw_row_len > raw.len) return error.UnexpectedEof;

        const filter_type = raw[row_start];
        const filtered = raw[row_start + 1 .. row_start + raw_row_len];

        switch (filter_type) {
            png_filter_none => {},
            png_filter_sub => {
                for (channels..filtered.len) |i| {
                    filtered[i] +%= filtered[i - channels];
                }
            },
            png_filter_up => {
                if (prev_row) |pr| {
                    for (0..filtered.len) |i| {
                        filtered[i] +%= pr[i];
                    }
                }
            },
            png_filter_average => {
                for (0..filtered.len) |i| {
                    const left: u16 = if (i >= channels) filtered[i - channels] else 0;
                    const above: u16 = if (prev_row) |pr| pr[i] else 0;
                    filtered[i] +%= @truncate((left + above) / 2);
                }
            },
            png_filter_paeth => {
                for (0..filtered.len) |i| {
                    const left: u8 = if (i >= channels) filtered[i - channels] else 0;
                    const above: u8 = if (prev_row) |pr| pr[i] else 0;
                    const upper_left: u8 = if (i >= channels)
                        (if (prev_row) |pr| pr[i - channels] else 0)
                    else
                        0;
                    filtered[i] +%= paeth(left, above, upper_left);
                }
            },
            else => return error.InvalidFilter,
        }

        prev_row = filtered;

        var x: u32 = 0;
        while (x < width) : (x += 1) {
            const px = @as(usize, x) * channels;
            const r = filtered[px];
            const g = filtered[px + 1];
            const b = filtered[px + 2];
            const a: u8 = if (channels == channels_rgba) filtered[px + 3] else default_alpha;
            bmp.setPixel(x, y, Color.rgba(r, g, b, a));
        }
    }

    return bmp;
}

/// Encodes a Bitmap into a PNG (Portable Network Graphics) image.
/// Outputs an 8-bit RGBA PNG with no filtering and zlib stored blocks (no compression).
/// https://www.w3.org/TR/png/
/// https://www.rfc-editor.org/rfc/rfc2083
pub fn encode(allocator: std.mem.Allocator, bmp: *const Bitmap) ![]u8 {
    var output = ByteList.init(allocator);
    errdefer output.deinit();

    // PNG signature
    try output.appendSlice(&png_signature);

    // IHDR chunk
    var ihdr: [ihdr_data_length]u8 = undefined;
    writeU32BE(&ihdr, 0, bmp.width);
    writeU32BE(&ihdr, 4, bmp.height);
    ihdr[8] = png_bit_depth_8;
    ihdr[9] = png_color_type_rgba;
    ihdr[10] = png_compression_deflate;
    ihdr[11] = png_filter_adaptive;
    ihdr[12] = png_interlace_none;
    try writeChunk(&output, "IHDR", &ihdr);

    // Prepare raw pixel data with filter type None per row
    const channels: u32 = channels_rgba;
    const raw_row_len = 1 + bmp.width * channels;
    const raw_size = raw_row_len * bmp.height;
    var raw = try allocator.alloc(u8, raw_size);
    defer allocator.free(raw);

    var y: u32 = 0;
    while (y < bmp.height) : (y += 1) {
        const row_start = @as(usize, y) * raw_row_len;
        raw[row_start] = png_filter_none;
        var x: u32 = 0;
        while (x < bmp.width) : (x += 1) {
            const color = bmp.getPixel(x, y);
            const px = row_start + 1 + @as(usize, x) * channels;
            raw[px] = color.r;
            raw[px + 1] = color.g;
            raw[px + 2] = color.b;
            raw[px + 3] = color.a;
        }
    }

    // Compress with zlib stored blocks (no compression, but valid zlib)
    const compressed = try zlibCompressStored(allocator, raw);
    defer allocator.free(compressed);

    // IDAT chunk
    try writeChunk(&output, "IDAT", compressed);

    // IEND chunk
    try writeChunk(&output, "IEND", &[0]u8{});

    return output.toOwnedSlice();
}

fn zlibCompressStored(allocator: std.mem.Allocator, input: []const u8) ![]u8 {
    const num_blocks = if (input.len == 0) 1 else (input.len + deflate_max_stored_block - 1) / deflate_max_stored_block;
    const out_size = 2 + num_blocks * 5 + input.len + 4;

    var out = try ByteList.initCapacity(allocator, out_size);
    errdefer out.deinit();

    try out.appendSlice(&zlib_header);

    // Deflate stored blocks
    var offset: usize = 0;
    while (true) {
        const remaining = input.len - offset;
        const block_len = @min(remaining, deflate_max_stored_block);
        const is_final: u8 = if (offset + block_len >= input.len) 1 else 0;
        const len: u16 = @intCast(block_len);

        try out.append(is_final);
        try out.append(@truncate(len));
        try out.append(@truncate(len >> 8));
        const nlen = ~len;
        try out.append(@truncate(nlen));
        try out.append(@truncate(nlen >> 8));
        try out.appendSlice(input[offset .. offset + block_len]);

        offset += block_len;
        if (offset >= input.len) break;
    }

    // Adler-32 checksum
    var s1: u32 = 1;
    var s2: u32 = 0;
    for (input) |byte| {
        s1 = (s1 + byte) % adler32_mod;
        s2 = (s2 + s1) % adler32_mod;
    }
    const adler = (s2 << 16) | s1;
    try out.append(@truncate(adler >> 24));
    try out.append(@truncate(adler >> 16));
    try out.append(@truncate(adler >> 8));
    try out.append(@truncate(adler));

    return out.toOwnedSlice();
}

fn writeChunk(output: *ByteList, chunk_type: *const [4]u8, data: []const u8) !void {
    var len_buf: [4]u8 = undefined;
    writeU32BE(&len_buf, 0, @intCast(data.len));
    try output.appendSlice(&len_buf);
    try output.appendSlice(chunk_type);
    try output.appendSlice(data);

    // CRC over type + data
    var crc_val = crc32(crc32_init, chunk_type);
    crc_val = crc32(crc_val, data);
    crc_val ^= crc32_init;
    var crc_buf: [4]u8 = undefined;
    writeU32BE(&crc_buf, 0, crc_val);
    try output.appendSlice(&crc_buf);
}
