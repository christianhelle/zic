const std = @import("std");
const bitmap = @import("bitmap.zig");
const Color = bitmap.Color;
const Bitmap = bitmap.Bitmap;
const ByteList = std.array_list.Managed(u8);

const png_signature= [_]u8{ 0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A };

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
const crc_table: [256]u32 = blk: {
    @setEvalBranchQuota(3000);
    var table: [256]u32 = undefined;
    for (0..256) |n| {
        var c: u32 = @intCast(n);
        for (0..8) |_| {
            if (c & 1 != 0) {
                c = 0xEDB88320 ^ (c >> 1);
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
            if (chunk_len < 13) return error.InvalidPng;
            width = readU32BE(chunk_data, 0);
            height = readU32BE(chunk_data, 4);
            bit_depth = chunk_data[8];
            color_type = chunk_data[9];
            if (bit_depth != 8) return error.UnsupportedBitDepth;
            if (color_type != 2 and color_type != 6) return error.UnsupportedColorType;
            if (chunk_data[12] != 0) return error.UnsupportedInterlace;
        } else if (std.mem.eql(u8, chunk_type[0..4], "IDAT")) {
            try idat_list.appendSlice(chunk_data);
        } else if (std.mem.eql(u8, chunk_type[0..4], "IEND")) {
            break;
        }

        pos = chunk_data_end + 4;
    }

    if (width == 0 or height == 0) return error.InvalidPng;

    const channels: u32 = if (color_type == 6) 4 else 3;
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
            0 => {}, // None
            1 => { // Sub
                for (channels..filtered.len) |i| {
                    filtered[i] +%= filtered[i - channels];
                }
            },
            2 => { // Up
                if (prev_row) |pr| {
                    for (0..filtered.len) |i| {
                        filtered[i] +%= pr[i];
                    }
                }
            },
            3 => { // Average
                for (0..filtered.len) |i| {
                    const left: u16 = if (i >= channels) filtered[i - channels] else 0;
                    const above: u16 = if (prev_row) |pr| pr[i] else 0;
                    filtered[i] +%= @truncate((left + above) / 2);
                }
            },
            4 => { // Paeth
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
            const a: u8 = if (channels == 4) filtered[px + 3] else 255;
            bmp.setPixel(x, y, Color.rgba(r, g, b, a));
        }
    }

    return bmp;
}

pub fn encode(allocator: std.mem.Allocator, bmp: *const Bitmap) ![]u8 {
    var output = ByteList.init(allocator);
    errdefer output.deinit();

    // PNG signature
    try output.appendSlice(&png_signature);

    // IHDR chunk
    var ihdr: [13]u8 = undefined;
    writeU32BE(&ihdr, 0, bmp.width);
    writeU32BE(&ihdr, 4, bmp.height);
    ihdr[8] = 8; // bit depth
    ihdr[9] = 6; // RGBA
    ihdr[10] = 0; // compression
    ihdr[11] = 0; // filter
    ihdr[12] = 0; // interlace
    try writeChunk(&output, "IHDR", &ihdr);

    // Prepare raw pixel data with filter type 0 (None) per row
    const channels: u32 = 4;
    const raw_row_len = 1 + bmp.width * channels;
    const raw_size = raw_row_len * bmp.height;
    var raw = try allocator.alloc(u8, raw_size);
    defer allocator.free(raw);

    var y: u32 = 0;
    while (y < bmp.height) : (y += 1) {
        const row_start = @as(usize, y) * raw_row_len;
        raw[row_start] = 0; // filter: None
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
    const max_block: usize = 65535;
    const num_blocks = if (input.len == 0) 1 else (input.len + max_block - 1) / max_block;
    const out_size = 2 + num_blocks * 5 + input.len + 4;

    var out = try ByteList.initCapacity(allocator, out_size);
    errdefer out.deinit();

    // Zlib header: CMF=0x78 (deflate, 32K window), FLG=0x01 (level 0, no dict)
    try out.appendSlice(&[_]u8{ 0x78, 0x01 });

    // Deflate stored blocks
    var offset: usize = 0;
    while (true) {
        const remaining = input.len - offset;
        const block_len = @min(remaining, max_block);
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
        s1 = (s1 + byte) % 65521;
        s2 = (s2 + s1) % 65521;
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
    var crc_val = crc32(0xFFFFFFFF, chunk_type);
    crc_val = crc32(crc_val, data);
    crc_val ^= 0xFFFFFFFF;
    var crc_buf: [4]u8 = undefined;
    writeU32BE(&crc_buf, 0, crc_val);
    try output.appendSlice(&crc_buf);
}
