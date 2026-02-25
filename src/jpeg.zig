const std = @import("std");
const bitmap = @import("bitmap.zig");
const Color = bitmap.Color;
const Bitmap = bitmap.Bitmap;
const ByteList = std.array_list.Managed(u8);

// ─── JPEG Marker Codes ───
// JPEG markers are two-byte sequences starting with 0xFF followed by a marker type byte.
// https://www.w3.org/Graphics/JPEG/itu-t81.pdf (Table B.1)
// https://en.wikipedia.org/wiki/JPEG#Syntax_and_structure

/// Marker prefix byte: all JPEG markers begin with 0xFF
const marker_prefix: u8 = 0xFF;

/// Start Of Image: marks the beginning of a JPEG file
const marker_soi: u16 = 0xFFD8;

/// End Of Image: marks the end of a JPEG file
const marker_eoi = [_]u8{ 0xFF, 0xD9 };

/// SOF0 (Start Of Frame, Baseline DCT): defines image dimensions, components, sampling
const marker_sof0: u8 = 0xC0;

/// DHT (Define Huffman Table): embeds a Huffman coding table
const marker_dht: u8 = 0xC4;

/// DQT (Define Quantization Table): embeds a quantization table
const marker_dqt: u8 = 0xDB;

/// DRI (Define Restart Interval): sets the MCU restart interval
const marker_dri: u8 = 0xDD;

/// SOS (Start Of Scan): begins the entropy-coded scan data
const marker_sos: u8 = 0xDA;

/// EOI marker type byte
const marker_eoi_type: u8 = 0xD9;

/// Restart marker range: RST0 (0xD0) through RST7 (0xD7)
const marker_rst0: u8 = 0xD0;
const marker_rst7: u8 = 0xD7;

/// APP0 (JFIF header) marker bytes
const marker_app0 = [_]u8{ 0xFF, 0xE0 };

/// Byte stuffing: 0xFF followed by 0x00 represents a literal 0xFF data byte
/// https://www.w3.org/Graphics/JPEG/itu-t81.pdf (Section B.1.1.5)
const byte_stuffing_suffix: u8 = 0x00;

// ─── JPEG Constants ───

/// JFIF APP0 segment: identifies JPEG File Interchange Format
/// https://en.wikipedia.org/wiki/JPEG_File_Interchange_Format
const jfif_app0_data = [_]u8{
    0x00, 0x10, // length = 16
    0x4A, 0x46, 0x49, 0x46, 0x00, // "JFIF\0"
    0x01, 0x01, // version 1.1
    0x00, // aspect ratio units (0 = no units)
    0x00, 0x01, // X density = 1
    0x00, 0x01, // Y density = 1
    0x00, 0x00, // no thumbnail
};

/// Baseline DCT sample precision: 8 bits per component
const jpeg_precision: u8 = 8;

/// Number of components in a YCbCr image
const jpeg_num_components: u8 = 3;

/// MCU block size in pixels for 4:2:0 subsampling (2x2 blocks of 8x8)
const mcu_size_420: u32 = 16;

/// DCT block size
const dct_block_size: u32 = 8;

/// DC level shift applied during IDCT (subtract 128 before FDCT, add 128 after IDCT)
/// https://www.w3.org/Graphics/JPEG/itu-t81.pdf (Section A.3.1)
const dc_level_shift: f64 = 128.0;

/// End-Of-Block (EOB) AC Huffman symbol: all remaining AC coefficients are zero
/// https://www.w3.org/Graphics/JPEG/itu-t81.pdf (Table F.1)
const ac_eob_symbol: u8 = 0x00;

/// Zero Run Length (ZRL) AC symbol: run of 16 zero coefficients
/// https://www.w3.org/Graphics/JPEG/itu-t81.pdf (Table F.1)
const ac_zrl_symbol: u8 = 0xF0;

/// Maximum AC run length encoded in a single ZRL symbol
const ac_zrl_run: u8 = 16;

/// Number of DCT coefficients per 8×8 block
const block_coeff_count: usize = 64;

// ─── Zigzag Scan Order ───
// Maps zigzag position to natural (row-major) 8×8 index.
// JPEG encodes DCT coefficients in zigzag order from low to high frequency.
// https://www.w3.org/Graphics/JPEG/itu-t81.pdf (Figure A.6)
const zigzag_order = [64]u8{
    0,  1,  8,  16, 9,  2,  3,  10,
    17, 24, 32, 25, 18, 11, 4,  5,
    12, 19, 26, 33, 40, 48, 41, 34,
    27, 20, 13, 6,  7,  14, 21, 28,
    35, 42, 49, 56, 57, 50, 43, 36,
    29, 22, 15, 23, 30, 37, 44, 51,
    58, 59, 52, 45, 38, 31, 39, 46,
    53, 60, 61, 54, 47, 55, 62, 63,
};

// Inverse zigzag: maps natural 8×8 index to zigzag position
// Used during encoding to reorder quantized DCT coefficients into zigzag order
const zigzag_inverse = blk: {
    var table: [64]u8 = undefined;
    for (0..64) |i| {
        table[zigzag_order[i]] = @intCast(i);
    }
    break :blk table;
};

// Standard luminance quantization table (ITU-T T.81 Annex K, Table K.1)
// These values are perceptually tuned for the human visual system's sensitivity
// to luminance spatial frequencies.
// https://www.w3.org/Graphics/JPEG/itu-t81.pdf (Section K.1)
const std_lum_qt = [64]u8{
    16, 11, 10, 16, 24,  40,  51,  61,
    12, 12, 14, 19, 26,  58,  60,  55,
    14, 13, 16, 24, 40,  57,  69,  56,
    14, 17, 22, 29, 51,  87,  80,  62,
    18, 22, 37, 56, 68,  109, 103, 77,
    24, 35, 55, 64, 81,  104, 113, 92,
    49, 64, 78, 87, 103, 121, 120, 101,
    72, 92, 95, 98, 112, 100, 103, 99,
};

// Standard chrominance quantization table (ITU-T T.81 Annex K, Table K.2)
// Chrominance tables have higher quantization values (more aggressive compression)
// because human vision is less sensitive to color detail than luminance detail.
// https://www.w3.org/Graphics/JPEG/itu-t81.pdf (Section K.1)
const std_chrom_qt = [64]u8{
    17, 18, 24, 47, 99, 99, 99, 99,
    18, 21, 26, 66, 99, 99, 99, 99,
    24, 26, 56, 99, 99, 99, 99, 99,
    47, 66, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99,
};

// Standard Huffman table specifications (ITU-T T.81 Annex K)
// Each table is defined by BITS (number of codes per bit length 1-16)
// and HUFFVAL (symbol values sorted by code length).
// https://www.w3.org/Graphics/JPEG/itu-t81.pdf (Section K.3)

// Luminance DC (Table K.3): codes for DC difference categories 0-11
const std_dc_lum_bits = [16]u8{ 0, 1, 5, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0 };
const std_dc_lum_vals = [12]u8{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 };

// Chrominance DC (Table K.4): codes for DC difference categories 0-11
const std_dc_chrom_bits = [16]u8{ 0, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0 };
const std_dc_chrom_vals = [12]u8{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 };

// Luminance AC (Table K.5): codes for run/size AC coefficient pairs
const std_ac_lum_bits = [16]u8{ 0, 2, 1, 3, 3, 2, 4, 3, 5, 5, 4, 4, 0, 0, 1, 0x7D };
const std_ac_lum_vals = [162]u8{
    0x01, 0x02, 0x03, 0x00, 0x04, 0x11, 0x05, 0x12,
    0x21, 0x31, 0x41, 0x06, 0x13, 0x51, 0x61, 0x07,
    0x22, 0x71, 0x14, 0x32, 0x81, 0x91, 0xA1, 0x08,
    0x23, 0x42, 0xB1, 0xC1, 0x15, 0x52, 0xD1, 0xF0,
    0x24, 0x33, 0x62, 0x72, 0x82, 0x09, 0x0A, 0x16,
    0x17, 0x18, 0x19, 0x1A, 0x25, 0x26, 0x27, 0x28,
    0x29, 0x2A, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39,
    0x3A, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49,
    0x4A, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59,
    0x5A, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69,
    0x6A, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79,
    0x7A, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89,
    0x8A, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98,
    0x99, 0x9A, 0xA2, 0xA3, 0xA4, 0xA5, 0xA6, 0xA7,
    0xA8, 0xA9, 0xAA, 0xB2, 0xB3, 0xB4, 0xB5, 0xB6,
    0xB7, 0xB8, 0xB9, 0xBA, 0xC2, 0xC3, 0xC4, 0xC5,
    0xC6, 0xC7, 0xC8, 0xC9, 0xCA, 0xD2, 0xD3, 0xD4,
    0xD5, 0xD6, 0xD7, 0xD8, 0xD9, 0xDA, 0xE1, 0xE2,
    0xE3, 0xE4, 0xE5, 0xE6, 0xE7, 0xE8, 0xE9, 0xEA,
    0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8,
    0xF9, 0xFA,
};

// Chrominance AC (Table K.6): codes for run/size AC coefficient pairs
const std_ac_chrom_bits = [16]u8{ 0, 2, 1, 2, 4, 4, 3, 4, 7, 5, 4, 4, 0, 1, 2, 0x77 };
const std_ac_chrom_vals = [162]u8{
    0x00, 0x01, 0x02, 0x03, 0x11, 0x04, 0x05, 0x21,
    0x31, 0x06, 0x12, 0x41, 0x51, 0x07, 0x61, 0x71,
    0x13, 0x22, 0x32, 0x81, 0x08, 0x14, 0x42, 0x91,
    0xA1, 0xB1, 0xC1, 0x09, 0x23, 0x33, 0x52, 0xF0,
    0x15, 0x62, 0x72, 0xD1, 0x0A, 0x16, 0x24, 0x34,
    0xE1, 0x25, 0xF1, 0x17, 0x18, 0x19, 0x1A, 0x26,
    0x27, 0x28, 0x29, 0x2A, 0x35, 0x36, 0x37, 0x38,
    0x39, 0x3A, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48,
    0x49, 0x4A, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58,
    0x59, 0x5A, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68,
    0x69, 0x6A, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78,
    0x79, 0x7A, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87,
    0x88, 0x89, 0x8A, 0x92, 0x93, 0x94, 0x95, 0x96,
    0x97, 0x98, 0x99, 0x9A, 0xA2, 0xA3, 0xA4, 0xA5,
    0xA6, 0xA7, 0xA8, 0xA9, 0xAA, 0xB2, 0xB3, 0xB4,
    0xB5, 0xB6, 0xB7, 0xB8, 0xB9, 0xBA, 0xC2, 0xC3,
    0xC4, 0xC5, 0xC6, 0xC7, 0xC8, 0xC9, 0xCA, 0xD2,
    0xD3, 0xD4, 0xD5, 0xD6, 0xD7, 0xD8, 0xD9, 0xDA,
    0xE2, 0xE3, 0xE4, 0xE5, 0xE6, 0xE7, 0xE8, 0xE9,
    0xEA, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8,
    0xF9, 0xFA,
};

// Precomputed cosine table for IDCT/FDCT
// cos_table[x][u] = cos((2x+1) * u * π / 16)
// Used in the separable 2D DCT/IDCT as defined in ITU-T T.81 Section A.3.3
// https://www.w3.org/Graphics/JPEG/itu-t81.pdf (Section A.3.3)
const cos_table: [8][8]f64 = blk: {
    var table: [8][8]f64 = undefined;
    for (0..8) |x| {
        for (0..8) |u| {
            table[x][u] = std.math.cos(
                (@as(f64, @floatFromInt(2 * x + 1)) * @as(f64, @floatFromInt(u)) * std.math.pi) / 16.0,
            );
        }
    }
    break :blk table;
};

// Huffman decoding table
// Stores precomputed min/max code values per bit length for fast lookup decoding.
// Based on the algorithm in ITU-T T.81 Figure F.16 (Huffman code table generation).
// https://www.w3.org/Graphics/JPEG/itu-t81.pdf (Section F.2.2.3)
const HuffTable = struct {
    /// Minimum code value for each bit length (1-16)
    min_code: [17]i32,
    /// Maximum code value for each bit length (1-16), -1 if no codes of that length
    max_code: [17]i32,
    /// Index into symbols[] for the first symbol of each bit length
    val_offset: [17]u16,
    /// Symbol values (HUFFVAL) in code order
    symbols: [256]u8,
    /// Total number of symbols in the table
    num_symbols: u16,
};

// Huffman encoding table
// Maps symbol values to their Huffman code and code length for entropy coding.
// Built from the standard Huffman table specification (BITS and HUFFVAL arrays).
// https://www.w3.org/Graphics/JPEG/itu-t81.pdf (Section C.2)
const HuffEncTable = struct {
    /// Huffman code for each symbol value (indexed by symbol)
    codes: [256]u16,
    /// Code length in bits for each symbol value
    sizes: [256]u8,
};

// JPEG frame component specification
// Describes one color component's sampling factors and quantization table assignment.
// Parsed from the SOF0 marker segment.
// https://www.w3.org/Graphics/JPEG/itu-t81.pdf (Section B.2.2, Table B.2)
const ComponentInfo = struct {
    /// Component identifier (e.g., 1=Y, 2=Cb, 3=Cr)
    id: u8,
    /// Horizontal sampling factor (1-4)
    h_samples: u8,
    /// Vertical sampling factor (1-4)
    v_samples: u8,
    /// Quantization table selector (0-3)
    qt_id: u8,
    /// DC Huffman table selector (assigned in SOS)
    dc_table_id: u8,
    /// AC Huffman table selector (assigned in SOS)
    ac_table_id: u8,
};

fn buildHuffTable(bits: []const u8, vals: []const u8) HuffTable {
    var table: HuffTable = .{
        .min_code = [_]i32{0} ** 17,
        .max_code = [_]i32{-1} ** 17,
        .val_offset = [_]u16{0} ** 17,
        .symbols = [_]u8{0} ** 256,
        .num_symbols = 0,
    };

    // Copy symbols
    for (0..vals.len) |i| {
        table.symbols[i] = vals[i];
    }
    table.num_symbols = @intCast(vals.len);

    // Build code tables
    var code: i32 = 0;
    var si: u16 = 0;
    for (1..17) |length| {
        table.min_code[length] = code;
        table.val_offset[length] = si;
        const count = bits[length - 1];
        si += count;
        code += count;
        table.max_code[length] = code - 1;
        code <<= 1;
    }

    return table;
}

fn buildEncTable(bits: []const u8, vals: []const u8) HuffEncTable {
    var table: HuffEncTable = .{
        .codes = [_]u16{0} ** 256,
        .sizes = [_]u8{0} ** 256,
    };

    var code: u16 = 0;
    var si: usize = 0;
    for (1..17) |length| {
        const count = bits[length - 1];
        for (0..count) |_| {
            table.codes[vals[si]] = code;
            table.sizes[vals[si]] = @intCast(length);
            si += 1;
            code += 1;
        }
        code <<= 1;
    }

    return table;
}

fn bitSize(value: i32) u5 {
    if (value == 0) return 0;
    var v: u32 = @intCast(@abs(value));
    var bits: u5 = 0;
    while (v > 0) {
        bits += 1;
        v >>= 1;
    }
    return bits;
}

fn clampU8(value: i32) u8 {
    if (value < 0) return 0;
    if (value > 255) return 255;
    return @intCast(value);
}

// ─── JPEG Decoder ───
// Sequential baseline JPEG decoder implementing ITU-T T.81.
// Parses marker segments (SOI, SOF0, DHT, DQT, SOS, DRI, EOI),
// performs Huffman decoding, dequantization, and inverse DCT.
// https://www.w3.org/Graphics/JPEG/itu-t81.pdf

const Decoder = struct {
    data: []const u8,
    pos: usize,
    width: u16,
    height: u16,
    num_components: u8,
    components: [4]ComponentInfo,
    qt: [4][64]u16,
    dc_huff: [4]HuffTable,
    ac_huff: [4]HuffTable,
    bit_buf: u32,
    bits_in: u8,
    dc_pred: [4]i32,
    restart_interval: u16,

    fn init(data: []const u8) Decoder {
        return .{
            .data = data,
            .pos = 0,
            .width = 0,
            .height = 0,
            .num_components = 0,
            .components = std.mem.zeroes([4]ComponentInfo),
            .qt = std.mem.zeroes([4][64]u16),
            .dc_huff = std.mem.zeroes([4]HuffTable),
            .ac_huff = std.mem.zeroes([4]HuffTable),
            .bit_buf = 0,
            .bits_in = 0,
            .dc_pred = [_]i32{0} ** 4,
            .restart_interval = 0,
        };
    }

    fn readU8(self: *Decoder) !u8 {
        if (self.pos >= self.data.len) return error.UnexpectedEof;
        const val = self.data[self.pos];
        self.pos += 1;
        return val;
    }

    fn readU16BE(self: *Decoder) !u16 {
        if (self.pos + 2 > self.data.len) return error.UnexpectedEof;
        const val = (@as(u16, self.data[self.pos]) << 8) | @as(u16, self.data[self.pos + 1]);
        self.pos += 2;
        return val;
    }

    fn nextBit(self: *Decoder) !u1 {
        if (self.bits_in == 0) {
            var byte = try self.readU8();
            if (byte == marker_prefix) {
                const marker = try self.readU8();
                if (marker == byte_stuffing_suffix) {
                    // Byte stuffing: 0xFF 0x00 → data byte 0xFF
                } else if (marker >= marker_rst0 and marker <= marker_rst7) {
                    // Restart marker: reset state
                    self.dc_pred = [_]i32{0} ** 4;
                    self.bits_in = 0;
                    self.bit_buf = 0;
                    byte = try self.readU8();
                    if (byte == marker_prefix) {
                        const m2 = try self.readU8();
                        if (m2 != byte_stuffing_suffix) return error.InvalidMarker;
                    }
                } else {
                    return error.InvalidMarker;
                }
            }
            self.bit_buf = byte;
            self.bits_in = 8;
        }
        self.bits_in -= 1;
        return @intCast((self.bit_buf >> @intCast(self.bits_in)) & 1);
    }

    fn readBits(self: *Decoder, count: u5) !u16 {
        var value: u16 = 0;
        for (0..count) |_| {
            value = (value << 1) | @as(u16, try self.nextBit());
        }
        return value;
    }

    fn huffDecode(self: *Decoder, table: *const HuffTable) !u8 {
        var code: i32 = 0;
        for (1..17) |length| {
            code = (code << 1) | @as(i32, try self.nextBit());
            if (table.max_code[length] >= 0 and code <= table.max_code[length]) {
                const idx = table.val_offset[length] +
                    @as(u16, @intCast(code - table.min_code[length]));
                return table.symbols[idx];
            }
        }
        return error.InvalidHuffmanCode;
    }

    fn decodeValue(self: *Decoder, category: u5) !i32 {
        if (category == 0) return 0;
        const bits = try self.readBits(category);
        const cat_minus_1: u5 = category - 1;
        if (bits < (@as(u16, 1) << @as(u4, @intCast(cat_minus_1)))) {
            return @as(i32, bits) - (@as(i32, 1) << category) + 1;
        }
        return @as(i32, bits);
    }

    fn decodeBlock(self: *Decoder, comp_idx: usize, block: *[64]i32) !void {
        @memset(block, 0);

        // DC coefficient
        const dc_table = &self.dc_huff[self.components[comp_idx].dc_table_id];
        const dc_cat = try self.huffDecode(dc_table);
        const dc_diff = try self.decodeValue(@intCast(dc_cat));
        self.dc_pred[comp_idx] += dc_diff;
        block[0] = self.dc_pred[comp_idx];

        // AC coefficients
        const ac_table = &self.ac_huff[self.components[comp_idx].ac_table_id];
        var i: usize = 1;
        while (i < 64) {
            const symbol = try self.huffDecode(ac_table);
            if (symbol == ac_eob_symbol) break; // EOB
            const run = symbol >> 4;
            const cat: u5 = @intCast(symbol & 0x0F);
            if (symbol == ac_zrl_symbol) {
                i += ac_zrl_run; // ZRL: 16 zeros
                continue;
            }
            i += run;
            if (i >= 64) break;
            block[zigzag_order[i]] = try self.decodeValue(cat);
            i += 1;
        }

        // Dequantize
        const qt = &self.qt[self.components[comp_idx].qt_id];
        for (0..64) |j| {
            block[j] *= @as(i32, qt[j]);
        }
    }

    fn idct(block: *[64]i32, output: *[64]u8) void {
        var temp: [64]f64 = undefined;

        // 1D IDCT on rows
        for (0..8) |y| {
            for (0..8) |x| {
                var sum: f64 = 0;
                for (0..8) |u| {
                    const coeff: f64 = @floatFromInt(block[y * 8 + u]);
                    const cu: f64 = if (u == 0) (1.0 / @sqrt(2.0)) else 1.0;
                    sum += cu * coeff * cos_table[x][u];
                }
                temp[y * 8 + x] = sum;
            }
        }

        // 1D IDCT on columns
        for (0..8) |x| {
            for (0..8) |y| {
                var sum: f64 = 0;
                for (0..8) |v| {
                    const cv: f64 = if (v == 0) (1.0 / @sqrt(2.0)) else 1.0;
                    sum += cv * temp[v * 8 + x] * cos_table[y][v];
                }
                // Scale by 1/4 and add DC level shift
                const val = sum / 4.0 + dc_level_shift;
                output[y * 8 + x] = clampU8(@intFromFloat(@round(val)));
            }
        }
    }

    fn parseMarkers(self: *Decoder) !void {
        // Verify SOI
        const soi = try self.readU16BE();
        if (soi != marker_soi) return error.InvalidJpeg;

        while (self.pos < self.data.len) {
            var marker = try self.readU8();
            if (marker != marker_prefix) continue;

            // Skip padding 0xFF bytes
            while (marker == marker_prefix and self.pos < self.data.len) {
                marker = try self.readU8();
            }

            switch (marker) {
                marker_sof0 => try self.parseSOF0(),
                marker_dht => try self.parseDHT(),
                marker_dqt => try self.parseDQT(),
                marker_sos => return, // SOS — scan data follows
                marker_dri => try self.parseDRI(),
                marker_eoi_type => return, // EOI
                marker_rst0...marker_rst7 => {}, // Restart markers
                else => {
                    // Skip unknown marker
                    if (self.pos + 2 <= self.data.len) {
                        const len = try self.readU16BE();
                        if (len >= 2) {
                            self.pos += len - 2;
                        }
                    }
                },
            }
        }
    }

    fn parseDQT(self: *Decoder) !void {
        var len = try self.readU16BE();
        len -= 2;
        var remaining: usize = len;

        while (remaining > 0) {
            const info = try self.readU8();
            remaining -= 1;
            const precision = info >> 4;
            const table_id = info & 0x0F;
            if (table_id > 3) return error.InvalidJpeg;

            if (precision == 0) {
                // 8-bit values
                for (0..64) |i| {
                    self.qt[table_id][zigzag_order[i]] = try self.readU8();
                    remaining -= 1;
                }
            } else {
                // 16-bit values
                for (0..64) |i| {
                    self.qt[table_id][zigzag_order[i]] = try self.readU16BE();
                    remaining -= 2;
                }
            }
        }
    }

    fn parseSOF0(self: *Decoder) !void {
        const len = try self.readU16BE();
        _ = len;
        const precision = try self.readU8();
        if (precision != jpeg_precision) return error.UnsupportedPrecision;

        self.height = try self.readU16BE();
        self.width = try self.readU16BE();
        self.num_components = try self.readU8();

        if (self.num_components > 4) return error.InvalidJpeg;

        for (0..self.num_components) |i| {
            self.components[i].id = try self.readU8();
            const sampling = try self.readU8();
            self.components[i].h_samples = sampling >> 4;
            self.components[i].v_samples = sampling & 0x0F;
            self.components[i].qt_id = try self.readU8();
        }
    }

    fn parseDHT(self: *Decoder) !void {
        var len = try self.readU16BE();
        len -= 2;
        var remaining: usize = len;

        while (remaining > 0) {
            const info = try self.readU8();
            remaining -= 1;
            const table_class = info >> 4; // 0=DC, 1=AC
            const table_id = info & 0x0F;
            if (table_id > 3) return error.InvalidJpeg;

            var bits: [16]u8 = undefined;
            var total: u16 = 0;
            for (0..16) |i| {
                bits[i] = try self.readU8();
                total += bits[i];
                remaining -= 1;
            }

            var vals: [256]u8 = [_]u8{0} ** 256;
            for (0..total) |i| {
                vals[i] = try self.readU8();
                remaining -= 1;
            }

            const table = buildHuffTable(&bits, vals[0..total]);
            if (table_class == 0) {
                self.dc_huff[table_id] = table;
            } else {
                self.ac_huff[table_id] = table;
            }
        }
    }

    fn parseDRI(self: *Decoder) !void {
        _ = try self.readU16BE(); // length
        self.restart_interval = try self.readU16BE();
    }

    fn parseSOS(self: *Decoder) !void {
        const len = try self.readU16BE();
        const ns = try self.readU8();

        for (0..ns) |i| {
            const comp_id = try self.readU8();
            const table_sel = try self.readU8();
            // Find component by ID
            for (0..self.num_components) |j| {
                if (self.components[j].id == comp_id) {
                    self.components[j].dc_table_id = table_sel >> 4;
                    self.components[j].ac_table_id = table_sel & 0x0F;
                    break;
                }
            }
            _ = i;
        }

        // Skip spectral selection and successive approximation
        const to_skip = len - 2 - 1 - @as(u16, ns) * 2;
        self.pos += to_skip;
    }
};

/// Decodes a JPEG (baseline DCT) image into a Bitmap.
/// Supports 8-bit YCbCr and grayscale images with Huffman coding and 4:2:0/4:4:4 subsampling.
/// Implements the decoding process defined in ITU-T T.81 (ISO/IEC 10918-1).
/// https://www.w3.org/Graphics/JPEG/itu-t81.pdf
/// https://en.wikipedia.org/wiki/JPEG#Decoding
pub fn decode(allocator: std.mem.Allocator, data: []const u8) !Bitmap {
    var dec = Decoder.init(data);

    try dec.parseMarkers();
    try dec.parseSOS();

    if (dec.width == 0 or dec.height == 0) return error.InvalidJpeg;

    // Determine MCU dimensions
    var max_h: u8 = 1;
    var max_v: u8 = 1;
    for (0..dec.num_components) |i| {
        if (dec.components[i].h_samples > max_h) max_h = dec.components[i].h_samples;
        if (dec.components[i].v_samples > max_v) max_v = dec.components[i].v_samples;
    }

    const mcu_width: u32 = @as(u32, max_h) * 8;
    const mcu_height: u32 = @as(u32, max_v) * 8;
    const mcus_x = (dec.width + @as(u16, @intCast(mcu_width)) - 1) / @as(u16, @intCast(mcu_width));
    const mcus_y = (dec.height + @as(u16, @intCast(mcu_height)) - 1) / @as(u16, @intCast(mcu_height));

    var bmp = try Bitmap.init(allocator, dec.width, dec.height);
    errdefer bmp.deinit();

    // Decode MCUs
    var mcu_count: u32 = 0;
    var mcu_y: u32 = 0;
    while (mcu_y < mcus_y) : (mcu_y += 1) {
        var mcu_x: u32 = 0;
        while (mcu_x < mcus_x) : (mcu_x += 1) {
            // Decode all blocks for this MCU
            var blocks: [4][4][64]i32 = undefined;
            var block_outputs: [4][4][64]u8 = undefined;

            for (0..dec.num_components) |comp| {
                const h_s = dec.components[comp].h_samples;
                const v_s = dec.components[comp].v_samples;
                var bv: u8 = 0;
                while (bv < v_s) : (bv += 1) {
                    var bh: u8 = 0;
                    while (bh < h_s) : (bh += 1) {
                        const block_idx = @as(usize, bv) * h_s + bh;
                        try dec.decodeBlock(comp, &blocks[comp][block_idx]);
                        Decoder.idct(&blocks[comp][block_idx], &block_outputs[comp][block_idx]);
                    }
                }
            }

            // Convert blocks to pixels
            var py: u32 = 0;
            while (py < mcu_height) : (py += 1) {
                var px: u32 = 0;
                while (px < mcu_width) : (px += 1) {
                    const img_x = mcu_x * mcu_width + px;
                    const img_y = mcu_y * mcu_height + py;
                    if (img_x >= dec.width or img_y >= dec.height) continue;

                    if (dec.num_components == 1) {
                        // Grayscale
                        const sample = block_outputs[0][0][py * 8 + px];
                        bmp.setPixel(img_x, img_y, Color.rgb(sample, sample, sample));
                    } else {
                        // YCbCr → RGB
                        const y_val = getSample(&block_outputs[0], px, py, dec.components[0].h_samples, dec.components[0].v_samples, max_h, max_v);
                        const cb_val = getSample(&block_outputs[1], px, py, dec.components[1].h_samples, dec.components[1].v_samples, max_h, max_v);
                        const cr_val = getSample(&block_outputs[2], px, py, dec.components[2].h_samples, dec.components[2].v_samples, max_h, max_v);

                        const yf: f64 = @floatFromInt(y_val);
                        const cbf: f64 = @as(f64, @floatFromInt(cb_val)) - dc_level_shift;
                        const crf: f64 = @as(f64, @floatFromInt(cr_val)) - dc_level_shift;

                        const r = clampU8(@intFromFloat(@round(yf + 1.402 * crf)));
                        const g = clampU8(@intFromFloat(@round(yf - 0.344136 * cbf - 0.714136 * crf)));
                        const b = clampU8(@intFromFloat(@round(yf + 1.772 * cbf)));
                        bmp.setPixel(img_x, img_y, Color.rgb(r, g, b));
                    }
                }
            }

            mcu_count += 1;
            if (dec.restart_interval > 0 and mcu_count % @as(u32, dec.restart_interval) == 0) {
                dec.dc_pred = [_]i32{0} ** 4;
                dec.bits_in = 0;
                dec.bit_buf = 0;
                // Skip to next byte-aligned position and find restart marker
                if (dec.pos < dec.data.len and dec.data[dec.pos] == marker_prefix) {
                    dec.pos += 1;
                    if (dec.pos < dec.data.len) dec.pos += 1;
                }
            }
        }
    }

    return bmp;
}

fn getSample(block_outputs: *const [4][64]u8, px: u32, py: u32, h_s: u8, v_s: u8, max_h: u8, max_v: u8) u8 {
    const sx = px * h_s / max_h;
    const sy = py * v_s / max_v;
    const bx = sx / 8;
    const by = sy / 8;
    const ix = sx % 8;
    const iy = sy % 8;
    const block_idx = by * h_s + bx;
    return block_outputs[block_idx][iy * 8 + ix];
}

// ─── JPEG Encoder ───

// Bit-level writer for JPEG entropy-coded data.
// Handles byte stuffing: any 0xFF byte in the output is followed by 0x00.
// https://www.w3.org/Graphics/JPEG/itu-t81.pdf (Section B.1.1.5)
const BitWriter = struct {
    output: *ByteList,
    buf: u8,
    bits: u4,

    fn init(output: *ByteList) BitWriter {
        return .{ .output = output, .buf = 0, .bits = 0 };
    }

    fn writeBits(self: *BitWriter, value: u16, count: u5) !void {
        var remaining: u5 = count;
        while (remaining > 0) {
            remaining -= 1;
            const bit: u8 = @truncate((value >> @as(u4, @intCast(remaining))) & 1);
            self.buf = (self.buf << 1) | bit;
            self.bits += 1;
            if (self.bits == 8) {
                try self.output.append(self.buf);
                if (self.buf == marker_prefix) {
                    try self.output.append(byte_stuffing_suffix);
                }
                self.buf = 0;
                self.bits = 0;
            }
        }
    }

    fn flush(self: *BitWriter) !void {
        if (self.bits > 0) {
            const shift: u3 = @intCast(8 - @as(u8, self.bits));
            self.buf <<= shift;
            self.buf |= @truncate((@as(u16, 1) << shift) - 1);
            try self.output.append(self.buf);
            if (self.buf == marker_prefix) {
                try self.output.append(byte_stuffing_suffix);
            }
            self.buf = 0;
            self.bits = 0;
        }
    }
};

fn fdct(input: *const [64]f64, output: *[64]f64) void {
    var temp: [64]f64 = undefined;

    // 1D DCT on rows
    for (0..8) |y| {
        for (0..8) |u| {
            const cu: f64 = if (u == 0) (1.0 / @sqrt(2.0)) else 1.0;
            var sum: f64 = 0;
            for (0..8) |x| {
                sum += input[y * 8 + x] * cos_table[x][u];
            }
            temp[y * 8 + u] = cu * sum / 2.0;
        }
    }

    // 1D DCT on columns
    for (0..8) |u| {
        for (0..8) |v| {
            const cv: f64 = if (v == 0) (1.0 / @sqrt(2.0)) else 1.0;
            var sum: f64 = 0;
            for (0..8) |y| {
                sum += temp[y * 8 + u] * cos_table[y][v];
            }
            output[v * 8 + u] = cv * sum / 2.0;
        }
    }
}

fn scaleQuantTable(base: *const [64]u8, quality: u8) [64]u16 {
    const q: u32 = if (quality < 1) 1 else if (quality > 100) 100 else quality;
    const scale: u32 = if (q < 50) 5000 / q else 200 - 2 * q;

    var table: [64]u16 = undefined;
    for (0..64) |i| {
        var val = (@as(u32, base[i]) * scale + 50) / 100;
        if (val < 1) val = 1;
        if (val > 255) val = 255;
        table[i] = @intCast(val);
    }
    return table;
}

/// Encodes a Bitmap into a baseline JPEG image with 4:2:0 chroma subsampling.
/// Applies forward DCT, quantization, and Huffman coding as defined in ITU-T T.81.
/// https://www.w3.org/Graphics/JPEG/itu-t81.pdf
/// https://en.wikipedia.org/wiki/JPEG#JPEG_codec_example
pub fn encode(allocator: std.mem.Allocator, bmp: *const Bitmap, quality: u8) ![]u8 {
    var output = ByteList.init(allocator);
    errdefer output.deinit();

    const lum_qt= scaleQuantTable(&std_lum_qt, quality);
    const chrom_qt = scaleQuantTable(&std_chrom_qt, quality);

    // Build encoding Huffman tables
    const dc_lum_enc = buildEncTable(&std_dc_lum_bits, &std_dc_lum_vals);
    const dc_chrom_enc = buildEncTable(&std_dc_chrom_bits, &std_dc_chrom_vals);
    const ac_lum_enc = buildEncTable(&std_ac_lum_bits, &std_ac_lum_vals);
    const ac_chrom_enc = buildEncTable(&std_ac_chrom_bits, &std_ac_chrom_vals);

    // SOI
    try output.appendSlice(&[_]u8{ marker_prefix, marker_soi & 0xFF });

    // APP0 (JFIF)
    try output.appendSlice(&marker_app0);
    try output.appendSlice(&jfif_app0_data);

    // DQT (luminance)
    try writeQuantTable(&output, 0, &lum_qt);
    // DQT (chrominance)
    try writeQuantTable(&output, 1, &chrom_qt);

    // SOF0 (baseline DCT, YCbCr 4:2:0)
    const width = bmp.width;
    const height = bmp.height;
    try output.appendSlice(&[_]u8{
        marker_prefix, marker_sof0,
        0x00, 0x11, // length = 17
        jpeg_precision,
    });
    try output.append(@truncate(height >> 8));
    try output.append(@truncate(height));
    try output.append(@truncate(width >> 8));
    try output.append(@truncate(width));
    try output.appendSlice(&[_]u8{
        jpeg_num_components,
        0x01, 0x22, 0x00, // Y: id=1, h=2,v=2, qt=0
        0x02, 0x11, 0x01, // Cb: id=2, h=1,v=1, qt=1
        0x03, 0x11, 0x01, // Cr: id=3, h=1,v=1, qt=1
    });

    // DHT tables
    try writeHuffTable(&output, 0x00, &std_dc_lum_bits, &std_dc_lum_vals);
    try writeHuffTable(&output, 0x10, &std_ac_lum_bits, &std_ac_lum_vals);
    try writeHuffTable(&output, 0x01, &std_dc_chrom_bits, &std_dc_chrom_vals);
    try writeHuffTable(&output, 0x11, &std_ac_chrom_bits, &std_ac_chrom_vals);

    // SOS
    try output.appendSlice(&[_]u8{
        marker_prefix, marker_sos,
        0x00, 0x0C, // length = 12
        jpeg_num_components,
        0x01, 0x00, // Y: dc=0, ac=0
        0x02, 0x11, // Cb: dc=1, ac=1
        0x03, 0x11, // Cr: dc=1, ac=1
        0x00, 0x3F, 0x00, // spectral selection, successive approx
    });

    // Encode scan data
    var bw = BitWriter.init(&output);
    var dc_pred = [_]i32{ 0, 0, 0 };

    const mcu_w: u32 = mcu_size_420;
    const mcu_h: u32 = mcu_size_420;
    const mcus_x = (width + mcu_w - 1) / mcu_w;
    const mcus_y = (height + mcu_h - 1) / mcu_h;

    var mcu_y: u32 = 0;
    while (mcu_y < mcus_y) : (mcu_y += 1) {
        var mcu_x: u32 = 0;
        while (mcu_x < mcus_x) : (mcu_x += 1) {
            // Collect Y, Cb, Cr samples for this MCU
            var y_samples: [4][64]f64 = undefined; // 4 Y blocks (2x2)
            var cb_samples: [64]f64 = undefined;
            var cr_samples: [64]f64 = undefined;
            @memset(&cb_samples, 0);
            @memset(&cr_samples, 0);

            // Sample Y at full resolution (4 blocks of 8x8)
            for (0..2) |by| {
                for (0..2) |bx| {
                    const block_idx = by * 2 + bx;
                    for (0..8) |iy| {
                        for (0..8) |ix| {
                            const img_x = @min(mcu_x * mcu_w + @as(u32, @intCast(bx)) * 8 + @as(u32, @intCast(ix)), width - 1);
                            const img_y = @min(mcu_y * mcu_h + @as(u32, @intCast(by)) * 8 + @as(u32, @intCast(iy)), height - 1);
                            const color = bmp.getPixel(img_x, img_y);
                            const rf: f64 = @floatFromInt(color.r);
                            const gf: f64 = @floatFromInt(color.g);
                            const bf: f64 = @floatFromInt(color.b);
                            y_samples[block_idx][iy * 8 + ix] = 0.299 * rf + 0.587 * gf + 0.114 * bf - dc_level_shift;
                        }
                    }
                }
            }

            // Sample Cb/Cr at half resolution (1 block each, average 2x2 pixels)
            for (0..8) |iy| {
                for (0..8) |ix| {
                    var cb_sum: f64 = 0;
                    var cr_sum: f64 = 0;
                    for (0..2) |dy| {
                        for (0..2) |dx| {
                            const img_x = @min(mcu_x * mcu_w + @as(u32, @intCast(ix)) * 2 + @as(u32, @intCast(dx)), width - 1);
                            const img_y = @min(mcu_y * mcu_h + @as(u32, @intCast(iy)) * 2 + @as(u32, @intCast(dy)), height - 1);
                            const color = bmp.getPixel(img_x, img_y);
                            const rf: f64 = @floatFromInt(color.r);
                            const gf: f64 = @floatFromInt(color.g);
                            const bf: f64 = @floatFromInt(color.b);
                            cb_sum += -0.168736 * rf - 0.331264 * gf + 0.5 * bf;
                            cr_sum += 0.5 * rf - 0.418688 * gf - 0.081312 * bf;
                        }
                    }
                    cb_samples[iy * 8 + ix] = cb_sum / 4.0;
                    cr_samples[iy * 8 + ix] = cr_sum / 4.0;
                }
            }

            // Encode Y blocks (4 blocks)
            for (0..4) |bi| {
                var dct_out: [64]f64 = undefined;
                fdct(&y_samples[bi], &dct_out);
                try encodeBlock(&bw, &dct_out, &lum_qt, &dc_lum_enc, &ac_lum_enc, &dc_pred[0]);
            }

            // Encode Cb block
            {
                var dct_out: [64]f64 = undefined;
                fdct(&cb_samples, &dct_out);
                try encodeBlock(&bw, &dct_out, &chrom_qt, &dc_chrom_enc, &ac_chrom_enc, &dc_pred[1]);
            }

            // Encode Cr block
            {
                var dct_out: [64]f64 = undefined;
                fdct(&cr_samples, &dct_out);
                try encodeBlock(&bw, &dct_out, &chrom_qt, &dc_chrom_enc, &ac_chrom_enc, &dc_pred[2]);
            }
        }
    }

    try bw.flush();

    // EOI
    try output.appendSlice(&marker_eoi);

    return output.toOwnedSlice();
}

fn encodeBlock(
    bw: *BitWriter,
    dct: *const [64]f64,
    qt: *const [64]u16,
    dc_enc: *const HuffEncTable,
    ac_enc: *const HuffEncTable,
    dc_pred: *i32,
) !void {
    // Quantize and zigzag
    var quantized: [64]i32 = undefined;
    for (0..64) |i| {
        const q: f64 = @floatFromInt(qt[i]);
        quantized[zigzag_inverse[i]] = @intFromFloat(@round(dct[i] / q));
    }

    // Encode DC
    const dc_val = quantized[0];
    const dc_diff = dc_val - dc_pred.*;
    dc_pred.* = dc_val;

    const dc_cat = bitSize(dc_diff);
    try bw.writeBits(dc_enc.codes[dc_cat], @intCast(dc_enc.sizes[dc_cat]));
    if (dc_cat > 0) {
        const dc_bits: u16 = if (dc_diff >= 0)
            @intCast(dc_diff)
        else
            @intCast(@as(i32, dc_diff) + (@as(i32, 1) << dc_cat) - 1);
        try bw.writeBits(dc_bits, dc_cat);
    }

    // Encode AC
    var last_nonzero: usize = 63;
    while (last_nonzero > 0 and quantized[last_nonzero] == 0) {
        last_nonzero -= 1;
    }

    if (last_nonzero == 0 and quantized[0] == dc_val) {
        // All AC coefficients are zero
        try bw.writeBits(ac_enc.codes[ac_eob_symbol], @intCast(ac_enc.sizes[ac_eob_symbol])); // EOB
        return;
    }

    var i: usize = 1;
    while (i <= last_nonzero) {
        var run: u8 = 0;
        while (i <= last_nonzero and quantized[i] == 0) {
            run += 1;
            i += 1;
        }

        while (run >= ac_zrl_run) {
            // ZRL: 16 zeros
            try bw.writeBits(ac_enc.codes[ac_zrl_symbol], @intCast(ac_enc.sizes[ac_zrl_symbol]));
            run -= ac_zrl_run;
        }

        if (i > last_nonzero) break;

        const ac_val = quantized[i];
        const ac_cat = bitSize(ac_val);
        const symbol = (run << 4) | ac_cat;
        try bw.writeBits(ac_enc.codes[symbol], @intCast(ac_enc.sizes[symbol]));
        if (ac_cat > 0) {
            const ac_bits: u16 = if (ac_val >= 0)
                @intCast(ac_val)
            else
                @intCast(@as(i32, ac_val) + (@as(i32, 1) << ac_cat) - 1);
            try bw.writeBits(ac_bits, ac_cat);
        }
        i += 1;
    }

    // EOB if we haven't reached position 63
    if (last_nonzero < block_coeff_count - 1) {
        try bw.writeBits(ac_enc.codes[ac_eob_symbol], @intCast(ac_enc.sizes[ac_eob_symbol]));
    }
}

fn writeQuantTable(output: *ByteList, table_id: u8, qt: *const [64]u16) !void {
    try output.appendSlice(&[_]u8{ marker_prefix, marker_dqt });
    try output.appendSlice(&[_]u8{ 0x00, 0x43 }); // length = 67
    try output.append(table_id); // 8-bit precision (0) | table ID
    for (0..64) |i| {
        try output.append(@intCast(qt[zigzag_order[i]]));
    }
}

fn writeHuffTable(output: *ByteList, class_id: u8, bits: *const [16]u8, vals: []const u8) !void {
    try output.appendSlice(&[_]u8{ marker_prefix, marker_dht });
    const len: u16 = @intCast(2 + 1 + 16 + vals.len);
    try output.append(@truncate(len >> 8));
    try output.append(@truncate(len));
    try output.append(class_id);
    try output.appendSlice(bits);
    try output.appendSlice(vals);
}
