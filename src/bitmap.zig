const std = @import("std");

pub const Color = struct {
    r: u8,
    g: u8,
    b: u8,
    a: u8,

    pub fn rgb(r: u8, g: u8, b: u8) Color {
        return .{ .r = r, .g = g, .b = b, .a = 255 };
    }

    pub fn rgba(r: u8, g: u8, b: u8, a: u8) Color {
        return .{ .r = r, .g = g, .b = b, .a = a };
    }
};

pub const Bitmap = struct {
    width: u32,
    height: u32,
    pixels: []Color,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, width: u32, height: u32) !Bitmap {
        const size = @as(usize, width) * @as(usize, height);
        const pixels = try allocator.alloc(Color, size);
        @memset(pixels, Color.rgba(0, 0, 0, 0));
        return .{
            .width = width,
            .height = height,
            .pixels = pixels,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Bitmap) void {
        self.allocator.free(self.pixels);
    }

    pub fn getPixel(self: *const Bitmap, x: u32, y: u32) Color {
        return self.pixels[@as(usize, y) * @as(usize, self.width) + @as(usize, x)];
    }

    pub fn setPixel(self: *Bitmap, x: u32, y: u32, color: Color) void {
        self.pixels[@as(usize, y) * @as(usize, self.width) + @as(usize, x)] = color;
    }
};
