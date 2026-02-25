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

// ─── Tests ───

const testing = std.testing;

test "Color.rgb creates opaque color" {
    const c = Color.rgb(10, 20, 30);
    try testing.expectEqual(@as(u8, 10), c.r);
    try testing.expectEqual(@as(u8, 20), c.g);
    try testing.expectEqual(@as(u8, 30), c.b);
    try testing.expectEqual(@as(u8, 255), c.a);
}

test "Color.rgba creates color with alpha" {
    const c = Color.rgba(100, 150, 200, 128);
    try testing.expectEqual(@as(u8, 100), c.r);
    try testing.expectEqual(@as(u8, 150), c.g);
    try testing.expectEqual(@as(u8, 200), c.b);
    try testing.expectEqual(@as(u8, 128), c.a);
}

test "Color.rgb with extremes" {
    const black = Color.rgb(0, 0, 0);
    try testing.expectEqual(@as(u8, 0), black.r);
    try testing.expectEqual(@as(u8, 0), black.g);
    try testing.expectEqual(@as(u8, 0), black.b);
    try testing.expectEqual(@as(u8, 255), black.a);

    const white = Color.rgb(255, 255, 255);
    try testing.expectEqual(@as(u8, 255), white.r);
    try testing.expectEqual(@as(u8, 255), white.g);
    try testing.expectEqual(@as(u8, 255), white.b);
}

test "Color.rgba fully transparent" {
    const c = Color.rgba(255, 0, 0, 0);
    try testing.expectEqual(@as(u8, 0), c.a);
}

test "Bitmap.init creates zeroed bitmap" {
    var bmp = try Bitmap.init(testing.allocator, 4, 3);
    defer bmp.deinit();

    try testing.expectEqual(@as(u32, 4), bmp.width);
    try testing.expectEqual(@as(u32, 3), bmp.height);
    try testing.expectEqual(@as(usize, 12), bmp.pixels.len);

    // All pixels should be zeroed (rgba 0,0,0,0)
    const px = bmp.getPixel(0, 0);
    try testing.expectEqual(@as(u8, 0), px.r);
    try testing.expectEqual(@as(u8, 0), px.g);
    try testing.expectEqual(@as(u8, 0), px.b);
    try testing.expectEqual(@as(u8, 0), px.a);
}

test "Bitmap.init with 1x1" {
    var bmp = try Bitmap.init(testing.allocator, 1, 1);
    defer bmp.deinit();
    try testing.expectEqual(@as(usize, 1), bmp.pixels.len);
}

test "Bitmap setPixel and getPixel" {
    var bmp = try Bitmap.init(testing.allocator, 3, 3);
    defer bmp.deinit();

    const red = Color.rgb(255, 0, 0);
    bmp.setPixel(1, 2, red);

    const got = bmp.getPixel(1, 2);
    try testing.expectEqual(@as(u8, 255), got.r);
    try testing.expectEqual(@as(u8, 0), got.g);
    try testing.expectEqual(@as(u8, 0), got.b);
    try testing.expectEqual(@as(u8, 255), got.a);
}

test "Bitmap setPixel overwrites previous value" {
    var bmp = try Bitmap.init(testing.allocator, 2, 2);
    defer bmp.deinit();

    bmp.setPixel(0, 0, Color.rgb(10, 20, 30));
    bmp.setPixel(0, 0, Color.rgb(40, 50, 60));

    const got = bmp.getPixel(0, 0);
    try testing.expectEqual(@as(u8, 40), got.r);
    try testing.expectEqual(@as(u8, 50), got.g);
    try testing.expectEqual(@as(u8, 60), got.b);
}

test "Bitmap setPixel at corners" {
    var bmp = try Bitmap.init(testing.allocator, 4, 4);
    defer bmp.deinit();

    bmp.setPixel(0, 0, Color.rgb(1, 0, 0));
    bmp.setPixel(3, 0, Color.rgb(2, 0, 0));
    bmp.setPixel(0, 3, Color.rgb(3, 0, 0));
    bmp.setPixel(3, 3, Color.rgb(4, 0, 0));

    try testing.expectEqual(@as(u8, 1), bmp.getPixel(0, 0).r);
    try testing.expectEqual(@as(u8, 2), bmp.getPixel(3, 0).r);
    try testing.expectEqual(@as(u8, 3), bmp.getPixel(0, 3).r);
    try testing.expectEqual(@as(u8, 4), bmp.getPixel(3, 3).r);
}

test "Bitmap pixel independence" {
    var bmp = try Bitmap.init(testing.allocator, 2, 2);
    defer bmp.deinit();

    bmp.setPixel(0, 0, Color.rgb(1, 2, 3));
    bmp.setPixel(1, 0, Color.rgb(4, 5, 6));
    bmp.setPixel(0, 1, Color.rgb(7, 8, 9));
    bmp.setPixel(1, 1, Color.rgb(10, 11, 12));

    try testing.expectEqual(@as(u8, 1), bmp.getPixel(0, 0).r);
    try testing.expectEqual(@as(u8, 4), bmp.getPixel(1, 0).r);
    try testing.expectEqual(@as(u8, 7), bmp.getPixel(0, 1).r);
    try testing.expectEqual(@as(u8, 10), bmp.getPixel(1, 1).r);
}
