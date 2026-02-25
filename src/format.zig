pub const Format = enum { bmp_fmt, png_fmt, jpg_fmt };

fn endsWith(haystack: []const u8, needle: []const u8) bool {
    if (haystack.len < needle.len) return false;
    const tail = haystack[haystack.len - needle.len ..];
    for (tail, needle) |a, b| {
        const la = if (a >= 'A' and a <= 'Z') a + 32 else a;
        if (la != b) return false;
    }
    return true;
}

pub fn detectFormat(path: []const u8) ?Format {
    if (endsWith(path, ".bmp")) return .bmp_fmt;
    if (endsWith(path, ".png")) return .png_fmt;
    if (endsWith(path, ".jpg") or
        endsWith(path, ".jpeg")) return .jpg_fmt;
    return null;
}

// ─── Tests ───

const testing = @import("std").testing;

test "endsWith matches exact suffix" {
    try testing.expect(endsWith("hello.bmp", ".bmp"));
    try testing.expect(endsWith(".bmp", ".bmp"));
}

test "endsWith rejects non-matching suffix" {
    try testing.expect(!endsWith("hello.png", ".bmp"));
    try testing.expect(!endsWith("hello", ".bmp"));
}

test "endsWith handles haystack shorter than needle" {
    try testing.expect(!endsWith("ab", ".bmp"));
    try testing.expect(!endsWith("", ".bmp"));
}

test "endsWith is case insensitive for uppercase" {
    try testing.expect(endsWith("image.BMP", ".bmp"));
    try testing.expect(endsWith("image.Bmp", ".bmp"));
    try testing.expect(endsWith("image.PNG", ".png"));
    try testing.expect(endsWith("image.JPG", ".jpg"));
    try testing.expect(endsWith("image.JPEG", ".jpeg"));
}

test "detectFormat returns bmp for .bmp" {
    try testing.expectEqual(Format.bmp_fmt, detectFormat("image.bmp").?);
    try testing.expectEqual(Format.bmp_fmt, detectFormat("path/to/image.BMP").?);
}

test "detectFormat returns png for .png" {
    try testing.expectEqual(Format.png_fmt, detectFormat("image.png").?);
    try testing.expectEqual(Format.png_fmt, detectFormat("path/to/image.PNG").?);
}

test "detectFormat returns jpg for .jpg and .jpeg" {
    try testing.expectEqual(Format.jpg_fmt, detectFormat("photo.jpg").?);
    try testing.expectEqual(Format.jpg_fmt, detectFormat("photo.jpeg").?);
    try testing.expectEqual(Format.jpg_fmt, detectFormat("photo.JPG").?);
    try testing.expectEqual(Format.jpg_fmt, detectFormat("photo.JPEG").?);
}

test "detectFormat returns null for unknown extension" {
    try testing.expect(detectFormat("file.gif") == null);
    try testing.expect(detectFormat("file.tiff") == null);
    try testing.expect(detectFormat("file.webp") == null);
    try testing.expect(detectFormat("noextension") == null);
    try testing.expect(detectFormat("") == null);
}

test "detectFormat handles paths with multiple dots" {
    try testing.expectEqual(Format.png_fmt, detectFormat("my.file.png").?);
    try testing.expectEqual(Format.bmp_fmt, detectFormat("a.b.c.bmp").?);
}

test "endsWith with equal length strings" {
    try testing.expect(endsWith(".bmp", ".bmp"));
    try testing.expect(!endsWith(".png", ".bmp"));
}
