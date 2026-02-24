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
