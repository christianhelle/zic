const std = @import("std");
const bitmap = @import("bitmap.zig");
const bmp = @import("bmp.zig");
const png = @import("png.zig");
const jpeg = @import("jpeg.zig");
const format = @import("format.zig");
const cli = @import("cli.zig");
const processor = @import("processor.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var args = try cli.Args.init(allocator);
    defer args.deinit();

    if (args.options.help) {
        cli.printUsage();
        if (args.hasPathArgs()) {
            std.debug.print("\nErrors:\n", .{});
            for (args.errors.items) |err| {
                std.debug.print("- {s}\n", .{err});
            }
        }
        return;
    }

    if (args.isBatchMode()) {
        try processor.processImages(allocator, args);
        return;
    }

    processor.processImage(
        allocator,
        args.input_path,
        args.output_path,
        args.options,
    ) catch |err| {
        std.debug.print("Error processing '{s}': {}\n", .{ args.input_path, err });
    };
}
