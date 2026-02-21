# zic — Zig Image Converter

A command-line image conversion tool written in Zig with zero third-party dependencies.
All image codec implementations (BMP, PNG, JPEG) are included in the repository.

## Supported Formats

| Format | Read | Write |
|--------|------|-------|
| BMP    | ✓    | ✓     |
| PNG    | ✓    | ✓     |
| JPEG   | ✓    | ✓     |

## Usage

```
zic -i input.jpg -o output.png
zic -i photo.bmp -o photo.jpg
zic -i image.png -o image.bmp
zic --help
```

### Options

| Flag | Description |
|------|-------------|
| `-i`, `--input`  | Input image file path |
| `-o`, `--output` | Output image file path |
| `-q`, `--quality`| JPEG output quality 1-100 (default: 85) |
| `-h`, `--help`   | Show help |

The output format is auto-detected from the file extension.

## Building

```
zig build
```

## Running

```
zig build run -- -i input.jpg -o output.png
```
