# vviewer

Image viewer for V, built with `gg`/sokol and the pure-V decoders from
[`vlib/image/decoders`](../../vlib/image/decoders/README.md). No
`stbi`/stb_image is involved: JPEG, PNG, BMP, WEBP, TIFF, and GIF bytes
are decoded by our own V code.

## Build and run

From this directory:

```sh
v run .            # run
v -prod .          # optimized build
```

A C compiler is required (the sokol backend is C). The viewer renders
through the standard sokol backend (OpenGL on Linux, D3D11 on Windows,
Metal on macOS); no custom shader compilation is needed.

## Usage

Command line — mix files, folders, and zip archives:

```sh
viewer img1.jpg img2.bmp folder1 folder2.zip
```

All folders and zips are scanned for images. You can also start the
viewer with no arguments and drag and drop files, folders, or zips onto
the window.

Accepted extensions: `.jpg`, `.jpeg`, `.png`, `.bmp`, `.gif`, `.webp`,
`.tiff`, and `.zip` archives containing them.

## Controls

| Input                    | Action                        |
| ------------------------ | ----------------------------- |
| `ESC` / `q`              | Quit                          |
| `right` / `left`         | Next / previous image         |
| `up` / `down`            | Next / previous folder        |
| `F`                      | Toggle full screen            |
| `R`                      | Rotate 90 degrees             |
| `I`                      | Toggle info text              |
| `P`                      | Pause / play animation        |
| `H`                      | Toggle help                   |
| `1` / `2` / `3`          | Sort by name / date / as loaded |
| Mouse wheel              | Next / previous image         |
| Left mouse button (hold) | Pan                           |
| Right mouse button (hold)| Zoom                          |
| Middle mouse button      | Reset view / fill window      |

Animation players and their encoded input are released when switching images,
including builds with `-gc none`.

## Limitations

- Animated GIF and animated WEBP are played back frame by frame. APNG
  files decode, but the viewer shows the static (first) frame only.
- Multi-page TIFF files show the first page only.
- 16-bit PNG and 12-bit JPEG are scaled down to 8-bit RGBA for display.
- Zip archives opened through the bundled miniz library cannot be opened
  directly from paths with non-ASCII characters on Windows; the viewer
  works around this by copying the archive to an ASCII-only temp file.
- The viewer displays images; it does not write or convert them. Use the
  decoder packages directly for that.
