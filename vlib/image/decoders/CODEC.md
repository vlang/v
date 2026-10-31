# Supported features per codec

This document lists what each decoder in `image.decoders` supports and what
it deliberately does not support. All decoders produce the same output
shape: a `common.Image` with a top-down, straight (non-premultiplied)
RGBA8 pixel buffer, or a `common.Animation` of coalesced frames.

## Common (`image.decoders.common`)

- `Image`: `width`, `height`, `rgba []u8` (`w * h * 4`, top-down, straight alpha).
- `Frame`: one coalesced animation frame with `delay_ms` and `disposal`.
- `Animation`: frame sequence plus `loop_count` (`0` = forever).
- `BitReader`: LSB-first and MSB-first bit reader used by all decoders.

## Dispatcher (`image.decoders.decoder`)

- `sniff(data)`: format detection by magic bytes (JPEG, PNG, BMP, GIF,
  WEBP, TIFF).
- `decode(data)`: dispatch to the matching decoder.
- `decode_animation(data)`: animated formats only (GIF); other formats
  fall back to the static `decode()` path.
- `decode_file(path)`: read a file from disk and decode it.

## JPEG (`image.decoders.jpeg`)

Supported:

- Baseline DCT (SOF0) and extended sequential (SOF1), Huffman coded.
- Progressive (SOF2), Huffman coded.
- Sequential (SOF9) and progressive (SOF10) arithmetic coding.
- 8-bit and 12-bit sample precision (12-bit output is scaled to 8-bit).
- Restart markers and restart intervals.
- Grayscale, 3-channel Y'CbCr, and 4-channel CMYK/YCCK (Adobe APP14).
- Common chroma subsampling ratios (4:4:4, 4:2:2, 4:2:0, 4:1:1).
- Multi-scan decoding with progressive refinement.

Not supported:

- Lossless (SOF3) and hierarchical modes.
- 16-bit precision.

## PNG (`image.decoders.png`)

Supported:

- Bit depths 1, 2, 4, 8, 16.
- Color types: grayscale, RGB, palette, grayscale+alpha, RGBA.
- Adam7 interlacing (all seven passes).
- `tRNS` transparency for gray, RGB, and palette images.
- Multi-`IDAT` streams.
- `gAMA` and `cHRM` parsing; optional color correction through
  `decode_color_corrected(data, target_gamma)`.
- APNG animation: `acTL`/`fcTL`/`fdAT`, disposal ops (none/background/
  previous), source/output blending, multi-`fdAT` frames.
- 16-bit images are scaled down to 8-bit RGBA output.

Not supported:

- 16-bit RGBA output (precision is reduced to 8 bits per channel).
- Text, time, and ancillary chunks beyond the color and animation ones.

## BMP (`image.decoders.bmp`)

Supported:

- OS/2 CORE (`12`) and Windows DIB headers `40`, `52`, `56`, `108`, `124`.
- Bit depths 1, 4, 8 (indexed), 16, 24, 32.
- `BI_RGB`, `BI_BITFIELDS` (including 5-6-5 and custom masks with alpha),
  `BI_RLE8`, `BI_RLE4`.
- Bottom-up and top-down rows (negative height), row padding.
- OS/2 12-byte headers with 1/4/8-bit indexed data.

Not supported:

- DDB/DDBh headers with embedded bitmaps.
- Halftoning hints and color spaces beyond mask interpretation.

## WEBP (`image.decoders.webp`)

Supported:

- RIFF/WEBP container and the `VP8X` extended header.
- `VP8L` lossless: meta-Huffman, color cache, LZ77 back-references, and
  the predictor, color, color-index, and delta-palette transforms.
- `VP8 ` lossy: keyframes with intra prediction and deblocking.
- `ALPH` compressed alpha plane (spatial prediction + entropy coding).
- `ICCP`, `EXIF`, `XMP` chunks are parsed and exposed.
- Output geometry through `Options`: crop and scale, using a port of the
  libwebp integer rescaler.
- Animation: `ANIM`/`ANMF` with dispose-to-background, keep, alpha
  blending, and loop count; `WebPAnimation`/`WebPFrame` expose the
  frame list, `total_duration_ms()`, and `frame_index_at()`.
- `WebpPlayer` for incremental frame stepping (used by the viewer).

Not supported:

- Lossless alpha stored outside `VP8L` (lossless images carry alpha
  inside `VP8L` itself).

## TIFF (`image.decoders.tiff`)

Supported:

- `II`/`MM` byte order, classic TIFF (magic 42), IFD chain.
- Strip and tile layouts.
- `PlanarConfiguration` 1 (chunky) and 2 (separate planes).
- Photometrics: WhiteIsZero, BlackIsZero, RGB, Palette; alpha through
  `ExtraSamples` (RGBA).
- Bit depths 1, 2, 4, 8, 16.
- Compression: None (1), LZW (5, with the TIFF early-change variant),
  PackBits (32773).
- Multi-page files through `decode_pages()` (returns a `common.Animation`
  of pages).

Not supported:

- BigTIFF (magic 43).
- JPEG-compressed TIFF, Deflate, CCITT/Huffman fax codes.

## GIF (`image.decoders.gif`)

Supported:

- GIF87a and GIF89a, Global and Local Color Tables.
- Graphic Control Extension: transparency index, per-frame delay,
  disposal methods (keep, restore-to-background, restore-to-previous).
- LZW image data (clear codes, 12-bit max width, `KwKwK` case, deferred
  clear at 4096).
- 4-pass interlace layout.
- NETSCAPE2.0 / ANIMEXTS1.0 loop count.
- Full animation coalescing: `decode_animation()` returns composited
  full-canvas frames with per-frame delays.
- `GifPlayer` for incremental frame stepping (used by the viewer).

Not supported:

- Decoding a single frame without coalescing the preceding frames.
