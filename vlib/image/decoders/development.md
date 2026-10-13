# image.decoders — development guide

This document describes how the decoders are structured, how to build and
test them, and how to maintain them. It describes the V implementation
only.

## Architecture

One package per codec, one `.v` file per package. Codec packages depend
only on `image.decoders.common` (plus stdlib); the dispatcher depends on
all of them. The import graph has no cycles:

```text
decoder -> {jpeg, png, bmp, webp, tiff, gif} -> common
```

Shared types live in `common`:

- `Image`: decoded image, `width`, `height`, `rgba []u8` (top-down,
  straight alpha, 4 bytes per pixel).
- `Frame` / `Animation`: coalesced animation frames with `delay_ms`,
  `disposal`, and `loop_count`.
- `BitReader`: LSB-first and MSB-first bit reader shared by all decoders.

Conventions:

- Decoders are pure: they take `[]u8` and return `!Image` / `!Animation`.
  No global state, no file access (except `decoder.decode_file`).
- Errors are reported with V results and descriptive messages that name
  the failing structure (chunk, marker, tag, ...).
- Malformed input must produce an error, never a panic or an out-of-bounds
  read. Bounds are checked before every read.
- Large scratch buffers are reused through the per-decoder arena pattern
  (see `bmp.v`) so repeated decoding does not churn memory.
- Bit accumulation is written as `x * (1 << k)` instead of `x << k`.
  The checker warns that shifting a signed `int` can change the sign;
  multiplication by a power of two is identical in two's complement and
  keeps the build silent. Keep this style in new code.
- The JPEG arithmetic decoder keeps the full JPEG-spec parameter list
  (`ss`/`se`/`ah`/`al`/`ci`) on every variant for symmetry with the
  Huffman path. Parameters a given variant does not use are prefixed
  with `_` (for example `_se`, `_ah`, `_ci`) to silence the unused
  parameter notice; do not drop them from the signature.

## Layout

```text
vlib/image/decoders/
  README.md      module front page
  CODEC.md       supported features per codec
  development.md this file
  common/        shared types and BitReader
  decoder/       magic-byte dispatcher
  jpeg/          jpeg.v, jpeg_test.v, testdata/
  png/           png.v,  png_test.v,  testdata/
  bmp/           bmp.v,  bmp_test.v,  testdata/
  webp/          webp.v, webp_test.v, testdata/
  tiff/          tiff.v, tiff_test.v, testdata/
  gif/           gif.v,  gif_test.v,  testdata/
```

## Testing

Each codec package is fully self-contained: its tests and its test data
live in the same directory.

```sh
v test vlib/image/decoders/        # all codecs
v test vlib/image/decoders/webp/   # one codec
```

Test data conventions, per codec directory:

- `testdata/<name>.<ext>`: the encoded fixture (`jpg`, `png`, `webp`, ...).
- `testdata/<name>.rgba`: the expected raw RGBA bytes (the golden).
- `testdata/<name>.txt`: one line of metadata, at least `width height`,
  sometimes more (frame counts, geometry parameters) depending on codec.
- Animation goldens use `<name>_frame_NNN.rgba` (or `_fN.rgba` for WEBP).

Tests resolve data through `@DIR`, so they work regardless of the current
working directory.

### Removing test data

Test data is intentionally kept per codec so it can be pruned cheaply:

- Deleting a codec's `testdata/` directory and its `*_test.v` file is
  sufficient and independent of every other codec.
- Nothing outside a codec directory references its `testdata/`.
- If a codec's tests are removed, keep the decoder source: the golden
  files can be regenerated later.

## Future work

- Prune or slim down the binary testdata (it is the largest part of the
  package) once a smaller representative set is agreed on.
- Consider exposing decoded images as `image.RGBA` buffers instead of
  plain `[]u8`.
- Optional: register formats with `image.register_format` so the base
  `image` module can decode through the dispatcher.
