# image.decoders

`image.decoders` provides pure-V decoders for six image formats:
JPEG, PNG, BMP, WEBP, TIFF, and GIF. Every decoder is a single `.v` file
with no C dependencies and no third-party libraries. All decoders produce
the same output shape: a `common.Image` with a top-down, straight
(non-premultiplied) RGBA8 pixel buffer, or a `common.Animation` of
coalesced frames.

## Packages

| Package                     | Contents                                              |
| --------------------------- | ----------------------------------------------------- |
| `image.decoders.common`     | `Image`, `Frame`, `Animation`, `BitReader`             |
| `image.decoders.decoder`    | Magic-byte detection and format dispatch               |
| `image.decoders.jpeg`       | JPEG decoder (baseline, progressive, arithmetic, 12-bit) |
| `image.decoders.png`        | PNG decoder (all color types, Adam7, APNG, gamma)      |
| `image.decoders.bmp`        | BMP decoder (BI_RGB, BI_BITFIELDS, RLE8/RLE4)          |
| `image.decoders.webp`       | WEBP decoder (VP8, VP8L, ALPH, animation)              |
| `image.decoders.tiff`       | TIFF decoder (strips/tiles, multi-page)                |
| `image.decoders.gif`        | GIF decoder (static and animated)                      |

See `CODEC.md` for the supported features of each format.

## Usage

Format-agnostic decoding (the dispatcher sniffs the magic bytes):

```v
import image.decoders.decoder

fn main() {
	img := decoder.decode_file('photo.jpg') or {
		eprintln('decode failed: ${err}')
		return
	}
	println('${img.width}x${img.height}')
}
```

Direct codec use, with animation:

```v
import image.decoders.gif

fn main() {
	data := []u8{} // your GIF bytes
	anim := gif.decode_animation(data) or { return }
	for frame in anim.frames {
		println('${frame.image.width}x${frame.image.height} @ ${frame.delay_ms}ms')
	}
}
```

Incremental playback (used by `examples/viewer`): `gif.GifPlayer` and
`webp.WebpPlayer` expose `step()` which returns the next composited
full-canvas RGBA buffer, plus `delay_ms()`, `loop_count()`, `reset()`,
and `free()`.

Decode dimensions, scratch buffers, and WEBP output geometry are checked against
the supported integer size before buffer allocation.
TIFF requires complete, matching strip or tile offset and byte count arrays.

## Testing

Each codec package ships its own test suite and test data:

```sh
v test vlib/image/decoders/
v test vlib/image/decoders/jpeg/
```

See `development.md` for the module layout and how to prune test data.
