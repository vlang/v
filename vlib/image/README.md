# image

`image` provides basic in-memory 2D image types and geometry helpers,
translated from Go's `image` package into V-style APIs.

The module currently includes:

- `Point` and `Rectangle` geometry helpers.
- Packed pixel buffers: `RGBA`, `RGBA64`, `NRGBA`, `NRGBA64`, `Alpha`,
  `Alpha16`, `Gray`, `Gray16`, `CMYK`, and `Paletted`.
- Y'CbCr buffers with common chroma subsampling ratios.
- `Uniform` images for a single color.
- `image.color`, with standard color types, color model conversion, palettes,
  Y'CbCr conversion, and CMYK conversion.
- Format registration hooks with `register_format`, `decode`, and
  `decode_config`.

```v
import image
import image.color

mut img := image.new_rgba(image.rect(0, 0, 2, 2))
img.set_rgba(0, 0, color.RGBA{
	r: 255
	g: 0
	b: 0
	a: 255
})

assert img.bounds().dx() == 2
assert img.rgba_at(0, 0).r == 255
```

Import `image.png` or `image.jpeg` to register those formats for `image.decode` and
`image.decode_config`. Both codecs use the existing bundled stb implementation and return
independent eight-bit `NRGBA` images; sixteen-bit PNG channels are reduced to eight bits.
Apple optimized PNGs containing a `CgBI` chunk are rejected by decoding and configuration
inspection because their premultiplied BGRA pixels are unsupported. JPEG encoding drops alpha.
GIF is not registered.

`png.encode(mut writer, img)` and `jpeg.encode(mut writer, img, quality: 85)` write to any
`io.Writer`, including files. `encode_to_bytes` returns encoded bytes for HTTP responses.
JPEG quality must be in `1 .. 100` and defaults to 75. PNG uses lossless RGBA encoding.
The decoders buffer encoded input in memory and propagate reader errors.
The stb backend does not validate PNG chunk CRCs. Its global vertical-flip settings also
affect these codecs, so applications using `stbi.set_flip_vertically_on_load` or
`stbi.set_flip_vertically_on_write` control their orientation through those settings.

`image.draw.resize(img, width, height)` produces an `NRGBA` image using bilinear sampling.
Pass `interpolation: .nearest_neighbor` for pixel art. Source bounds can start at any coordinate,
and bilinear interpolation uses premultiplied channels to preserve transparent edges.
Empty or oversized image dimensions return an error.

```v ignore
import image
import image.draw
import image.jpeg
import image.png
import os

mut input := os.open('input.png')!
img, _ := image.decode(input)!
input.close()
thumbnail := draw.resize(img, 320, 240)!
mut output := os.create('thumbnail.jpg')!
jpeg.encode(mut output, thumbnail, quality: 85)!
output.close()
```

Pure-V decoders for JPEG, PNG, BMP, WEBP, TIFF, and GIF also live in the
`image.decoders` packages; see `decoders/README.md` and `decoders/CODEC.md`.
They produce their own RGBA buffers and are not wired into this module's
`register_format` hooks.

Sub-images share their pixel storage with the parent image, including the Y, Cb, Cr,
and alpha planes of `YCbCr` and `NYCbCrA`. Changes through either image are visible in both.
