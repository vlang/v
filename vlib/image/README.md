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

Decoders for common formats (JPEG, PNG, BMP, WEBP, TIFF, GIF) live in the
`image.decoders` packages; see `decoders/README.md` and `decoders/CODEC.md`
for details. They decode to their own RGBA buffers and are not yet wired
into the `register_format` hooks of this module.

Sub-images share their pixel storage with the parent image, including the Y, Cb, Cr,
and alpha planes of `YCbCr` and `NYCbCrA`. Changes through either image are visible in both.
