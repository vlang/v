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

The codec packages from Go's image tree, such as PNG, JPEG, and GIF, are not
part of this module yet. They can be added later by registering decode
callbacks with this module.
