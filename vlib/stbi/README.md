## Description

`stbi` is a thin wrapper around [stb](https://github.com/nothings/stb)'s stb_image.h, which in turn
is "a public domain image loader" for popular graphics image file formats.

By default, `stbi.load` and `stbi.load_from_memory` convert images to 4-channel RGBA pixel data.
`Image.nr_channels` reflects that in-memory layout, while `Image.original_nr_channels` keeps the
channel count from the source file as metadata.

When uploading to `sokol.gfx`, size the upload from `width * height * nr_channels`.
If you load with `desired_channels: 0`, convert the data yourself or use a matching pixel format
before creating a texture.
Also call `gfx.setup(...)` before `gfx.make_image(...)` and `gfx.make_sampler(...)`.

`stbi.info_from_memory` reads only the header of an encoded image and returns its `width`, `height`
and `nr_channels`, without decoding any pixel. Use it to size, filter or route images before paying
for a full `load`. Its `nr_channels` is what the file declares, the counterpart of
`Image.original_nr_channels` rather than of `Image.nr_channels`.
