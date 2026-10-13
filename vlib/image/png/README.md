# image.png

PNG decoding, configuration inspection, and lossless eight-bit RGBA encoding. Importing this
module registers PNG with `image.decode`. See [image](../README.md) for examples and limits.

Apple optimized PNGs containing a `CgBI` chunk are unsupported. Both decoding and configuration
inspection return an error for them.
