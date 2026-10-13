# image.draw

`resize` scales any `image.Image` using bilinear interpolation by default, or nearest-neighbor
sampling with `interpolation: .nearest_neighbor`. Bilinear sampling respects premultiplied alpha.
See [image](../README.md) for an image-file resizing example.
