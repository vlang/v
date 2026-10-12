## Description

`szip` is a thin wrapper around [miniz.h](https://github.com/richgel999/miniz),
which in turn is "Single C source file zlib-replacement library,
originally from code.google.com/p/miniz".
It provides utility functions for reading/writing .zip files.

Opening an entry by name in read-only mode returns an error when that entry is missing.

On macOS, the bundled C implementation is compiled once into a cached native object.
Generated program C uses V declarations and contains no ZIP or system header includes.

`Fn_on_extract_entry` matches the native callback: `fn (&char, voidptr) i32`.
Its arguments are the extracted file name and the caller's context; returning zero continues
extraction. ZIP read counts, entry indexes, and entry totals use the native pointer-sized
signed result type.
