# Cross-platform documentation

By default, `v doc` documents a module for the platform that it runs on, and `-os` selects
another one, like `v doc -os windows os`. With `-os cross` (or `-os all`), it documents the
symbols of all platforms together, like `v doc -os cross os`.

The files of each platform are the ones that the compiler builds for it, so `_nix.c.v` files
are for all platforms but Windows, `_darwin.c.v` files are for macOS, and a `_default.c.v` file
is for the platforms without their own file. A symbol that is only available on some
platforms has a note, like `Available on Windows only.`, which is a `//` comment in the text
output. A symbol that is available on all platforms has none.

A symbol that is declared differently on some platforms, like a struct with other fields on
Windows, is documented once for each declaration, each with its own note. Declarations that
only differ in their attributes, comments, layout, or the names of their parameters are
documented as one. Constants that follow each other and are available on the same platforms
are documented together. Differences inside string literals remain distinct constant values.

Examples, with `-check-examples` or `-unsafe-run-examples`, are only checked for the symbols
that are available on the platform that `v doc` runs on. When declarations have different
examples, the examples of the declaration for that platform are selected.
