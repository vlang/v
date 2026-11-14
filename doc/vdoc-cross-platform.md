# Cross-platform documentation

By default, `v doc` documents a module for the platform that it runs on, and `-os` selects
another one, like `v doc -os windows os`. With `-os cross` (or `-os all`), it documents the
symbols of all platforms together, like `v doc -os cross os`.

For the standard library, `v doc -m -os cross vlib/` uses `vlib/.vdocignore` to exclude
alternative builtin runtimes, including the bare targets. These sources also declare
`module builtin` and would otherwise overwrite the main `builtin` documentation page.
Public submodules such as `builtin.wchar` remain included.

The files of each platform are the ones that the compiler builds for it, so `_nix.c.v` files
are for all platforms but Windows, `_darwin.c.v` files are for macOS, and a `_default.c.v` file
is for the platforms without their own file. A symbol that is only available on some
platforms has a note, like `Available on Windows only.`, which is a `//` comment in the text
output. A symbol that is available on all platforms has none.

A symbol that is declared differently on some platforms, like a struct with other fields on
Windows, or a constant with another value, is documented once for each declaration, together and
each with its own note. Declarations that only differ in their attributes, layout, or the names
of their parameters are documented as one, unless they have other doc comments or examples, so
that each platform shows its own. A doc comment that is only written on one platform documents
all of them. Differences inside string literals remain distinct constant values. Constants that
follow each other and are available on the same platforms are documented together, unless they
have another value on other platforms.

In the html output, a selector next to the name of the module shows only the symbols and the
declarations of one platform. The selected platform is kept for the other modules, and applied
while their pages load. Changing the platform keeps the section that is being read in place.

Examples, with `-check-examples` or `-unsafe-run-examples`, are only checked for the symbols
that are available on the platform that `v doc` runs on. When declarations have different
examples, the examples of the declaration for that platform are selected. This also applies
when otherwise identical declarations are documented together.
