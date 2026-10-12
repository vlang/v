## Description

`fontstash` is a thin wrapper over <https://github.com/memononen/fontstash>,
which in turn is a light-weight online font texture atlas builder written in C.
It uses stb_truetype to render fonts on demand to a texture atlas

On macOS fontstash is compiled into a cached native object. The V bindings provide its public ABI
without including the implementation in generated C. The native allocator follows the selected V
GC mode, including font data transferred with `free_data: true`.
