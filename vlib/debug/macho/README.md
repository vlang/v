# macho

`debug.macho` reads 64-bit Mach-O headers and `LC_SEGMENT_64` section metadata
on any host platform. Both little-endian and big-endian files are supported.

```v
import debug.macho
import os

data := os.read_bytes('app')!
f := macho.new_file(data)!
println(f.header.cpu_type)
for s in f.sections {
	println(s.name)
}
```

`new_file` returns an error if the header, load command area, command count,
command sizes, or section tables are truncated or inconsistent. It reads section
names and segment names from each section header and preserves section order.
Numeric fields use host byte order; `header.magic` is always `magic_64`.
Unknown load commands are skipped after their size and alignment are checked.

This is a metadata reader, not a complete executable validator. Section contents,
symbols, relocations, and the contents of other load commands are not decoded or
validated. Fat/universal binaries and 32-bit Mach-O files are not supported, even
though their magic constants are available for identification.

See Apple's [Mach-O definitions][mach-o] for the header layout.

[mach-o]: https://github.com/apple-oss-distributions/xnu/blob/main/EXTERNAL_HEADERS/mach-o/loader.h
