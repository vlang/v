# pe

`debug.pe` reads the COFF machine identifier and section headers of Windows
Portable Executable (PE) images on any host platform. It accepts PE32 and PE32+
images with DOS and PE signatures; standalone COFF object files are not supported.

```v
import debug.pe
import os

data := os.read_bytes('app.exe')!
f := pe.new_file(data)!
println(f.machine)
for s in f.sections {
	println(s.name)
}
```

`new_file` returns an error for invalid signatures, an invalid PE-header offset,
or truncated COFF, optional, or section headers. It returns all section headers
in file order. `section_by_name` returns the first matching section, or `none`.
Names occupy at most eight bytes and end at the first NUL byte.

This is a header reader, not a complete executable validator. The optional header
is skipped using its declared size. Section contents, imports, symbols,
relocations, and loader-specific constraints are not decoded or validated.
The module does not write executables or depend on compiler code generation.

See Microsoft's [PE format specification][pe-format] for the header layout.

[pe-format]: https://learn.microsoft.com/en-us/windows/win32/debug/pe-format
