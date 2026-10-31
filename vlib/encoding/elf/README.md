# encoding.elf

`parse_file(path)!` or `parse_bytes(data)!` reads ELF64 little-endian header fields,
section headers, and symbol tables. Other classes, endianness, extended section numbering,
invalid headers, and truncated tables return errors. Files without a section table or
section names are supported.

`file.machine` identifies known architectures and uses `.unknown` for other values.
`file.entry` is the entry address. `file.sections()` returns headers in file order.
`file.symbols()!` reads all `symtab` and `dynsym` sections and resolves their names through
linked string tables. Each `Symbol` exposes its name, value, size, section index, binding
via `bind()`, and type via `typ()`. A file without a symbol table returns an error.

Section and symbol tables are checked against the input size before reading. Invalid
symbol entry sizes, incomplete entries, wrong string-table types, and unterminated names
return errors. The reader does not load executables, process relocations, or verify code.
