# dwarf

Module `dwarf` reads DWARF debugging information (milestone 1).

Supported:

- `.debug_info` compilation units, versions 2-5 (DWARF32 only).
- `.debug_abbrev` tables, DIE trees with decoded attributes.
- All classic attribute forms; DWARF v5 indexed forms report
  `unsupported form` instead of misdecoding.

Not yet supported: line tables, ranges, location lists, type units,
skeleton/split units, 64-bit DWARF.
