# dwarf

Module `dwarf` reads DWARF debugging information (milestone 1).

Supported:

- `.debug_info` compilation units, versions 2-5 (DWARF32 only).
- `.debug_abbrev` tables, DIE trees with decoded attributes.
- All classic attribute forms; DWARF v5 indexed forms report
  `unsupported form` instead of misdecoding.
- `.debug_line` tables, versions 2-5, including the line program,
  ranges via `parse_ranges`, and location expressions via
  `parse_loc_expr`. Validated against real gcc output as well as
  crafted fixtures.

Not yet supported: `.debug_rnglists`/`.debug_loclists` (v5 indexed
lists), type units, skeleton/split units, 64-bit DWARF.
