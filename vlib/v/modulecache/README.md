The `v.modulecache` module stores V3 module interfaces, compiled objects, and program plans.

Use `module_headers_with_const_order` when generating interfaces for several modules from the
same checked AST. It builds the declaration, module, and import indexes once for the batch and
preserves each module's source order, attributes, implicit imports, and runtime constant order.
For a single module, `module_header_with_const_order` provides the same header text.

Struct interfaces preserve explicit field types, including names such as `i64 i64`, alongside
embedded fields and field attributes. Cached imports retain the same field reflection and JSON keys.

Cached builds preserve the same module initialization order as uncached builds.
Dependencies initialize before their importers, and direct imports are visited
in source order even when the importer has no `init()` function of its own.

Declaration headers preserve macros that define static storage, including generated string literals.
Parallel C compilation can include these definitions in each translation unit.

Cleanup hooks follow the reverse dependency order, including through modules without
initialization or cleanup hooks. Cleanup-only modules remain included when no startup or
import metadata references them.

`Manager.discard_program_entries(source_files)` invalidates the whole-program C plan,
prepared plan, generic and incremental snapshots, and cached executable for that source set.
It removes their validation stamps while preserving the artifact files and compiled module entries.
When C compilation rejects an incrementally restored program, the driver uses this method before
retrying from sources with incremental restoration disabled.
