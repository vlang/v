The `v.modulecache` module stores V3 module interfaces, compiled objects, and program plans.

Use `module_headers_with_const_order` when generating interfaces for several modules from the
same checked AST. It builds the declaration, module, and import indexes once for the batch and
preserves each module's source order, attributes, implicit imports, and runtime constant order.
For a single module, `module_header_with_const_order` provides the same header text.

Cached builds preserve the same module initialization order as uncached builds.
Dependencies initialize before their importers, and direct imports are visited
in source order even when the importer has no `init()` function of its own.

Cleanup hooks follow the reverse dependency order, including through modules without
initialization or cleanup hooks. Cleanup-only modules remain included when no startup or
import metadata references them.
