The `v.modulecache` module stores V3 module interfaces, compiled objects, and program plans.

Use `module_headers_with_const_order` when generating interfaces for several modules from the
same checked AST. It builds the declaration, module, and import indexes once for the batch and
preserves each module's source order, attributes, implicit imports, and runtime constant order.
For a single module, `module_header_with_const_order` provides the same header text.
