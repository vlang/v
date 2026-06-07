## Description

`v2` is a namespace containing a new version of the compiler (still in development).

## Flat AST

`v2.ast` is the default index-based AST graph used by the v2 builder for
parsing, type checking, markused, and native SSA input:

- `ast.flatten_files(files)` builds `ast.FlatAst` with contiguous `nodes` and `edges`.
- `ast.legacy_ast_stats(files)` estimates memory/shape metrics for recursive AST files.
- `flat.stats()` and `flat.count_reachable_nodes()` report flat graph size and reachability.
- `V2_LEGACY_AST=1` keeps the old recursive-AST pipeline available for comparison.
