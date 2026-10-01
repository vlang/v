---
name: v-mcp
description: Use the V MCP server (`v mcp serve`) to read and change V code through the compiler itself - AST, symbols, references, diagnostics, rename and formatting. Read this before editing a V file through an agent.
---

# Working with V through the MCP server

`v mcp serve` speaks MCP over stdio (or `--http` for a socket). It is not a
separate index: it calls the V parser, checker and formatter in process, so it
answers correctly about code that does not compile yet. That is its whole reason
to exist, and it is why you should prefer it over reading files as text.

Start the server with:

```bash
v mcp serve              # stdio, for a client that spawns it
v mcp serve --root .     # point it at a project other than the current directory
v mcp serve --read-only  # register only the tools that cannot write
```

## A working order

1. `v_project_info` once per session. It names the module, the compiler binary,
   the workspace root every path is relative to, and whether this is the V
   checkout itself. If it says `is_v_checkout`, a change to `vlib/v/` or
   `cmd/v/` affects the compiler and needs a `./v self` rebuild.
2. `v_files` when you do not know the layout. It lists sources with line counts,
   so you can pick a file before reading it.
3. `v_symbols` to learn what a file declares. It carries doc comments, so it
   usually answers "what is this file for" without reading the body.
4. `v_ast` when you need the shape of an expression, in the same JSON the
   `v ast -p` command prints.
5. `v_check` after every change. It is the only authoritative answer to "does it
   compile", and it returns records with file, line and column.
6. `v_test_run` for behaviour. `v_check` cannot catch a wrong result.

## Finding what you are looking at

| Question | Tool |
| --- | --- |
| What does this file declare? | `v_symbols` with `path` |
| What is under the cursor? | `v_symbol_at` with `line`, `column` |
| Where is this used? | `v_references` with `name` |
| What does this stdlib function take? | `v_stdlib_doc` with `symbol` |
| Which modules can I import? | `v_modules` |
| What are the routes of this app? | `v_veb_routes` |

`v_stdlib_doc` takes a bare module (`strings`) to list its documented symbols, or
a dotted name (`strings.Builder`) for one member. Use it rather than guessing a
signature: an invented signature costs a compile cycle at best.

## Changing code

Three tools write, and all three default to a plan rather than a write.

- `v_rename_symbol` defaults to `dry_run: true`. The response lists every hit it
  would change, with line, column and length. Read the plan, then pass
  `dry_run: false`. It works from the AST, so a comment or a string that happens
  to hold the name is left alone.
- `v_edit_replace` requires `expected_old`: read the range first and pass it back
  verbatim. It refuses to write when the file no longer matches, reporting
  `expected` and `actual` instead. A concurrent edit by someone else surfaces as
  a refusal, not as an overwrite. An empty `expected_old` inserts; omitting
  `new_text` deletes.
- `v_format` defaults to `write: false` and reports `before` and `after`.

In `--read-only` mode none of the three is registered at all.

## Rules that bite

- A module name must match its directory name, or the import fails silently.
- Function arguments are immutable by default; add `mut` to change one.
- `$if`, `$for` and the other `$` forms are compile time. A runtime `if` that
  mentions a platform specific symbol does not compile.
- `?T` is an option that can be `none`; `!T` is a result that can error. They are
  unwrapped with different syntax.
- Every file you touch goes through `v_format`.

## When a tool is not enough

`v_run` starts a compiled program, `v_eval` compiles a snippet, and `v_doctor`
reports the installation. They shell out to the compiler, so they are slower than
the in-process tools and unavailable when the compiler cannot be started. Prefer
the in-process tools for anything you do repeatedly.