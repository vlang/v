---
name: v-mcp
description: Using the V MCP server, `v mcp serve`, to read and change V code through the compiler itself - the AST, declarations, references, diagnostics, stdlib docs, an AST-aware rename, a guarded edit and the formatter. Use when an agent needs to know what a V file declares, whether V code compiles, where a symbol is used, or what a stdlib function takes, and to make a change safely across files. Does not cover the language rules themselves (see v-lang), the build and test loop (see v-workflow), writing test cases (see v-testing), scripting a task in V (see v-scripts), or the command surface outside the MCP server (see v-tools).
license: MIT
---

# The V MCP server

`v mcp serve` speaks MCP over stdio, or over HTTP with `--http`. It is the V
compiler exposed as tools: it calls the parser, the checker and the formatter **in
process**, so it answers correctly about code that does not compile yet.

```json
{
  "mcpServers": {
    "v": { "command": "v", "args": ["mcp", "serve"] }
  }
}
```

## Resource Routing

- `references/TOOLS.md` - Read for the arguments of a specific tool, or when you
  need the tool that answers a particular question.
- `references/EDITING.md` - Read before changing anything through the server. It
  covers the guards and why each one exists.

## Quick Reference

| Question | Tool |
| --- | --- |
| What project is this? | `v_project_info` |
| What modules can I import? | `v_modules` |
| What files are here? | `v_files` |
| What does this file declare? | `v_symbols` |
| What is at the cursor? | `v_symbol_at` |
| Where is this used? | `v_references` |
| What does this stdlib call take? | `v_stdlib_doc` |
| Does it compile? | `v_check` |
| Do the tests pass? | `v_test_run` |
| What are this app's routes? | `v_veb_routes` |
| Rename a symbol | `v_rename_symbol` |
| Change a known range | `v_edit_replace` |
| Format a file | `v_format` |

## A working order

1. **`v_project_info` once per session.** It names the module, the compiler, the
   root every path is relative to, and whether this is the V checkout itself.
2. **`v_symbols`** to learn what a file declares. It carries the doc comments, so
   it usually answers "what is this file for" without reading the body.
3. **`v_check` after every change.** It is the only authoritative answer to "does
   it compile", and it returns records with file, line and column.
4. **`v_test_run`** for behaviour. `v_check` cannot catch a wrong result.

## Prefer asking over reading

The whole point of the server is that it knows more than a file read does. Reach
for it before guessing:

- a signature you are unsure of: `v_stdlib_doc`, not an invented one
- whether an edit is safe: `v_references`, then `v_rename_symbol`
- what a file contains: `v_symbols`, not a full read
- what broke: `v_check`, not a re-read

An invented stdlib signature costs a compile cycle at best. A hand-written rename
misses the comment that mentions the name and the call in a file you never opened.

## Editing is guarded by default

Three tools write. None of them writes unless asked twice:

- `v_rename_symbol` defaults to `dry_run: true` and lists every position it would
  change.
- `v_format` defaults to `write: false` and reports `before` and `after`.
- `v_edit_replace` requires `expected_old`: you read the range and pass it back
  verbatim, so it refuses to write over a change you did not see.

Read the plan, then pass the flag that applies it. See `references/EDITING.md`.

`--read-only` does not register these three at all, so a session started that way
cannot write by accident.

## Before guessing, ask

- **A language rule**: the `v-lang` skill covers the parts agents get wrong, most
  of all `?T` versus `!T` and comptime code.
- **A build or module question**: the `v-workflow` skill, and `v_modules` for what
  is importable right now.
- **The code's shape**: `v_ast`, in the same JSON `v ast -p` prints.

## When the tools cannot answer

`v_run`, `v_test_run`, `v_doctor` and `v_eval` start the compiler. They are
slower than the in-process tools and unavailable when the compiler cannot be
started. A tool that could not start the compiler says so explicitly — it reports
`started: false` with an error rather than an exit code, so a sandbox that cannot
spawn processes is never mistaken for a failed check.

If you see that, the answer is about the environment, not about the code. Do not
start changing the code to satisfy a tool that never ran.

## Validation

The server is the checker. Do not finish a change without it:

```json
{"name": "v_check", "arguments": {"path": "src/main.v"}}
```

Then format what you touched with `v_format`, and run the tests with
`v_test_run`. See [v-workflow](../v-workflow/SKILL.md) for the equivalent shell
commands.

## Related Skills

- **The language rules**: see [v-lang](../v-lang/SKILL.md) for `?T` versus `!T`,
  sum types, `mut` and comptime code.
- **The build loop**: see [v-workflow](../v-workflow/SKILL.md) for `v.mod`,
  dependencies, flags and when `./v self` is required.
- **Tests**: see [v-testing](../v-testing/SKILL.md) for what to assert.
- **Web apps**: see [v-veb](../v-veb/SKILL.md) for veb, and use `v_veb_routes` to
  read the routes of an app you have not opened.