---
name: v-workflow
description: How to build, type-check, format, vet, test and ship V code, and how the v.mod module system resolves imports. Use before declaring a V change finished, when a build or test command is needed, when a dependency will not resolve, when working in the V compiler's own source tree, or when a v.mod needs reading or writing. Covers flag placement, the difference between -check and a real build, and when ./v self is required. Does not cover the language rules themselves (see v-lang), writing test cases (see v-testing), working through the MCP server (see v-mcp), scripting a task in V (see v-scripts), or the wider command surface (see v-tools).
license: MIT
---

# The V build loop

V's compiler is fast and its checker is the authority. A V change is not finished
until the checker, the formatter and the tests have all said so.

## Resource Routing

- `references/FLAGS.md` - Read when a flag is ignored or has to be moved, or when
  choosing between `-prod`, `-g`, `-cflags` and `-ldflags`.
- `references/V-MOD.md` - Read when a dependency will not resolve, or when
  deciding between `v install`, `v link` and `v unlink`.
- `references/COMPILER-WORKFLOW.md` - Read only when the change is inside the V
  compiler's own tree, where the rules above are not enough.

## Flags go before the subcommand

This is the first thing that goes wrong. Every compiler option comes **before**
the command; anything after it is passed to the command, not the compiler.

```bash
v -prod run main.v      # correct: -prod is a compiler option
v run main.v -prod      # wrong: main.v receives -prod as an argument
```

`VFLAGS` sets options for every invocation, which is how CI pins a C compiler:

```bash
export VFLAGS='-cc gcc'
```

## The loop

Run these in order. Each one is cheaper than the one after it.

```bash
v -check path/to/file.v        # type-check only, no binary produced
v fmt -verify path/to/file.v   # would the formatter change it?
v -stats test path/to/file_test.v
v vet -W path/to/file.v        # suspicious constructs, failures as errors
```

For **library** code rather than a `main` module, `-check` and `vet` need
`-shared`, or they fail with *"project must include a `main` module"*:

```bash
v -check -shared vlib/v/skills/
```

**Validation**: `v fmt -verify` and `v -check` are the two that gate everything
else. Run them on every file you touched before reporting the change done. A
green test run does not imply a formatted file, and a formatted file does not
imply it compiles.

## Quick Reference

| Question | Command |
| --- | --- |
| Does it type-check? | `v -check file.v` |
| Is it formatted? | `v fmt -verify file.v` |
| Format it | `v fmt -w file.v` |
| Does it pass? | `v -stats test dir/` |
| Only some tests | `VTEST_ONLY='pattern' v test file_test.v` |
| Does the formatter agree with CI? | `v -silent test-fmt` |
| Anything suspicious? | `v vet -W file.v` |
| What does this symbol resolve to? | `v where Symbol` |
| Does this still work? | `v doctor` |
| Add a dependency | `v install module.name` |

## Errors worth reading properly

V's diagnostics carry a file, a line and a column, and the column is usually
exact:

```
main.v:12:5: error: unknown function: nope
   12 |     nope()
      |     ~~~~
```

The caret span is the whole expression. Read the message first, then look at the
span — the span is frequently wider than you expect, because it covers the call
rather than the name.

Do not fix a V error by reading the file again. Fix it from the span.

## The V compiler's own tree

The rules change when the change is inside the compiler itself. A compiler change
means a rebuild before anything else, or you test a stale binary:

```bash
./v self                      # rebuild ./v from the current sources
./v -silent test vlib/v/      # the compiler's own tests
./v -silent vlib/v/compiler_errors_test.v
```

`vlib/v/` and `cmd/v/` changes additionally mean the test runner, the formatter
and the language server should be tested too. See `references/COMPILER-WORKFLOW.md`
for the full list and what each one catches.

## Related Skills

- **The language rules**: see [v-lang](../v-lang/SKILL.md) when the compiler
  rejects an option, a result, a match or an assignment.
- **Writing tests**: see [v-testing](../v-testing/SKILL.md) for assertion forms,
  fixtures and running a subset.
- **Reading the project**: see [v-mcp](../v-mcp/SKILL.md) to get declarations,
  references and diagnostics without reading files, and to edit through an
  AST-aware rename.
- **Dependencies and modules**: `references/V-MOD.md` here covers the module
  system; [vpm](https://vlang.io/vpm.html) documents what is on the registry.