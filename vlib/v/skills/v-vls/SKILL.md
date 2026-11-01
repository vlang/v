---
name: v-vls
description: Using VLS, the V Language Server, for IDE-grade features on V code - completion, hover, go-to-definition, references, rename, signature help, diagnostics, workspace symbols, code actions, and refactors. Use when an agent needs semantic information beyond what the compiler alone provides, when working in a VLS-equipped editor, or when the user asks about VLS setup, configuration, performance, or troubleshooting. Does not cover the V language rules (see v-lang), the build loop (see v-workflow), or the V MCP server (see v-mcp).
license: MIT
---

# VLS — the V Language Server

VLS is a separate binary (`vls`) that provides LSP features for V code. It runs
the compiler in persistent processes and maintains an incremental symbol index,
so completion, hover, definition, references, rename, and diagnostics are fast
and project-aware.

## When to use VLS vs V MCP

- **V MCP (`v mcp serve`)**: in-process compiler facts — `v_check`, `v_format`,
  `v_symbols`, `v_references`, `v_rename_symbol`. Works without VLS; ideal for
  agents that edit code and need ground truth.
- **VLS**: IDE-grade features — ranking completion candidates, hover docs across
  modules, workspace symbols, refactor code actions, fast/slow diagnostics tiers.
  Use when the user is in an editor, or when you need ranked completion or
  refactor suggestions.

Prefer V MCP for "does it compile / what does this declare"; prefer VLS for
"what can I complete here / rename safely / which symbols match".

## Install and run

```sh
# Build from source (VLS repo)
cd /path/to/vls && v .

# Or update an existing install
v ls --update

# Run as a language server (stdio by default)
vls

# Single-file compile check (agent-friendly)
vls check path/to/file.v

# MCP mode (experimental): speaks MCP over stdio
vls --mcp
```

MCP tools exposed:
- `vls_check`: compile-check a file, returns diagnostics.
- `vls_hover`: hover information at a position.
- `vls_definition`: go to definition at a position.
- `vls_references`: find references at a position.

VLS is usually launched by the editor (VS Code, etc.), not by hand. For agent
use, invoke `vls` over stdio with LSP framing, or use the editor's LSP client.

## Configuration

Project-level config lives in `vls.json` at the project root. Editor settings
override it; `VLS_DEFINES` (env) provides defines. Defines are salted into the
diagnostics fingerprint.

Key settings:
- `vls.inlayHints.variableTypes` / `.parameterNames` (default on)
- diagnostics enabled (fast 150 ms / slow 800 ms debounce tiers)
- compiler path via `VLS_V_COMMAND` or `VFLAGS`

## Performance and diagnostics

- Set `VLS_PERF_LOG=1` to get per-request and per-diagnostics timing.
- Fast tier answers from the index (unknown-import checks) within ~150 ms.
- Slow tier runs the full compiler check after ~800 ms idle.
- A persistent diagnostics fingerprint cache serves unchanged programs in ~15 ms.
- Index limits: 20 000 files, 2 MiB/file, 48 MiB aggregate (Windows) / 256 MiB
  (other platforms).

## Troubleshooting

- `v ls --update` compiles VLS from source; needs a working V compiler.
- Missing compat compiler: VLS probes `unknown/direct/compat/missing` modes.
- `make` is required for the V1 compatibility compiler on some hosts.
- For large projects, watch `VLS_PERF_LOG` output for compiler-dominated time.

## Related skills

- **The language rules**: see [v-lang](../v-lang/SKILL.md).
- **The build loop**: see [v-workflow](../v-workflow/SKILL.md).
- **The V MCP server**: see [v-mcp](../v-mcp/SKILL.md) for compiler-level tools.
- **Tests**: see [v-testing](../v-testing/SKILL.md).