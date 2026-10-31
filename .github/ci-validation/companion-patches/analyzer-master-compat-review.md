## Problem

V's Apps CI clones v-analyzer's `master` branch. That branch still fails to build with the current V compiler: it uses the removed `json` module and older Option, smartcast, process, and native-pointer APIs. The existing migration in [#187](https://github.com/vlang/v-analyzer/pull/187) targets `main`, so it does not repair the branch that CI actually checks.

## Change

Backport the compatibility changes needed by `master` from @five07's `port-latest-v` work in #187. This is based on `e3a3351608243326a55ddd310a07997cc8bc070c`; it does not repeat the source-tree move or unrelated index, byte-format, installer, and testdata changes.

The patch updates JSON decoding, preserves JSON-RPC null-result signaling through an explicit error marker, adjusts PSI interface conversions and smartcast/Option handling, initializes required tree-sitter node fields, and uses current process and native-pointer APIs. The two process call changes are required by the original release build with `VFLAGS=-w`.

A focused JSON-RPC regression checks that an empty handler result still writes `null` for a request, writes no response for a notification, and propagates the existing null-result signal in both cases.

The two whitespace-only formatter corrections are a separate patch. The complete original formatter command also passes when that patch is composed with this one.

## Validation

All builds use the current compiler with `-new-compiler -no-retry-compilation`; there is no legacy compiler fallback.

- Reproduced the original debug build failure on unchanged `master`.
- `v -check -no-parallel .`: passed.
- Original `v build.vsh debug`: passed with native GCC.
- Original `VFLAGS=-w v build.vsh release`: passed with GCC's static, optimized release build.
- `v -cc gcc test jsonrpc`: both test files passed, including the new request/notification regression.
- `v -cc gcc test tree_sitter_v/bindings/simple_test.v lsp/log/log_test.v`: both passed.
- Exact CI formatter command and exclusions: passed with the separate formatter patch applied; those two files were restored afterward.
- `git diff --check`: passed.

The tree-sitter submodule remains pinned at `cd4b6e2ef996d4baca12caadb78dffc8b55bc869` as in the original recursive checkout. This validation covers the affected builds and focused tests, not the full language-server integration suite.
