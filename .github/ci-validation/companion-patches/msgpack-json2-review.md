## Problem

Current V removed the legacy `json` module. The issue #12 regression still
imports it and calls its old decode API, so `v test` stops before the
JSON-to-MessagePack round trip can run.

## Change

Use `json2` and `json2.decode[UsedIdsMap](data)!` in the existing regression.
The input, struct, MessagePack encode/decode calls, and all assertions stay the
same. This is a two-line test-only change against current `master` at
`dd4f46dc04a61b112e7a247602db036081aabc59`.

## Validation and dependencies

- The affected master test type-checks with current V after this change.
- The exact two hunks also apply to existing
  [PR #16](https://github.com/vlang/msgpack/pull/16) at
  `f8c9f971a4cc489f401cff062367e900f43f2f15`.
- With PR #16 plus these two hunks, and V's smartcast-copy correction
  [vlang/v#31579](https://github.com/vlang/v/pull/31579), all three original V CI
  commands pass: shared-library build, all 13 test files, and every example in
  `should-compile-all`.
- Formatting verification and `git diff --check` pass.

PR #16 still supplies the decoder and encoder fixes, including the benchmark
example's JSON2 migration. This patch does not duplicate its implementation or
claim that the unmerged master decoder passes without it. The validation overlay
retains PR #16's API adaptations in this same test file and changes only its JSON
preprocessing call.
