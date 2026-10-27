# RFC: Compile-Time-Generated Typed Configuration Loading

Status: RFC, docs only. Extends `vlib/config` (`config.load[T]`,
`vlib/config/config.v`, `vlib/config/README.md`) rather than a
new module.

## Existing behavior (kept)

`config.load[T](cli:, env_prefix:, files:)` fills `string`/`int`/
`bool` fields via `$for` (no runtime reflection), precedence
`cli > env > files > @[default]`, `@[env:'NAME']`, `@[required]`,
strict int/bool parsing. Runtime-only; builds never read env.

## Proposed deltas

1. Types: add `u16`/`i64`/`f64`, nested structs, and documented
   collections. Other types stay compile errors via
   `$compile_error`, as today.
2. Defaults: reconcile native `= 8080` with `@[default: '8080']`
   string defaults used by `resolve_raw`.
3. `@[secret]`: redaction helper; error paths must not interpolate
   raw values (today `'not a valid int: ${raw}'` leaks).
4. Diagnostics: `setting/source/expected/reason` triple instead of
   bare messages; missing/invalid values error, never silent
   fallback.
5. Precedence stays `defaults < file < env < cli`; a source
   overrides only when it provides the key.

## Non-goals (kept)

No mandatory framework, no macro system, no build-time env reads,
no network during compilation, no encrypted-secret claims.

## Acceptance

Specialized loader per struct, documented precedence, useful
errors, nested/primitive coverage, secret redaction, no runtime
reflection, build/env independence. Implemented and tested inside
`vlib/config`.
