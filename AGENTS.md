# V Repo Guide

Repository rules for work on the V compiler, standard library, and tools. See
[CONTRIBUTING.md](CONTRIBUTING.md) for architecture and contribution details,
[TESTS.md](TESTS.md) for test infrastructure, and [doc/docs.md](doc/docs.md) for language reference.
The local `./v` workflow and test requirements below take precedence over examples in those docs.

## Scope and safety

- Change only what the request requires. Keep refactors small and within files already being edited.
- Preserve and report unrelated work; do not modify or stash it without explicit instruction.
- Ask before large refactors, changes spanning multiple root directories, or large changes to parser
  rules, resolution, codegen, diagnostics, or CLI behavior. Docs-only edits across directories need
  no approval. Ask if the scope is unclear.
- Ask before editing `ci/`, `Dockerfile*`, or `thirdparty/` unless explicitly requested. For CI,
  clarify whether local validation or CI-only coverage is expected.
- Reading, building, targeted testing, and formatting need no approval. Ask before `./v test-all`;
  broad refactors require that suite.
- Never change tests to hide failures. Update `.out` expectations only for intended behavior changes
  and explain why. Treat `.vv` files as fixtures; avoid formatting them unless changing their
  behavior and expectations.
- Update relevant README, module docs, `doc/`, or `tutorials/` for public API or behavior changes,
  including compiler flags, diagnostics, CLI output, and output ordering. Internal refactors need no
  doc changes. Ask if unsure whether behavior is public.
- Edit `CHANGELOG.md` and `ROADMAP.md` only when explicitly requested.

## Build and format

1. Work from the repo root. Check `git status` and whether `./v` exists before starting.
2. Use `./v` for builds, runs, tests, and tools. Put compiler flags before the subcommand or file:
   `./v -g run file.v`. Flags after a subcommand are passed to that subcommand.
3. If `./v` is missing, run `make` (`makev.bat` on Windows). Requires Git, make, and clang or gcc.
4. Rebuild in place with `./v self` after changes to `vlib/v/`, `cmd/v/`, or core modules
   (`builtin`, `strings`, `os`, `strconv`, `time`), before testing. Rebuild if unsure whether `./v`
   is current. Overwriting it is intentional.
5. Format touched and new `.v`/`.vsh` files with `./v fmt -w file.v`. Check touched Markdown with
   `./v check-md file.md` before committing; keep lines at most 100 characters. Preserve existing
   formatting in other files.

If the compiler becomes unusable, check `git status` before recovery. Only stash for recovery when
the compiler is bricked or the user authorizes it; the recovery sequence is `git stash`, `make`,
`git stash apply`. Keep unrelated work safe.

## Required checks

Start with the smallest relevant tests and run all applicable checks. For compiler changes in
`vlib/v/` or `cmd/v/`, run both:

```sh
./v -silent vlib/v/compiler_errors_test.v
./v -silent test vlib/v/
```

Additional checks by scope:

| Change | Check |
| --- | --- |
| Docs only | `./v check-md file.md`; no rebuild or other tests unless they read those docs |
| Core or other standard library module | Nearest `*_test.v` or `./v -silent test vlib/path/` |
| Parser | `./v -silent test vlib/v/parser/` |
| Checker | `./v -silent test vlib/v/types/` |
| Comptime in `types/` or `transform/` | `./v -silent test vlib/v/tests/` and comptime tests |
| Diagnostics, error reporting, output | `./v -silent vlib/v/slow_tests/inout/compiler_test.v` |
| C code generation | `./v -silent test vlib/v/gen/c/` |
| REPL | `./v -silent vlib/v/slow_tests/repl/repl_test.v` |
| Tools in `cmd/tools/` | Tool tests, or the smallest test exercising the tool |

- For vfmt, run `cmd/tools/vfmt_test.v` and `vlib/v/gen/v/gen_test.v`; for vdoc, run
  `cmd/tools/vdoc/vdoc_test.v` with `./v`.
- For JS/native/WASM changes, use focused tests with `./v -b js|native|wasm ...`; avoid broad
  backend test runs. Consider `./v -cstrict -cc clang -silent test vlib/v/` for C codegen changes.
- Add regression tests for option/result changes in `vlib/v/tests/` and for new public APIs.
- Run `./v -silent test-fmt` only when asked or validating the whole tree. Run `./v vet vlib/v` only
  when asked or changing more than three checker files in `vlib/v/types/`.
- Ask if test selection is unclear. When time-constrained, prioritize compiler error tests and
  focused regressions; report skipped coverage. Docs-only work still requires `check-md`.
- Output tests match exactly, including whitespace (`.vv` + `.out`, or `.run.out`). Use
  `VAUTOFIX=1` only for intended output changes; run the affected test twice when updating outputs.
- `VTEST_ONLY=glob_pattern` filters tests; `VTEST_HIDE_OK=1` hides successful results.

## Code conventions

- Add doc comments immediately before new or modified public functions and methods, starting with
  the function name: `// the_name does ...`. Keep other comments concise; preserve correct comments.
- Keep `unsafe` blocks limited to necessary low-level operations, with a comment explaining why.
  Do not wrap ordinary V logic in `unsafe`.
- Avoid hidden or bidirectional Unicode in source and Markdown.
- Module names match the directory name; imports follow the directory hierarchy (`import abc.def`).
- Prefer platform/backend file suffixes when an entire file is specific to one environment:
  `.c.v`, `.js.v`, `.native.v`, `.wasm.v`, `_windows.c.v`, `_linux.c.v`, `_nix.c.v`, `_default.c.v`.
  Custom flags use `_d_flag.v` or `_notd_flag.v`. Do not combine flag and OS suffixes; use `$if`
  inside the flag-specific file.

## C and JS interop

- The V compiler must never parse, scan, expand, or infer declarations from C headers, including
  bundled headers. Leave preprocessing and include cycles to the C toolchain.
- Track native dependencies through compiler-produced manifests or opaque cache inputs. Bypass
  reuse when the manifest cannot establish safety. Header inspection must not be reintroduced for
  caches, native bridges, backend selection, or portable C output.
- Put `C.` and `JS.` uses in `.c.v` and `.js.v` files. `-Wimpure-v` detects accidental impurity.
  Leave existing interop alone unless needed for the task; prefer moving touched interop to the
  appropriate file. Use correct `const_` prefixes in C function redeclarations.

## V pitfalls

- Use `$if` for platform/compiler conditions; runtime `if` cannot exclude unavailable symbols.
  Custom defines require the question mark: `$if myflag ? { ... }` with `-d myflag`.
- Reflection uses `$for` over type metadata and is unrolled at compile time. Use `$if` for metadata
  type checks. Reflected method calls belong inside a compile-time method iteration.
- `$embed_file` returns `EmbedFileData`; use `.to_string()` or `.to_bytes()`. `$env` reads at
  compile time; `os.getenv` reads at runtime.
- Prefer unprefixed `typeof`, `sizeof`, `isreftype`, `__offsetof`, and `dump` over their aliases.
- `?Type` is optional; `!Type` is a result. Check option handling in guards, struct initialization,
  ternaries, SQL, and fixed arrays when changing related codegen.
- If a tool behaves unexpectedly, check whether it shells out to system `v` instead of `./v`.

## Debugging and performance

- Keep generated C with `./v -keepc -cg run file.v`; use `-g` for V lines, `-showcc` for the C
  command, and `-show-c-output` for C diagnostics. Use `-printfn main__main -o file.c` to isolate
  a function.
- Compiler tracing uses `-d trace_scanner`, `trace_parser`, `trace_checker`, or `trace_gen`; stage
  timing uses `-d time_parsing` or `time_checking`. Remove temporary debug tags before committing.
- `VFLAGS` and `VEXE` can override invocations; `TMPDIR` controls temporary C output.
  `SOURCE_DATE_EPOCH` fixes build timestamps for reproducibility.
- Every compiler fix must preserve or improve efficiency. Read
  [vself_speed.md](vself_speed.md#measurement-instructions-for-agents) before compiler work.
  Follow its fixed-fixture benchmark protocol; investigate instruction-count regressions before
  merging.
- Compare retired instructions and instructions per parsed source line, retaining elapsed time,
  cycles, and peak memory. Use matched production compilers, verify generated C, and preserve logs.
  Require one warmup per role and ten alternating measured pairs with positive hardware counters.
  An idle host is unnecessary for instruction counts; latency claims need controlled measurements.

## Repository map

- Compiler entry: `cmd/v/v.v`; pipeline: `v.driver` orchestrates `scanner` -> `parser`/`flat` ->
  `types` -> `transform` -> `markused` -> `gen/c`, under `vlib/v/`.
- Other backends: `vlib/v/gen/fastc/`, `gen/arm64/`, and `gen/wasm/`, with SSA/MIR lowerings.
- Diagnostics: token spans in `v.token.Pos`, type checking in `v.types.TypeChecker`, formatting in
  `vlib/v/errors/`.
- Compiler feature tests: `vlib/v/tests/`; output and slow tests: `vlib/v/slow_tests/`.
- Standard library: `vlib/`; tools: `cmd/tools/`; examples: `examples/`.

## Commits and reporting

- Keep commits focused on one logical change; follow [CONTRIBUTING.md](CONTRIBUTING.md).
- Ask before force-pushing or dropping hunks. If branch divergence requires a rebase or merge,
  explain the conflict and ask when the strategy is ambiguous; prefer rebase for linear history.
- Confirm touched files with `git status` before reporting. Be concise and include behavior changes
  (or none), tests run and skipped with reasons, touched paths, doc updates, and unrelated changes.
  Explain `.out` updates. If blocked, state what was attempted, why it failed, and what remains.
