## Problem

V's Apps CI explicitly checks out this repository's `master` branch. Its formatter step fails on the field alignment in `lsp/initialize.v` and `tree_sitter_v/bindings/bindings.v`.

## Change

Apply the formatter's alignment to those two files only. The diff changes three lines of whitespace and has no token or behavior changes. This PR targets `master`, the branch used by the failing CI job. The separate build migration in #187 targets `main` and retains both formatting errors at its current head.

## Validation

On `master` at `e3a3351608243326a55ddd310a07997cc8bc070c`, the exact [V Apps CI formatter step](https://github.com/vlang/v/actions/runs/41182529622/job/124482684817) reports precisely these two formatting errors before the patch and exits successfully afterward:

```bash
fmt_files=($(git ls-files '*.v' '*.vsh' ':!:metadata/stubs/*.v' ':!:src/metadata/stubs/*.v'))
v fmt -no-migrate-json2 -c "${fmt_files[@]}"
```

Validation retains the workflow's original exclusions and exit-code handling. `git diff -w --exit-code` and `git diff --check` both pass. No application build or new tests are needed for this whitespace-only change.
