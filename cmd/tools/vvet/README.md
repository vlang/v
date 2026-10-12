# V vet

`v vet file.v` checks formatting and documentation of public functions.
`v vet directory/` applies the same checks to each V source file in the directory,
including subdirectories, and reports their collected diagnostics.

Use `-F` for function-size notices, `-r` for repeated-expression notices, and `-I`
for possible inlining notices. These options follow `vet`:

```sh
v vet -F -r -I directory/
```

Repeated-expression and inlining notices use occurrence thresholds. Configure them
with `VET_CALLEXPR_CUTOFF`, `VET_INFIXEXPR_CUTOFF`, or `VET_FNS_CALL_CUTOFF` when
checking smaller examples.
