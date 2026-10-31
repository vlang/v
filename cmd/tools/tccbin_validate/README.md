## TCC smoke command validation

The update workflow runs this helper with `SHOWCC_OUTPUT`, `EXPECTED_TCC`,
`EXPECTED_LIBGC`, `EXPECTED_RPATH`, and `EXPECTED_BINARY` in its environment.
It validates one compiler command and exact compiler, dynamic GC, runtime search
path, and output tokens. It parses the command without executing it.
Run its regression checks with `./v -silent test cmd/tools/tccbin_validate/`.
