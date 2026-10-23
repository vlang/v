# Command line tools

V runs bundled tools with `v <tool> [arguments]`. The compiled `cover` and `gret`
executables also accept their arguments directly, including the first option or path:

```sh
v cover --version
v -o cover cmd/tools/vcover
./cover --version
```

`v doctor` refreshes its comparison with `V_REPO/master` using a bounded Git fetch.
Use `v doctor -skip-github` to skip that fetch.

Shell completion suggests commands after `v`, global flags, or `help`, for example
`v -prod ru<Tab>` and `v help ru<Tab>`. It suggests paths after a command such as `v run`.
For example, `v run e<Tab>` completes an `examples/` directory when it exists.

The doc-comment punctuation checker accepts sentences wrapped across comment lines.
The summary must contain a period before any example or code block.

`gret` runs on Linux. Both `v gret --version` and a compiled `gret --version` report
the version. Running either form without arguments prints its usage and exits with status 1.

`v test` forwards `VFLAGS` once to each test compilation in strict compiler mode.
Option/value pairs retain their order, and quoted arguments such as a `-cflags` value
containing spaces remain one argument. A standalone `vtest` executable also accepts
compiler flags from `VFLAGS`; strict mode clears the variable before running test binaries.
