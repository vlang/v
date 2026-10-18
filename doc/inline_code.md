# Running inline V code

Use `v -e 'println(1)'` or `v -e='println(1)'` to compile and run a short V script.
Imports and top-level statements work just as they do in a `.vsh` file.

Compiler options go before `-e`. Arguments after the code are passed to the program,
including compiler command names and flags such as `version`, `doc`, and `--help`:

```sh
v -prod -e 'import os; println(os.args[1..])' first 'with spaces'
```

The command returns the program's exit status. A temporary source file in the current
working directory and the implicit executable are removed when the compilation or program
finishes, including when interrupted with `Ctrl-C` or `Ctrl-\`.
On Unix, `SIGTERM` and `SIGHUP` are forwarded to the running child. V waits for
the child, removes the temporary files, and exits with `128 + signal`. Signals inherited as
ignored remain ignored, including `SIGHUP` when running under `nohup`.

Use `v -http` to serve the current directory on `localhost:4001`. This is equivalent to:

```sh
v -e 'import net.http.file; file.serve()'
```

For another directory or port, pass `file.serve` options in the inline source:

```sh
v -e 'import net.http.file; file.serve(folder: "/tmp", on: ":5002")'
```
