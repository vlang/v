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
On Unix, the program runs in a separate process group. Handled signals sent to V's
group are forwarded once. V forwards `SIGTERM`, `SIGHUP`, `SIGINT`, and `SIGQUIT` and waits for the
program before removing temporary files. After `SIGTERM` or `SIGHUP`, V exits with
`128 + signal`. Signals inherited as ignored remain ignored, including `SIGHUP` under `nohup`.

Interactive programs receive the foreground terminal while running. Terminal input and
shell job control (`Ctrl-Z`, `fg`, and `bg`) continue to work. Catchable job-control stop signals
sent to V are forwarded to the program. Uncatchable `SIGKILL` and `SIGSTOP` sent only to V
cannot be forwarded to the separate program group. With piped or redirected stdin, the
original job keeps the foreground terminal so pipeline peers can read it. A program that
opens `/dev/tty` directly in this configuration runs in a background group and may stop
when accessing that terminal.

Use `v -http` to serve the current directory on `localhost:4001`. This is equivalent to:

```sh
v -e 'import net.http.file; file.serve()'
```

For another directory or port, pass `file.serve` options in the inline source:

```sh
v -e 'import net.http.file; file.serve(folder: "/tmp", on: ":5002")'
```
