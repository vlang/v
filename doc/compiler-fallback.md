# Compiler fallback diagnostics

V automatically retries with the V 0.5.2 compatibility compiler only after a
`c_compilation_error` failure. V errors (`compiler_error`), including parser and
checker errors, and unsupported inline assembly (`inline_asm`) do not trigger a
compatibility retry. They retain the original compiler's failure exit status,
without locating, installing, or launching the compatibility compiler.

For a C compiler failure, V prints the saved output when available, without compiling
again, before retrying. It is labeled `C compiler output from the default V compiler:`.

If V diagnostics were deferred while a failure marker was armed, V replays only the
default compiler with fallback disabled to display them. This diagnostic replay is
not a compatibility retry and cannot turn the original failure into success.
When saved C output is missing or empty, the same replay recovers the C diagnostic
before the compatibility retry. Replayed output is labeled
`Compiler output from the default V compiler:`.
The replay uses `-skip-running` and `VNORUN=1`, so user programs and `.vsh` scripts
are not executed even if compilation succeeds on the second attempt. Already-merged
`VFLAGS` are not applied twice, and the replay's environment changes do not affect
the parent or an actual C-error retry. If the replay produces no output, V reports
that explicitly instead of silently omitting the diagnostic.

V diagnostics use the compatibility compiler's colors: errors are red, notices are
yellow, and warnings and conflicting declarations are magenta. Source locations,
severity labels, and underlines are bold; the highlighted source span uses the
severity's color. Diagnostic replay preserves the parent terminal's color support
even though it captures the compiler output through a pipe. Use `-color` or `-nocolor`
to override detection, or `VCOLORS=always` / `VCOLORS=never` to set its default.
Redirected diagnostics remain plain unless colors are explicitly enabled.

Diagnostic output does not depend on automatic bug reporting being enabled. For C
errors, it is shown before locating or launching the fallback, even when that
fallback succeeds. A successful C-error fallback still returns success, and
unsuccessful retries retain their exit status. Diagnostics already shown are not
replayed again if the fallback is unavailable.

Use `v -new-compiler ...` to disable the C-error compatibility fallback as well.
An explicit `v -old-compiler ...` request still selects V 0.5.2 directly and does not
have a failed default compilation to display.
