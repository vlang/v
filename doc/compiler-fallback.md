# Compiler fallback diagnostics

When the default compiler fails, V prints its diagnostics to standard error before
retrying with the V 0.5.2 compatibility compiler. This includes `compiler_error`
failures from V itself, `c_compilation_error` failures, and `inline_asm` fallbacks.

For a C compiler failure, V uses the saved output when available, without compiling
again. It is labeled `C compiler output from the default V compiler:`.

For other failures, or when saved C output is missing or empty, V replays compilation
with fallback disabled and prints `Compiler output from the default V compiler:`.
The replay sets `VNORUN=1`, so `run`, tests, and `.vsh` scripts are not executed even
if compilation succeeds on the second attempt. Already-merged `VFLAGS` are not
applied twice, and the replay's environment changes do not affect the actual retry.
If the replay produces no output, V reports that explicitly instead of silently
omitting the diagnostic.

V diagnostics use the compatibility compiler's colors: errors are red, notices are
yellow, and warnings and conflicting declarations are magenta. Source locations,
severity labels, and underlines are bold; the highlighted source span uses the
severity's color. Diagnostic replay preserves the parent terminal's color support
even though it captures the compiler output through a pipe. Use `-color` or `-nocolor`
to override detection, or `VCOLORS=always` / `VCOLORS=never` to set its default.
Redirected diagnostics remain plain unless colors are explicitly enabled.

Diagnostic output does not depend on automatic bug reporting being enabled. It is
shown before locating or launching the fallback, even when that fallback succeeds.
A successful fallback still returns success, and unsuccessful retries retain their
exit status. Diagnostics already shown are not replayed again if the fallback is
unavailable.

Use `v -new-compiler ...` to disable the compatibility fallback. An explicit
`v -old-compiler ...` request does not have a failed default compilation to display.
