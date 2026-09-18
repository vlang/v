# Compiler fallback diagnostics

When the default compiler fails with `c_compilation_error`, V prints the saved C compiler
output to standard error before retrying with the V 0.5.2 compatibility compiler.
The diagnostic is labeled `C compiler output from the default V compiler:` so it can be
distinguished from output produced by the compatibility retry.

This displays the original failure without compiling the program again. It does not depend
on automatic bug reporting being enabled, and a successful fallback still returns success.
Missing or empty saved output does not prevent the compatibility retry.

Use `v -new-compiler ...` to disable the compatibility fallback. An explicit
`v -old-compiler ...` request does not have a failed default compilation to display.
