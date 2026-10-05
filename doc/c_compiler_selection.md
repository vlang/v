# C compiler selection

For ordinary C builds without `-cc`, V uses an available TinyCC when the target and build
options allow it. For binary output, V checks native flags and probes link dependencies before
transforming or monomorphizing the program. Known incompatible C++ or Objective-C inputs, external
objects on macOS, and missing or incompatible dependencies select the platform compiler.
Deferred compile-time branches are excluded from this probe until their conditions are known.
C and object output skip the link dependency probe.

If headers or generated program symbols expose a later TinyCC failure, V regenerates the program
for the platform compiler. Regeneration re-evaluates compiler-specific conditions such as
`$if tinyc`. On POSIX systems it replaces the compiler process, releasing the earlier AST and C
buffers before the retry. Windows waits for the retry to preserve its exit status.

An implicit compiler switch prints its reason after semantic validation, including when TinyCC was
skipped. Invalid programs retain their existing error diagnostics. `-silent`
suppresses this warning. An explicit `-cc tcc` bypasses early preflight and retains the existing
retry behavior for recognized TinyCC failures. `-no-retry-compilation` disables these retries.
Passing `-cc clang`, `-cc gcc`, or `-cc cc` selects that compiler directly.
