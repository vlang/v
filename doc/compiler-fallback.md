# Compiler fallback diagnostics

V automatically retries with the V 0.5.2 compatibility compiler only after a
`c_compilation_error` failure. V errors (`compiler_error`), including parser and
checker errors, and unsupported inline assembly (`inline_asm`) do not trigger a
compatibility retry. They retain the original compiler's failure exit status,
without locating, installing, or launching the compatibility compiler.

If a V executable embeds an incomplete `vlib/v/gen/c/manual_stdlib_c_headers.h`, builds that
need its manual libc preamble report the damaged header before generating C and exit 1.
The diagnostic appears once and does not trigger a compatibility retry. Restore the header
with `git restore vlib/v/gen/c/manual_stdlib_c_headers.h`, then rebuild V with `make`.
Headerless C output and `-target-libc-headers` builds do not use this embedded header.

Hard checker errors in dependency functions referenced by the selected source files,
including callbacks, stored function values, and transitive calls, are reported at the
dependency's source location before C generation. This also applies to standard-library
and installed modules. Dependency warnings and notices remain limited to project-owned files.

Compiler builds report ordinary checker diagnostics before C generation, including `v -check cmd/v`.
The compiler entry paths select self-build optimizations; naming an ordinary program `v.v` does
not. Explicit `-building-v` builds still validate assignments, field access, and mutability.

For a C compiler failure, V prints the saved output when available, without compiling
again, before retrying. It is labeled `C compiler output from the default V compiler:`.
Internal arguments used to restart the default compiler are removed before launching
the compatibility compiler, so a prior implicit TCC warning cannot cause an unknown-argument error.
Option values and arguments passed to a program remain intact during this filtering.

Literal-output programs retain the array iteration helpers used by Linux backtrace formatting.
For example, `v -show-timings examples/hello_world.v` builds without a missing `array__get` symbol.

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

When a compatibility compiler must be built, V searches PATH for `make`, then `gmake`.
On Windows it also accepts MSYS2's `mingw32-make`. Install GNU make and ensure that
MSYS2's make executable and `sh` are on PATH: the `make v1` target uses POSIX shell recipes.

The `-vls-mode` compatibility protocol suppresses successful on-demand installation progress
so the first response contains only the requested compiler output. Installation failures still
include the installer's output and a failing exit status. Other commands retain installation
progress.

If a build needs a missing bundled Boehm GC archive, V reports the missing library
before invoking the C compiler. Reinstall V to restore the bundled libraries, or
pass `-d use_bundled_libgc` to build GC from source. `-gc none` compiles without GC.
Generating C or an object file does not require this archive. Missing system `-lgc`
libraries retain the linker diagnostic and advice to install the development package.

On Linux amd64, V's TCC fence shim uses a private symbol so it can link alongside
TCC's atomic runtime helpers. Boehm GC builds use the canonical GC header under
TCC, avoiding a duplicate compatibility definition of `GC_noop1_ptr`. These builds
can use TCC directly without a duplicate-symbol warning and a retry with `cc`.

The compatibility compiler cache uses `V1_FALLBACK_CACHE_DIR` when set, followed by
`XDG_CACHE_HOME/v/v1-fallback` and `HOME/.cache/v/v1-fallback`. On Windows, when those
variables are unset, it uses `LOCALAPPDATA\v\v1-fallback`, reusing the same cache
across invocations. Looking up this path does not create directories. If none of
these variables is set, V reserves a private cache under the temporary directory;
on Windows, that last-resort cache has a fresh random name.

Implicit C compiler selection excludes TCC for `-race`, because TCC has no ThreadSanitizer
runtime. Test build facts follow this rule on every host, including Windows, where race
builds themselves are unsupported. Explicit compiler requests are checked separately.

User builds have a default memory safety limit of 10176 MiB. Set `-memory-limit <size>`
(or `--memory-limit <size>`) before the source or subcommand to choose another limit.
Unsuffixed values and `M`/`m` values use MiB; `K`/`k` use KiB and `G`/`g` use GiB.
The value must be a nonnegative integer whose converted KiB value fits in a signed 64-bit
integer. Missing, empty, malformed, negative, and overflowing values produce a CLI error.
An explicit zero disables the limit, as does `-no-memory-limit`.

The Windows TCC backend supports `-cstrict`, including the startup code that sets the compiler's
`VEXE` environment variable. Generated CRT calls retain explicit declarations and strict checks
for implicit function declarations.
