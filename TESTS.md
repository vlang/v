# Automated tests

TLDR: do run `v test-all` locally, after making your changes,
and before submitting PRs.

Tip: use `v -cc tcc` when compiling tests, because TCC is much faster,
compared to most other C compilers like clang/gcc/msvc. Most test commands
will use the V compiler and the V tools many times, potentially
hundreds/thousands of times.

## `v test-all`

Test and build *everything*. Useful to verify *locally*, that the CI will
most likely pass. Slowest, but most comprehensive.

It works, by running these in succession:

* `v test-cleancode`
* `v test-self`
* `v test-fmt`
* `v build-tools`
* `v build-examples`
* `v check-md -hide-warnings .`
* `v install nedpals.args`

The self-build checks include serial transformation and C generation with `-no-parallel`.
Transformation always runs in the current compiler; the historical `-new-transformer` switch
is no longer needed.

# Details:

In the `v` repo there are many tests. The main types are:

## `_test.v` tests - these are the normal V test files.

All `test_` functions in these files, will be ran automatically by
V's test framework.

NB 1: You can run test files one by one, with:
`v file_test.v` - this will run the test_ functions in file_test.v,
and will exit with a 0 exit code, if they all had 0 failing assertions.

`v -stats file_test.v` - this will run the test_ functions, and show a
report about how much time it took to run each of them too.

Test executables and their macOS debug symbols are removed after the test process exits,
including failed assertions, panics, and nonzero exits. This also applies with `-stats`, `-keepc`,
and `v run file_test.v`; `-keepc` still retains generated C. Explicit `-o` outputs are also removed
after execution. To keep an executable for later execution or debugging, compile without running
it using `v -skip-running -o test_binary file_test.v`.
`-skip-running` takes precedence over `-stats` and `VTEST_SHOW_ASSERTS`.

NB 2: You can also run many test files at once (in parallel, depending on
how many cores you have), with:
`v test folder` - this will run *all* `_test.v` files in `folder`,
recursively.

`v -stats test folder` - same, but will also produce timing reports
about how fast each test_ function in each _test.v file ran.

Test binaries are removed after execution even with `VTEST_KEEP_SESSION=1`. That setting keeps
the session folder and other artifacts for inspection.

The `v test` runner is built for the host using the C backend. Backend and target options such
as `-b arm64` apply to the test programs it compiles. Architecture-suffixed test discovery
uses the requested `-arch`, including when it differs from the runner's host architecture.

By default, `v test` uses one parallel worker per CPU and budgets one worker per 2 GiB of
memory, whichever is lower. On Linux it uses the lower of physical memory and the active
cgroup memory limit, and the lower of the CPU count and the cgroup CPU quota.
Set `VJOBS` to a positive value to explicitly choose a different worker count when your test
workload and machine capacity are known.

The test builds of a session share one V3 module cache, `vtest_module_cache` in `VTMP`. It is
kept apart from the default cache of your own builds, and it persists across sessions, so a
module is compiled again only when its sources or its dependencies change. One build at a time
populates it; the builds that start beside it wait and then read what it published. Set
`V3CACHE` to use another directory, or `V3_TEST_ISOLATE_CACHE=1` for a cache per compiler.
Tests of the cache itself stay out of the shared cache: the files in `vlib/v/modulecache/`,
`*cache*_test.v`, `unchanged_build_*` and `program_executable_*` files, and tests that pass
their own `-nocache` or `V3CACHE`. They are compiled with `-nocache` and run with a `V3CACHE`
of their own.

Set `VTEST_NOCACHE=1` to compile every test file from scratch, with `-nocache`. A cached test
run of the compiler's own modules reads most of them from the cache, so it does not cover their
complete uncached compilation; `./v self` and a test run with `VTEST_NOCACHE=1` do.

When at least two of the selected test files build the compiler from `vlib/v/v.v` themselves,
`v test` builds it once at the start, and the `v` launcher serves their plain `-gc none`
compiler builds with copies of it, as `vlib/v/test_all.vsh` does. It does the same for the
`-gc none -d ownership` compiler of the ownership tests. Builds with other options still build
their own. Set `VTEST_NO_SHARED_V3=1` to let every test build its own compiler.
Each session also shares the cached C objects of thirdparty code (`v3_thirdparty_objs`) with
the module cache root, unless `V3_TEST_ISOLATE_CACHE=1`.

A module's internal tests (`_test.v` files that declare the module of their directory) spend most
of their time compiling that module, and small `main` tests the fixed cost of a whole program.
`v test` builds up to 16 test files of one directory and module into one binary and runs it once:
the compiler accepts several distinct `_test.v` and `_test.c.v` files of one directory as its
inputs, `v -skip-running -o bin a_test.v b_test.v`, and adds the module's other sources once.
Files whose top-level names collide go to different batches; text in string literals does not
count, and test functions may share a name (the compiler renames them in a batch).

Files that need something of their own share batches only with files that need the same: the
same `// vtest vflags:`, `-enable-globals` (`global` in the path), a `testsuite_begin` that only
unsets environment variables, `// vtest retry:`, or changes to environment variables or to the
working directory that the file saves and restores (`os.getenv_opt`/`os.getwd()` and a `defer`).
`// vtest build:` constraints that hold and `// vtest ownership` do not keep a file out. Any other
`// vtest` directive, other suite hooks, process state the file changes without restoring it, its
arguments or `exit`, and a `main` test with C flags, headers, exported or global symbols run the
file on its own.

When a batch fails to compile or to run, its files run again one by one, so failures are reported
as without batching. `VTEST_BATCH_SIZE` sets the largest batch; `VTEST_NO_BATCH=1` runs every file
on its own. A batch compiles with a memory limit of 4096 MiB (`VTEST_BATCH_MEMORY_LIMIT`), since
several programs built as one can need far more memory than each alone; over the limit, its files
run one by one. With `VTEST_SHOW_CMD=1` the runner names the files of a failed batch and why it
failed: a batch that fails only together points to a compiler bug or to shared state.
Batching applies only to plain `v test` runs, without `-stats`, `-b` or `VTEST_RESUME_DIR`.
`vlib/v/test_all.vsh` passes neighbouring test files of one directory to one `v test`. With
`VTEST_FAILED_LIST=path`, `v test` writes the files that failed to `path`, one per line; the script
records those, and runs the files of a failing group again one by one only when the list is empty.

`cmd/tools/test_if_v_test_system_works.v` checks direct and nested test invocations,
including expected failures. It streams child output to stderr while retaining that output
for its assertions, so a watchdog failure leaves the nested command's progress in the log.
Set `VTOOLS_CACHE_VERBOSE=1`, `VTEST_SHOW_CMD=1`, and `VTEST_SHOW_START=1` to include
tool-cache decisions and test compilation/execution commands in that progress.

Skipped test paths are resolved before comparison, so selecting a file through a symlink
does not bypass its platform or architecture exclusion.

On Linux, `-race` test builds use `_setjmp` and `_longjmp` to continue after failed assertions,
avoiding ThreadSanitizer's incompatible `longjmp` interceptor on glibc 2.44. Later tests and
race detection still run. A later race report can retain frames from the failed test because
the sanitizer does not observe that jump.

Tests ending in `_windows_test.v` or `_windows_test.c.v` run only on Windows.
The compound `_android_outside_termux_test.v` and `_android_outside_termux_test.c.v`
suffixes select Android outside Termux; `_termux_test.v` and `_termux_test.c.v`
select Termux.

## `v test vlib/v/tests`:

This folder contains _test.v files, testing the different features of the V
compiler. Each of them will be compiled, and all the features in them have
to work (verified by assertions).

## `v vlib/v/slow_tests/inout/compiler_test.v`

This is a *test runner*, that checks whether the output of running a V program,
matches an expected .out file. You can also check for code that does panic
using this test runner - just paste the start of the `panic()` output in the
corresponding .out file.

> [!NOTE]
> These tests, expect to find a pair of `.vv` and `.out` files, in the folder:
> vlib/v/slow_tests/inout

The test runner will run each `.vv` file, and will check that its output, matches
the contents of the `.out` file with the same base name. This is particularly useful
for checking that errors and panics are printed.

## `v test vlib/v/gen/c/`

The C backend has focused unit and integration tests beside its implementation.
Many tests compile a small V source to C and assert on the generated declarations,
expressions, ABI, linker inputs, or runtime behavior.

## `v test cmd/v/`

The compiler process regressions workflow runs every launcher test in `cmd/v/` on Linux,
macOS, and Windows with the default compiler. This covers argument routing, diagnostics,
executable discovery, and tool-cache behavior, including platform-specific code.

## Line coverage

Collect coverage with `v -coverage coverage_dir path/to/file_test.v`, then inspect it with
`v cover coverage_dir`. Add `-no-skip-unused` when compiling to include uncalled functions
in the report as well as executed code.

On Windows, test statistics and coverage use a 64-bit monotonic clock resolved at runtime,
so they also work with bundled TCC versions whose import libraries omit `GetTickCount64`.

## REPL tests

The test runner for these is `vlib/v/slow_tests/repl/repl_test.v`.

The test cases for the V REPL, are stored in .repl files, in the folder
`vlib/v/slow_tests/repl/`. Each .repl file in this folder, contains several
lines of input to the V repl, followed by a single line of `===output===`,
and then the output lines, that the repl would normally show for the input
lines.

If you change the compiler or the REPL source, and you have breaks in those
.repl files, you can replace the current output in them, by running several
times `VAUTOFIX=1 ./vlib/v/slow_tests/repl/repl_test.v`, until all the
files are fixed and the test pass.

## `v vlib/v/slow_tests/run_project_folders_test.v`

This *test runner*, checks whether whole project folders, can be compiled, and run.

> [!NOTE]
> Each project in these folders, should finish with an exit code of 0,
> and it should output `OK` as its last stdout line.

## `v vlib/v/tests/known_errors/known_errors_test.v`

This *test runner*, checks whether a known program, that was expected to compile,
but did NOT, due to a buggy checker, parser or cgen, continues to fail.
The negative programs are collected in the `vlib/v/tests/known_errors/testdata/` folder.
Each of them should FAIL to compile, due to a known/confirmed compiler bug/limitation.

The intended use of this, is for providing samples, that currently do NOT compile,
but that a future compiler improvement WILL be able to compile, and to
track, whether they were not fixed incidentally, due to an unrelated
change/improvement. For example, code that triggers generating invalid C code can go here,
and later when a bug is fixed, can be moved to a proper _test.v or .vv/.out pair, outside of
the `vlib/v/tests/known_errors/testdata/` folder.

## Test building of actual V programs (examples, tools, V itself)

* `v build-tools`
* `v build-examples`
* `v build-vbinaries`

`v build-examples` discovers files with a `main`, `no_main`, or implicit main module,
and builds configured projects as folders. Leading comments, attributes, and directives do not
affect module discovery. Library modules are checked when the programs that import them compile.
The macOS tray example is built as a project on macOS and excluded on other hosts.

When `VFLAGS` selects TinyCC, `v build-vbinaries` uses an installed GCC or Clang for its
production variants. Debug variants retain TinyCC. Other compiler selections are preserved.
The five builds run side by side, up to `VJOBS`, while their estimated memory (8 GB for a
production build with debug information, 4 GB without, 2 GB for a debug build) stays within three
quarters of the physical memory; on a 16 GB machine two production builds overlap.

## Formatting tests

`cmd/tools/vfmt_test.v` checks the formatter command, while
`vlib/v/gen/v/gen_test.v` and the other tests in `vlib/v/gen/v/` check
flat-AST-to-V formatting and round trips.

* `v test-cleancode`

Check that most .v files, are invariant of `v fmt` runs.

It runs `v vet` and `v fmt -verify` over up to 64 files per command, since both check every file
they are given and name the ones they reject. A command that fails runs again file by file, so
the report is the same as without batching; `VTEST_NO_BATCH=1` runs one command per file.

* `v test-fmt`

This tests that `v fmt` can format all .v files in the current folder, and lists the ones that
need formatting. It runs `v fmt -inprocess -l` over up to 64 files per command, like
`v test-cleancode`.

## Markdown/documentation checks:

* `v check-md -hide-warnings .`

Ensure that all .md files in the project are formatted properly,
and that the V code block examples in them can be compiled/formatted too.

Note: if that command finds formatting errors, they can be fixed with:
`VAUTOFIX=1 ./v check-md file.md` or with `v check-md -fix file.md`.

## `v test-self`

Run `vlib` module tests, *including* the compiler tests.
Test discovery includes architecture-suffixed files such as `_test.amd64.v` when
the suffix matches the host architecture; files for other architectures are excluded.

To run the same suite across separate machines, set `VTEST_SELF_SHARD_COUNT` to the number of
machines and set `VTEST_SELF_SHARD_INDEX` to a different zero-based index on each one. For example,
`VTEST_SELF_SHARD_COUNT=5 VTEST_SELF_SHARD_INDEX=0 ./v test-self vlib` runs the first shard.
Every test file belongs to exactly one shard. Leave both variables unset for the full suite.

## `v vlib/v/compiler_errors_test.v`

This runs tests for:

* `vlib/v/scanner/tests/*.vv`
* `vlib/v/checker/tests/*.vv`
* `vlib/v/parser/tests/*.vv`

The suite runs each fixture with the current compiler.

> [!NOTE]
> There are special folders, that compiler_errors_test.v will try to
> run/compile with specific options:

vlib/v/checker/tests/globals_run/ - `-enable-globals run`;
results stored in `.run.out` files, matching the .vv ones.

NB 2: in case you need to modify many .out files, run *twice* in a row:
`VAUTOFIX=1 ./v vlib/v/compiler_errors_test.v`
This will fail the first time, but it will record the new output for each
.vv file, and store it into the corresponding .out file. The second run
should be now successful, and so you can inspect the difference, and
commit the new .out files with minimum manual effort.

NB 3: To run only some of the tests, use:
`VTEST_ONLY=mismatch ./v vlib/v/compiler_errors_test.v`
This will check only the .vv files, whose paths match the given filter.

NB 4: To run tests, but without printing status lines for all the successful
ones, use:
`VTEST_HIDE_OK=1 ./v test vlib/math/`
This will print only the total stats, and the failing tests, but otherwise
it will be silent. It is useful, when you have hundreds or thousands of
individual `_test.v` files, and you want to avoid scrolling.

NB 5: To show only *the currently running test*, use:
`./v -progress test vlib/math/`
In this mode, the output lines will be limited, no matter how many `_test.v`
files there are. The output will contain the total stats and the output of
the failing tests too.

NB 6: Set `VTEST_SKIP_OWNERSHIP=1` to omit ownership and autofree tests from
`v test`, `v test-self`, and `vlib/v/test_all.vsh`. GitHub Actions enables this
behavior automatically while ownership/autofree coverage is disabled there.
A `_test.v` file is an ownership/autofree test when it is in an `ownership`
folder, its name contains `ownership` or `autofree`, its `// vtest vflags:` line
has `-ownership`, `-autofree` or `-d ownership`, or it has this line:
```v ignore
// vtest ownership
```
Add that line to a test that builds the ownership compiler, or compiles with one
of those flags (for example with `os.exec([vexe, '-ownership', ...])`). Flags
elsewhere in a test's source are not enough, since many tests pass them only to
check how they are parsed or rejected.

## `.github/workflows/ci.yml`

This is a Github Actions configuration file, that runs various CI
tests in the main V repository, for example:

* `v vet vlib/v` - run a style checker.
* `v test-self` (run self tests) in various compilation modes.

> [!NOTE]
The VDOC test vdoc_file_test.v now also supports VAUTOFIX, which is
useful, if you change anything inside cmd/tools/vdoc,
or inside the modules that it depends on (like markdown).
After such changes, just run this command *2 times*, and commit the
resulting changes in `cmd/tools/vdoc/testdata` as well:
`VAUTOFIX=1 ./v cmd/tools/vdoc/vdoc_file_test.v`
