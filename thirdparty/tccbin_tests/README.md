# tccbin conformance tests

Shared compatibility tests for the prebuilt `tcc`/`libgc.a` pairs shipped
on each platform branch of [vlang/tccbin](https://github.com/vlang/tccbin).
Each `thirdparty-<platform>` branch there is an independent orphan branch
with its own binaries and no shared history, so there was previously no
single place testing "does this tcc still work as a C compiler + GC
backend" the same way across all of them - regressions could land on one
platform's rebuild and go unnoticed unless someone happened to test that
specific platform by hand.

This directory is that single place. It doesn't contain any binaries
itself - platform branches pull it in via a sparse checkout (see
"Adopting this in a platform branch's CI" below) and run it against
whatever `tcc`/`libgc.a` they just built.

## Layout

```
shared/            tests every platform is expected to pass identically
platform/<name>/   additional tests specific to one platform
run.ps1             test runner for Windows
run.sh               test runner for POSIX platforms
```

Put a test in `shared/` unless it's actually testing something
platform-specific (e.g. `platform/windows/crash_message.c` checks the
exact wording of tcc's own SEH-based crash diagnostic - a Windows
implementation detail, not something other platforms produce or need
to reproduce). Most tests belong in `shared/`; `platform/` should stay
small.

## Test format

Each test is a pair of files with the same base name:

- `<name>.c` - the test program.
- `<name>.expected` - 1 or 2 lines:
  - line 1: the expected exit code, or the literal word `nonzero`
    (for tests that are supposed to crash/fail on purpose).
  - line 2 (optional): a substring that must appear somewhere in the
    program's combined stdout+stderr. Omit this line if you only care
    about the exit code.

## Running

```
# Windows
.\run.ps1 -Tcc <path to tcc.exe> [-Platform windows] -- <extra tcc args>

# POSIX
./run.sh <path to tcc> [platform] -- <extra tcc args>
```

"Extra args" are whatever the given tcc build needs on that platform to
link a program that uses the GC - e.g. on Windows:
`-DGC_NOT_DLL -DGC_WIN32_THREADS -DGC_THREADS -DGC_BUILTIN_ATOMIC -I <path to thirdparty/libgc/include> -bt25 -municode -ldbghelp -luser32 <path to lib/libgc.a>`.
The runners don't hardcode any platform-specific flags themselves -
each platform branch's CI supplies them, since they genuinely differ
(different GC threading defines, different Windows-only libs, etc).

## Adopting this in a platform branch's CI

The windows-amd64 branch's `.github/workflows/build-and-test.yml`
(in [vlang/tccbin](https://github.com/vlang/tccbin), `thirdparty-windows-amd64`
branch) is the reference implementation. The shape to copy for another
platform:

1. Sparse-checkout this repo (`vlang/v`) for `thirdparty/tccbin_tests`
   and `thirdparty/libgc` (the latter only if the platform also needs
   to rebuild `libgc.a` from source - if the platform branch just
   ships a prebuilt one, skip it).
2. Build that platform's `tcc` (and `libgc.a`, if applicable) as the
   branch already does.
3. Run `run.sh <tcc> <platform-name> -- <that platform's link flags>`
   and fail the CI job if it exits nonzero.
4. If there's a `platform/<platform-name>/` directory here with tests
   specific to that platform, they'll be picked up automatically by
   step 3 - add one (following `platform/windows/`'s example) if a
   platform-specific regression is worth pinning down permanently.

`run.sh` has been verified against a native Linux tcc (built from this
same source via `./configure && make install`, WSL/Ubuntu 24.04, gcc
16) linked against `libgc-dev` 8.2.6 from apt - `shared/hello.c`,
`shared/gc_alloc.c`, and `shared/crash.c` all pass, stress-tested over
10 consecutive runs with no flakiness. It has *not* been run on macOS
or any BSD - the differences there (different GC threading defines,
different linker behavior) are unverified; treat those as untested
until a real platform branch adopts it.
