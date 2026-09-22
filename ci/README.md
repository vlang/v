# Local CI runners

## Resuming the Linux CI jobs

From the repository root of a built checkout on Linux, run:

```sh
./v ci/linux_ci.vsh ci
```

This runs the script tasks from `.github/workflows/linux_ci.yml` in workflow order:
TCC, GCC, then Clang. It does not bootstrap V or run the workflow's separate shell
canaries. The tasks include dependency installation with `sudo apt`; use the Ubuntu
24.04 environment expected by the workflow, with all three C compilers available.
The command explicitly selects each job's compiler (`-cc tcc -no-retry-compilation`,
`-cc gcc`, or `-cc clang`), sets the corresponding `GITHUB_JOB`, and disables the
compatibility compiler fallback. Existing task bodies and test exclusions are unchanged.

Like the macOS runner, it saves the current task **before** executing it and sets
`VTEST_FAIL_FAST=1` and `VJOBS=1`. Fix a failure and repeat the command to resume that
same task. Successful test files are retained through the shared per-test resume
mechanism described below; failed, interrupted and not-yet-run files are retried.
Non-test commands restart at the beginning of their task.

Linux progress is stored in `/tmp/v-linux-ci-<uid>-<checkout-hash>.progress`, with
per-test records in the sibling `.progress.d` directory. It is separate from macOS
progress and from other users and checkouts. Edits and compiler rebuilds preserve
progress. Missing or invalid cursors, changed task plans or compiler flags, and
`--reset` discard per-test state as well. A complete run clears both levels of progress.
Read/write errors stop execution instead of silently losing progress.

After fixing compiler/library bugs, perform a fresh validation of earlier work with:

```sh
./v ci/linux_ci.vsh ci --reset
```

A resumed pass alone does not revalidate earlier successes against shared source
changes. Reset after switching branches or removing build artifacts, too. Do not run
aggregate CI jobs concurrently in one checkout. Individual tasks and `all` retain
their existing behavior and do not consume the aggregate runner's checkpoint.

To test Linux task resume, compiler/job switching and workflow-plan parity without
running the CI workloads or installing dependencies:

```sh
sh ci/linux_ci_resume_test.sh
```

The macOS integration test and shared resume unit test listed below also run on Linux
and cover the unchanged per-file resume implementation. Set `VEXE` to an absolute
compiler path to select the compiler used to build the integration-test runner.

## Resuming the macOS CI job

From the repository root, run:

```sh
./v run ci/macos_ci.vsh ci
```

The `ci` command saves the current **CI task** before executing it. After a failure,
fix the code and run the same command again: previously completed tasks are skipped,
and the failed task is retried before proceeding with the remaining tasks.
An interrupted run also retains the task it was running. A fully completed job removes
its checkpoints, so the following invocation starts from the beginning.

The aggregate `ci` command sets `VTEST_FAIL_FAST=1` and `VJOBS=1`, overriding inherited
values. Test and example-build sessions stop at the first reported failure, with no
other files already running in parallel. `test-cleancode` also stops after a failed
vetting session instead of starting formatting checks. The failed task's checkpoint
is retained. Existing per-test retry and known-flaky-test policies are unchanged.

Progress is stored in `/tmp/v-macos-ci-<uid>-<checkout-hash>.progress`; the command
prints the exact path. The hash uses the canonical checkout directory, not a commit
or source timestamp, so edits, commits and compiler rebuilds do not discard progress.
Other users and checkouts have separate checkpoints. Missing, invalid or outdated
checkpoint contents restart the sequence; a changed ordered task list also restarts it.
Read/write errors stop the command rather than continuing without reliable progress.
Writes use a private temporary directory and a same-filesystem rename.

To discard saved progress and run every task:

```sh
./v run ci/macos_ci.vsh ci --reset
```

Test sessions also record each successful `_test.v`, `_test.c.v` and `_test.js.v` file.
For example, when `self_tests` fails, the next invocation skips unchanged files that
already passed and retries failed, interrupted and not-yet-run files. Compilation
alone is not a pass. Skipped tests and ignored flaky failures are not recorded as
successful. Recording is independent of the reporter and works with hidden OK output
and `-stats`. Existing test discovery, exclusions and retry policies still apply.

These records live in a sibling `<progress-path>.d` directory, separated by task,
command, compiler options and test path. Editing a test file invalidates its saved
success. Nested test runners do not reuse their parent's records. Reset, an invalid
or missing task cursor, a changed task plan and successful completion clear this
additional state too. Normal individual-task and `all` invocations remain exhaustive.

`run_sanitizers` currently has two compiler commands, rather than a `_test.v` session.
It checkpoints the successful instrumented compiler build. A retry runs the failing
self-compilation without rebuilding `v2`, unless that binary is missing or changed.
Other non-test commands retain their existing task-level retry behavior.

A resumed pass is not a fresh validation of earlier tasks or tests against shared
source changes. Compiler and library edits deliberately preserve progress. Use
`ci --reset` for a complete check, after switching branches, or after removing build
artifacts needed by later tasks. Do not run multiple `ci` jobs in the same checkout
concurrently; they share the checkpoint and build outputs.

### Testing the resume behavior

On macOS or Linux with a working V compiler, run:

```sh
sh ci/macos_ci_resume_test.sh
./v test cmd/tools/testing/resume_test.v
```

Set `VEXE` to an absolute compiler path to use a different V executable. The test
compiles the real runner, then replaces its task subprocesses through `V_CI_VEXE`
with a small shell fixture. It tests failures, retries, successful cleanup, reset,
invalid checkpoints, checkout isolation, checkpoint write failures and fail-fast
environment propagation. It also compiles the real `test-cleancode` tool and runs it
against a tiny checkout with mocked vet/fmt commands, checking both fail-fast and
normal session sequencing. The real `v test` runner is exercised with mocked test
compilation/execution to check per-file retries after compile and runtime failures,
source edits, stats mode, ignored flaky failures and nested tests. Sanitizer fixtures
check reuse and invalidation of the built compiler. Unit tests cover success-record
validation, compiler-option isolation, file suffixes, nested-run ownership and I/O
errors. No macOS workloads or package installations are run.
