# Local CI runners

## Resuming the macOS CI job

From the repository root, run:

```sh
./v run ci/macos_ci.vsh ci
```

The `ci` command saves the current **CI task** before executing it. After a failure,
fix the code and run the same command again: previously completed tasks are skipped,
and the failed task is retried before proceeding with the remaining tasks.
An interrupted run also retains the task it was running. A fully completed job removes
its checkpoint, so the following invocation starts from the beginning.

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

Resume operates at the named task boundary: for example, a failure inside `self_tests`
reruns `self_tests`, not just an individual `_test.v` file within it. A task with multiple
commands is likewise retried from its beginning. Individual task invocations and `all`
retain their existing behavior and do not use or clear this checkpoint.

A resumed pass is not a fresh validation of earlier tasks against your edits. Use
`ci --reset` for a complete check, after switching branches, or after removing build
artifacts needed by later tasks. Do not run multiple `ci` jobs in the same checkout
concurrently; they share the checkpoint and build outputs.

### Testing the resume behavior

On macOS or Linux with a working V compiler, run:

```sh
sh ci/macos_ci_resume_test.sh
```

Set `VEXE` to an absolute compiler path to use a different V executable. The test
compiles the real runner, then replaces its task subprocesses through `V_CI_VEXE`
with a small shell fixture. It tests failures, retries, successful cleanup, reset,
invalid checkpoints, checkout isolation and checkpoint write failures without
running the macOS workloads or installing packages.
