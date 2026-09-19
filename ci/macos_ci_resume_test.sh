#!/bin/sh
# Exercise the real runner with cheap subprocesses, not the macOS CI workloads.
set -eu

repo=$(CDPATH= cd "$(dirname "$0")/.." && pwd -P)
vexe=${VEXE:-"$repo/v"}
work=$(mktemp -d "${TMPDIR:-/tmp}/v-macos-ci-resume.XXXXXX")
checkpoint=
other_checkpoint=
trap 'rm -f "$checkpoint" "$other_checkpoint"; rm -rf "$work"' EXIT
trap 'exit 130' INT
trap 'exit 143' TERM

fail() {
    echo "FAIL: $*" >&2
    cat "$work/output" >&2
    exit 1
}

runner="$work/runner"
"$vexe" -o "$runner" "$repo/ci/macos_ci.vsh"
checkout="$work/checkout with spaces"
mkdir "$checkout"
fake_v="$work/fake v"
cat > "$fake_v" <<'MOCK'
#!/bin/sh
set -eu
if [ "$1" = doctor ]; then
    [ "${VTEST_FAIL_FAST:-}" = 0 ] && [ "${VJOBS:-}" = 8 ] || exit 94
    echo doctor >> direct.log
    exit 0
fi
[ "$1" = run ] && [ "$2" = ci/macos_ci.vsh ] || exit 90
[ "$CI" = true ] && [ "$GITHUB_ACTIONS" = true ] || exit 91
[ "$VFLAGS" = '-cc clang' ] && [ "$V_MACOS_V3_NO_FALLBACK" = 1 ] || exit 92
[ "${VTEST_FAIL_FAST:-}" = 1 ] && [ "${VJOBS:-}" = 1 ] || exit 93
printf '%s\n' "$3" >> tasks.log
if [ -f fail-task ] && [ "$3" = "$(cat fail-task)" ]; then
    exit 7
fi
MOCK
chmod +x "$fake_v"

run_ci() {
    expected_status=$1
    shift
    : > "$checkout/tasks.log"
    status=0
    # ci must override inherited non-fail-fast/parallel settings; direct tasks must not.
    (cd "$checkout" && VTEST_FAIL_FAST=0 VJOBS=8 V_CI_VEXE="$fake_v" \
        "$runner" "$@") > "$work/output" 2>&1 || status=$?
    [ "$status" -eq "$expected_status" ] || fail "exit $status, expected $expected_status"
}

# Read the ordered plan, so adding a CI task does not require copying its name here.
sed -n '/^const ci_tasks = \[/,/^\]/p' "$repo/ci/macos_ci.vsh" |
    sed -n "s/^[[:space:]]*'\([^']*\)',$/\1/p" > "$work/tasks"
[ -s "$work/tasks" ] || fail 'could not read the CI task list'
failed_task=verify_v_test_works
failed_line=$(grep -n "^$failed_task$" "$work/tasks" | cut -d: -f1)
printf '%s\n' "$failed_task" > "$checkout/fail-task"
head -n "$failed_line" "$work/tasks" > "$work/prefix"
tail -n "+$failed_line" "$work/tasks" > "$work/suffix"

run_ci 7 ci
checkpoint=$(sed -n 's/^CI progress: //p' "$work/output")
[ -f "$checkpoint" ] || fail 'failure did not leave a checkpoint'
case "$checkpoint" in /tmp/v-macos-ci-*.progress) ;; *) fail 'checkpoint is not in /tmp' ;; esac
[ "$(sed -n '2p' "$checkpoint")" = "$failed_task" ] || fail 'saved the wrong task'
cmp "$work/prefix" "$checkout/tasks.log"
cp "$checkpoint" "$work/saved"

# An unchanged failure retries only that task, retaining the cursor and exit status.
run_ci 7 ci
printf '%s\n' "$failed_task" > "$work/one"
cmp "$work/one" "$checkout/tasks.log"
cmp "$work/saved" "$checkpoint"

# A fix resumes at the failed task; completion clears progress; the next run is full.
rm "$checkout/fail-task"
run_ci 0 ci
cmp "$work/suffix" "$checkout/tasks.log"
[ ! -e "$checkpoint" ] || fail 'successful run did not clear progress'
run_ci 0 ci
cmp "$work/tasks" "$checkout/tasks.log"

# Explicit reset ignores saved progress, while invalid arguments must not run tasks.
cp "$work/saved" "$checkpoint"
printf '%s\n' "$failed_task" > "$checkout/fail-task"
run_ci 7 ci --reset
cmp "$work/prefix" "$checkout/tasks.log"
run_ci 1 ci --typo
[ ! -s "$checkout/tasks.log" ] || fail 'invalid arguments ran tasks'
cmp "$work/saved" "$checkpoint"

# Truncated data, an unknown task, and a changed ordered plan all restart safely.
for state in truncated unknown changed; do
    case "$state" in
        truncated) printf 'macos-ci-v1\n' > "$checkpoint" ;;
        unknown) sed "2s/.*/no_such_task/" "$work/saved" > "$checkpoint" ;;
        changed) sed 's/^test_symlink$/changed_task/' "$work/saved" > "$checkpoint" ;;
    esac
    run_ci 7 ci
    cmp "$work/prefix" "$checkout/tasks.log"
    grep -q 'Ignoring invalid or outdated CI progress' "$work/output"
done

# Individual workflow task invocations neither read nor clear the ci cursor.
run_ci 0 v_doctor
cmp "$work/saved" "$checkpoint"
[ "$(cat "$checkout/direct.log")" = doctor ] || fail 'individual task did not run'

# A second checkout must not consume the first checkout's saved progress.
first_checkout=$checkout
checkout="$work/other checkout"
mkdir "$checkout"
printf '%s\n' "$failed_task" > "$checkout/fail-task"
run_ci 7 ci
other_checkpoint=$(sed -n 's/^CI progress: //p' "$work/output")
[ "$checkpoint" != "$other_checkpoint" ] || fail 'checkouts share a checkpoint'
cmp "$work/prefix" "$checkout/tasks.log"
cmp "$work/saved" "$checkpoint"
checkout=$first_checkout

# Even the final task is retried after failure, rather than skipped as already done.
last_task=$(tail -n 1 "$work/tasks")
printf '%s\n' "$last_task" > "$checkout/fail-task"
run_ci 7 ci
[ "$(sed -n '2p' "$checkpoint")" = "$last_task" ] || fail 'final task not saved'
rm "$checkout/fail-task"
run_ci 0 ci
printf '%s\n' "$last_task" > "$work/one"
cmp "$work/one" "$checkout/tasks.log"
[ ! -e "$checkpoint" ] || fail 'final-task retry did not clear progress'

# A failed checkpoint write must stop before executing any task.
mkdir "$checkpoint"
run_ci 1 ci --reset
[ ! -s "$checkout/tasks.log" ] || fail 'tasks ran despite a checkpoint write failure'
rmdir "$checkpoint"

# Use the real cleancode runner and TestSession, mocking only individual vet/fmt
# commands. A failed vet session must not start the independent fmt session.
cleancode="$work/cleancode"
"$vexe" -o "$cleancode" "$repo/cmd/tools/vtest-cleancode.v"
cleanroot="$work/clean code"
for directory in vlib/v vlib/json2 vlib/x/ttf cmd/v cmd/tools/testing \
    examples/2048 examples/tetris examples/term.ui tutorials; do
    mkdir -p "$cleanroot/$directory"
done
# The reporter path must exist, as must the formatter's exception paths. Their
# contents are never compiled by the fixture compiler below.
for source in cmd/tools/testing/output_normal.v vlib/v/first.v vlib/v/second.v \
    vlib/veb/tests/graceful_shutdown_test.v vlib/sync/arc/arc_d_ownership.v \
    vlib/v/tests/structs/anon_struct_local_init_test.v \
    vlib/v/tests/bench/bench_json_vs_json2.v; do
    mkdir -p "$cleanroot/$(dirname "$source")"
    printf 'module main\n' > "$cleanroot/$source"
done
cat > "$cleanroot/v" <<'MOCK'
#!/bin/sh
set -eu
for arg do
    case "$arg" in
        vet|fmt)
            printf '%s\n' "$arg" >> "$CLEAN_LOG"
            if [ "$arg" = "$CLEAN_FAIL" ]; then
                echo "intentional $arg fixture failure" >&2
                exit 7
            fi
            exit 0
            ;;
    esac
done
exit 0
MOCK
chmod +x "$cleanroot/v"
mkdir "$work/cleancode-tmp"

run_cleancode() {
    fast=$1
    failing_phase=$2
    expected_status=$3
    : > "$cleanroot/commands.log"
    status=0
    (cd "$cleanroot" && VEXE="$cleanroot/v" VTMP="$work/cleancode-tmp" \
        VFLAGS= VTEST_ONLY= VTEST_ONLY_FN= VTEST_FAIL_FAST="$fast" VJOBS=1 \
        VTEST_MAX_COMPILATION_RETRIES=1 CLEAN_FAIL="$failing_phase" \
        CLEAN_LOG="$cleanroot/commands.log" "$cleancode" test-cleancode) \
        > "$work/output" 2>&1 || status=$?
    [ "$status" -eq "$expected_status" ] || fail "cleancode exit $status, expected $expected_status"
}

run_cleancode 1 vet 1
printf 'vet\n' > "$work/one"
cmp "$work/one" "$cleanroot/commands.log" || fail 'fail-fast continued after the first vet error'

# Without fail-fast, the existing collect-errors behavior must still run fmt.
run_cleancode 0 vet 1
grep -q '^fmt$' "$cleanroot/commands.log" || fail 'normal cleancode did not continue to fmt'

run_cleancode 1 fmt 1
grep -q '^vet$' "$cleanroot/commands.log" || fail 'cleancode skipped successful vetting'
[ "$(grep -c '^fmt$' "$cleanroot/commands.log")" -eq 1 ] || fail 'fmt continued after an error'

run_cleancode 1 none 0
grep -q '^vet$' "$cleanroot/commands.log" || fail 'successful cleancode did not run vet'
grep -q '^fmt$' "$cleanroot/commands.log" || fail 'successful cleancode did not run fmt'

echo 'PASS: macOS CI checkpoint/resume and fail-fast integration tests'
