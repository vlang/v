#!/bin/sh
# Exercise the real runner with cheap subprocesses, not the macOS CI workloads.
set -eu
unset VTEST_RESUME_DIR VTEST_RESUME_OWNER V_MACOS_CI_TASK_PROGRESS

repo=$(CDPATH= cd "$(dirname "$0")/.." && pwd -P)
vexe=${VEXE:-"$repo/v"}
work=$(mktemp -d "${TMPDIR:-/tmp}/v-macos-ci-resume.XXXXXX")
checkpoint=
other_checkpoint=
cleanup() {
    [ -z "$checkpoint" ] || rm -rf "$checkpoint" "$checkpoint.d"
    [ -z "$other_checkpoint" ] || rm -rf "$other_checkpoint" "$other_checkpoint.d"
    rm -rf "$work"
}
trap cleanup EXIT
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
    [ -z "${V_MACOS_CI_TASK_PROGRESS:-}" ] && [ -z "${VTEST_RESUME_DIR:-}" ] || exit 95
    echo doctor >> direct.log
    exit 0
fi
[ "$1" = run ] && [ "$2" = ci/macos_ci.vsh ] || exit 90
[ "$CI" = true ] && [ "$GITHUB_ACTIONS" = true ] || exit 91
[ "$VFLAGS" = '-cc clang' ] && [ "$V_MACOS_V3_NO_FALLBACK" = 1 ] || exit 92
[ "${VTEST_FAIL_FAST:-}" = 1 ] && [ "${VJOBS:-}" = 1 ] || exit 93
case "$V_MACOS_CI_TASK_PROGRESS" in */"$3") ;; *) exit 96 ;; esac
case "$VTEST_RESUME_DIR" in "$V_MACOS_CI_TASK_PROGRESS"/tests/*) ;; *) exit 97 ;; esac
mkdir -p "$V_MACOS_CI_TASK_PROGRESS"
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
touch "$checkpoint.d/retained"

# An unchanged failure retries only that task, retaining the cursor and exit status.
run_ci 7 ci
printf '%s\n' "$failed_task" > "$work/one"
cmp "$work/one" "$checkout/tasks.log"
cmp "$work/saved" "$checkpoint"
[ -f "$checkpoint.d/retained" ] || fail 'retry discarded per-test progress'

# A fix resumes at the failed task; completion clears progress; the next run is full.
rm "$checkout/fail-task"
run_ci 0 ci
cmp "$work/suffix" "$checkout/tasks.log"
[ ! -e "$checkpoint" ] || fail 'successful run did not clear progress'
[ ! -e "$checkpoint.d" ] || fail 'successful run did not clear per-test progress'
run_ci 0 ci
cmp "$work/tasks" "$checkout/tasks.log"

# Explicit reset ignores saved progress, while invalid arguments must not run tasks.
cp "$work/saved" "$checkpoint"
printf '%s\n' "$failed_task" > "$checkout/fail-task"
mkdir -p "$checkpoint.d"
touch "$checkpoint.d/stale"
run_ci 7 ci --reset
[ ! -e "$checkpoint.d/stale" ] || fail 'reset retained per-test progress'
cmp "$work/prefix" "$checkout/tasks.log"
run_ci 1 ci --typo
[ ! -s "$checkout/tasks.log" ] || fail 'invalid arguments ran tasks'
cmp "$work/saved" "$checkpoint"

# Truncated data, an unknown task, and a changed ordered plan all restart safely.
for state in truncated unknown changed; do
    touch "$checkpoint.d/stale"
    case "$state" in
        truncated) printf 'macos-ci-v1\n' > "$checkpoint" ;;
        unknown) sed "2s/.*/no_such_task/" "$work/saved" > "$checkpoint" ;;
        changed) sed 's/^test_symlink$/changed_task/' "$work/saved" > "$checkpoint" ;;
    esac
    run_ci 7 ci
    cmp "$work/prefix" "$checkout/tasks.log"
    grep -q 'Ignoring invalid or outdated CI progress' "$work/output"
    [ ! -e "$checkpoint.d/stale" ] || fail 'invalid cursor retained per-test progress'
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

# Exercise per-file checkpoints through the real v test tool. Only compilation
# and the resulting test binaries are fixtures; discovery, retries and progress
# persistence use the production TestSession implementation, with OK logs hidden.
vtest="$work/vtest"
"$vexe" -o "$vtest" "$repo/cmd/tools/vtest.v"
testroot="$work/test checkout"
mkdir -p "$testroot/tests" "$testroot/nested" "$testroot/cmd/tools/testing" "$work/test-tmp"
: > "$testroot/cmd/tools/testing/output_normal.v"
for name in a b c; do
    printf 'fn test_ok() {}\n' > "$testroot/tests/${name}_test.v"
done
printf 'fn test_ok() {}\n' > "$testroot/nested/inner_test.v"
cat > "$testroot/v" <<'MOCK'
#!/bin/sh
set -eu
binary=
file=
run_now=1
while [ "$#" -gt 0 ]; do
    case "$1" in
        -o) shift; binary=$1 ;;
        -skip-running) run_now=0 ;;
        *_test.v) file=$1 ;;
    esac
    shift
done
[ -n "$binary" ] && [ -n "$file" ] || exit 90
name=$(basename "$file")
printf 'compile:%s\n' "$name" >> "$RESUME_LOG"
if [ "$name" = b_test.v ] && [ "$FAIL_PHASE" = compile ]; then
    echo 'intentional compilation failure' >&2
    exit 7
fi
cat > "$binary" <<PROGRAM
#!/bin/sh
set -eu
printf '%s\n' 'run:$name' >> "\$RESUME_LOG"
if [ '$name' = a_test.v ] && [ "\$FAIL_PHASE" = nested ]; then
    "\$TEST_DRIVER" test "\$TEST_NESTED"
    echo 'intentional failure after nested test' >&2
    exit 7
fi
if [ '$name' = b_test.v ] && [ "\$FAIL_PHASE" = runtime ]; then
    echo 'intentional runtime failure' >&2
    exit 7
fi
PROGRAM
chmod +x "$binary"
if [ "$run_now" = 1 ]; then
    exec "$binary"
fi
MOCK
chmod +x "$testroot/v"

run_tests() {
    expected_status=$1
    phase=$2
    shift 2
    : > "$testroot/commands.log"
    status=0
    (cd "$testroot" && VEXE="$testroot/v" VTMP="$work/test-tmp" \
        VFLAGS= VTEST_ONLY= VTEST_ONLY_FN= VTEST_FAIL_FAST=1 VJOBS=1 \
        VTEST_HIDE_OK=1 VTEST_FAIL_FLAKY=0 VTEST_MAX_COMPILATION_RETRIES=1 \
        VTEST_RESUME_DIR="$testroot/progress" VTEST_RESUME_OWNER= \
        RESUME_LOG="$testroot/commands.log" FAIL_PHASE="$phase" \
        TEST_DRIVER="$vtest" TEST_NESTED="$testroot/nested/inner_test.v" \
        "$vtest" "$@" test "$testroot/tests") > "$work/output" 2>&1 || status=$?
    [ "$status" -eq "$expected_status" ] || fail "test exit $status, expected $expected_status"
}

expect_commands() {
    printf '%s\n' "$@" > "$work/expected-commands"
    cmp "$work/expected-commands" "$testroot/commands.log" || fail 'unexpected test executions'
}

for phase in compile runtime; do
    rm -rf "$testroot/progress"
    run_tests 1 "$phase"
    grep -q '^run:a_test.v$' "$testroot/commands.log" || fail 'first test did not run'
    if grep -q 'c_test.v' "$testroot/commands.log"; then
        fail 'fail-fast ran a later test'
    fi
    run_tests 1 "$phase"
    if [ "$phase" = compile ]; then
        expect_commands compile:b_test.v
    else
        expect_commands compile:b_test.v run:b_test.v
    fi
    run_tests 0 none
    expect_commands compile:b_test.v run:b_test.v compile:c_test.v run:c_test.v
    run_tests 0 none
    [ ! -s "$testroot/commands.log" ] || fail 'passed tests were run again'
done

# Editing one successful file makes only that file pending again.
printf '// edit\n' >> "$testroot/tests/a_test.v"
run_tests 0 none
expect_commands compile:a_test.v run:a_test.v

# Stats mode has a separate configuration and records actual execution success.
run_tests 1 runtime -stats
grep -q '^run:a_test.v$' "$testroot/commands.log"
run_tests 1 runtime -stats
expect_commands compile:b_test.v run:b_test.v
run_tests 0 none -stats
expect_commands compile:b_test.v run:b_test.v compile:c_test.v run:c_test.v
run_tests 0 none -stats
[ ! -s "$testroot/commands.log" ] || fail 'stats-mode successes were not checkpointed'

# Ignored flaky failures must not become successful per-file checkpoints.
rm -rf "$testroot/progress"
printf '// vtest flaky: true\nfn test_ok() {}\n' > "$testroot/tests/b_test.v"
run_tests 0 runtime
run_tests 0 runtime
expect_commands compile:b_test.v run:b_test.v

# A failed outer test must run its nested tests afresh on every retry.
rm -rf "$testroot/progress"
run_tests 1 nested
run_tests 1 nested
expect_commands compile:a_test.v run:a_test.v compile:inner_test.v run:inner_test.v

# The sanitizer task contains a compiler build followed by an instrumented
# self-compilation, not a test-file session. Reuse only an unchanged built v2.
sanitize_root="$work/sanitizers"
mkdir "$sanitize_root"
cat > "$sanitize_root/compiler" <<'MOCK'
#!/bin/sh
set -eu
echo build >> commands.log
cat > v2 <<'PROGRAM'
#!/bin/sh
echo sanitize >> commands.log
[ ! -f fail-sanitizer ]
PROGRAM
chmod +x v2
MOCK
chmod +x "$sanitize_root/compiler"
run_sanitizers() {
    status=0
    (cd "$sanitize_root" && V_CI_VEXE="$sanitize_root/compiler" \
        V_MACOS_CI_TASK_PROGRESS="$sanitize_root/progress" \
        "$runner" run_sanitizers) > "$work/output" 2>&1 || status=$?
    [ "$status" -eq "$1" ] || fail "sanitizer exit $status, expected $1"
}
touch "$sanitize_root/fail-sanitizer"
run_sanitizers 1
run_sanitizers 1
[ "$(grep -c '^build$' "$sanitize_root/commands.log")" -eq 1 ] || fail 'rebuilt successful sanitizer compiler'
rm "$sanitize_root/v2"
run_sanitizers 1
[ "$(grep -c '^build$' "$sanitize_root/commands.log")" -eq 2 ] || fail 'reused missing sanitizer compiler'
printf '\n# changed\n' >> "$sanitize_root/v2"
run_sanitizers 1
[ "$(grep -c '^build$' "$sanitize_root/commands.log")" -eq 3 ] || fail 'reused changed sanitizer compiler'
rm "$sanitize_root/fail-sanitizer"
run_sanitizers 0
[ "$(grep -c '^build$' "$sanitize_root/commands.log")" -eq 3 ] || fail 'rebuilt sanitizer compiler on retry'

echo 'PASS: macOS CI task, test-file and sanitizer checkpoint/resume integration tests'
