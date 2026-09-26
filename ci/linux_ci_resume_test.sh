#!/bin/sh
# Exercise the real Linux runner without installing packages or running CI workloads.
set -eu
unset VTEST_RESUME_DIR VTEST_RESUME_OWNER V_CI_TASK_PROGRESS V_MACOS_CI_TASK_PROGRESS

repo=$(CDPATH= cd "$(dirname "$0")/.." && pwd -P)
vexe=${VEXE:-"$repo/v"}
work=$(mktemp -d "${TMPDIR:-/tmp}/v-linux-ci-resume.XXXXXX")
work=$(cd "$work" && pwd -P)
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
    [ ! -f "$work/output" ] || cat "$work/output" >&2
    exit 1
}

# Guard against accidentally omitting or reordering any workflow script task.
sed -n '/^const ci_tasks = \[/,/^\]/p' "$repo/ci/linux_ci.vsh" |
    sed -n "s/^[[:space:]]*'\([^']*\)',$/\1/p" > "$work/tasks"
sed -n 's/^[[:space:]]*run: v run ci\/linux_ci.vsh \([^ ]*\)$/\1/p' \
    "$repo/.github/workflows/linux_ci.yml" > "$work/workflow-tasks"
[ -s "$work/tasks" ] || fail 'could not read the CI task list'
cmp "$work/tasks" "$work/workflow-tasks" || fail 'CI task plan differs from the workflow'

runner="$work/runner"
"$vexe" -o "$runner" "$repo/ci/linux_ci.vsh"
checkout="$work/checkout with spaces"
mkdir "$checkout"
fake_v="$work/fake v"
cat > "$fake_v" <<'MOCK'
#!/bin/sh
set -eu
if [ "$1" = doctor ]; then
    [ "${VTEST_FAIL_FAST:-}" = 0 ] && [ "${VJOBS:-}" = 8 ] || exit 94
    [ -z "${V_CI_TASK_PROGRESS:-}" ] && [ -z "${VTEST_RESUME_DIR:-}" ] || exit 95
    [ "${VTEST_SKIP_OWNERSHIP:-}" = 0 ] || exit 95
    [ "$VFLAGS" = '-cc inherited' ] || exit 98
    echo doctor >> direct.log
    exit 0
fi
[ "$1" = run ] && [ "$2" = ci/linux_ci.vsh ] || exit 90
[ "$CI" = true ] && [ "$GITHUB_ACTIONS" = true ] && [ "$RUNNER_OS" = Linux ] || exit 91
[ "$V_MACOS_V3_NO_FALLBACK" = 1 ] || exit 92
[ "${VTEST_FAIL_FAST:-}" = 1 ] && [ "${VJOBS:-}" = 8 ] || exit 93
[ "${VTEST_SKIP_OWNERSHIP:-}" = 1 ] || exit 93
[ -z "${VTEST_RESUME_OWNER:-}" ] || exit 99
case "$3" in
    *_gcc) compiler=gcc; flags='' ;;
    *_clang) compiler=clang; flags='-cc clang' ;;
    *) compiler=tcc; flags='-cc tcc -no-retry-compilation' ;;
esac
[ "$GITHUB_JOB" = "$compiler-linux" ] && [ "$VFLAGS" = "$flags" ] || exit 92
case "$V_CI_TASK_PROGRESS" in */"$3") ;; *) exit 96 ;; esac
case "$VTEST_RESUME_DIR" in "$V_CI_TASK_PROGRESS"/tests/*) ;; *) exit 97 ;; esac
# The failing/interrupted task must already be durable before it starts.
progress_dir=${V_CI_TASK_PROGRESS%/*}
[ "$(sed -n '2p' "${progress_dir%.d}")" = "$3" ] || exit 98
mkdir -p "$V_CI_TASK_PROGRESS"
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
    # Aggregate CI must select the workflow compiler without changing parallelism.
    (cd "$checkout" && VTEST_FAIL_FAST=0 VJOBS=8 VFLAGS='-cc inherited' \
        VTEST_SKIP_OWNERSHIP=0 VTEST_RESUME_OWNER=inherited V_MACOS_V3_NO_FALLBACK=0 \
        XDG_CACHE_HOME="$work/cache" \
        V_CI_VEXE="$fake_v" "$runner" "$@") > "$work/output" 2>&1 || status=$?
    [ "$status" -eq "$expected_status" ] || fail "exit $status, expected $expected_status"
}

failed_task=verify_v_test_works_tcc
failed_line=$(grep -n "^$failed_task$" "$work/tasks" | cut -d: -f1)
printf '%s\n' "$failed_task" > "$checkout/fail-task"
head -n "$failed_line" "$work/tasks" > "$work/prefix"
tail -n "+$failed_line" "$work/tasks" > "$work/suffix"

run_ci 7 ci
checkpoint=$(sed -n 's/^CI progress: //p' "$work/output")
[ -f "$checkpoint" ] || fail 'failure did not leave a checkpoint'
case "$checkpoint" in "$work/cache"/v-linux-ci-*.progress) ;; *) fail 'wrong checkpoint namespace' ;; esac
[ "$(sed -n '2p' "$checkpoint")" = "$failed_task" ] || fail 'saved the wrong task'
cmp "$work/prefix" "$checkout/tasks.log"
cp "$checkpoint" "$work/saved"
touch "$checkpoint.d/retained"

run_ci 7 ci
printf '%s\n' "$failed_task" > "$work/one"
cmp "$work/one" "$checkout/tasks.log"
cmp "$work/saved" "$checkpoint"
[ -f "$checkpoint.d/retained" ] || fail 'retry discarded per-test progress'

# A fix resumes at the failure and then switches compiler/job for GCC and Clang.
rm "$checkout/fail-task"
run_ci 0 ci
cmp "$work/suffix" "$checkout/tasks.log"
[ ! -e "$checkpoint" ] && [ ! -e "$checkpoint.d" ] || fail 'success retained progress'
run_ci 0 ci
cmp "$work/tasks" "$checkout/tasks.log"

# Invalid arguments must leave progress untouched and execute no tasks.
cp "$work/saved" "$checkpoint"
printf '%s\n' "$failed_task" > "$checkout/fail-task"
mkdir -p "$checkpoint.d"
touch "$checkpoint.d/stale"
for args in 'ci --typo' 'ci --reset extra'; do
    # Intentional splitting: these are fixed argument fixtures, not user input.
    run_ci 1 $args
    [ ! -s "$checkout/tasks.log" ] || fail 'invalid arguments ran tasks'
    cmp "$work/saved" "$checkpoint"
    [ -f "$checkpoint.d/stale" ] || fail 'invalid arguments changed progress'
done
run_ci 7 ci --reset
[ ! -e "$checkpoint.d/stale" ] || fail 'reset retained per-test progress'
cmp "$work/prefix" "$checkout/tasks.log"

# Reject corruption and changes to the ordered plan.
for state in truncated unknown changed; do
    touch "$checkpoint.d/stale"
    case "$state" in
        truncated) printf 'linux-ci-v1\n' > "$checkpoint" ;;
        unknown) sed '2s/.*/no_such_task/' "$work/saved" > "$checkpoint" ;;
        changed) sed 's/build_v_with_prealloc/changed_task/' "$work/saved" > "$checkpoint" ;;
    esac
    run_ci 7 ci
    cmp "$work/prefix" "$checkout/tasks.log"
    grep -q 'Ignoring invalid or outdated CI progress' "$work/output"
    [ ! -e "$checkpoint.d/stale" ] || fail 'invalid cursor retained per-test progress'
done

rm "$checkpoint"
touch "$checkpoint.d/stale"
run_ci 7 ci
cmp "$work/prefix" "$checkout/tasks.log"
[ ! -e "$checkpoint.d/stale" ] || fail 'missing cursor retained per-test progress'

# Direct workflow tasks must not adopt aggregate CI settings or consume its cursor.
run_ci 0 v_doctor_tcc
cmp "$work/saved" "$checkpoint"
[ "$(cat "$checkout/direct.log")" = doctor ] || fail 'individual task did not run'

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

# Resume directly into each later backend, including a failure in the final task.
last_task=$(tail -n 1 "$work/tasks")
for task in verify_v_test_works_gcc verify_v_test_works_clang "$last_task"; do
    printf '%s\n' "$task" > "$checkout/fail-task"
    run_ci 7 ci
    [ "$(sed -n '2p' "$checkpoint")" = "$task" ] || fail 'saved wrong backend task'
    run_ci 7 ci
    printf '%s\n' "$task" > "$work/one"
    cmp "$work/one" "$checkout/tasks.log"
done
rm "$checkout/fail-task"
run_ci 0 ci
cmp "$work/one" "$checkout/tasks.log"
[ ! -e "$checkpoint" ] && [ ! -e "$checkpoint.d" ] || fail 'final retry retained progress'

# An unreadable cursor or failed atomic replacement must stop before running tasks.
mkdir "$checkpoint"
run_ci 1 ci
[ ! -s "$checkout/tasks.log" ] || fail 'tasks ran despite a checkpoint read failure'
run_ci 1 ci --reset
[ ! -s "$checkout/tasks.log" ] || fail 'tasks ran despite a checkpoint write failure'
rmdir "$checkpoint"

echo 'PASS: Linux CI task checkpoint/resume integration tests'
