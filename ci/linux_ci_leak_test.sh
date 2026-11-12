#!/bin/sh
# Run the real leak-check task with deterministic empty and nonempty stderr.
set -eu
repo=$(CDPATH= cd "$(dirname "$0")/.." && pwd -P)
vexe=${VEXE:-"$repo/v"}
work=$(mktemp -d "${TMPDIR:-/tmp}/v-linux-ci-leak.XXXXXX")
trap 'rm -rf "$work"' EXIT
"$vexe" -o "$work/runner" "$repo/ci/linux_ci.vsh"
cat > "$work/compiler" <<'MOCK'
#!/bin/sh
set -eu
[ "$1" = -o ] && [ "$2" = testcase_leak ]
[ "$3" = vlib/v/tests/testcase_leak.vv ]
cat > testcase_leak <<'PROGRAM'
#!/bin/sh
if [ "${LEAK_TEST_OUTPUT:-}" = nonempty ]; then
    echo 'unexpected leak detector output' >&2
fi
PROGRAM
chmod +x testcase_leak
MOCK
chmod +x "$work/compiler"
(cd "$work" && V_CI_VEXE="$work/compiler" ./runner test_leak_detector_not_active_tcc)
status=0
(cd "$work" && LEAK_TEST_OUTPUT=nonempty V_CI_VEXE="$work/compiler" \
    ./runner test_leak_detector_not_active_tcc) > "$work/output" 2>&1 || status=$?
[ "$status" -ne 0 ] || { echo 'nonempty leak output was accepted' >&2; exit 1; }
grep -q 'unexpected leak detector output' "$work/output"
echo 'Linux CI leak check passed'
