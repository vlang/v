#!/bin/sh
# Exercise the optimized test path without replacing its compiler or dropping the debug check.
set -eu
repo=$(CDPATH= cd "$(dirname "$0")/.." && pwd -P)
vexe=${VEXE:-"$repo/v"}
work=$(mktemp -d "${TMPDIR:-/tmp}/v-linux-ci-vprod.XXXXXX")
trap 'rm -rf "$work"' EXIT
"$vexe" -skip-running -o "$work/runner" "$repo/ci/linux_ci.vsh"
cat > "$work/vprod" <<'MOCK'
#!/bin/sh
set -eu
[ "$VFLAGS" = '-cc tcc -no-retry-compilation' ]
if [ "$1" = -keepc ]; then
    [ "$2" = -cc ] && [ "$3" = tcc ] && [ "$4" = -g ]
    [ "$5" = -o ] && [ "$6" = v_tcc_debug ] && [ "$7" = self ]
    echo debug-build >> events
    [ "${FAIL_STAGE:-}" != build ] || exit 7
    cat > v_tcc_debug <<'PROGRAM'
#!/bin/sh
set -eu
[ "$1" = run ] && [ "$2" = examples/hello_world.v ]
echo debug-run >> events
[ "${FAIL_STAGE:-}" != debug ] || exit 8
PROGRAM
    chmod +x v_tcc_debug
    exit 0
fi
[ "$#" -eq 3 ] && [ "$1" = -W ] && [ "$2" = test-self ] && [ "$3" = vlib ]
echo tests >> events
[ "${FAIL_STAGE:-}" != tests ] || exit 9
MOCK
chmod +x "$work/vprod"
cp "$work/vprod" "$work/original"
for stage in success build debug tests; do
    : > "$work/events"
    status=0
    (cd "$work" && GITHUB_JOB=tcc-linux VFLAGS='-cc tcc -no-retry-compilation' \
        V_CI_VEXE="$work/vprod" FAIL_STAGE="$stage" \
        ./runner self_tests_vprod_tcc) > "$work/output" 2>&1 || status=$?
    case "$stage" in
        success) expected=0; printf 'debug-build\ndebug-run\ntests\n' > "$work/expected" ;;
        build) expected=7; printf 'debug-build\n' > "$work/expected" ;;
        debug) expected=8; printf 'debug-build\ndebug-run\n' > "$work/expected" ;;
        tests) expected=9; printf 'debug-build\ndebug-run\ntests\n' > "$work/expected" ;;
    esac
    [ "$status" -eq "$expected" ] || { cat "$work/output"; exit 1; }
    cmp "$work/events" "$work/expected"
    cmp "$work/vprod" "$work/original"
done
echo 'optimized Linux CI compiler preservation and failure propagation passed'
