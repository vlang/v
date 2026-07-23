#!/usr/bin/env bash
# Runs the shared tccbin conformance tests (and, if a platform name is
# given, that platform's additional regression tests) against a built tcc.
#
# Usage:
#   run.sh <path to tcc> [platform] [-- <extra tcc args>]
#
# Extra args (everything after --) are passed to every test compile -
# e.g. GC defines, -bt flags, path to libgc.a. These vary per platform,
# so the caller supplies them; this script doesn't hardcode anything
# platform-specific itself.
#
# Each test is a pair: <name>.c and <name>.expected. The .expected
# file has 1 or 2 lines:
#   line 1: expected exit code, or the literal word "nonzero"
#   line 2 (optional): a substring that must appear in the program's
#                       combined stdout+stderr. Omit for no check.
set -u

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
tcc="$1"; shift
platform=""
if [ "$#" -gt 0 ] && [ "$1" != "--" ]; then
    platform="$1"
    shift
fi
if [ "$#" -gt 0 ] && [ "$1" = "--" ]; then
    shift
fi
extra_args=("$@")

dirs=("$here/shared")
if [ -n "$platform" ] && [ -d "$here/platform/$platform" ]; then
    dirs+=("$here/platform/$platform")
fi

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

passed=0
failed=0

for dir in "${dirs[@]}"; do
    for src in "$dir"/*.c; do
        [ -e "$src" ] || continue
        name=$(basename "$src" .c)
        expected="$dir/$name.expected"
        if [ ! -f "$expected" ]; then
            echo "FAIL $name (no .expected file)"
            failed=$((failed + 1))
            continue
        fi
        expect_exit=$(sed -n '1p' "$expected")
        expect_substr=$(sed -n '2p' "$expected")

        exe="$work/$name.exe"
        if ! "$tcc" "$src" "${extra_args[@]}" -o "$exe" >/dev/null 2>&1; then
            echo "FAIL $name (compile error)"
            failed=$((failed + 1))
            continue
        fi

        out=$("$exe" 2>&1)
        code=$?

        ok=1
        if [ "$expect_exit" = "nonzero" ]; then
            [ "$code" -eq 0 ] && ok=0
        else
            [ "$code" -eq "$expect_exit" ] || ok=0
        fi
        if [ -n "$expect_substr" ]; then
            case "$out" in
                *"$expect_substr"*) ;;
                *) ok=0 ;;
            esac
        fi

        if [ "$ok" -eq 1 ]; then
            echo "PASS $name"
            passed=$((passed + 1))
        else
            echo "FAIL $name (exit=$code, output=$out)"
            failed=$((failed + 1))
        fi
    done
done

echo "---"
echo "$passed passed, $failed failed"
[ "$failed" -eq 0 ]
