#!/usr/bin/env bash
# Build a standalone V3 compiler with Clang's profile-guided optimization.
set -euo pipefail

if (( $# > 2 )); then
    echo "usage: $0 [bootstrap-compiler [output]]" >&2
    exit 1
fi

script_dir="$(CDPATH= cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
bootstrap="${1:-$script_dir/v3}"
output="${2:-$script_dir/v3-pgo}"
[[ "$bootstrap" = /* ]] || bootstrap="$PWD/$bootstrap"
[[ "$output" = /* ]] || output="$PWD/$output"
clang="${CC:-clang}"
profdata="${LLVM_PROFDATA:-llvm-profdata}"
if [[ -z "${LLVM_PROFDATA:-}" ]] && ! command -v "$profdata" >/dev/null 2>&1 \
    && command -v xcrun >/dev/null 2>&1; then
    profdata="$(xcrun --find llvm-profdata)"
fi
command -v "$clang" >/dev/null || { echo "Clang executable not found: $clang" >&2; exit 1; }
command -v "$profdata" >/dev/null || { echo "profile tool not found: $profdata" >&2; exit 1; }
case "$("$clang" --version)" in
    *clang*) ;;
    *) echo "profile-guided builds require Clang: $clang" >&2; exit 1 ;;
esac
if [[ ! -x "$bootstrap" ]]; then
    echo "bootstrap compiler is not executable: $bootstrap" >&2
    exit 1
fi

# A fresh directory prevents profiles from unrelated compiler revisions mixing.
# Its path has no shell metacharacters when passed through V's -cflags parser.
pgo_dir="$(mktemp -d /tmp/v3-pgo.XXXXXXXX)"
trap 'rm -rf -- "$pgo_dir"' EXIT
mkdir "$pgo_dir/profiles"
cd -- "$script_dir"
common=(-gc none -prealloc -prod -nocache -building-v -silent -cc "$clang")

printf '%s\n' 'Building the instrumented compiler...'
"$bootstrap" "${common[@]}" \
    -cflags "-fprofile-generate=$pgo_dir/profiles -fprofile-update=atomic" \
    -o "$pgo_dir/instrumented" v3.v

for run in 1 2 3; do
    printf 'Collecting self-compilation profile %s/3...\n' "$run"
    LLVM_PROFILE_FILE="$pgo_dir/profiles/default-%m.profraw" \
        "$pgo_dir/instrumented" -nocache -building-v -silent \
        -o "$pgo_dir/training.c" v3.v
done
"$profdata" merge "$pgo_dir"/profiles/*.profraw -o "$pgo_dir/merged.profdata"

printf '%s\n' 'Building the optimized compiler...'
"$bootstrap" "${common[@]}" \
    -cflags "-fprofile-use=$pgo_dir/merged.profdata ${V3_PGO_CFLAGS:-}" \
    -o "$output" v3.v
printf 'Built %s\n' "$output"
