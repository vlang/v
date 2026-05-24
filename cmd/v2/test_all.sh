#!/usr/bin/env bash
set -euo pipefail

script_dir="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
repo_root="$(CDPATH= cd -- "${script_dir}/../.." && pwd)"

v1_compiler="${V:-${repo_root}/v}"
if [ ! -x "${v1_compiler}" ]; then
  echo "FAIL: v1 compiler not found: ${v1_compiler}"
  exit 1
fi

exec "${v1_compiler}" run "${script_dir}/test_all.vsh" "$@"
