#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd -- "${script_dir}/../.." && pwd)"

v2_source="${script_dir}/v2.v"
v2_bin="${script_dir}/v2"
v3_bin="${script_dir}/v3"
v4_bin="${script_dir}/v4"
v5_bin="${script_dir}/v5"

# Variable for the backend (originally cleanc)
backend="cleanc"

preferred_v1_compiler="${HOME}/code/v/v"
v1_compiler="${V:-${preferred_v1_compiler}}"
if [[ ! -x "${v1_compiler}" ]]; then
    v1_compiler="${repo_root}/v"
fi
if [[ ! -x "${v1_compiler}" ]]; then
    echo "FAILURE: v1 compiler not found or not executable: ${v1_compiler}"
    exit 1
fi

# V1's formatter may clobber v2 source files — backup and restore.
v2_src="${repo_root}/vlib/v2"
v2_bak="/tmp/v2_src_bak_self_test"
rm -rf "${v2_bak}"
cp -R "${v2_src}" "${v2_bak}"

# Build v2 with v1.
rm -f "${v2_bin}" "${v3_bin}" "${v3_bin}.c" "${v4_bin}" "${v4_bin}.c" "${v5_bin}" "${v5_bin}.c"
"${v1_compiler}" -skip-unused -cc cc -o "${v2_bin}" "${v2_source}"

# Restore v2 sources after V1 build.
rsync -a --delete "${v2_bak}/" "${v2_src}/"

# Use clang instead of TCC for v2-compiled C code.
export V2CC="${V2CC:-cc}"

# Use v2 to compile itself to v3 (using defined backend).
"${v2_bin}" -gc none -o "${v3_bin}" -backend "${backend}" "${v2_source}"

echo "SUCCESS: v2 successfully compiled itself to v3"
echo "v3 binary size: $(ls -lh "${v3_bin}" | awk '{print $5}')"

"${v3_bin}" -gc none -o "${v4_bin}" -backend "${backend}" "${v2_source}"
printf '\nV4 compiled\n\n'

"${v4_bin}" -gc none  -o "${v5_bin}" -backend "${backend}" "${v2_source}"
printf '\nV5 compiled\n\n'

# Test that v3 runs and produces expected output.
output="$("${v3_bin}" 2>&1 || true)"
expected='At least 1 .v file expected'

if echo "${output}" | grep -q "${expected}"; then
    echo "SUCCESS"
else
    echo "FAILURE: Expected '${expected}' but got:"
    echo "${output}"
    exit 1
fi

echo ''
echo '=== SELF-COMPILATION TEST PASSED ==='
