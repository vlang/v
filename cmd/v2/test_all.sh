#!/usr/bin/env bash
set -euo pipefail

script_dir="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
repo_root="$(CDPATH= cd -- "${script_dir}/../.." && pwd)"

cd "${script_dir}"

v1_compiler="${V:-${repo_root}/v}"
if [ ! -x "${v1_compiler}" ]; then
  echo "FAIL: v1 compiler not found: ${v1_compiler}"
  exit 1
fi

# V1's formatter may clobber v2 source files during rebuild.
# Back up the entire v2 tree and restore after each V1 build.
V2_SRC="../../vlib/v2"
V2_BAK="/tmp/v2_src_bak_test_all"

backup_v2_src() {
  rm -rf "$V2_BAK"
  cp -R "$V2_SRC" "$V2_BAK"
}

restore_v2_src() {
  rsync -a --delete "$V2_BAK/" "$V2_SRC/"
}

KNOWN_FAILURES=0

echo "=== 1/14: ARM64 self-host hello world ==="
backup_v2_src
"${v1_compiler}" -skip-unused -cc cc -o v2 v2.v
restore_v2_src
./v2 -backend arm64 -gc none -nocache -o v3 v2.v
./v3 -backend arm64 -o hello_arm hello.v
./hello_arm

echo ""
echo "=== 2/14: ARM64 self-host chain (v2->v3->v4->v5->v6) ==="
echo "  Building v3 from v2..."
./v2 -nocache -gc none -backend arm64 -o v3_chain v2.v
echo "  Building v4 from v3..."
./v3_chain -nocache -gc none -backend arm64 -o v4_chain v2.v
echo "  Building v5 from v4..."
./v4_chain -nocache -gc none -backend arm64 -o v5_chain v2.v
echo "  Building v6 from v5..."
./v5_chain -nocache -gc none -backend arm64 -o v6_chain v2.v
V5_SIZE=$(wc -c < v5_chain)
V6_SIZE=$(wc -c < v6_chain)
if [ "$V5_SIZE" -eq "$V6_SIZE" ]; then
  echo "  v5=v6 ($V5_SIZE bytes) — chain converged"
else
  echo "  FAIL: v5 ($V5_SIZE) != v6 ($V6_SIZE)"
  exit 1
fi
rm -f v3_chain v4_chain v5_chain v6_chain

echo ""
echo "=== 3/14: Self-host test ==="
bash test_v2_self.sh

echo ""
echo "=== 4/14: Builtin test files (cleanc) ==="
rm -rf /tmp/v2_cleanc_obj_cache
./v2 ../../vlib/builtin/array_test.v
./v2 ../../vlib/builtin/string_test.v
./v2 ../../vlib/builtin/map_test.v

echo ""
echo "=== 5/14: Builtin test files (arm64) ==="
./v2 -backend arm64 ../../vlib/builtin/array_test.v
./v2 -backend arm64 ../../vlib/builtin/string_test.v
./v2 -backend arm64 ../../vlib/builtin/map_test.v

echo ""
echo "=== 6/14: Math test ==="
./v2 ../../vlib/math/math_test.v

echo ""
echo "=== 7/14: Math test (arm64) ==="
./v2 -backend arm64 ../../vlib/math/math_test.v

echo ""
echo "=== 8/14: Sumtype tests ==="
./v2 test_sumtype.v
./v2 test_sumtype2.v
./v2 test_sumtype3.v
./v2 test_sumtype4.v
./v2 test_sumtype_pos.v
./v2 test_sumtype_data.v
./v2 test_sumtype_ifexpr.v
./v2 test_sumtype_nested.v
./v2 test_sumtype_many.v
./v2 test_sumtype_global.v

echo ""
echo "=== 9/14: Sumtype tests (arm64) ==="
./v2 -backend arm64 test_sumtype.v
./v2 -backend arm64 test_sumtype2.v
./v2 -backend arm64 test_sumtype3.v
./v2 -backend arm64 test_sumtype4.v
./v2 -backend arm64 test_sumtype_pos.v
./v2 -backend arm64 test_sumtype_data.v
./v2 -backend arm64 test_sumtype_ifexpr.v
./v2 -backend arm64 test_sumtype_nested.v
./v2 -backend arm64 test_sumtype_many.v
./v2 -backend arm64 test_sumtype_global.v

echo ""
echo "=== 10/14: SSA backends test (arm64) ==="
"${v1_compiler}" -gc none run test_ssa_backends.v arm64

echo ""
echo "=== 11/14: SSA backends test (cleanc) ==="
"${v1_compiler}" -gc none run test_ssa_backends.v cleanc

echo ""
echo "=== 12/14: Transformer unit tests ==="
"${v1_compiler}" ../../vlib/v2/transformer/transformer_test.v

echo ""
echo "=== 13/14: Transformer integration test ==="
"${v1_compiler}" ../../vlib/v2/transformer/transformer_v2_darwin_test.v

echo ""
echo "=== 14/14: Cleanc runtime tests ==="
"${v1_compiler}" -gc none run ../../vlib/v2/gen/cleanc/tests/run_tests.v

echo ""
if [ "$KNOWN_FAILURES" -gt 0 ]; then
  echo "=== ALL TESTS PASSED ($KNOWN_FAILURES known failures skipped) ==="
else
  echo "=== ALL TESTS PASSED ==="
fi
