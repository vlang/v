#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

echo "=== 1/12: Self-host test ==="
bash test_v2_self.sh

echo ""
echo "=== 2/12: Rebuild v2 and run builtin test files ==="
rm -rf /tmp/v2_cleanc_obj_cache
v self && v -o v2 v2.v
./v2 ../../vlib/builtin/array_test.v
./v2 ../../vlib/builtin/string_test.v
./v2 ../../vlib/builtin/map_test.v

echo ""
echo "=== 3/12: Builtin test files (arm64) ==="
./v2 -backend arm64 ../../vlib/builtin/array_test.v
./v2 -backend arm64 ../../vlib/builtin/string_test.v
./v2 -backend arm64 ../../vlib/builtin/map_test.v

echo ""
echo "=== 4/12: Math test ==="
./v2 ../../vlib/math/math_test.v

echo ""
echo "=== 5/12: Math test (arm64) ==="
./v2 -backend arm64 ../../vlib/math/math_test.v

echo ""
echo "=== 6/12: Sumtype tests ==="
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
echo "=== 7/12: Sumtype tests (arm64) ==="
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
echo "=== 8/12: SSA backends test (arm64) ==="
v -gc none run test_ssa_backends.v arm64

echo ""
echo "=== 9/12: SSA backends test (cleanc) ==="
v -gc none run test_ssa_backends.v cleanc

echo ""
echo "=== 10/12: Transformer unit tests ==="
v ../../vlib/v2/transformer/transformer_test.v

echo ""
echo "=== 11/12: Transformer integration test ==="
v ../../vlib/v2/transformer/transformer_v2_darwin_test.v

echo ""
echo "=== 12/12: Cleanc runtime tests ==="
v -gc none run ../../vlib/v2/gen/cleanc/tests/run_tests.v

echo ""
echo "=== ALL TESTS PASSED ==="
