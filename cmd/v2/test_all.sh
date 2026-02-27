#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

echo "=== 1/7: Self-host test ==="
bash test_v2_self.sh

echo ""
echo "=== 2/7: Rebuild v2 and run builtin test files ==="
v self && v -o v2 v2.v
./v2 ../../vlib/builtin/array_test.v
./v2 ../../vlib/builtin/string_test.v
./v2 ../../vlib/builtin/map_test.v

echo ""
echo "=== 3/7: Math test ==="
./v2 ../../vlib/math/math_test.v

echo ""
echo "=== 4/7: SSA backends test (arm64) ==="
v -gc none run test_ssa_backends.v arm64

echo ""
echo "=== 5/7: SSA backends test (cleanc) ==="
v -gc none run test_ssa_backends.v cleanc

echo ""
echo "=== 6/7: Transformer test ==="
v ../../vlib/v2/transformer/transformer_v2_darwin_test.v

echo ""
echo "=== 7/7: Cleanc runtime tests ==="
v -gc none run ../../vlib/v2/gen/cleanc/tests/run_tests.v

echo ""
echo "=== ALL TESTS PASSED ==="
