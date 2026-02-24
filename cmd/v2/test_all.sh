#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

echo "=== 1/6: Self-host test ==="
bash test_v2_self.sh

echo ""
echo "=== 2/6: Rebuild v2 and run builtin test files ==="
v self && v -o v2 v2.v
./v2 ../../vlib/builtin/array_test.v
./v2 ../../vlib/builtin/string_test.v
./v2 ../../vlib/builtin/map_test.v

echo ""
echo "=== 3/6: SSA backends test (arm64) ==="
v -gc none run test_ssa_backends.v arm64

echo ""
echo "=== 4/6: SSA backends test (cleanc) ==="
v -gc none run test_ssa_backends.v cleanc

echo ""
echo "=== 5/6: Transformer v2 nix test ==="
v ../../vlib/v2/transformer/transformer_v2_nix_test.v

echo ""
echo "=== ALL TESTS PASSED ==="
