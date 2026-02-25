#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

echo "=== 1/7: Self-host test ==="
bash test_v2_self.sh

echo ""
echo "=== 2/7: Rebuild v2 and run builtin test files ==="
v self && v -o v2 v2.v
./v2 -o array_test ../../vlib/builtin/array_test.v && ./array_test && rm -f array_test array_test.c
./v2 -o string_test ../../vlib/builtin/string_test.v && ./string_test && rm -f string_test string_test.c
./v2 -o map_test ../../vlib/builtin/map_test.v && ./map_test && rm -f map_test map_test.c

echo ""
echo "=== 3/7: Math test ==="
./v2 -o math_test ../../vlib/math/math_test.v && ./math_test && rm -f math_test math_test.c

echo ""
echo "=== 4/7: SSA backends test (arm64) ==="
v -gc none run test_ssa_backends.v arm64

echo ""
echo "=== 5/7: SSA backends test (cleanc) ==="
v -gc none run test_ssa_backends.v cleanc

echo ""
echo "=== 6/7: Transformer v2 nix test ==="
v ../../vlib/v2/transformer/transformer_v2_nix_test.v

echo ""
echo "=== ALL TESTS PASSED ==="
