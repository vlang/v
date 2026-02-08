#!/bin/bash
set -e

# Build v2 with v1
rm v2 || true
V=${V:-$HOME/code/v/v}
$V v2.v

rm v3 || true
rm v3.c || true
# Use v2 to compile itself to v3 (using cleanc backend)
./v2 -o v3 -backend cleanc v2.v || exit

echo "SUCCESS: v2 successfully compiled itself to v3"
echo "v3 binary size: $(ls -lh v3 | awk '{print $5}')"

# Test that v3 runs and produces expected output
OUTPUT=$(./v3 2>&1 || true)
EXPECTED="At least 1 .v file expected"

if echo "$OUTPUT" | grep -q "$EXPECTED"; then
    echo "SUCCESS: v3 runs correctly and prints expected message"
else
    echo "FAILURE: Expected '$EXPECTED' but got:"
    echo "$OUTPUT"
    exit 1
fi

echo ""
echo "=== SELF-COMPILATION TEST PASSED ==="
