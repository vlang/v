#!/bin/bash
set -e

# Build v2 with v1
v v2.v

# Use v2 to compile itself to v3 (using cleanc backend)
./v2 -o v3 -backend cleanc v2.v || exit

# Use v3 to compile hello world
#./v3 -backend cleanc hello.v
./v3 || true

# Test that v3 without args prints the expected error message
OUTPUT=$(./v3 2>&1 || true)
EXPECTED="At least 1 .v file expected"

if echo "$OUTPUT" | grep -q "$EXPECTED"; then
    echo "SUCCESS: v3 works correctly and prints expected message"
else
    echo "FAILURE: Expected '$EXPECTED' but got:"
    echo "$OUTPUT"
    exit 1
fi
