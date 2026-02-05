#!/bin/bash
# Build script for vbeam runtime library
# Compiles all Erlang modules to .beam files

set -e

# Check for erlc
if ! command -v erlc &> /dev/null; then
    echo "ERROR: erlc not found in PATH"
    echo ""
    echo "Please install Erlang/OTP:"
    echo "  macOS:   brew install erlang"
    echo "  Ubuntu:  apt install erlang"
    echo "  Fedora:  dnf install erlang"
    echo "  asdf:    asdf install erlang latest"
    echo ""
    exit 1
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
RUNTIME_DIR="$(dirname "$SCRIPT_DIR")"
EBIN_DIR="${RUNTIME_DIR}/ebin"

# Create ebin directory
mkdir -p "$EBIN_DIR"

echo "Building vbeam runtime..."
echo "Source: ${RUNTIME_DIR}"
echo "Output: ${EBIN_DIR}"

# Compile all .erl files
for erl_file in "${RUNTIME_DIR}"/*.erl; do
    if [ -f "$erl_file" ]; then
        module=$(basename "$erl_file" .erl)
        echo "  Compiling ${module}..."
        erlc -o "$EBIN_DIR" "$erl_file"
    fi
done

echo ""
echo "Build complete. BEAM files in: ${EBIN_DIR}"
echo ""
echo "To use the runtime:"
echo "  1. Add to ERL_LIBS: export ERL_LIBS=${RUNTIME_DIR}"
echo "  2. Or copy .beam files to your project's .beam/ directory"
echo "  3. Or use: erl -pa ${EBIN_DIR} ..."
