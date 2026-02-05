#!/bin/bash
# Install vbeam runtime to a V project's .beam output directory
# Usage: install_runtime.sh /path/to/project.beam

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
RUNTIME_DIR="$(dirname "$SCRIPT_DIR")"
EBIN_DIR="${RUNTIME_DIR}/ebin"

# Check arguments
if [ -z "$1" ]; then
    echo "Usage: $0 /path/to/project.beam"
    echo ""
    echo "Installs vbeam runtime .beam files to the specified directory."
    echo "The runtime must be built first: ./build_runtime.sh"
    exit 1
fi

TARGET_DIR="$1"

# Check if runtime is built
if [ ! -d "$EBIN_DIR" ] || [ -z "$(ls -A "$EBIN_DIR" 2>/dev/null)" ]; then
    echo "ERROR: Runtime not built. Run build_runtime.sh first."
    exit 1
fi

# Check if target directory exists
if [ ! -d "$TARGET_DIR" ]; then
    echo "ERROR: Target directory does not exist: $TARGET_DIR"
    exit 1
fi

# Copy runtime files
echo "Installing vbeam runtime to: $TARGET_DIR"
cp "$EBIN_DIR"/*.beam "$TARGET_DIR/"
echo "Done. Installed:"
ls -la "$TARGET_DIR"/vbeam_*.beam
