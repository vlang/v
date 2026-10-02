#!/usr/bin/env bash

# Compile the single vc snapshot with Linux-only prctl guards for older versions.
# pipefail ensures that a failed filter cannot be hidden by a successful compiler.
set -euo pipefail

snapshot=$1
shift
script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)

awk -f "$script_dir/macos_vc_compat.awk" "$snapshot" | "$@" -x c -
