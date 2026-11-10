#!/usr/bin/env bash
set -euo pipefail

if [ "$#" -ne 2 ]; then
  echo "usage: $0 archive.zip destination" >&2
  exit 2
fi

archive=$1
destination=$2
mkdir -p "$destination"

if [ "$(uname -s)" = Darwin ]; then
  # Use the native macOS archive tool for the release's symbolic links.
  ditto -x -k "$archive" "$destination"
else
  unzip -q "$archive" -d "$destination"
fi

if [ ! -x "$destination/v/v" ]; then
  echo "Release compiler is missing or not executable: $destination/v/v" >&2
  exit 1
fi
