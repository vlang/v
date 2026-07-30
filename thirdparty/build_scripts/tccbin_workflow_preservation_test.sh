#!/usr/bin/env bash

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"

for script in \
  thirdparty-linux-amd64_tcc.sh \
  thirdparty-freebsd-amd64_tcc.sh \
  thirdparty-openbsd-amd64_tcc.sh
do
  script_path="$script_dir/$script"
  test "$(grep -Fc 'rsync -a --delete' "$script_path")" -eq 1
  grep -Fq -- "rsync -a --delete --exclude='/.github/' tinycc/\$TCC_FOLDER/" "$script_path"
done

test_root="$(mktemp -d)"
trap 'rm -rf "$test_root"' EXIT

source_dir="$test_root/tinycc/thirdparty/tcc"
target_dir="$test_root/thirdparty/tcc"
mkdir -p "$source_dir" "$target_dir/.github/workflows"
printf 'fresh\n' > "$source_dir/fresh.txt"
printf 'obsolete\n' > "$target_dir/obsolete.txt"
printf 'sentinel:\n  preserved: true\n' > "$test_root/sentinel.yml"
cp "$test_root/sentinel.yml" "$target_dir/.github/workflows/sentinel.yml"

rsync -a --delete --exclude='/.github/' "$source_dir/" "$target_dir/"

test "$(cat "$target_dir/fresh.txt")" = fresh
test ! -e "$target_dir/obsolete.txt"
cmp -s "$test_root/sentinel.yml" "$target_dir/.github/workflows/sentinel.yml"
