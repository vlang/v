#!/usr/bin/env sh
set -eu

script_dir=$(CDPATH= cd "$(dirname "$0")" && pwd)
repo_root=$(CDPATH= cd "$script_dir/../../../.." && pwd)
cd "$repo_root"

target="vlib/x/executor"

if [ ! -d "$target" ]; then
	echo "$target does not exist" >&2
	exit 1
fi

if find "$target" -type f -name '*.v' -print | xargs grep -n 'import x\.async' >/tmp/xexecutor_async_imports.$$ 2>/dev/null; then
	echo "x.executor must not import x.async:" >&2
	cat /tmp/xexecutor_async_imports.$$ >&2
	rm -f /tmp/xexecutor_async_imports.$$
	exit 1
fi
rm -f /tmp/xexecutor_async_imports.$$

if [ -d "$target/examples" ] && find "$target/examples" -type f -print | xargs grep -n 'x\.async' >/tmp/xexecutor_async_examples.$$ 2>/dev/null; then
	echo "x.executor examples must not mention or use x.async:" >&2
	cat /tmp/xexecutor_async_examples.$$ >&2
	rm -f /tmp/xexecutor_async_examples.$$
	exit 1
fi
rm -f /tmp/xexecutor_async_examples.$$

if find "$target" -type f -name '*.v' | grep -Ei '(^|/)(async|.*bridge.*)\.v$' >/tmp/xexecutor_async_bridge_files.$$ 2>/dev/null; then
	echo "x.executor must not add async or bridge V files:" >&2
	cat /tmp/xexecutor_async_bridge_files.$$ >&2
	rm -f /tmp/xexecutor_async_bridge_files.$$
	exit 1
fi
rm -f /tmp/xexecutor_async_bridge_files.$$

echo "x.executor dependency check passed"
