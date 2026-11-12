#!/usr/bin/env bash

set -e

phase="${1-all}"
if [[ $# -gt 1 ]] || [[ "$phase" != all && "$phase" != tests && "$phase" != examples ]]; then
	printf 'Usage: %s [all|tests|examples]\n' "$0" >&2
	exit 2
fi

function show() {
	printf "\u001b[35m$1\u001b[0m\n"
}

rm -rf ~/.vmodules/gui/ ~/.vmodules/vglyph/

export VJOBS=1
show "Clone https://github.com/vlang/vglyph"
v retry -- git clone --filter=blob:none --quiet https://github.com/vlang/vglyph ~/.vmodules/vglyph/
show "Use latest vlang/vglyph commit"
git -C ~/.vmodules/vglyph/ log -1 --oneline
show "Clone https://github.com/vlang/gui"
v retry -- git clone --filter=blob:none --quiet https://github.com/vlang/gui ~/.vmodules/gui/
show "Use latest vlang/gui commit"
git -C ~/.vmodules/gui/ log -1 --oneline
if [[ "$(uname)" == 'Darwin' ]]; then
	export VFLAGS="${VFLAGS:-} -cc clang"
fi
if [[ "$phase" != examples ]]; then
	show "Check module for syntax and semantic errors"
	v -shared -check ~/.vmodules/gui
	show "Execute Tests"
	v test ~/.vmodules/gui/
fi
if [[ "$phase" != tests ]]; then
	show "Compile Examples"
	v should-compile-all -no-parallel ~/.vmodules/gui/examples/
fi
rm -rf ~/.vmodules/gui/ ~/.vmodules/vglyph/
