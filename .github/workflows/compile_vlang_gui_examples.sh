#!/usr/bin/env bash

set -e

function show() {
	printf "\u001b[35m$1\u001b[0m\n"
}

rm -rf ~/.vmodules/gui/

export VJOBS=1
show "Clone https://github.com/vlang/gui"
v retry -- git clone --filter=blob:none --quiet https://github.com/vlang/gui ~/.vmodules/gui/
show "Use latest vlang/gui commit"
git -C ~/.vmodules/gui/ log -1 --oneline
if [[ "$(uname)" == 'Darwin' ]]; then
	export VFLAGS="${VFLAGS:-} -cc clang"
fi
show "Check module for syntax and semantic errors"
v -shared -check ~/.vmodules/gui
show "Execute Tests"
v test ~/.vmodules/gui/
show "Compile Examples"
v should-compile-all -no-parallel ~/.vmodules/gui/examples/
rm -rf ~/.vmodules/gui/
