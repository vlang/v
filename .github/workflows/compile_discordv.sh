#!/usr/bin/env bash

set -e

function show() {
	printf "\u001b[35m$1\u001b[0m\n"
}

## NOTE: this step is disabled in v_apps_and_modules_compile_ci.yml via `${{ false && ... }}`.
## Reason: discord.v uses x.json2.raw_decode which was deprecated-as-error in V on 2025-10-10.
## The upstream repo (vcv88/discord.v) has not been updated since Dec 2024.
## Re-enable both the yml step and this script once vcv88/discord.v#21 is resolved.
## Track: https://github.com/vlang/v/issues/26853

rm -rf discord/

show "Clone https://github.com/vcv88/discord.v"
v retry -- git clone --filter=blob:none --quiet https://github.com/vcv88/discord.v discord/
cd discord/
show "Checkout last known good commit"
git checkout ce9ff457fce92d5bb15df2974440cd8292457ee0
show "Execute Tests"
v test .
cd ..
rm -rf discord/
