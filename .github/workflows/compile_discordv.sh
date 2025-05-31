#!/usr/bin/env bash

set -e

function show() {
	printf "\u001b[35m$1\u001b[0m\n"
}

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
