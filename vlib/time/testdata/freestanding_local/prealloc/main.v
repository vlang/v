@[has_globals]
module main

import time

__global calls int

fn require(ok bool) { if !ok { panic('prealloc local time check failed') } }

fn loader(name string) ![]u8 {
	if name != 'Custom/Persistent' { return error('not this loader') }
	calls++
	return $embed_file('../test.tzif').to_bytes()
}

fn main() {
	scope := unsafe { prealloc_scope_begin() }
	time.register_zoneinfo_loader(loader)
	local := time.unix_nanosecond(1_730_615_400, 123_000_000).local()
	require(local.hour == 1 && local.unix() == 1_730_615_400)
	unsafe { prealloc_scope_end(scope) }
	require(local.unix() == 1_730_615_400 && local.to_json() == '\"2024-11-03T06:30:00.123Z\"')
	again := time.unix(1_730_615_400).local()
	require(again.hour == 1 && calls == 2)
	println('prealloc local time ok')
}
