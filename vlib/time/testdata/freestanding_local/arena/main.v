@[has_globals]
module main

import time
import arena

__global calls int

fn require(ok bool) { if !ok { panic('arena local time check failed') } }

fn loader(name string) ![]u8 {
	if name != 'Custom/Persistent' { return error('not this loader') }
	calls++
	return $embed_file('../test.tzif').to_bytes()
}

fn main() {
	mut storage := arena.new(chunk_size: 4096)
	storage.push()
	time.register_zoneinfo_loader(loader)
	local := time.unix_nanosecond(1_730_615_400, 123_000_000).local()
	require(local.hour == 1 && local.unix() == 1_730_615_400 && storage.is_current())
	storage.pop()
	storage.free()
	require(local.unix() == 1_730_615_400 && local.local_to_utc().hour == 6)
	require(local.to_json() == '\"2024-11-03T06:30:00.123Z\"')
	// Cold scope-owned Location must not be cached. The registry node survives
	// independently and the next lookup safely publishes stable heap data.
	again := time.unix(1_730_615_400).local()
	require(again.hour == 1 && calls == 2)
	mut fresh := arena.new(chunk_size: 4096)
	fresh.push()
	before := fresh.used()
	for _ in 0 .. 1000 { require(time.unix(1_730_615_400).local().unix() == 1_730_615_400) }
	require(fresh.used() == before && fresh.is_current() && calls == 2)
	fresh.pop()
	fresh.free()
	println('arena local time ok')
}
