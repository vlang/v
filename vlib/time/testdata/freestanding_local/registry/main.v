@[has_globals]
module main

import time

__global calls int

fn require(ok bool) { if !ok { panic('timezone registry check failed') } }

fn loader(name string) ![]u8 {
	if name != 'Custom/Persistent' { return error('not this loader') }
	calls++
	return $embed_file('../test.tzif').to_bytes()
}

fn missed(name string) ![]u8 { return error(name) }

fn main() {
	// An unavailable configured zone falls back to UTC until registration.
	require(time.unix(1_730_615_400).local().hour == 6)
	time.register_zoneinfo_loader(loader)
	require(time.unix(1_730_615_400).local().hour == 1 && calls == 1)
	require(time.unix(1_730_615_400).local().hour == 1 && calls == 1)
	explicit := time.load_location('Local') or { panic(err) }
	require((explicit.offset_at(1_730_615_400) or { panic(err) }) == -18000 && calls == 2)
	time.register_zoneinfo_loader(missed)
	require(time.unix(1_730_615_400).local().hour == 1 && calls == 3)
	println('timezone registry ok')
}
