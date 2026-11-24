module main

import time

fn loader(name string) ![]u8 {
	if name != 'Custom/Persistent' { return error('not this loader') }
	return $embed_file('../test.tzif').to_bytes()
}

fn register_in_scope() {
	scope := unsafe { prealloc_scope_begin() }
	time.register_zoneinfo_loader(loader)
	unsafe { prealloc_scope_end(scope) }
}

fn main() {
	register_in_scope()
	loc := time.load_location('Custom/Persistent') or { panic(err) }
	if (loc.offset_at(1_730_615_400) or { panic(err) }) != -18000 {
		panic('scoped registry lost loader')
	}
	println('scoped registry autofree ok')
}
