module main

import arena

// Keep an arena active while the foreign host coordinates its worker threads.
@[export: 'v_arena_library_probe']
fn library_probe(value u64, checkpoint fn ()) i32 {
	if arena.active() { return 0 }
	mut storage := arena.new(chunk_size: 256)
	storage.push()
	address := unsafe { malloc(32) }
	for i in 0 .. 32 {
		unsafe { address[i] = u8(value + u64(i)) }
	}
	token := arena.lifetime_token(address) or { return 0 }
	checkpoint()
	mut valid := storage.is_current() && storage.owns(address) && token.matches(address)
	for i in 0 .. 32 { valid = valid && unsafe { address[i] } == u8(value + u64(i)) }
	unsafe { free(address) }
	storage.pop()
	valid = valid && token.matches(address) && !arena.active()
	storage.reset()
	valid = valid && !token.matches(address)
	storage.free()
	return if valid { 1 } else { 0 }
}
