module main

import arena
import strings

@[aligned: 64]
struct AlignedBlock {
mut:
	values [17]u64
}

__global arena_workers_ready u32
__global arena_workers_go u32

fn require_arena(condition bool) {
	if !condition { exit(93) }
}

fn worker(value u64) {
	require_arena(!arena.active())
	mut storage := arena.new(chunk_size: 256)
	storage.push()
	address := unsafe { malloc(32) }
	for i in 0 .. 32 {
		unsafe { address[i] = u8(value + u64(i)) }
	}
	token := arena.lifetime_token(address) or { exit(94) }
	heap := arena.heap_allocator()
	retained := unsafe { heap.calloc(16) }
	require_arena(!storage.owns(retained))
	unsafe { retained[0] = u8(value) }
	for {
		old := vlibc_load_u32(&arena_workers_ready)
		if vlibc_cas_u32(&arena_workers_ready, old, old + 1) { break }
	}
	for vlibc_load_u32(&arena_workers_go) == 0 {
	}
	require_arena(storage.is_current())
	for i in 0 .. 32 { require_arena(unsafe { address[i] } == u8(value + u64(i))) }
	used := storage.used()
	unsafe { free(address) }
	require_arena(storage.used() == used)
	storage.pop()
	require_arena(token.matches(address))
	storage.reset()
	require_arena(!token.matches(address))
	storage.free()
	require_arena(!arena.active())
	require_arena(unsafe { retained[0] } == u8(value))
	unsafe { free(retained) }
}

fn main() {
	mut outer := arena.new(chunk_size: 256)
	mut inner := arena.new(chunk_size: 256)
	outer_allocator := outer.allocator()
	heap_allocator := arena.heap_allocator()
	outer.push()
	address := unsafe { malloc(32) }
	for i in 0 .. 32 {
		unsafe { address[i] = u8(i) }
	}
	token := arena.lifetime_token(address) or { exit(94) }
	inner.push()
	inner_value := unsafe { malloc(32) }
	inner_token := arena.lifetime_token(inner_value) or { exit(94) }
	mut buffer := strings.new_buffer(1)
	buffer.write_string('inner text')
	transferred := buffer.take_string()
	require_arena(inner.owns(transferred.str))
	mut source := [257]u8{}
	for i in 0 .. source.len { source[i] = u8(i) }
	for alignment in [16, 32, 64, 4096] {
		copied := unsafe { &u8(memdup_align(&source[0], source.len, alignment)) }
		require_arena(inner.owns(copied) && uint(copied) % uint(alignment) == 0)
		for i in 0 .. source.len { require_arena(unsafe { copied[i] } == source[i]) }
		unsafe { free(copied) }
	}
	mut aligned := &AlignedBlock{}
	require_arena(inner.owns(aligned) && uint(aligned) % 64 == 0)
	aligned.values[16] = 731
	require_arena(aligned.values[16] == 731)
	inner_used := inner.used()
	parent_value := unsafe { outer_allocator.calloc(64) }
	require_arena(outer.owns(parent_value))
	require_arena(inner.used() == inner_used)
	require_arena(inner.is_current())
	for i in 0 .. 64 { require_arena(unsafe { parent_value[i] } == 0) }
	retained := unsafe { heap_allocator.calloc(64) }
	require_arena(!inner.owns(retained) && !outer.owns(retained))
	unsafe { retained[0] = 123 }
	// An individual free is a no-op even from an inner scope or after pop.
	unsafe { free(address) }
	for i in 0 .. 32 { require_arena(unsafe { address[i] } == u8(i)) }
	inner.pop()
	unsafe { free(inner_value) }
	require_arena(inner_token.matches(inner_value))
	require_arena(transferred == 'inner text')
	outer.pop()
	unsafe { free(address) }
	require_arena(token.matches(address))
	// Allocate into an inactive target while an unrelated arena is current.
	inner.push()
	sibling_used := inner.used()
	inactive_value := unsafe { outer_allocator.calloc(16) }
	require_arena(outer.owns(inactive_value))
	require_arena(inner.is_current() && inner.used() == sibling_used)
	for i in 0 .. 16 { require_arena(unsafe { inactive_value[i] } == 0) }
	inner.pop()
	inner.reset()
	require_arena(!inner_token.matches(inner_value))
	inner.free()
	// Spawn while the main thread has an active arena. Fresh threads must
	// start with their own empty allocator stack, not inherit its current top.
	outer.push()
	mut first, first_error := vlibc_thread_spawn(worker, 1)
	mut second, second_error := vlibc_thread_spawn(worker, 2)
	require_arena(first_error == .enoerror && second_error == .enoerror)
	for vlibc_load_u32(&arena_workers_ready) != 2 {
	}
	require_arena(outer.is_current())
	for i in 0 .. 32 { require_arena(unsafe { address[i] } == u8(i)) }
	vlibc_store_u32(&arena_workers_go, 1)
	first_join := vlibc_thread_join(mut first)
	second_join := vlibc_thread_join(mut second)
	require_arena(first_join == .enoerror && second_join == .enoerror)
	require_arena(outer.is_current())
	outer.pop()
	outer.reset()
	require_arena(!token.matches(address))
	outer.free()
	require_arena(unsafe { retained[0] } == 123)
	unsafe { free(retained) }
	println('freestanding arena passed')
}
