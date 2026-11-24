@[has_globals]
module main

import arena
import os

// The linker replaces these functions with stateful fault injectors. Do not
// let libc builtin assumptions remove or reorder the calls before link time.
#flag -fno-builtin-malloc -fno-builtin-calloc -fno-builtin-free
#flag -Wl,--wrap=malloc -Wl,--wrap=calloc -Wl,--wrap=free
#flag @DIR/hooks.c

fn C.allocation_failure_hooks_clean() int

fn C.__real_malloc(uint) voidptr
fn C.__real_calloc(uint, uint) voidptr
fn C.__real_free(voidptr)

struct FaultState {
mut:
	armed      bool
	fail_after int
	calls      int
	successes  int
	freed      int
	last       voidptr = unsafe { nil }
}

__global fault FaultState

@[export: '__wrap_malloc'; manualfree; no_profile; no_trace]
fn fault_malloc(size uint) voidptr {
	if fault.armed {
		fault.calls++
		if fault.calls > fault.fail_after { return unsafe { nil } }
		result := C.__real_malloc(size)
		if !isnil(result) {
			fault.last = result
			fault.successes++
		}
		return result
	}
	return C.__real_malloc(size)
}

@[export: '__wrap_calloc'; manualfree; no_profile; no_trace]
fn fault_calloc(count uint, size uint) voidptr {
	if fault.armed {
		fault.calls++
		if fault.calls > fault.fail_after { return unsafe { nil } }
		result := C.__real_calloc(count, size)
		if !isnil(result) {
			fault.last = result
			fault.successes++
		}
		return result
	}
	return C.__real_calloc(count, size)
}

@[export: '__wrap_free'; manualfree; no_profile; no_trace]
fn fault_free(pointer voidptr) {
	if fault.armed && pointer == fault.last && !isnil(pointer) { fault.freed++ }
	C.__real_free(pointer)
}

fn allocate(allocator arena.Allocator, size int) ?&u8 {
	return unsafe { allocator.try_calloc(size) }
}

fn require(ok bool, message string) {
	if !ok { panic(message) }
}

fn run_failure(allocator arena.Allocator, fail_after int, size int) FaultState {
	fault = FaultState{ armed: true, fail_after: fail_after }
	mut succeeded := false
	if _ := allocate(allocator, size) { succeeded = true }
	fault.armed = false
	require(!succeeded, 'faulted allocation succeeded')
	require(C.allocation_failure_hooks_clean() == 1, 'failed storage was reported as a live allocation')
	return fault
}

fn infallible_heap_allocation() bool {
	defer {
		if message := recover() {
			fault.armed = false
			require(message == 'memory allocation failure', 'unexpected heap panic')
		}
	}
	fault = FaultState{ armed: true }
	pointer := unsafe { arena.heap_allocator().calloc(8) }
	fault.armed = false
	unsafe { free(pointer) }
	return true
}

fn check_reusable(mut storage arena.Arena) {
	pointer := allocate(storage.allocator(), 8) or { panic('arena left unusable') }
	require(storage.owns(pointer), 'allocation fell back to heap')
	require(storage.capacity() == 128, 'failed growth changed the next chunk size')
	unsafe {
		for i in 0 .. 8 { require(pointer[i] == 0, 'storage not zeroed') }
	}
	storage.push()
	require(storage.is_current(), 'arena access state not restored')
	storage.pop()
}

fn main() {
	require(os.args.len == 2, 'expected one fault case')
	mut storage := arena.new(chunk_size: 128)
	match os.args[1] {
		'heap' {
			storage.push()
			state := run_failure(arena.heap_allocator(), 0, 8)
			require(state.calls == 1 && state.successes == 0, 'heap failure allocated additional storage')
			pointer := allocate(arena.heap_allocator(), 32) or { panic('heap recovery failed') }
			require(storage.is_current(), 'heap allocation changed the current arena')
			require(storage.used() == 0 && storage.capacity() == 0, 'heap allocation used the current arena')
			unsafe {
				for i in 0 .. 32 { require(pointer[i] == 0, 'heap storage not zeroed') }
			}
			storage.pop()
			unsafe { free(pointer) }
			require(!infallible_heap_allocation(), 'infallible heap allocation did not panic')
			require(C.allocation_failure_hooks_clean() == 1, 'failed infallible allocation was reported live')
		}
		'chunk' {
			state := run_failure(storage.allocator(), 0, 8)
			require(state.calls == 1 && state.successes == 0, 'chunk failure allocated additional storage')
			require(storage.used() == 0 && storage.capacity() == 0, 'failed chunk was published')
			check_reusable(mut storage)
		}
		'existing-chunk', 'existing-registry' {
			pointer := allocate(storage.allocator(), 64) or { panic('setup failed') }
			unsafe {
				for i in 0 .. 64 { pointer[i] = u8(i + 1) }
			}
			token := arena.lifetime_token(pointer) or { panic('missing existing token') }
			used, capacity := storage.used(), storage.capacity()
			mut owners := []arena.Arena{}
			if os.args[1] == 'existing-registry' {
				for _ in 0 .. 15 {
					owner := arena.new(chunk_size: 128)
					_ = allocate(owner.allocator(), 8) or { panic('registry setup failed') }
					owners << owner
				}
			}
			fail_after := if owners.len == 0 { 0 } else { 1 }
			state := run_failure(storage.allocator(), fail_after, 512)
			require(state.calls == fail_after + 1 && state.successes == fail_after
				&& state.freed == fail_after, 'existing-arena rollback leaked storage')
			require(storage.used() == used && storage.capacity() == capacity,
				'failed growth changed existing arena metadata')
			require(token.matches(pointer), 'failed growth invalidated existing lifetime')
			unsafe {
				for i in 0 .. 64 {
					require(pointer[i] == u8(i + 1), 'failed growth changed existing data')
				}
			}
			storage.push()
			resized := unsafe { realloc_data(pointer, 64, 32) }
			require(resized == pointer, 'failed growth changed the latest allocation')
			storage.pop()
			require(storage.used() == used - 32, 'latest allocation did not resize in place')
			next := allocate(storage.allocator(), 128) or { panic('existing arena not reusable') }
			require(storage.owns(next) && storage.capacity() == capacity + 256,
				'failed growth changed regular chunk policy')
			for mut owner in owners { owner.free() }
		}
		'registry-first', 'registry-grow' {
			mut owners := []arena.Arena{}
			mut pointers := []&u8{}
			mut tokens := []arena.LifetimeToken{}
			if os.args[1] == 'registry-grow' {
				for _ in 0 .. 16 {
					owner := arena.new(chunk_size: 128)
					pointer := allocate(owner.allocator(), 8) or { panic('registry setup failed') }
					owners << owner
					pointers << pointer
					tokens << arena.lifetime_token(pointer) or { panic('missing lifetime token') }
				}
			}
			state := run_failure(storage.allocator(), 1, 8)
			require(state.calls == 2 && state.successes == 1 && state.freed == 1,
				'registry failure did not release exactly its unpublished chunk')
			require(storage.used() == 0 && storage.capacity() == 0, 'registry failure published a chunk')
			for i, token in tokens {
				require(token.matches(pointers[i]), 'existing registry entry changed')
			}
			check_reusable(mut storage)
			for mut owner in owners { owner.free() }
		}
		else { panic('unknown fault case') }
	}
	storage.free()
	println('fallible arena fault rollback passed')
}
