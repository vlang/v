import arena

#flag -I @VEXEROOT/vlib/arena/testdata
#include "deny_chunk_malloc.h"

fn C.fail_arena_malloc_at(int)

struct BackingAttempt {
mut:
	failed bool
	ptr voidptr
}

fn allocate_or_report(fail_at int, mut out BackingAttempt) {
	defer {
		if msg := recover() {
			out.failed = (arena.allocation_failure() or { panic(msg) }) == .backing_allocator
		}
	}
	C.fail_arena_malloc_at(fail_at)
	out.ptr = unsafe { vcalloc(64) }
}

fn test_backing_and_registry_failure_leave_the_first_chunk_unpublished() {
	$if gcboehm ? { return }
	mut owner := arena.new(limits: arena.ChunkLimits{max_total_bytes: 1024})
	defer { owner.free() }
	for fail_at in [1, 2] {
		mut out := BackingAttempt{}
		owner.push()
		allocate_or_report(fail_at, mut out)
		owner.pop()
		assert out.failed
		assert owner.capacity() == 0
		assert (owner.chunk_usage() or { panic('missing budget') }) == arena.ChunkUsage{}
	}
	mut retry := BackingAttempt{}
	owner.push()
	allocate_or_report(0, mut retry)
	owner.pop()
	assert !retry.failed && !isnil(retry.ptr)
}
