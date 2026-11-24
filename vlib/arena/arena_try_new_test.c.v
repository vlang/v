import arena

#flag -I @VEXEROOT/vlib/arena/testdata
#include "deny_calloc.h"

fn C.arena_deny_calloc_once()

fn test_failed_arena_construction_preserves_the_active_allocator_and_allows_retry() {
	mut parent := arena.new()
	defer { parent.free() }
	parent.push()
	C.arena_deny_calloc_once()
	if mut unexpected := arena.try_new() {
		unexpected.free()
		assert false, 'try_new succeeded after control block allocation failed'
	}
	assert parent.is_current()
	mut child := arena.try_new(chunk_size: 128) or { panic('cannot create child arena') }
	assert child.capacity() == 0
	parent.pop()
	parent.reset()
	child.push()
	value := unsafe { vcalloc(25) }
	assert child.owns(value)
	child.pop()
	child.free()
}
