module main

fn require(ok bool) { if !ok { panic('persistent prealloc check failed') } }

fn main() {
	require(vcalloc_heap(0) == unsafe { nil })
	scope := unsafe { prealloc_scope_begin() }
	heap := vcalloc_heap(4096)
	scoped := vcalloc(4096)
	require(unsafe { prealloc_scope_owns(scope, scoped) })
	require(!unsafe { prealloc_scope_owns(scope, heap) })
	for i in 0 .. 4096 {
		unsafe {
			require(heap[i] == 0)
			heap[i] = u8(i % 251)
		}
	}
	unsafe { prealloc_scope_end(scope) }
	for i in 0 .. 4096 {
		unsafe { require(heap[i] == u8(i % 251)) }
	}
	unsafe { vfree_heap(heap) }
	println('persistent prealloc ok')
}
