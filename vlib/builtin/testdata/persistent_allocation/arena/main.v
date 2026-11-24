module main

import arena

fn require(ok bool) { if !ok { panic('persistent arena check failed') } }

fn main() {
	require(vcalloc_heap(0) == unsafe { nil })
	mut outer := arena.new(chunk_size: 1024)
	mut inner := arena.new(chunk_size: 1024)
	outer.push()
	inner.push()
	heap := vcalloc_heap(4096)
	scoped := vcalloc(4096)
	require(inner.is_current() && inner.owns(scoped))
	require(!inner.owns(heap) && !outer.owns(heap))
	for i in 0 .. 4096 {
		unsafe {
			require(heap[i] == 0)
			heap[i] = u8(i % 251)
		}
	}
	inner.pop()
	require(outer.is_current())
	outer.pop()
	inner.free()
	outer.free()
	for i in 0 .. 4096 {
		unsafe { require(heap[i] == u8(i % 251)) }
	}
	unsafe { vfree_heap(heap) }
	println('persistent arena ok')
}
