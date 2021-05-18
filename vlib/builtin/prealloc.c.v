module builtin

const prealloc_block_size = 16777216

__global g_memory_block &VMemoryBlock
[heap]
struct VMemoryBlock {
mut:
	id        int
	cap       int
	start     &byte = 0
	previous  &VMemoryBlock = 0
	remaining int
	current   &byte = 0
	mallocs   int
}

[unsafe]
fn vmemory_block_new(prev &VMemoryBlock, at_least int) &VMemoryBlock {
	mut v := unsafe { &VMemoryBlock(C.calloc(1, sizeof(VMemoryBlock))) }
	if prev != 0 {
		v.id = prev.id + 1
	}
	v.previous = prev
	block_size := if at_least < prealloc_block_size { prealloc_block_size } else { at_least }
	v.start = unsafe { C.malloc(block_size) }
	v.cap = block_size
	v.remaining = block_size
	v.current = v.start
	return v
}

[unsafe]
fn vmemory_block_malloc(n int) &byte {
	unsafe {
		if g_memory_block.remaining < n {
			g_memory_block = vmemory_block_new(g_memory_block, n)
		}
		mut res := &byte(0)
		res = g_memory_block.current
		g_memory_block.remaining -= n
		g_memory_block.mallocs++
		g_memory_block.current += n
		return res
	}
}

/////////////////////////////////////////////////

[unsafe]
fn prealloc_vinit() {
	unsafe {
		g_memory_block = vmemory_block_new(voidptr(0), prealloc_block_size)
		$if !freestanding {
			C.atexit(prealloc_vcleanup)
		}
	}
}

[unsafe]
fn prealloc_vcleanup() {
	$if prealloc_stats ? {
		// NB: we do 2 loops here, because string interpolation
		// in the first loop may still use g_memory_block
		// The second loop however should *not* allocate at all.
		mut nr_mallocs := i64(0)
		mut mb := g_memory_block
		for mb != 0 {
			nr_mallocs += mb.mallocs
			eprintln('> freeing mb.id: ${mb.id:3} | cap: ${mb.cap:7} | rem: ${mb.remaining:7} | start: ${voidptr(mb.start)} | current: ${voidptr(mb.current)} | diff: ${u64(mb.current) - u64(mb.start):7} bytes | mallocs: $mb.mallocs')
			mb = mb.previous
		}
		eprintln('> nr_mallocs: $nr_mallocs')
	}
	unsafe {
		for g_memory_block != 0 {
			C.free(g_memory_block.start)
			g_memory_block = g_memory_block.previous
		}
	}
}

[unsafe]
fn prealloc_malloc(n int) &byte {
	return unsafe { vmemory_block_malloc(n) }
}

[unsafe]
fn prealloc_realloc(old_data &byte, old_size int, new_size int) &byte {
	new_ptr := unsafe { vmemory_block_malloc(new_size) }
	min_size := if old_size < new_size { old_size } else { new_size }
	unsafe { C.memcpy(new_ptr, old_data, min_size) }
	return new_ptr
}

[unsafe]
pub fn prealloc_calloc(n int) &byte {
	new_ptr := unsafe { vmemory_block_malloc(n) }
	unsafe { C.memset(new_ptr, 0, n) }
	return new_ptr
}
