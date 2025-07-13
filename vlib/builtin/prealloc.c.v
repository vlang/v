@[has_globals]
module builtin

// With -prealloc, V calls libc's malloc to get chunks, each at least 16MB
// in size, as needed. Once a chunk is available, all malloc() calls within
// V code, that can fit inside the chunk, will use it instead, each bumping a
// pointer, till the chunk is filled. Once a chunk is filled, a new chunk will
// be allocated by calling libc's malloc, and the process continues.
// Each new chunk has a pointer to the old one, and at the end of the program,
// the entire linked list of chunks is freed.
// The goal of all this is to amortize the cost of calling libc's malloc,
// trading higher memory usage for a compiler (or any single threaded batch
// mode program), for a ~8-10% speed increase.
// Note: `-prealloc` is NOT safe to be used for multithreaded programs!

// size of the preallocated chunk
const prealloc_block_size = 16 * 1024 * 1024

__global g_memory_block &VMemoryBlock
@[heap]
struct VMemoryBlock {
mut:
	current  &u8           = 0 // 8
	stop     &u8           = 0 // 8
	start    &u8           = 0 // 8
	previous &VMemoryBlock = 0 // 8
	next     &VMemoryBlock = 0 // 8
	id       int // 4
	mallocs  int // 4
}

fn vmemory_abort_on_nil(p voidptr, bytes isize) {
	if unsafe { p == 0 } {
		C.fprintf(C.stderr, c'could not allocate %td bytes\n', bytes)
		exit(1)
	}
}

@[unsafe]
fn vmemory_block_new(prev &VMemoryBlock, at_least isize, align isize) &VMemoryBlock {
	vmem_block_size := sizeof(VMemoryBlock)
	mut v := unsafe { &VMemoryBlock(C.calloc(1, vmem_block_size)) }
	vmemory_abort_on_nil(v, vmem_block_size)
	if unsafe { prev != 0 } {
		v.id = prev.id + 1
	}

	v.previous = prev
	if unsafe { prev != 0 } {
		prev.next = v
	}
	base_block_size := if at_least < isize(prealloc_block_size) {
		isize(prealloc_block_size)
	} else {
		at_least
	}
	block_size := if align > 0 {
		if base_block_size % align == 0 {
			base_block_size
		} else {
			base_block_size + align - (base_block_size % align)
		}
	} else {
		base_block_size
	}
	$if prealloc_trace_malloc ? {
		C.fprintf(C.stderr, c'vmemory_block_new id: %d, block_size: %lld, at_least: %lld, align: %lld\n',
			v.id, block_size, at_least, align)
	}

	fixed_align := if align <= 1 { 1 } else { align }
	$if windows {
		v.start = unsafe { C._aligned_malloc(block_size, fixed_align) }
	} $else {
		if fixed_align == 1 {
			v.start = unsafe { C.malloc(block_size) }
		} else {
			v.start = unsafe { C.aligned_alloc(fixed_align, block_size) }
		}
	}
	vmemory_abort_on_nil(v.start, block_size)
	$if prealloc_memset ? {
		unsafe { C.memset(v.start, int($d('prealloc_memset_value', 0)), block_size) }
	}
	v.stop = unsafe { &u8(i64(v.start) + block_size) }
	v.current = v.start
	return v
}

@[unsafe]
fn vmemory_block_malloc(n isize, align isize) &u8 {
	$if prealloc_trace_malloc ? {
		C.fprintf(C.stderr, c'vmemory_block_malloc g_memory_block.id: %d, n: %lld align: %d\n',
			g_memory_block.id, n, align)
	}
	unsafe {
		remaining := i64(g_memory_block.stop) - i64(g_memory_block.current)
		if _unlikely_(remaining < n) {
			g_memory_block = vmemory_block_new(g_memory_block, n, align)
		}
		res := &u8(g_memory_block.current)
		g_memory_block.current += n
		$if prealloc_stats ? {
			g_memory_block.mallocs++
		}
		return res
	}
}

/////////////////////////////////////////////////

@[unsafe]
fn prealloc_vinit() {
	$if prealloc_trace_vinit ? {
		C.fprintf(C.stderr, c'prealloc_vinit started\n')
	}
	unsafe {
		g_memory_block = vmemory_block_new(nil, isize(prealloc_block_size), 0)
		at_exit(prealloc_vcleanup) or {}
	}
}

@[unsafe]
fn prealloc_vcleanup() {
	$if prealloc_trace_vcleanup ? {
		C.fprintf(C.stderr, c'prealloc_vcleanup started\n')
	}
	$if prealloc_stats ? {
		// Note: we do 2 loops here, because string interpolation
		// in the first loop may still use g_memory_block
		// The second loop however should *not* allocate at all.
		mut nr_mallocs := i64(0)
		mut total_used := i64(0)
		mut mb := g_memory_block
		for unsafe { mb != 0 } {
			nr_mallocs += mb.mallocs
			used := i64(mb.current) - i64(mb.start)
			total_used += used
			remaining := i64(mb.stop) - i64(mb.current)
			size := i64(mb.stop) - i64(mb.start)
			C.fprintf(C.stderr, c'> freeing mb: %16p, mb.id: %3d | size: %10lld | rem: %10lld | start: %16p | current: %16p | used: %10lld bytes | mallocs: %6d\n',
				mb, mb.id, size, remaining, mb.start, mb.current, used, mb.mallocs)
			mb = mb.previous
		}
		C.fprintf(C.stderr, c'> nr_mallocs: %lld, total_used: %lld bytes\n', nr_mallocs,
			total_used)
	}
	$if prealloc_dump ? {
		C.fprintf(C.stderr, c'prealloc_vcleanup dumping memory contents ...\n')
		mut start := g_memory_block
		unsafe {
			for start.previous != 0 {
				start = start.previous
			}
			C.fprintf(C.stderr, c'prealloc_vcleanup      start: %p\n', start)
			C.fprintf(C.stderr, c'prealloc_vcleanup   start.id: %d\n', start.id)
			C.fprintf(C.stderr, c'prealloc_vcleanup start.next: %p\n', start.next)

			mut total_used := u64(0)
			path := $d('memdumpfile', 'memdump.bin')
			C.fprintf(C.stderr, c'prealloc_vcleanup dumping process memory to path: %s\n',
				path.str)
			stream := C.fopen(path.str, c'wb')
			mut mb := start
			for {
				used := u64(mb.current) - u64(mb.start)
				total_used += used
				C.fprintf(C.stderr, c'prealloc_vcleanup dumping mb: %p, mb.id: %d, used: %10lld bytes\n',
					mb, mb.id, used)

				mut ptr := mb.start
				mut remaining_bytes := isize(used)
				mut x := isize(0)
				for remaining_bytes > 0 {
					x = isize(C.fwrite(ptr, 1, remaining_bytes, stream))
					ptr += x
					remaining_bytes -= x
				}

				if mb.next == 0 {
					break
				}
				mb = mb.next
			}
			C.fclose(stream)
			C.fprintf(C.stderr, c'prealloc_vcleanup total dump size in bytes: %lld\n',
				total_used)
		}
	}
	unsafe {
		for g_memory_block != 0 {
			$if windows {
				// Warning! On windows, we always use _aligned_free to free memory.
				C._aligned_free(g_memory_block.start)
			} $else {
				C.free(g_memory_block.start)
			}
			tmp := g_memory_block
			g_memory_block = g_memory_block.previous
			// free the link node
			C.free(tmp)
		}
	}
}

@[unsafe]
fn prealloc_malloc(n isize) &u8 {
	return unsafe { vmemory_block_malloc(n, 0) }
}

@[unsafe]
fn prealloc_realloc(old_data &u8, old_size isize, new_size isize) &u8 {
	new_ptr := unsafe { vmemory_block_malloc(new_size, 0) }
	min_size := if old_size < new_size { old_size } else { new_size }
	unsafe { C.memcpy(new_ptr, old_data, min_size) }
	return new_ptr
}

@[unsafe]
fn prealloc_calloc(n isize) &u8 {
	new_ptr := unsafe { vmemory_block_malloc(n, 0) }
	unsafe { C.memset(new_ptr, 0, n) }
	return new_ptr
}

@[unsafe]
fn prealloc_malloc_align(n isize, align isize) &u8 {
	return unsafe { vmemory_block_malloc(n, align) }
}
