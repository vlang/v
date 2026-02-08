@[has_globals]
module builtin

// v_memory_panic will be true, *only* when a call to malloc/realloc/vcalloc etc could not succeed.
// In that situation, functions that are registered with at_exit(), should be able to limit their
// activity accordingly, by checking this flag.
// The V compiler itself for example registers a function with at_exit(), for showing timers.
// Without a way to distinguish, that we are in a memory panic, that would just display a second panic,
// which would be less clear to the user.
__global v_memory_panic = false

@[noreturn]
fn _memory_panic(fname string, size isize) {
	v_memory_panic = true
	// Note: do not use string interpolation here at all, since string interpolation itself allocates
	eprint(fname)
	eprint('(')
	$if freestanding || vinix {
		eprint('size') // TODO: use something more informative here
	} $else {
		C.fprintf(C.stderr, c'%p', voidptr(size))
	}
	if size < 0 {
		eprint(' < 0')
	}
	eprintln(')')
	panic('memory allocation failure')
}

__global total_m = i64(0)
// malloc dynamically allocates a `n` bytes block of memory on the heap.
// malloc returns a `byteptr` pointing to the memory address of the allocated space.
// unlike the `calloc` family of functions - malloc will not zero the memory block.
@[unsafe]
pub fn malloc(n isize) &u8 {
	$if trace_malloc ? {
		total_m += n
		C.fprintf(C.stderr, c'_v_malloc %6d total %10d\n', n, total_m)
		// print_backtrace()
	}
	if n < 0 {
		_memory_panic(@FN, n)
	} else if n == 0 {
		return &u8(unsafe { nil })
	}
	mut res := &u8(unsafe { nil })
	$if prealloc {
		return unsafe { prealloc_malloc(n) }
	} $else $if gcboehm ? {
		unsafe {
			res = C.GC_MALLOC(n)
		}
	} $else $if freestanding {
		// todo: is this safe to call malloc there? We export __malloc as malloc and it uses dlmalloc behind the scenes
		// so theoretically it is safe
		res = unsafe { __malloc(usize(n)) }
	} $else {
		$if windows {
			// Warning! On windows, we always use _aligned_malloc to allocate memory.
			// This ensures that we can later free the memory with _aligned_free
			// without needing to track whether the memory was originally allocated
			// by malloc or _aligned_malloc.
			res = unsafe { C._aligned_malloc(n, 1) }
		} $else {
			res = unsafe { C.malloc(n) }
		}
	}
	if res == 0 {
		_memory_panic(@FN, n)
	}
	$if debug_malloc ? {
		// Fill in the memory with something != 0 i.e. `M`, so it is easier to spot
		// when the calling code wrongly relies on it being zeroed.
		unsafe { C.memset(res, 0x4D, n) }
	}
	return res
}

@[unsafe]
pub fn malloc_noscan(n isize) &u8 {
	$if trace_malloc ? {
		total_m += n
		C.fprintf(C.stderr, c'malloc_noscan %6d total %10d\n', n, total_m)
		// print_backtrace()
	}
	if n < 0 {
		_memory_panic(@FN, n)
	}
	mut res := &u8(unsafe { nil })
	$if native {
		res = unsafe { C.malloc(n) }
	} $else $if prealloc {
		return unsafe { prealloc_malloc(n) }
	} $else $if gcboehm ? {
		$if gcboehm_opt ? {
			unsafe {
				res = C.GC_MALLOC_ATOMIC(n)
			}
		} $else {
			unsafe {
				res = C.GC_MALLOC(n)
			}
		}
	} $else $if freestanding {
		res = unsafe { __malloc(usize(n)) }
	} $else {
		$if windows {
			// Warning! On windows, we always use _aligned_malloc to allocate memory.
			// This ensures that we can later free the memory with _aligned_free
			// without needing to track whether the memory was originally allocated
			// by malloc or _aligned_malloc.
			res = unsafe { C._aligned_malloc(n, 1) }
		} $else {
			res = unsafe { C.malloc(n) }
		}
	}
	if res == 0 {
		_memory_panic(@FN, n)
	}
	$if debug_malloc ? {
		// Fill in the memory with something != 0 i.e. `M`, so it is easier to spot
		// when the calling code wrongly relies on it being zeroed.
		unsafe { C.memset(res, 0x4D, n) }
	}
	return res
}

@[inline]
fn __at_least_one(how_many u64) u64 {
	// handle the case for allocating memory for empty structs, which have sizeof(EmptyStruct) == 0
	// in this case, just allocate a single byte, avoiding the panic for malloc(0)
	if how_many == 0 {
		return 1
	}
	return how_many
}

// malloc_uncollectable dynamically allocates a `n` bytes block of memory
// on the heap, which will NOT be garbage-collected (but its contents will).
@[unsafe]
pub fn malloc_uncollectable(n isize) &u8 {
	$if trace_malloc ? {
		total_m += n
		C.fprintf(C.stderr, c'malloc_uncollectable %6d total %10d\n', n, total_m)
		// print_backtrace()
	}
	if n < 0 {
		_memory_panic(@FN, n)
	}

	mut res := &u8(unsafe { nil })
	$if prealloc {
		return unsafe { prealloc_malloc(n) }
	} $else $if gcboehm ? {
		unsafe {
			res = C.GC_MALLOC_UNCOLLECTABLE(n)
		}
	} $else $if freestanding {
		res = unsafe { __malloc(usize(n)) }
	} $else {
		$if windows {
			// Warning! On windows, we always use _aligned_malloc to allocate memory.
			// This ensures that we can later free the memory with _aligned_free
			// without needing to track whether the memory was originally allocated
			// by malloc or _aligned_malloc.
			res = unsafe { C._aligned_malloc(n, 1) }
		} $else {
			res = unsafe { C.malloc(n) }
		}
	}
	if res == 0 {
		_memory_panic(@FN, n)
	}
	$if debug_malloc ? {
		// Fill in the memory with something != 0 i.e. `M`, so it is easier to spot
		// when the calling code wrongly relies on it being zeroed.
		unsafe { C.memset(res, 0x4D, n) }
	}
	return res
}

// v_realloc resizes the memory block `b` with `n` bytes.
// The `b byteptr` must be a pointer to an existing memory block
// previously allocated with `malloc` or `vcalloc`.
// Please, see also realloc_data, and use it instead if possible.
@[unsafe]
pub fn v_realloc(b &u8, n isize) &u8 {
	$if trace_realloc ? {
		C.fprintf(C.stderr, c'v_realloc %6d\n', n)
	}
	mut new_ptr := &u8(unsafe { nil })
	$if prealloc {
		unsafe {
			new_ptr = malloc(n)
			C.memcpy(new_ptr, b, n)
		}
		return new_ptr
	} $else $if gcboehm ? {
		new_ptr = unsafe { C.GC_REALLOC(b, n) }
	} $else {
		$if windows {
			// Warning! On windows, we always use _aligned_realloc to reallocate memory.
			// This ensures that we can later free the memory with _aligned_free
			// without needing to track whether the memory was originally allocated
			// by malloc or _aligned_malloc/_aligned_realloc.
			new_ptr = unsafe { C._aligned_realloc(b, n, 1) }
		} $else {
			new_ptr = unsafe { C.realloc(b, n) }
		}
	}
	if new_ptr == 0 {
		_memory_panic(@FN, n)
	}
	return new_ptr
}

// realloc_data resizes the memory block pointed by `old_data` to `new_size`
// bytes. `old_data` must be a pointer to an existing memory block, previously
// allocated with `malloc` or `vcalloc`, of size `old_data`.
// realloc_data returns a pointer to the new location of the block.
// Note: if you know the old data size, it is preferable to call `realloc_data`,
// instead of `v_realloc`, at least during development, because `realloc_data`
// can make debugging easier, when you compile your program with
// `-d debug_realloc`.
@[unsafe]
pub fn realloc_data(old_data &u8, old_size int, new_size int) &u8 {
	$if trace_realloc ? {
		C.fprintf(C.stderr, c'realloc_data old_size: %6d new_size: %6d\n', old_size, new_size)
	}
	$if prealloc {
		return unsafe { prealloc_realloc(old_data, old_size, new_size) }
	}
	$if debug_realloc ? {
		// Note: this is slower, but helps debugging memory problems.
		// The main idea is to always force reallocating:
		// 1) allocate a new memory block
		// 2) copy the old to the new
		// 3) fill the old with 0x57 (`W`)
		// 4) free the old block
		// => if there is still a pointer to the old block somewhere
		//    it will point to memory that is now filled with 0x57.
		unsafe {
			new_ptr := malloc(new_size)
			min_size := if old_size < new_size { old_size } else { new_size }
			C.memcpy(new_ptr, old_data, min_size)
			C.memset(old_data, 0x57, old_size)
			free(old_data)
			return new_ptr
		}
	}
	mut nptr := &u8(unsafe { nil })
	$if gcboehm ? {
		nptr = unsafe { C.GC_REALLOC(old_data, new_size) }
	} $else {
		$if windows {
			// Warning! On windows, we always use _aligned_realloc to reallocate memory.
			// This ensures that we can later free the memory with _aligned_free
			// without needing to track whether the memory was originally allocated
			// by malloc or _aligned_malloc/_aligned_realloc.
			nptr = unsafe { C._aligned_realloc(old_data, new_size, 1) }
		} $else {
			nptr = unsafe { C.realloc(old_data, new_size) }
		}
	}
	if nptr == 0 {
		_memory_panic(@FN, isize(new_size))
	}
	return nptr
}

// vcalloc dynamically allocates a zeroed `n` bytes block of memory on the heap.
// vcalloc returns a `byteptr` pointing to the memory address of the allocated space.
// vcalloc checks for negative values given in `n`.
pub fn vcalloc(n isize) &u8 {
	$if trace_vcalloc ? {
		total_m += n
		C.fprintf(C.stderr, c'vcalloc %6d total %10d\n', n, total_m)
	}
	if n < 0 {
		_memory_panic(@FN, n)
	} else if n == 0 {
		return &u8(unsafe { nil })
	}
	$if prealloc {
		return unsafe { prealloc_calloc(n) }
	} $else $if native {
		return unsafe { C.calloc(1, n) }
	} $else $if gcboehm ? {
		return unsafe { &u8(C.GC_MALLOC(n)) }
	} $else {
		$if windows {
			// Warning! On windows, we always use _aligned_malloc to allocate memory.
			// This ensures that we can later free the memory with _aligned_free
			// without needing to track whether the memory was originally allocated
			// by malloc or _aligned_malloc/_aligned_realloc/_aligned_recalloc.
			ptr := unsafe { C._aligned_malloc(n, 1) }
			if ptr != &u8(unsafe { nil }) {
				unsafe { C.memset(ptr, 0, n) }
			}
			return ptr
		} $else {
			return unsafe { C.calloc(1, n) }
		}
	}
	return &u8(unsafe { nil }) // not reached, TODO: remove when V's checker is improved
}

// special versions of the above that allocate memory which is not scanned
// for pointers (but is collected) when the Boehm garbage collection is used
pub fn vcalloc_noscan(n isize) &u8 {
	$if trace_vcalloc ? {
		total_m += n
		C.fprintf(C.stderr, c'vcalloc_noscan %6d total %10d\n', n, total_m)
	}
	$if prealloc {
		return unsafe { prealloc_calloc(n) }
	} $else $if gcboehm ? {
		if n < 0 {
			_memory_panic(@FN, n)
		}
		$if gcboehm_opt ? {
			res := unsafe { C.GC_MALLOC_ATOMIC(n) }
			unsafe { C.memset(res, 0, n) }
			return &u8(res)
		} $else {
			res := unsafe { C.GC_MALLOC(n) }
			return &u8(res)
		}
	} $else {
		return unsafe { vcalloc(n) }
	}
	return &u8(unsafe { nil }) // not reached, TODO: remove when V's checker is improved
}

// free allows for manually freeing memory allocated at the address `ptr`.
@[unsafe]
pub fn free(ptr voidptr) {
	$if trace_free ? {
		C.fprintf(C.stderr, c'free ptr: %p\n', ptr)
	}
	$if builtin_free_nop ? {
		return
	}
	if ptr == unsafe { 0 } {
		$if trace_free_nulls ? {
			C.fprintf(C.stderr, c'free null ptr\n', ptr)
		}
		$if trace_free_nulls_break ? {
			break_if_debugger_attached()
		}
		return
	}
	$if prealloc {
		return
	} $else $if gcboehm ? {
		// It is generally better to leave it to Boehm's gc to free things.
		// Calling C.GC_FREE(ptr) was tried initially, but does not work
		// well with programs that do manual management themselves.
		//
		// The exception is doing leak detection for manual memory management:
		$if gcboehm_leak ? {
			unsafe { C.GC_FREE(ptr) }
		}
	} $else {
		$if windows {
			// Warning! On windows, we always use _aligned_free to free memory.
			unsafe { C._aligned_free(ptr) }
		} $else {
			C.free(ptr)
		}
	}
}

// memdup dynamically allocates a `sz` bytes block of memory on the heap
// memdup then copies the contents of `src` into the allocated space and
// returns a pointer to the newly allocated space.
@[unsafe]
pub fn memdup(src voidptr, sz isize) voidptr {
	$if trace_memdup ? {
		C.fprintf(C.stderr, c'memdup size: %10d\n', sz)
	}
	if sz == 0 {
		return vcalloc(1)
	}
	unsafe {
		mem := malloc(sz)
		return C.memcpy(mem, src, sz)
	}
}

@[unsafe]
pub fn memdup_noscan(src voidptr, sz isize) voidptr {
	$if trace_memdup ? {
		C.fprintf(C.stderr, c'memdup_noscan size: %10d\n', sz)
	}
	if sz == 0 {
		return vcalloc_noscan(1)
	}
	unsafe {
		mem := malloc_noscan(sz)
		return C.memcpy(mem, src, sz)
	}
}

// memdup_uncollectable dynamically allocates a `sz` bytes block of memory
// on the heap, which will NOT be garbage-collected (but its contents will).
// memdup_uncollectable then copies the contents of `src` into the allocated
// space and returns a pointer to the newly allocated space.
@[unsafe]
pub fn memdup_uncollectable(src voidptr, sz isize) voidptr {
	$if trace_memdup ? {
		C.fprintf(C.stderr, c'memdup_uncollectable size: %10d\n', sz)
	}
	if sz == 0 {
		return vcalloc(1)
	}
	unsafe {
		mem := malloc_uncollectable(sz)
		return C.memcpy(mem, src, sz)
	}
}

// memdup_align dynamically allocates a memory block of `sz` bytes on the heap,
// copies the contents from `src` into the allocated space, and returns a pointer
// to the newly allocated memory. The returned pointer is aligned to the specified `align` boundary.
//   - `align` must be a power of two and at least 1
//   - `sz` must be non-negative
//   - The memory regions should not overlap
@[unsafe]
pub fn memdup_align(src voidptr, sz isize, align isize) voidptr {
	$if trace_memdup ? {
		C.fprintf(C.stderr, c'memdup_align size: %10d align: %10d\n', sz, align)
	}
	if sz == 0 {
		return vcalloc(1)
	}
	n := sz
	$if trace_malloc ? {
		total_m += n
		C.fprintf(C.stderr, c'_v_memdup_align %6d total %10d\n', n, total_m)
		// print_backtrace()
	}
	if n < 0 {
		_memory_panic(@FN, n)
	}
	mut res := &u8(unsafe { nil })
	$if prealloc {
		res = prealloc_malloc_align(n, align)
	} $else $if gcboehm ? {
		unsafe {
			res = C.GC_memalign(align, n)
		}
	} $else $if freestanding {
		// todo: is this safe to call malloc there? We export __malloc as malloc and it uses dlmalloc behind the scenes
		// so theoretically it is safe
		panic('memdup_align is not implemented with -freestanding')
		res = unsafe { __malloc(usize(n)) }
	} $else {
		$if windows {
			// Warning! On windows, we always use _aligned_malloc to allocate memory.
			// This ensures that we can later free the memory with _aligned_free
			// without needing to track whether the memory was originally allocated
			// by malloc or _aligned_malloc.
			res = unsafe { C._aligned_malloc(n, align) }
		} $else {
			res = unsafe { C.aligned_alloc(align, n) }
		}
	}
	if res == 0 {
		_memory_panic(@FN, n)
	}
	$if debug_malloc ? {
		// Fill in the memory with something != 0 i.e. `M`, so it is easier to spot
		// when the calling code wrongly relies on it being zeroed.
		unsafe { C.memset(res, 0x4D, n) }
	}
	return C.memcpy(res, src, sz)
}

// GCHeapUsage contains stats about the current heap usage of your program.
pub struct GCHeapUsage {
pub:
	heap_size      usize
	free_bytes     usize
	total_bytes    usize
	unmapped_bytes usize
	bytes_since_gc usize
}

// gc_heap_usage returns the info about heap usage.
pub fn gc_heap_usage() GCHeapUsage {
	$if gcboehm ? {
		mut res := GCHeapUsage{}
		C.GC_get_heap_usage_safe(&res.heap_size, &res.free_bytes, &res.unmapped_bytes,
			&res.bytes_since_gc, &res.total_bytes)
		return res
	} $else {
		return GCHeapUsage{}
	}
}

// gc_memory_use returns the total memory use in bytes by all allocated blocks.
pub fn gc_memory_use() usize {
	$if gcboehm ? {
		return C.GC_get_memory_use()
	} $else {
		return 0
	}
}
