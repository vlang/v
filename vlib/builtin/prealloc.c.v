@[has_globals]
module builtin

#insert "@VEXEROOT/vlib/builtin/prealloc_atomics.h"

fn C.v_prealloc_atomic_add_i32(ptr &int, delta int) int
fn C.v_prealloc_atomic_load_i32(ptr &int) int
fn C.v_prealloc_atomic_store_i32(ptr &int, val int) int
fn C.v_prealloc_atomic_cas_i32(ptr &int, expected int, desired int) int

// With -prealloc, V calls libc's malloc to get chunks, each at least 16MB
// in size, as needed. Once a chunk is available, all malloc() calls within
// V code, that can fit inside the chunk, will use it instead, each bumping a
// pointer, till the chunk is filled. Once a chunk is filled, a new chunk will
// be allocated by calling libc's malloc, and the process continues.
// Each new chunk has a pointer to the old one. The base arena is thread-local;
// scoped arenas can be freed earlier with `prealloc_scope_end` or transferred
// and freed later with `prealloc_scope_free_after`.
// The goal of all this is to amortize the cost of calling libc's malloc,
// trading higher memory usage for a compiler (or any single threaded batch
// mode program), for a ~8-10% speed increase.

// size of the process/thread preallocated chunk
const prealloc_block_size = 16 * 1024 * 1024

// size of the first chunk for a scoped prealloc arena. Request-scoped arenas
// should not force a 16MB libc allocation for every request.
const prealloc_scope_block_size = 256 * 1024

// `malloc` has to return memory suitably aligned for any V value. Keep the
// default at the common max alignment used by libc malloc on current targets.
const prealloc_default_align = sizeof(voidptr) * 2

__global g_memory_block &VMemoryBlock
@[heap]
struct VMemoryBlock {
mut:
	current        &u8             = 0 // 8
	stop           &u8             = 0 // 8
	start          &u8             = 0 // 8
	previous       &VMemoryBlock   = 0 // 8
	next           &VMemoryBlock   = 0 // 8
	scope          &VPreallocScope = 0
	min_block_size isize
	is_scope       bool
	id             int // 4
	mallocs        int // 4
}

@[heap]
struct VPreallocScope {
mut:
	previous       &VMemoryBlock = 0
	first          &VMemoryBlock = 0
	refs           int
	free_requested int
	abandoned      int
	finalized      int
}

fn vmemory_abort_on_nil(p voidptr, bytes isize) {
	if unsafe { p == 0 } {
		C.fprintf(C.stderr, c'could not allocate %td bytes\n', bytes)
		exit(1)
	}
}

fn vmemory_effective_align(align isize) isize {
	default_align := isize(prealloc_default_align)
	if align > default_align {
		return align
	}
	return default_align
}

@[unsafe]
fn vmemory_align_up(ptr &u8, align isize) &u8 {
	if align <= 1 {
		return ptr
	}
	addr := u64(ptr)
	alignment := u64(align)
	offset := addr % alignment
	if offset == 0 {
		return ptr
	}
	return unsafe { &u8(i64(addr + alignment - offset)) }
}

fn vmemory_block_used(mb &VMemoryBlock) i64 {
	return unsafe { i64(mb.current) - i64(mb.start) }
}

fn vmemory_block_size(mb &VMemoryBlock) i64 {
	return unsafe { i64(mb.stop) - i64(mb.start) }
}

@[unsafe]
fn prealloc_trace_scope(action &char, scope &VPreallocScope) {
	$if trace_prealloc ? {
		if scope == unsafe { nil } {
			C.fprintf(C.stderr, c'[trace_prealloc] scope %s scope=%p\n', action, scope)
			return
		}
		unsafe {
			mut blocks := 0
			mut used := i64(0)
			mut size := i64(0)
			mut mallocs := 0
			mut mb := scope.first
			for mb != 0 {
				blocks++
				used += vmemory_block_used(mb)
				size += vmemory_block_size(mb)
				mallocs += mb.mallocs
				mb = mb.next
			}
			C.fprintf(C.stderr,
				c'[trace_prealloc] scope %s scope=%p previous=%p first=%p blocks=%d used=%lld size=%lld mallocs=%d\n',
				action, scope, scope.previous, scope.first, blocks, used, size, mallocs)
		}
	}
}

@[unsafe]
fn vmemory_block_new(prev &VMemoryBlock, at_least isize, align isize) &VMemoryBlock {
	return unsafe { vmemory_block_new_sized(prev, at_least, align, isize(prealloc_block_size)) }
}

@[unsafe]
fn vmemory_block_new_sized(prev &VMemoryBlock, at_least isize, align isize, min_block_size isize) &VMemoryBlock {
	vmem_block_size := sizeof(VMemoryBlock)
	mut v := unsafe { &VMemoryBlock(C.calloc(1, vmem_block_size)) }
	vmemory_abort_on_nil(v, vmem_block_size)
	if unsafe { prev != 0 } {
		v.id = prev.id + 1
	}

	v.previous = prev
	if unsafe { prev != 0 } {
		prev.next = v
		v.is_scope = prev.is_scope
	}
	effective_min_block_size := if min_block_size > 0 {
		min_block_size
	} else {
		isize(prealloc_block_size)
	}
	v.min_block_size = effective_min_block_size
	base_block_size := if at_least < effective_min_block_size {
		effective_min_block_size
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
		C.fprintf(C.stderr,
			c'vmemory_block_new id: %d, block_size: %lld, at_least: %lld, align: %lld\n', v.id,
			block_size, at_least, align)
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
	$if trace_prealloc ? {
		if v.is_scope {
			C.fprintf(C.stderr,
				c'[trace_prealloc] block alloc block=%p previous=%p id=%d size=%lld at_least=%lld align=%lld start=%p stop=%p\n',
				v, prev, v.id, block_size, at_least, align, v.start, v.stop)
		}
	}
	return v
}

@[unsafe]
fn vmemory_block_malloc(n isize, align isize) &u8 {
	unsafe {
		// Lazy per-thread initialization: when g_memory_block is thread-local,
		// new threads start with a null pointer and need their own arena.
		if g_memory_block == nil {
			g_memory_block = vmemory_block_new(nil, isize(prealloc_block_size), 0)
		}
	}
	$if prealloc_trace_malloc ? {
		C.fprintf(C.stderr, c'vmemory_block_malloc g_memory_block.id: %d, n: %lld align: %d\n',
			g_memory_block.id, n, align)
	}
	unsafe {
		fixed_align := vmemory_effective_align(align)
		mut current := vmemory_align_up(g_memory_block.current, fixed_align)
		remaining := i64(g_memory_block.stop) - i64(current)
		if _unlikely_(remaining < n) {
			was_scope := g_memory_block.is_scope
			scope := g_memory_block.scope
			min_block_size := if g_memory_block.min_block_size > 0 {
				g_memory_block.min_block_size
			} else {
				isize(prealloc_block_size)
			}
			g_memory_block = vmemory_block_new_sized(g_memory_block, n, fixed_align, min_block_size)
			g_memory_block.is_scope = was_scope
			g_memory_block.scope = scope
			current = vmemory_align_up(g_memory_block.current, fixed_align)
		}
		res := &u8(current)
		g_memory_block.current = current
		g_memory_block.current += n
		$if prealloc_stats ? {
			g_memory_block.mallocs++
		} $else {
			$if trace_prealloc ? {
				g_memory_block.mallocs++
			}
		}
		$if trace_prealloc ? {
			if g_memory_block.is_scope {
				used := vmemory_block_used(g_memory_block)
				size := vmemory_block_size(g_memory_block)
				C.fprintf(C.stderr,
					c'[trace_prealloc] alloc block=%p ptr=%p size=%lld align=%lld used=%lld/%lld mallocs=%d\n',
					g_memory_block, res, n, fixed_align, used, size, g_memory_block.mallocs)
			}
		}
		return res
	}
}

@[unsafe]
fn vmemory_block_free(mb &VMemoryBlock) {
	$if trace_prealloc ? {
		if mb.is_scope {
			C.fprintf(C.stderr,
				c'[trace_prealloc] block free block=%p id=%d start=%p used=%lld size=%lld mallocs=%d\n',
				mb, mb.id, mb.start, vmemory_block_used(mb), vmemory_block_size(mb), mb.mallocs)
		}
	}
	$if windows {
		// Warning! On windows, we always use _aligned_free to free memory.
		C._aligned_free(mb.start)
	} $else {
		C.free(mb.start)
	}
	C.free(mb)
}

@[unsafe]
fn vmemory_block_free_after(marker &VMemoryBlock) {
	if marker == unsafe { nil } {
		return
	}
	unsafe {
		mut mb := marker.next
		marker.next = nil
		vmemory_block_free_chain(mb)
	}
}

@[unsafe]
fn vmemory_block_free_chain(first &VMemoryBlock) {
	unsafe {
		mut mb := first
		for mb != 0 {
			next := mb.next
			vmemory_block_free(mb)
			mb = next
		}
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
			C.fprintf(C.stderr,
				c'> freeing mb: %16p, mb.id: %3d | size: %10lld | rem: %10lld | start: %16p | current: %16p | used: %10lld bytes | mallocs: %6d\n',
				mb, mb.id, size, remaining, mb.start, mb.current, used, mb.mallocs)
			mb = mb.previous
		}
		C.fprintf(C.stderr, c'> nr_mallocs: %lld, total_used: %lld bytes\n', nr_mallocs, total_used)
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
			C.fprintf(C.stderr, c'prealloc_vcleanup dumping process memory to path: %s\n', path.str)
			stream := C.fopen(path.str, c'wb')
			mut mb := start
			for {
				used := u64(mb.current) - u64(mb.start)
				total_used += used
				C.fprintf(C.stderr,
					c'prealloc_vcleanup dumping mb: %p, mb.id: %d, used: %10lld bytes\n', mb,
					mb.id, used)

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
			C.fprintf(C.stderr, c'prealloc_vcleanup total dump size in bytes: %lld\n', total_used)
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

// prealloc_scope_begin starts a nested arena on the current thread. All V
// allocations after this call use the nested arena until `prealloc_scope_end`.
// The returned scope can be passed across threads and later freed with
// `prealloc_scope_free_after`, which is useful when a response buffer outlives
// the request handler thread.
@[unsafe]
pub fn prealloc_scope_begin() voidptr {
	unsafe {
		scope := &VPreallocScope(C.calloc(1, sizeof(VPreallocScope)))
		vmemory_abort_on_nil(scope, sizeof(VPreallocScope))
		scope.previous = g_memory_block
		scope.first = vmemory_block_new_sized(scope.previous, isize(prealloc_scope_block_size), 0,
			isize(prealloc_scope_block_size))
		scope.first.is_scope = true
		scope.first.scope = scope
		g_memory_block = scope.first
		prealloc_trace_scope(c'begin', scope)
		return scope
	}
}

@[unsafe]
pub fn prealloc_scope_checkpoint(label &char) {
	$if trace_prealloc ? {
		unsafe {
			if g_memory_block == 0 || !g_memory_block.is_scope {
				return
			}
			mut blocks := 0
			mut used := i64(0)
			mut size := i64(0)
			mut mallocs := 0
			mut first := g_memory_block
			for first.previous != 0 && first.previous.is_scope {
				first = first.previous
			}
			mut mb := first
			for mb != 0 {
				blocks++
				used += vmemory_block_used(mb)
				size += vmemory_block_size(mb)
				mallocs += mb.mallocs
				mb = mb.next
			}
			C.fprintf(C.stderr,
				c'[trace_prealloc] checkpoint label=%s first=%p current=%p blocks=%d used=%lld size=%lld mallocs=%d\n',
				label, first, g_memory_block, blocks, used, size, mallocs)
		}
	}
}

@[unsafe]
fn prealloc_scope_free_blocks(scope &VPreallocScope) {
	if scope == unsafe { nil } {
		return
	}
	unsafe {
		if scope.previous != 0 {
			scope.previous.next = nil
		}
		vmemory_block_free_chain(scope.first)
	}
}

@[unsafe]
fn prealloc_scope_request_free(scope &VPreallocScope, abandoned bool) {
	if scope == unsafe { nil } {
		return
	}
	unsafe {
		if abandoned {
			C.v_prealloc_atomic_store_i32(&scope.abandoned, 1)
		}
		C.v_prealloc_atomic_store_i32(&scope.free_requested, 1)
		prealloc_scope_finish_if_ready(scope)
	}
}

@[unsafe]
fn prealloc_scope_finish_if_ready(scope &VPreallocScope) {
	if scope == unsafe { nil } {
		return
	}
	unsafe {
		if C.v_prealloc_atomic_load_i32(&scope.free_requested) == 0 {
			return
		}
		if C.v_prealloc_atomic_load_i32(&scope.refs) != 0 {
			return
		}
		if C.v_prealloc_atomic_cas_i32(&scope.finalized, 0, 1) == 0 {
			return
		}
		if C.v_prealloc_atomic_load_i32(&scope.abandoned) == 0 {
			prealloc_scope_free_blocks(scope)
		}
		C.free(scope)
	}
}

@[unsafe]
fn prealloc_scope_detach_current(scope &VPreallocScope) {
	if scope == unsafe { nil } {
		return
	}
	unsafe {
		previous := scope.previous
		if previous != 0 {
			previous.next = nil
		}
		if g_memory_block != 0 && g_memory_block.is_scope && g_memory_block.scope == scope {
			g_memory_block = previous
		}
		scope.previous = nil
	}
}

// prealloc_scope_retain_current keeps the current scoped arena alive after the
// owner calls `prealloc_scope_end`/`prealloc_scope_free_after`. It is used by
// generated `spawn` wrappers so detached threads can safely receive arguments
// allocated in a request arena.
@[unsafe]
pub fn prealloc_scope_retain_current() voidptr {
	$if prealloc {
		unsafe {
			if g_memory_block == 0 || !g_memory_block.is_scope || g_memory_block.scope == 0 {
				return nil
			}
			scope := g_memory_block.scope
			C.v_prealloc_atomic_add_i32(&scope.refs, 1)
			$if trace_prealloc ? {
				prealloc_trace_scope(c'retain', scope)
			}
			return scope
		}
	} $else {
		return unsafe { nil }
	}
}

@[unsafe]
pub fn prealloc_scope_release(scope_ptr voidptr) {
	$if prealloc {
		if scope_ptr == unsafe { nil } {
			return
		}
		unsafe {
			scope := &VPreallocScope(scope_ptr)
			C.v_prealloc_atomic_add_i32(&scope.refs, -1)
			$if trace_prealloc ? {
				prealloc_trace_scope(c'release', scope)
			}
			prealloc_scope_finish_if_ready(scope)
		}
	}
}

// prealloc_scope_end frees a nested arena and restores the current thread arena
// to the state before `prealloc_scope_begin`.
@[unsafe]
pub fn prealloc_scope_end(scope_ptr voidptr) {
	if scope_ptr == unsafe { nil } {
		return
	}
	unsafe {
		scope := &VPreallocScope(scope_ptr)
		prealloc_trace_scope(c'end', scope)
		prealloc_scope_detach_current(scope)
		prealloc_scope_request_free(scope, false)
	}
}

// prealloc_scope_leave restores the current thread arena without freeing the
// scoped blocks. Call this before another thread takes ownership of the scope.
@[unsafe]
pub fn prealloc_scope_leave(scope_ptr voidptr) {
	if scope_ptr == unsafe { nil } {
		return
	}
	unsafe {
		scope := &VPreallocScope(scope_ptr)
		prealloc_trace_scope(c'leave', scope)
		prealloc_scope_detach_current(scope)
	}
}

// prealloc_scope_abandon restores the current thread arena and intentionally
// leaks the scoped blocks. It is only for APIs that transfer request state to
// user code without providing a close hook yet.
@[unsafe]
pub fn prealloc_scope_abandon(scope_ptr voidptr) {
	if scope_ptr == unsafe { nil } {
		return
	}
	unsafe {
		scope := &VPreallocScope(scope_ptr)
		prealloc_trace_scope(c'abandon', scope)
		prealloc_scope_leave(scope_ptr)
		prealloc_scope_request_free(scope, true)
	}
}

// prealloc_scope_free_after frees a nested arena from a marker without touching
// the caller's thread-local arena pointer. Use this when another thread finishes
// sending data that was allocated in the request thread.
@[unsafe]
pub fn prealloc_scope_free_after(scope_ptr voidptr) {
	if scope_ptr == unsafe { nil } {
		return
	}
	unsafe {
		scope := &VPreallocScope(scope_ptr)
		prealloc_trace_scope(c'free-after', scope)
		prealloc_scope_request_free(scope, false)
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
