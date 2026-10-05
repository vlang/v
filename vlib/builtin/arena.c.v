@[has_globals]
module builtin

// Scoped arenas: the per-thread allocator stack behind the `arena` module.
//
// While an arena is pushed on a thread, V's allocation entry points (malloc,
// malloc_noscan, vcalloc, memdup, ...) called on that thread bump-allocate
// from it. free() of arena memory is a no-op. realloc of arena memory copies
// it to the thread's current allocator (or grows it in place), so a container
// built in an arena keeps working after the arena is popped. The chunks of an
// arena are released in bulk by v_arena_reset and v_arena_free.
//
// Every live chunk is listed in a process-wide, lock-protected registry, so
// free() and realloc on any thread can tell arena memory from heap memory.
// Programs that never create an arena only pay a check of one global in each
// allocation entry point (see g_arena_alloc_hook). -prealloc and -gc vgc
// builds never call into this file.

const arena_magic = u32(0x41524e41)
const arena_default_chunk_size = isize(64 * 1024)
const arena_max_chunk_size = isize(4 * 1024 * 1024)

// VArenaChunk is the header at the start of each arena chunk.
struct VArenaChunk {
mut:
	prev   &VArenaChunk = unsafe { nil }
	start  &u8          = unsafe { nil }
	cursor &u8          = unsafe { nil }
	stop   &u8          = unsafe { nil }
}

// VArena is the control block of an arena. It is allocated with C.calloc, so
// it is neither part of another arena nor collected by the GC.
struct VArena {
mut:
	magic u32
	// 0: idle, 1: pushed on a thread's allocator stack, 2: being reset or freed.
	// Accessed through the C `_i32` atomics.
	state          i32
	chunk          &VArenaChunk = unsafe { nil } // serves new allocations
	last           &u8          = unsafe { nil } // the most recent allocation
	below          &VArena      = unsafe { nil } // next arena down the thread's stack
	chunk_size     isize // size of the next regular chunk
	max_chunk_size isize
}

struct VArenaRange {
mut:
	start usize
	stop  usize
	owner &VArena = unsafe { nil }
}

// The allocation entry points call these hooks, which v_arena_new installs
// and nothing clears. Until then, each entry point only checks one global, and
// programs that never create an arena do not even contain the arena code.
// A thread that reads a stale nil has not pushed an arena itself, and it can
// only get arena memory from another thread through a synchronizing operation
// that also publishes the hooks.
__global g_arena_alloc_hook fn (n isize, align isize) &u8
__global g_arena_realloc_hook fn (old_data &u8, old_size isize, new_size isize) &u8
__global g_arena_owns_hook fn (ptr voidptr) bool
// g_arena_top is the innermost arena pushed on the current thread. It is
// thread-local (see `is_builtin_arena_top` in the C backend), so spawned
// threads start with the default allocator.
__global g_arena_top &VArena
// The registry of live chunks, sorted by start address, guarded by g_arena_lock.
__global g_arena_lock i32
__global g_arena_ranges &VArenaRange
__global g_arena_ranges_len i32
__global g_arena_ranges_cap int

@[inline]
fn arena_align() isize {
	return isize(sizeof(voidptr) * 2)
}

@[inline]
fn arena_chunk_header_size() isize {
	align := arena_align()
	return (isize(sizeof(VArenaChunk)) + align - 1) / align * align
}

@[inline]
fn arena_registry_lock() {
	for C.v_prealloc_atomic_cas_i32(&g_arena_lock, 0, 1) == 0 {
		for C.v_prealloc_atomic_load_i32(&g_arena_lock) != 0 {
		}
	}
}

@[inline]
fn arena_registry_unlock() {
	// A CAS is a full barrier with every supported C compiler; a plain store
	// would not publish the registry writes on weakly ordered CPUs.
	C.v_prealloc_atomic_cas_i32(&g_arena_lock, 1, 0)
}

// arena_registry_upper returns the index of the first range starting after
// `addr`. The registry lock must be held.
@[direct_array_access; unsafe]
fn arena_registry_upper(addr usize) int {
	mut lo := 0
	mut hi := int(g_arena_ranges_len)
	for lo < hi {
		mid := lo + (hi - lo) / 2
		if g_arena_ranges[mid].start <= addr {
			lo = mid + 1
		} else {
			hi = mid
		}
	}
	return lo
}

@[unsafe]
fn arena_registry_add(owner &VArena, chunk &VArenaChunk) {
	arena_registry_lock()
	unsafe {
		len := int(g_arena_ranges_len)
		if len == g_arena_ranges_cap {
			new_cap := if g_arena_ranges_cap == 0 { 16 } else { g_arena_ranges_cap * 2 }
			bytes := usize(new_cap) * sizeof(VArenaRange)
			ranges := &VArenaRange(C.realloc(g_arena_ranges, bytes))
			vmemory_abort_on_nil(ranges, isize(bytes))
			g_arena_ranges = ranges
			g_arena_ranges_cap = new_cap
		}
		start := usize(chunk.start)
		i := arena_registry_upper(start)
		C.memmove(&g_arena_ranges[i + 1], &g_arena_ranges[i], usize(len - i) * sizeof(VArenaRange))
		g_arena_ranges[i] = VArenaRange{
			start: start
			stop:  usize(chunk.stop)
			owner: owner
		}
		C.v_prealloc_atomic_add_i32(&g_arena_ranges_len, 1)
	}
	arena_registry_unlock()
}

@[unsafe]
fn arena_registry_remove(chunk &VArenaChunk) {
	arena_registry_lock()
	unsafe {
		start := usize(chunk.start)
		i := arena_registry_upper(start) - 1
		len := int(g_arena_ranges_len)
		if i >= 0 && g_arena_ranges[i].start == start {
			C.memmove(&g_arena_ranges[i], &g_arena_ranges[i + 1], usize(len - i - 1) * sizeof(VArenaRange))
			C.v_prealloc_atomic_add_i32(&g_arena_ranges_len, -1)
		}
	}
	arena_registry_unlock()
}

// arena_owner returns the arena that owns `ptr` (nil for any other memory) and
// the end of the chunk that contains it.
@[unsafe]
fn arena_owner(ptr voidptr) (&VArena, usize) {
	addr := usize(ptr)
	unsafe {
		// Only this thread changes the arenas on its own stack, so they can be
		// searched without the lock. They own most memory freed in a scope.
		mut a := g_arena_top
		for a != nil {
			mut c := a.chunk
			for c != nil {
				if addr >= usize(c.start) && addr < usize(c.stop) {
					return a, usize(c.stop)
				}
				c = c.prev
			}
			a = a.below
		}
		if C.v_prealloc_atomic_load_i32(&g_arena_ranges_len) == 0 {
			return nil, 0
		}
		mut owner := &VArena(nil)
		mut stop := usize(0)
		arena_registry_lock()
		i := arena_registry_upper(addr) - 1
		if i >= 0 && addr < g_arena_ranges[i].stop {
			owner = g_arena_ranges[i].owner
			stop = g_arena_ranges[i].stop
		}
		arena_registry_unlock()
		return owner, stop
	}
}

// arena_owns_ptr reports whether `ptr` is memory of a live arena, which free()
// must leave alone.
fn arena_owns_ptr(ptr voidptr) bool {
	owner, _ := unsafe { arena_owner(ptr) }
	return owner != unsafe { nil }
}

@[unsafe]
fn arena_chunk_release(chunk &VArenaChunk) {
	unsafe {
		arena_registry_remove(chunk)
		$if gcboehm ? {
			C.GC_FREE(chunk)
		} $else {
			C.free(chunk)
		}
	}
}

// arena_grow adds a chunk with room for at least `need` bytes.
@[noinline; unsafe]
fn arena_grow(mut a VArena, need isize) &VArenaChunk {
	mut size := a.chunk_size
	if need > size {
		size = need
	} else if a.chunk_size < a.max_chunk_size {
		a.chunk_size *= 2
		if a.chunk_size > a.max_chunk_size {
			a.chunk_size = a.max_chunk_size
		}
	}
	header := arena_chunk_header_size()
	total := header + size
	mut mem := voidptr(unsafe { nil })
	$if gcboehm ? {
		// Uncollectable chunks are still scanned, so GC objects that are only
		// referenced from arena memory stay alive.
		mem = C.GC_MALLOC_UNCOLLECTABLE(usize(total))
	} $else {
		mem = C.malloc(usize(total))
	}
	vmemory_abort_on_nil(mem, total)
	unsafe {
		mut c := &VArenaChunk(mem)
		c.prev = a.chunk
		c.start = &u8(mem) + header
		c.cursor = c.start
		c.stop = c.start + size
		a.chunk = c
		arena_registry_add(a, c)
		return c
	}
}

@[unsafe]
fn arena_alloc(mut a VArena, n isize, align isize) &u8 {
	default_align := arena_align()
	fixed_align := if align > default_align { align } else { default_align }
	unsafe {
		mut c := a.chunk
		mut p := &u8(nil)
		if c != nil {
			p = vmemory_align_up(c.cursor, fixed_align)
		}
		if c == nil || i64(c.stop) - i64(p) < i64(n) {
			c = arena_grow(mut a, n + fixed_align)
			p = vmemory_align_up(c.cursor, fixed_align)
		}
		c.cursor = p + n
		a.last = p
		return p
	}
}

// arena_alloc_current returns `n` bytes from the current thread's arena, or
// nil when no arena is active.
fn arena_alloc_current(n isize, align isize) &u8 {
	mut a := g_arena_top
	if a == unsafe { nil } {
		return unsafe { nil }
	}
	return unsafe { arena_alloc(mut a, n, align) }
}

// arena_realloc resizes `old_data` when it is arena memory, and returns nil
// otherwise. `old_size` is negative when unknown. The most recent allocation
// of the current arena grows in place; other arena memory is copied to the
// current allocator and left in its arena. realloc(nil, n) allocates from the
// current arena.
fn arena_realloc(old_data &u8, old_size isize, new_size isize) &u8 {
	unsafe {
		if old_data == nil {
			if new_size <= 0 {
				return nil
			}
			return arena_alloc_current(new_size, 0)
		}
		owner, chunk_stop := arena_owner(old_data)
		if owner == nil {
			return nil
		}
		if new_size <= 0 {
			return old_data
		}
		mut top := g_arena_top
		if top == owner && top.last == old_data {
			mut c := top.chunk
			if i64(c.stop) - i64(old_data) >= i64(new_size) {
				c.cursor = old_data + new_size
				return old_data
			}
		}
		new_ptr := if top != nil { arena_alloc(mut top, new_size, 0) } else { malloc(new_size) }
		mut n := new_size
		if old_size >= 0 && old_size < n {
			n = old_size
		}
		// Without the old size, copy what the chunk holds: it is mapped memory.
		available := isize(chunk_stop - usize(old_data))
		if available < n {
			n = available
		}
		C.memcpy(new_ptr, old_data, usize(n))
		return new_ptr
	}
}

// arena_release_chunks frees the chunks of `a`. With `keep_one`, the largest
// regular chunk is kept for reuse, so an arena that is reset in a loop stops
// allocating once it has grown to the size the loop needs.
@[unsafe]
fn arena_release_chunks(mut a VArena, keep_one bool) {
	unsafe {
		mut keep := &VArenaChunk(nil)
		if keep_one {
			mut c := a.chunk
			for c != nil {
				size := i64(c.stop) - i64(c.start)
				if size <= i64(a.max_chunk_size)
					&& (keep == nil || size > i64(keep.stop) - i64(keep.start)) {
					keep = c
				}
				c = c.prev
			}
		}
		mut c := a.chunk
		for c != nil {
			prev := c.prev
			if c != keep {
				arena_chunk_release(c)
			}
			c = prev
		}
		if keep != nil {
			$if gcboehm ? {
				// Stale pointers in reused memory would keep GC objects alive.
				C.memset(keep.start, 0, usize(i64(keep.cursor) - i64(keep.start)))
			}
			keep.prev = nil
			keep.cursor = keep.start
		}
		a.chunk = keep
		a.last = nil
	}
}

fn arena_checked(handle voidptr) &VArena {
	if handle == unsafe { nil } {
		panic('arena: the arena was freed, or was not created with arena.new()')
	}
	a := unsafe { &VArena(handle) }
	if a.magic != arena_magic {
		panic('arena: invalid arena handle (was the arena freed through a copy?)')
	}
	return a
}

// v_arena_new creates an arena whose first chunk holds `chunk_size` bytes (0
// selects 64KB). It is the low-level hook behind `arena.new()`; use that.
@[unsafe]
pub fn v_arena_new(chunk_size isize) voidptr {
	$if prealloc || vgc ?|| freestanding || vinix {
		panic('arena: scoped arenas are not supported with -prealloc, -gc vgc, -freestanding or on Vinix')
	}
	mut a := unsafe { &VArena(C.calloc(1, sizeof(VArena))) }
	vmemory_abort_on_nil(a, isize(sizeof(VArena)))
	a.magic = arena_magic
	a.chunk_size = if chunk_size > 0 { chunk_size } else { arena_default_chunk_size }
	a.max_chunk_size = if a.chunk_size > arena_max_chunk_size {
		a.chunk_size
	} else {
		arena_max_chunk_size
	}
	if g_arena_alloc_hook == unsafe { nil } {
		g_arena_realloc_hook = arena_realloc
		g_arena_owns_hook = arena_owns_ptr
		g_arena_alloc_hook = arena_alloc_current
	}
	return a
}

// v_arena_push makes `handle` the allocator of the current thread. It is the
// low-level hook behind `Arena.push()`; use that.
@[unsafe]
pub fn v_arena_push(handle voidptr) {
	mut a := arena_checked(handle)
	if C.v_prealloc_atomic_cas_i32(&a.state, 0, 1) == 0 {
		panic('arena: push() of an arena that is already active on this or another thread')
	}
	a.below = g_arena_top
	g_arena_top = a
}

// v_arena_pop restores the allocator that was current before `handle` was
// pushed. It is the low-level hook behind `Arena.pop()`; use that.
@[unsafe]
pub fn v_arena_pop(handle voidptr) {
	mut a := arena_checked(handle)
	top := g_arena_top
	if top == unsafe { nil } {
		panic('arena: pop() without an active arena on this thread')
	}
	if top != a {
		panic('arena: pop() of an arena that is not the innermost active arena of this thread')
	}
	g_arena_top = a.below
	a.below = unsafe { nil }
	C.v_prealloc_atomic_cas_i32(&a.state, 1, 0)
}

// v_arena_reset releases the memory of an inactive arena for reuse. It is
// the low-level hook behind `Arena.reset()`; use that.
@[unsafe]
pub fn v_arena_reset(handle voidptr) {
	mut a := arena_checked(handle)
	if C.v_prealloc_atomic_cas_i32(&a.state, 0, 2) == 0 {
		panic('arena: reset() of an active arena; pop() it first')
	}
	unsafe { arena_release_chunks(mut a, true) }
	C.v_prealloc_atomic_cas_i32(&a.state, 2, 0)
}

// v_arena_free releases an inactive arena and all of its memory. It is the
// low-level hook behind `Arena.free()`; use that.
@[unsafe]
pub fn v_arena_free(handle voidptr) {
	mut a := arena_checked(handle)
	if C.v_prealloc_atomic_cas_i32(&a.state, 0, 2) == 0 {
		panic('arena: free() of an active arena; pop() it first')
	}
	unsafe {
		arena_release_chunks(mut a, false)
		a.magic = 0
		C.free(a)
	}
}

// v_arena_current returns the innermost arena of the current thread, or nil.
pub fn v_arena_current() voidptr {
	if g_arena_alloc_hook == unsafe { nil } {
		return unsafe { nil }
	}
	return g_arena_top
}

// v_arena_stats returns the bytes handed out by an arena since its last
// reset, and the bytes of its chunks.
pub fn v_arena_stats(handle voidptr) (isize, isize) {
	a := arena_checked(handle)
	mut used := i64(0)
	mut capacity := i64(0)
	mut c := a.chunk
	for c != unsafe { nil } {
		used += i64(c.cursor) - i64(c.start)
		capacity += i64(c.stop) - i64(c.start)
		c = c.prev
	}
	return isize(used), isize(capacity)
}

// v_arena_owns reports whether `ptr` points into the memory of an arena.
pub fn v_arena_owns(handle voidptr, ptr voidptr) bool {
	a := arena_checked(handle)
	addr := usize(ptr)
	mut c := a.chunk
	for c != unsafe { nil } {
		if addr >= usize(c.start) && addr < usize(c.stop) {
			return true
		}
		c = c.prev
	}
	return false
}
