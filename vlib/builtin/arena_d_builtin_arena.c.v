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
// Every live chunk is listed in a process-wide registry, so free() and
// realloc on any thread can tell arena memory from heap memory. Its readers
// take no lock (see arena_registry_find).
//
// Only programs that use the `arena` module contain this file and the hooks
// in the allocation entry points: the compiler defines `builtin_arena` for
// them. Until such a program creates its first arena, each entry point only
// checks one atomic global (see g_arena_hooks_ready). -prealloc and -gc vgc builds
// never call into this file.

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

// VArenaRegistry lists the live chunks of all arenas, sorted by start address.
// Readers search it without a lock, so a registry that was replaced by a bigger
// one is never freed: a reader may still be searching it. Each replacement
// doubles the capacity, so the retired ones hold fewer entries than the live one.
struct VArenaRegistry {
	cap    int
	ranges &VArenaRange = unsafe { nil }
}

fn C.atomic_load_u32(voidptr) u32
fn C.atomic_store_u32(voidptr, u32)
fn C.atomic_load_ptr(voidptr) voidptr
fn C.atomic_store_ptr(voidptr, voidptr)

// The allocation entry points call these hooks, which v_arena_new installs
// and nothing clears. Publishing g_arena_hooks_ready after installation makes
// all three immutable hooks visible together to allocation entry points.
__global g_arena_hooks_ready u32
__global g_arena_alloc_hook fn (n isize, align isize) &u8
__global g_arena_realloc_hook fn (old_data &u8, old_size isize, new_size isize) &u8
__global g_arena_owns_hook fn (ptr voidptr) bool
// g_arena_top is the innermost arena pushed on the current thread. It is
// thread-local (see `is_builtin_arena_top` in the C backend), so spawned
// threads start with the default allocator.
__global g_arena_top &VArena
// g_arena_lock serializes the writers of the registry and the installation of
// the hooks. g_arena_seq is odd while a writer changes the registry; a reader
// that sees it change during a search searches again. Writers store the
// registry fields with atomics, so that readers never race with them.
__global g_arena_lock i32
__global g_arena_seq u32
__global g_arena_registry &VArenaRegistry
__global g_arena_ranges_len u32

// arena_align is the alignment of arena allocations without an explicit one:
// that of malloc, which is 16 bytes on 32-bit targets too.
@[inline]
fn arena_align() isize {
	return 16
}

@[inline]
fn arena_chunk_header_size() isize {
	align := arena_align()
	return (isize(sizeof(VArenaChunk)) + align - 1) / align * align
}

@[inline]
fn arena_lock() {
	for C.v_prealloc_atomic_cas_i32(&g_arena_lock, 0, 1) == 0 {
		for C.v_prealloc_atomic_load_i32(&g_arena_lock) != 0 {
		}
	}
}

@[inline]
fn arena_unlock() {
	// A CAS is a full barrier with every supported C compiler; a plain store
	// would not publish the registry writes on weakly ordered CPUs.
	C.v_prealloc_atomic_cas_i32(&g_arena_lock, 1, 0)
}

// arena_registry_begin_write locks the registry for a change.
@[inline]
fn arena_registry_begin_write() {
	arena_lock()
	C.atomic_store_u32(&g_arena_seq, C.atomic_load_u32(&g_arena_seq) + 1)
}

@[inline]
fn arena_registry_end_write() {
	C.atomic_store_u32(&g_arena_seq, C.atomic_load_u32(&g_arena_seq) + 1)
	arena_unlock()
}

@[inline; unsafe]
fn arena_range_start(r &VArenaRange) usize {
	return usize(C.atomic_load_ptr(&r.start))
}

@[inline; unsafe]
fn arena_range_stop(r &VArenaRange) usize {
	return usize(C.atomic_load_ptr(&r.stop))
}

@[inline; unsafe]
fn arena_range_set(mut r VArenaRange, value VArenaRange) {
	C.atomic_store_ptr(&r.start, voidptr(value.start))
	C.atomic_store_ptr(&r.stop, voidptr(value.stop))
	C.atomic_store_ptr(&r.owner, value.owner)
}

// arena_registry_upper returns the index of the first of the `len` ranges of
// `reg` that starts after `addr`.
@[direct_array_access; unsafe]
fn arena_registry_upper(reg &VArenaRegistry, len int, addr usize) int {
	mut lo := 0
	mut hi := len
	for lo < hi {
		mid := lo + (hi - lo) / 2
		if arena_range_start(&reg.ranges[mid]) <= addr {
			lo = mid + 1
		} else {
			hi = mid
		}
	}
	return lo
}

@[unsafe]
fn arena_registry_add(owner &VArena, chunk &VArenaChunk) {
	arena_registry_begin_write()
	unsafe {
		len := int(g_arena_ranges_len)
		mut reg := g_arena_registry
		if reg == nil || len == reg.cap {
			new_cap := if reg == nil { 16 } else { reg.cap * 2 }
			bytes := sizeof(VArenaRegistry) + usize(new_cap) * sizeof(VArenaRange)
			mut grown := &VArenaRegistry(C.malloc(bytes))
			vmemory_abort_on_nil(grown, isize(bytes))
			grown.cap = new_cap
			grown.ranges = &VArenaRange(&u8(grown) + sizeof(VArenaRegistry))
			if len > 0 {
				C.memcpy(grown.ranges, reg.ranges, usize(len) * sizeof(VArenaRange))
			}
			// The old registry is left allocated (see VArenaRegistry).
			C.atomic_store_ptr(&g_arena_registry, grown)
			reg = grown
		}
		start := usize(chunk.start)
		i := arena_registry_upper(reg, len, start)
		for j := len; j > i; j-- {
			arena_range_set(mut reg.ranges[j], reg.ranges[j - 1])
		}
		arena_range_set(mut reg.ranges[i], VArenaRange{
			start: start
			stop:  usize(chunk.stop)
			owner: owner
		})
		C.atomic_store_u32(&g_arena_ranges_len, u32(len + 1))
	}
	arena_registry_end_write()
}

@[unsafe]
fn arena_registry_remove(chunk &VArenaChunk) {
	arena_registry_begin_write()
	unsafe {
		reg := g_arena_registry
		len := int(g_arena_ranges_len)
		start := usize(chunk.start)
		i := arena_registry_upper(reg, len, start) - 1
		if i >= 0 && reg.ranges[i].start == start {
			for j in i .. len - 1 {
				arena_range_set(mut reg.ranges[j], reg.ranges[j + 1])
			}
			C.atomic_store_u32(&g_arena_ranges_len, u32(len - 1))
		}
	}
	arena_registry_end_write()
}

// arena_registry_find returns the arena that owns `addr` (nil for any other
// memory) and the end of the chunk that contains it. It takes no lock, so
// free() on many threads does not serialize: a search that overlapped a change
// of the registry is repeated. Its loads stay inside the registry it searches,
// whatever a writer does meanwhile.
@[direct_array_access; unsafe]
fn arena_registry_find(addr usize) (&VArena, usize) {
	for {
		seq := C.atomic_load_u32(&g_arena_seq)
		if seq & 1 != 0 {
			continue
		}
		reg := &VArenaRegistry(C.atomic_load_ptr(&g_arena_registry))
		mut owner := &VArena(nil)
		mut stop := usize(0)
		if reg != nil {
			mut len := int(C.atomic_load_u32(&g_arena_ranges_len))
			if len > reg.cap {
				// The length of a registry that replaced this one.
				len = reg.cap
			}
			i := arena_registry_upper(reg, len, addr) - 1
			if i >= 0 {
				chunk_stop := arena_range_stop(&reg.ranges[i])
				if addr < chunk_stop {
					owner = &VArena(C.atomic_load_ptr(&reg.ranges[i].owner))
					stop = chunk_stop
				}
			}
		}
		if C.atomic_load_u32(&g_arena_seq) == seq {
			return owner, stop
		}
	}
	return nil, 0
}

// arena_owner returns the arena that owns `ptr` (nil for any other memory) and
// the end of the chunk that contains it.
@[unsafe]
fn arena_owner(ptr voidptr) (&VArena, usize) {
	addr := usize(ptr)
	unsafe {
		// Only this thread changes the arenas on its own stack, so they can be
		// searched without the registry. They own most memory freed in a scope.
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
		if C.atomic_load_u32(&g_arena_ranges_len) == 0 {
			return nil, 0
		}
		return arena_registry_find(addr)
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
	// Like malloc, fail for a size that no chunk can hold, before computing the
	// size of that chunk overflows.
	if usize(n) > (~usize(0) >> 1) - usize(fixed_align + arena_chunk_header_size()) {
		_memory_panic(@FN, n)
	}
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
		// A new block in the same chunk starts at or after the end of the old
		// one, so the bytes before it hold all of the old block.
		if usize(new_ptr) > usize(old_data) && usize(new_ptr) - usize(old_data) < usize(n) {
			n = isize(usize(new_ptr) - usize(old_data))
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
	// The lock orders the installation with the one of other threads, and the
	// atomic ready flag publishes the immutable hooks to lock-free readers.
	arena_lock()
	if C.atomic_load_u32(&g_arena_hooks_ready) == 0 {
		g_arena_realloc_hook = arena_realloc
		g_arena_owns_hook = arena_owns_ptr
		g_arena_alloc_hook = arena_alloc_current
		C.atomic_store_u32(&g_arena_hooks_ready, 1)
	}
	arena_unlock()
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
	if C.atomic_load_u32(&g_arena_hooks_ready) == 0 {
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
