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
// The hooks in the allocation entry points are part of every program. Until
// the program creates its first arena, each entry point only checks one
// atomic global (see g_arena_hooks_ready). -prealloc and -gc vgc builds never
// call into this file, and `-d no_builtin_arena`, which the FastC backend sets,
// leaves it out.

const arena_magic = u32(0x41524e41)
const arena_default_chunk_size = int(64 * 1024)
const arena_max_chunk_size = int(4 * 1024 * 1024)

// VArenaChunk is the header at the start of each arena chunk.
struct VArenaChunk {
mut:
	prev   &VArenaChunk = unsafe { nil }
	start  &u8          = unsafe { nil }
	cursor &u8          = unsafe { nil }
	stop   &u8          = unsafe { nil }
}

// VArena is the control block of an arena. It uses the ordinary heap directly,
// so it is neither part of another arena nor collected by the GC.
// Use voidptr comparisons for arena identity: struct equality may read another
// thread's mutable control state before the access check.
struct VArena {
mut:
	magic  u32
	budget &VArenaBudget = unsafe { nil }
	epoch  u64
	// 0: idle, 1: pushed on a thread's allocator stack, 2: being reset or freed.
	// Accessed through the C `_i32` atomics.
	state          i32
	chunk          &VArenaChunk = unsafe { nil } // serves new allocations
	last           &u8          = unsafe { nil } // the most recent allocation
	below          &VArena      = unsafe { nil } // next arena down the thread's stack
	chunk_size     int // size of the next regular chunk
	max_chunk_size int
}

// VArenaSelection is the opaque stack state paired by arena.Allocator.run.
// Its fields are private so applications cannot assemble a restoration token.
@[noinit]
pub struct VArenaSelection {
	previous voidptr
	selected voidptr
	pushed   bool
}

struct VArenaRange {
mut:
	start uint
	stop  uint
	owner &VArena = unsafe { nil }
	// Split into atomic u32 loads so readers also work on 32-bit targets.
	epoch_low  u32
	epoch_high u32
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
__global g_arena_alloc_hook fn (n int, align int) &u8
__global g_arena_realloc_hook fn (old_data &u8, old_size int, new_size int) &u8
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
__global g_arena_epoch u64

// Arena control blocks and registry storage must bypass scoped allocation.
// Freestanding builds use the selected bare runtime's canonical heap; hosted
// builds use libc directly so these allocations cannot belong to another arena.
@[inline; unsafe]
fn arena_raw_malloc(size uint) voidptr {
	$if freestanding {
		return unsafe { __malloc(size) }
	} $else {
		return unsafe { C.malloc(size) }
	}
}

@[inline; unsafe]
fn arena_raw_calloc(size uint) voidptr {
	$if freestanding {
		return unsafe { __calloc(1, size) }
	} $else {
		return unsafe { C.calloc(1, size) }
	}
}

@[inline; unsafe]
fn arena_raw_free(ptr voidptr) {
	$if freestanding {
		unsafe { __free(ptr) }
	} $else {
		unsafe { C.free(ptr) }
	}
}

// arena_next_epoch is called with the registry writer lock held.
fn arena_next_epoch() u64 {
	g_arena_epoch++
	if g_arena_epoch == 0 {
		panic('arena: lifetime token space exhausted')
	}
	return g_arena_epoch
}

// arena_align is the alignment of arena allocations without an explicit one:
// that of malloc, which is 16 bytes on 32-bit targets too.
@[inline]
fn arena_align() int {
	return 16
}

@[inline]
fn arena_chunk_header_size() int {
	align := arena_align()
	return (int(sizeof(VArenaChunk)) + align - 1) / align * align
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
fn arena_range_start(r &VArenaRange) uint {
	return uint(C.atomic_load_ptr(&r.start))
}

@[inline; unsafe]
fn arena_range_stop(r &VArenaRange) uint {
	return uint(C.atomic_load_ptr(&r.stop))
}

@[inline; unsafe]
fn arena_range_set(mut r VArenaRange, value VArenaRange) {
	C.atomic_store_ptr(&r.start, voidptr(value.start))
	C.atomic_store_ptr(&r.stop, voidptr(value.stop))
	C.atomic_store_ptr(&r.owner, value.owner)
	C.atomic_store_u32(&r.epoch_low, value.epoch_low)
	C.atomic_store_u32(&r.epoch_high, value.epoch_high)
}

// arena_registry_upper returns the index of the first of the `len` ranges of
// `reg` that starts after `addr`.
@[direct_array_access; unsafe]
fn arena_registry_upper(reg &VArenaRegistry, len int, addr uint) int {
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
fn arena_registry_try_add(owner &VArena, chunk &VArenaChunk, mut failure_size int) bool {
	arena_registry_begin_write()
	unsafe {
		len := int(g_arena_ranges_len)
		if u64(len) == u64(max_u32) {
			failure_size = max_int
			arena_registry_end_write()
			return false
		}
		mut reg := g_arena_registry
		if reg == nil || len == reg.cap {
			if reg != nil && reg.cap > max_int / 2 {
				failure_size = max_int
				arena_registry_end_write()
				return false
			}
			new_cap := if reg == nil { 16 } else { reg.cap * 2 }
			if uint(new_cap) > (~uint(0) - sizeof(VArenaRegistry)) / sizeof(VArenaRange) {
				failure_size = max_int
				arena_registry_end_write()
				return false
			}
			bytes := sizeof(VArenaRegistry) + uint(new_cap) * sizeof(VArenaRange)
			mut grown := &VArenaRegistry(arena_raw_malloc(bytes))
			if grown == nil {
				failure_size = int(bytes)
				arena_registry_end_write()
				return false
			}
			grown.cap = new_cap
			grown.ranges = &VArenaRange(&u8(grown) + sizeof(VArenaRegistry))
			if len > 0 {
				C.memcpy(grown.ranges, reg.ranges, uint(len) * sizeof(VArenaRange))
			}
			// The old registry is left allocated (see VArenaRegistry).
			C.atomic_store_ptr(&g_arena_registry, grown)
			reg = grown
		}
		start := uint(chunk.start)
		i := arena_registry_upper(reg, len, start)
		for j := len; j > i; j-- {
			arena_range_set(mut reg.ranges[j], reg.ranges[j - 1])
		}
		arena_range_set(mut reg.ranges[i], VArenaRange{
			start:      start
			stop:       uint(chunk.stop)
			owner:      owner
			epoch_low:  u32(owner.epoch)
			epoch_high: u32(owner.epoch >> 32)
		})
		C.atomic_store_u32(&g_arena_ranges_len, u32(len + 1))
	}
	arena_registry_end_write()
	return true
}

@[unsafe]
fn arena_registry_remove(chunk &VArenaChunk) {
	arena_registry_begin_write()
	unsafe {
		reg := g_arena_registry
		len := int(g_arena_ranges_len)
		start := uint(chunk.start)
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

// arena_registry_lookup returns the arena that owns `addr` (nil for any other
// memory), the end of its chunk, and optionally its lifetime epoch. It takes no lock, so
// free() on many threads does not serialize: a search that overlapped a change
// of the registry is repeated. Its loads stay inside the registry it searches,
// whatever a writer does meanwhile.
@[direct_array_access; inline; unsafe]
fn arena_registry_lookup(addr uint, with_epoch bool) (&VArena, uint, u64) {
	for {
		seq := C.atomic_load_u32(&g_arena_seq)
		if seq & 1 != 0 {
			continue
		}
		reg := &VArenaRegistry(C.atomic_load_ptr(&g_arena_registry))
		mut owner := &VArena(nil)
		mut stop := uint(0)
		mut epoch := u64(0)
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
					if with_epoch {
						epoch = u64(C.atomic_load_u32(&reg.ranges[i].epoch_low)) |
							(u64(C.atomic_load_u32(&reg.ranges[i].epoch_high)) << 32)
					}
				}
			}
		}
		if C.atomic_load_u32(&g_arena_seq) == seq {
			return owner, stop, epoch
		}
	}
	return nil, 0, 0
}

@[inline; unsafe]
fn arena_registry_find(addr uint) (&VArena, uint) {
	owner, stop, _ := unsafe { arena_registry_lookup(addr, false) }
	return owner, stop
}

// arena_owner returns the arena that owns `ptr` (nil for any other memory) and
// the end of the chunk that contains it.
@[unsafe]
fn arena_owner(ptr voidptr) (&VArena, uint) {
	addr := uint(ptr)
	unsafe {
		// Only this thread changes the arenas on its own stack, so they can be
		// searched without the registry. They own most memory freed in a scope.
		mut a := g_arena_top
		for a != nil {
			mut c := a.chunk
			for c != nil {
				if addr >= uint(c.start) && addr < uint(c.stop) {
					return a, uint(c.stop)
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
			arena_raw_free(chunk)
		}
	}
}

// arena_grow adds a chunk with room for at least `need` bytes.
@[noinline; unsafe]
fn arena_grow(mut a VArena, need int) &VArenaChunk {
	mut failure_size := 0
	result := unsafe { arena_try_grow(mut a, need, mut failure_size) }
	if isnil(result) {
		if failure_size == -1 { panic_allocation_failure(arena_chunk_limit_message) }
		_memory_panic(@FN, failure_size)
	}
	return result
}

// Commit chunk policy and linkage only after both the chunk and its registry
// entry exist. Failure releases the unpublished chunk and leaves a unchanged.
@[noinline; unsafe]
fn arena_try_grow(mut a VArena, need int, mut failure_size int) &VArenaChunk {
	mut size := a.chunk_size
	mut next_size := a.chunk_size
	if need > size {
		size = need
	} else if next_size < a.max_chunk_size {
		next_size = if next_size > a.max_chunk_size / 2 { a.max_chunk_size } else { next_size * 2 }
	}
	header := arena_chunk_header_size()
	if size <= 0 || uint(size) > (~uint(0) >> 1) - uint(header) {
		failure_size = size
		return unsafe { nil }
	}
	mut total := header + size
	if !isnil(a.budget) {
		total = arena_reserve_budget(a.budget, need + header, total)
		if total == 0 {
			failure_size = -1
			return unsafe { nil }
		}
		size = total - header
	}
	mut mem := voidptr(unsafe { nil })
	$if gcboehm ? {
		// Keep GC objects referenced only by arena memory alive.
		mem = C.GC_MALLOC_UNCOLLECTABLE(uint(total))
	} $else {
		mem = unsafe { arena_raw_malloc(uint(total)) }
	}
	if mem == unsafe { nil } {
		if !isnil(a.budget) { arena_finish_budget(a.budget, total, false) }
		failure_size = total
		return unsafe { nil }
	}
	unsafe {
		mut c := &VArenaChunk(mem)
		c.prev = a.chunk
		c.start = &u8(mem) + header
		c.cursor = c.start
		c.stop = c.start + size
		if !arena_registry_try_add(a, c, mut failure_size) {
			$if gcboehm ? {
				C.GC_FREE(mem)
			} $else {
				arena_raw_free(mem)
			}
			if !isnil(a.budget) { arena_finish_budget(a.budget, total, false) }
			return nil
		}
		if !isnil(a.budget) { arena_finish_budget(a.budget, total, true) }
		a.chunk_size = next_size
		a.chunk = c
		return c
	}
}

@[unsafe]
fn arena_alloc(mut a VArena, n int, align int) &u8 {
	default_align := arena_align()
	fixed_align := if align > default_align { align } else { default_align }
	// Like malloc, fail for a size that no chunk can hold, before computing the
	// size of that chunk overflows.
	if uint(n) > (~uint(0) >> 1) - uint(fixed_align + arena_chunk_header_size()) {
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

// The explicit fallible path shares chunk/registry management with the normal
// allocator. Keeping the normal bump path separate adds no per-allocation test
// for an allocation-failure policy to implicit V allocation.
@[unsafe]
fn arena_try_alloc(mut a VArena, n int, align int) &u8 {
	default_align := arena_align()
	fixed_align := if align > default_align { align } else { default_align }
	if n < 0 || uint(n) > (~uint(0) >> 1) - uint(fixed_align + arena_chunk_header_size()) {
		return unsafe { nil }
	}
	unsafe {
		mut c := a.chunk
		mut p := &u8(nil)
		if c != nil { p = vmemory_align_up(c.cursor, fixed_align) }
		if c == nil || i64(c.stop) - i64(p) < i64(n) {
			mut failure_size := 0
			c = arena_try_grow(mut a, n + fixed_align, mut failure_size)
			if c == nil { return nil }
			p = vmemory_align_up(c.cursor, fixed_align)
		}
		c.cursor = p + n
		a.last = p
		return p
	}
}

// arena_alloc_current returns `n` bytes from the current thread's arena, or
// nil when no arena is active.
fn arena_alloc_current(n int, align int) &u8 {
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
fn arena_realloc(old_data &u8, old_size int, new_size int) &u8 {
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
		if voidptr(top) == voidptr(owner) && top.last == old_data {
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
		available := int(chunk_stop - uint(old_data))
		if available < n {
			n = available
		}
		// A new block in the same chunk starts at or after the end of the old
		// one, so the bytes before it hold all of the old block.
		if uint(new_ptr) > uint(old_data) && uint(new_ptr) - uint(old_data) < uint(n) {
			n = int(uint(new_ptr) - uint(old_data))
		}
		C.memcpy(new_ptr, old_data, uint(n))
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
			if voidptr(c) != voidptr(keep) {
				bytes := int(c.stop - c.start) + arena_chunk_header_size()
				arena_chunk_release(c)
				if !isnil(a.budget) { arena_release_budget(a.budget, bytes) }
			}
			c = prev
		}
		if keep != nil {
			$if gcboehm ? {
				// Stale pointers in reused memory would keep GC objects alive.
				C.memset(keep.start, 0, uint(i64(keep.cursor) - i64(keep.start)))
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
pub fn v_arena_new(chunk_size int) voidptr {
	handle := unsafe { v_arena_try_new(chunk_size) }
	if isnil(handle) { _memory_panic(@FN, int(sizeof(VArena))) }
	return handle
}

// v_arena_try_new creates an arena control block, returning nil on allocation failure.
@[unsafe]
pub fn v_arena_try_new(chunk_size int) voidptr {
	$if prealloc || vgc ?|| vinix {
		panic('arena: scoped arenas are not supported with -prealloc, -gc vgc or on Vinix')
	}
	$if freestanding {
		$if native || !(linux || macos) || !(amd64 || arm64) {
			panic('arena: freestanding arenas require the Linux or macOS amd64/arm64 C TLS runtime')
		}
	}
	mut a := unsafe { &VArena(arena_raw_calloc(sizeof(VArena))) }
	if isnil(a) {
		return unsafe { nil }
	}
	a.budget = arena_inherit_budget()
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
	a.epoch = arena_next_epoch()
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
	if voidptr(top) != voidptr(a) {
		panic('arena: pop() of an arena that is not the innermost active arena of this thread')
	}
	g_arena_top = a.below
	a.below = unsafe { nil }
	C.v_prealloc_atomic_cas_i32(&a.state, 1, 0)
}

// v_arena_suspend temporarily selects the ordinary allocator for runtime
// bookkeeping, without popping or releasing any arena. This is a low-level
// runtime hook: pair it with v_arena_resume on the same thread in LIFO order,
// before invoking user callbacks. The returned arena must remain alive.
@[inline; unsafe]
pub fn v_arena_suspend() voidptr {
	previous := voidptr(g_arena_top)
	g_arena_top = unsafe { nil }
	return previous
}

// v_arena_resume restores an allocator suspended by v_arena_suspend. No arena
// may remain pushed inside the suspended scope. A nil token is valid for a
// nested suspension or a thread that had no current arena.
@[inline; unsafe]
pub fn v_arena_resume(previous voidptr) {
	if voidptr(g_arena_top) != unsafe { nil } {
		panic('arena: resume() with an active inner arena')
	}
	g_arena_top = unsafe { &VArena(previous) }
}

// v_arena_scope_enter selects an allocator for arena.Allocator.run. The returned
// state must be restored on this thread in nested order by v_arena_scope_leave.
// Unlike suspend/resume, this can select an inactive arena or a current ancestor.
// The caller keeps the selected and hidden arenas alive throughout the scope.
@[unsafe]
pub fn v_arena_scope_enter(handle voidptr) VArenaSelection {
	previous := voidptr(g_arena_top)
	if handle == unsafe { nil } {
		g_arena_top = unsafe { nil }
		return VArenaSelection{ previous: previous }
	}
	mut a := arena_checked(handle)
	mut cursor := g_arena_top
	mut active_here := false
	for cursor != unsafe { nil } {
		if voidptr(cursor) == handle {
			active_here = true
			break
		}
		cursor = cursor.below
	}
	if !active_here {
		if C.v_prealloc_atomic_cas_i32(&a.state, 0, 1) == 0 {
			panic('arena: selected allocator is active outside the current allocator stack')
		}
		a.below = g_arena_top
	}
	g_arena_top = a
	return VArenaSelection{ previous: previous, selected: handle, pushed: !active_here }
}

// v_arena_scope_leave restores the matching arena.Allocator.run scope. The work
// must have popped its inner arenas and restored its own nested selections.
@[unsafe]
pub fn v_arena_scope_leave(selection &VArenaSelection) {
	if voidptr(g_arena_top) != selection.selected {
		panic('arena: selected allocator scope ended with another allocator active')
	}
	if selection.pushed {
		v_arena_pop(selection.selected)
	} else {
		g_arena_top = unsafe { &VArena(selection.previous) }
	}
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
	arena_registry_begin_write()
	a.epoch = arena_next_epoch()
	// reset keeps one chunk. Invalidate tokens for it even if its address is
	// reused by the very next allocation. Never query a freed control block.
	if a.chunk != unsafe { nil } {
		unsafe {
			reg := g_arena_registry
			i := arena_registry_upper(reg, int(g_arena_ranges_len), uint(a.chunk.start)) - 1
			C.atomic_store_u32(&reg.ranges[i].epoch_low, u32(a.epoch))
			C.atomic_store_u32(&reg.ranges[i].epoch_high, u32(a.epoch >> 32))
		}
	}
	arena_registry_end_write()
	C.v_prealloc_atomic_cas_i32(&a.state, 2, 0)
}

// v_arena_lifetime_token returns the lifetime epoch for a live arena chunk,
// or zero for other addresses. This is the low-level hook behind
// arena.lifetime_token; use that. It never dereferences ptr or its former owner.
pub fn v_arena_lifetime_token(ptr voidptr) u64 {
	if C.atomic_load_u32(&g_arena_hooks_ready) == 0 {
		return 0
	}
	_, _, epoch := unsafe { arena_registry_lookup(uint(ptr), true) }
	return epoch
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
		arena_drop_budget(a.budget)
		a.magic = 0
		arena_raw_free(a)
	}
}

// v_arena_current returns the innermost arena of the current thread, or nil.
pub fn v_arena_current() voidptr {
	if C.atomic_load_u32(&g_arena_hooks_ready) == 0 {
		return unsafe { nil }
	}
	return g_arena_top
}

// v_arena_calloc_in allocates in a selected arena or V's ordinary allocator.
// It is the low-level hook behind arena.Allocator.calloc; use that. The caller
// keeps the arena alive and synchronizes reset/free with use of the allocation.
@[unsafe]
pub fn v_arena_calloc_in(handle voidptr, size int) &u8 {
	if size < 0 {
		panic('arena: negative allocation size')
	}
	if size == 0 {
		return unsafe { nil }
	}
	if handle == unsafe { nil } {
		// Reuse the canonical allocation path, including GC, tracking and tracing.
		// In particular, never clear the thread's current arena temporarily.
		return vcalloc_impl(size, false)
	}
	mut a := unsafe { arena_checked(handle) }
	mut cursor := g_arena_top
	mut active_here := false
	for cursor != unsafe { nil } {
		if voidptr(cursor) == voidptr(a) {
			active_here = true
			break
		}
		cursor = cursor.below
	}
	if !active_here && C.v_prealloc_atomic_cas_i32(&a.state, 0, 2) == 0 {
		panic('arena: allocation in an arena active on another thread or being modified')
	}
	if active_here {
		result := unsafe { arena_alloc(mut a, size, 0) }
		unsafe { C.memset(result, 0, size) }
		return result
	}
	defer { C.v_prealloc_atomic_cas_i32(&a.state, 2, 0) }
	result := unsafe { arena_alloc(mut a, size, 0) }
	unsafe { C.memset(result, 0, size) }
	return result
}

// v_arena_try_calloc_in is the fallible explicit allocation hook. It returns
// none without allocating an error object, and never falls back from a selected
// arena to a different allocator. Use arena.Allocator.try_calloc instead.
@[unsafe]
pub fn v_arena_try_calloc_in(handle voidptr, size int) ?&u8 {
	if size < 0 || uint(size) > (~uint(0) >> 1) - uint(arena_align() + arena_chunk_header_size()) {
		return none
	}
	if size == 0 { return unsafe { &u8(nil) } }
	if handle == unsafe { nil } {
		result := vcalloc_scoped_impl(size, false, false, true)
		if result == unsafe { nil } { return none }
		return result
	}
	mut a := arena_checked(handle)
	mut cursor := g_arena_top
	mut active_here := false
	for cursor != unsafe { nil } {
		if voidptr(cursor) == voidptr(a) {
			active_here = true
			break
		}
		cursor = cursor.below
	}
	if !active_here && C.v_prealloc_atomic_cas_i32(&a.state, 0, 2) == 0 {
		panic('arena: allocation in an arena active on another thread or being modified')
	}
	defer {
		if !active_here { C.v_prealloc_atomic_cas_i32(&a.state, 2, 0) }
	}
	result := unsafe { arena_try_alloc(mut a, size, 0) }
	if result == unsafe { nil } { return none }
	unsafe { C.memset(result, 0, size) }
	return result
}

// v_arena_stats returns the bytes handed out by an arena since its last
// reset, and the bytes of its chunks.
pub fn v_arena_stats(handle voidptr) (int, int) {
	a := arena_checked(handle)
	mut used := i64(0)
	mut capacity := i64(0)
	mut c := a.chunk
	for c != unsafe { nil } {
		used += i64(c.cursor) - i64(c.start)
		capacity += i64(c.stop) - i64(c.start)
		c = c.prev
	}
	return int(used), int(capacity)
}

// v_arena_owns reports whether `ptr` points into the memory of an arena.
pub fn v_arena_owns(handle voidptr, ptr voidptr) bool {
	a := arena_checked(handle)
	addr := uint(ptr)
	mut c := a.chunk
	for c != unsafe { nil } {
		if addr >= uint(c.start) && addr < uint(c.stop) {
			return true
		}
		c = c.prev
	}
	return false
}
