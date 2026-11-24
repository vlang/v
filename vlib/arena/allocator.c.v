module arena

// Allocator is a copyable handle for explicit zeroed allocation. It refers to
// an arena or V's ordinary allocator without changing the current allocator.
// An arena allocator must not be used after its Arena is freed. The handle does
// not retain that arena or synchronize its lifetime with other threads.
@[noinit]
pub struct Allocator {
	handle voidptr
}

// current_allocator captures the allocator currently serving this thread.
pub fn current_allocator() Allocator {
	return Allocator{ handle: v_arena_current() }
}

// heap_allocator returns V's ordinary allocator, even inside an arena scope.
// It uses the configured GC or manual allocator through V's allocation path.
pub fn heap_allocator() Allocator {
	return Allocator{}
}

// allocator returns an explicit allocator for a. The arena may be inactive or
// active on this thread. It must remain alive while the handle is used.
pub fn (a &Arena) allocator() Allocator {
	if a.handle == unsafe { nil } {
		panic('arena: allocator() of a freed arena')
	}
	return Allocator{ handle: a.handle }
}

// is_current reports whether this handle selects the current thread's allocator.
pub fn (allocator Allocator) is_current() bool {
	return allocator.handle == v_arena_current()
}

// calloc allocates size zeroed bytes with V's ordinary allocation alignment.
// Zero returns nil without allocating; a negative size panics. Allocation may
// target an inactive arena or an active ancestor on this thread. Allocating in
// an arena active on another thread or already being modified panics.
// The caller must keep an arena handle alive and synchronize reset/free with
// use of its allocations. Heap memory follows the configured GC/manual rules.
@[unsafe]
pub fn (allocator Allocator) calloc(size int) &u8 {
	return unsafe { v_arena_calloc_in(allocator.handle, size) }
}

// try_calloc allocates zeroed storage, or returns none for a negative/unsupported
// size or an allocation failure. Failure allocates no error object. Zero succeeds
// with a nil pointer. Arena failure preserves existing allocations and releases
// an unpublished chunk; it never falls back to the current arena or heap.
// Lifetime, thread access and alignment rules match calloc. Invalid handle or
// concurrent arena access still panics. Supports -gc none and Boehm GC.
@[unsafe]
pub fn (allocator Allocator) try_calloc(size int) ?&u8 {
	return unsafe { v_arena_try_calloc_in(allocator.handle, size) }
}

// run calls work synchronously with this allocator serving implicit V allocations,
// then restores the caller's allocator, including when a panic unwinds the call.
// The arena may be inactive, current, or an ancestor in the current allocator stack.
// An arena active outside that stack is rejected. Work must balance its own pushed
// arenas and nested selections. Keep every involved arena alive, synchronize its
// lifetime, and respect the lifetime of allocations retained in context. Passing
// context explicitly lets a noncapturing callback avoid a closure allocation.
@[unsafe]
pub fn (allocator Allocator) run[T](mut context T, work fn (mut T)) {
	selection := unsafe { v_arena_scope_enter(allocator.handle) }
	defer { unsafe { v_arena_scope_leave(&selection) } }
	work(mut context)
}
