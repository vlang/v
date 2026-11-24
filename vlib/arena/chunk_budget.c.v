module arena

// ChunkLimits bounds the bytes reserved from the backing allocator for arena
// chunks, including chunk headers. Nested arenas inherit all active limits.
pub struct ChunkLimits {
pub:
	max_live_bytes  ?u64
	max_total_bytes ?u64
}

// ChunkUsage counts successful backing allocations under a configured limit.
pub struct ChunkUsage {
pub:
	live_bytes  u64
	peak_bytes  u64
	total_bytes u64
}

// chunk_usage returns counters for the nearest configured budget. Its live
// count includes chunks retained across reset and all still-live child arenas.
pub fn (a &Arena) chunk_usage() ?ChunkUsage {
	configured, live, peak, total := v_arena_budget_usage(a.handle)
	if !configured { return none }
	return ChunkUsage{ live_bytes: live, peak_bytes: peak, total_bytes: total }
}

// AllocationFailure identifies the allocator failure observed by a recovering
// caller. The backing allocator includes V allocations made outside arenas.
pub enum AllocationFailure {
	backing_allocator
	chunk_limit
}

// allocation_failure reports the active allocator failure in a deferred panic
// handler. Read it alongside recover() before returning from the deferred block.
pub fn allocation_failure() ?AllocationFailure {
	active, limited := v_arena_allocation_failure()
	if !active { return none }
	if limited { return .chunk_limit }
	return .backing_allocator
}

// budget_active reports whether the current thread's selected arena has a
// chunk budget. Specialized allocators can use it to honor that owner.
@[inline]
pub fn budget_active() bool {
	return v_arena_has_current_budget()
}
