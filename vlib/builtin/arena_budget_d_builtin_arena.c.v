module builtin

const arena_chunk_limit_message = 'arena: chunk reservation limit exceeded'

// v_arena_allocation_failure identifies a physical failure or chunk denial in
// the current deferred panic handler. A user panic with the same text has no
// reserved record, and a nested ordinary panic masks its enclosing failure.
pub fn v_arena_allocation_failure() (bool, bool) {
	if !g_panic_state.memory_record_active
		|| g_panic_state.memory_record_index != g_panic_state.len - 1 {
		return false, false
	}
	return true, g_panic_state.memory_record.msg == arena_chunk_limit_message
}

// v_arena_has_current_budget lets specialized backing allocators honor a
// caller-selected bounded arena without acquiring the registry lock.
@[inline]
pub fn v_arena_has_current_budget() bool {
	return !isnil(g_arena_top) && !isnil(g_arena_top.budget)
}

// v_arena_budget_oom reports an infallible arena construction failure.
@[noreturn]
pub fn v_arena_budget_oom() {
	_memory_panic(@FN, int(sizeof(VArenaBudget)))
}

// Arena controls own references to their nearest configured budget. Each budget
// owns a reference to its parent, so a child can outlive its creating arena.
// Only chunk growth, release and control construction take the registry lock.
struct VArenaBudget {
	parent    &VArenaBudget = unsafe { nil }
	max_live  u64
	max_total u64
mut:
	refs    int
	pending u64
	live    u64
	peak    u64
	total   u64
}

fn arena_inherit_budget() &VArenaBudget {
	parent := g_arena_top
	if isnil(parent) || isnil(parent.budget) { return unsafe { nil } }
	mut b := parent.budget
	arena_lock()
	b.refs++
	arena_unlock()
	return b
}

fn arena_drop_budget(group &VArenaBudget) {
	mut b := unsafe { group }
	if isnil(b) { return }
	arena_lock()
	for !isnil(b) {
		b.refs--
		if b.refs > 0 { break }
		parent := b.parent
		unsafe {
			arena_raw_free(b)
		}
		b = parent
	}
	arena_unlock()
}

// v_arena_try_set_budget attaches limits to a newly constructed, unpublished arena.
@[unsafe]
pub fn v_arena_try_set_budget(handle voidptr, max_live u64, max_total u64) bool {
	mut arena := arena_checked(handle)
	mut group := unsafe {
		&VArenaBudget(arena_raw_calloc(sizeof(VArenaBudget)))
	}
	if isnil(group) { return false }
	// Transfer the arena's inherited reference into the new group.
	unsafe { *group = VArenaBudget{ parent: arena.budget, max_live: max_live, max_total: max_total, refs: 1 } }
	arena.budget = group
	return true
}

// Precharge outstanding allocations across threads before requesting backing
// memory. Clamp geometric growth to the available capacity when a smaller
// chunk still fits. A rejected request never changes a published counter.
fn arena_reserve_budget(group &VArenaBudget, need int, desired int) int {
	mut b := unsafe { group }
	arena_lock()
	mut size := u64(desired)
	for !isnil(b) {
		live_room := b.max_live - b.live - b.pending
		total_room := b.max_total - b.total - b.pending
		if size > live_room { size = live_room }
		if size > total_room { size = total_room }
		b = b.parent
	}
	if size >= u64(need) {
		b = unsafe { group }
		for !isnil(b) {
			b.pending += size
			b = b.parent
		}
	} else {
		size = 0
	}
	arena_unlock()
	return int(size)
}

fn arena_finish_budget(group &VArenaBudget, size int, committed bool) {
	mut b := unsafe { group }
	arena_lock()
	for !isnil(b) {
		b.pending -= u64(size)
		if committed {
			b.live += u64(size)
			b.total += u64(size)
			if b.peak < b.live { b.peak = b.live }
		}
		b = b.parent
	}
	arena_unlock()
}

fn arena_release_budget(group &VArenaBudget, size int) {
	mut b := unsafe { group }
	arena_lock()
	for !isnil(b) {
		b.live -= u64(size)
		b = b.parent
	}
	arena_unlock()
}

// v_arena_budget_usage returns backing chunk bytes in this arena's nearest budget.
pub fn v_arena_budget_usage(handle voidptr) (bool, u64, u64, u64) {
	group := arena_checked(handle).budget
	if isnil(group) { return false, 0, 0, 0 }
	arena_lock()
	live, peak, total := group.live, group.peak, group.total
	arena_unlock()
	return true, live, peak, total
}
