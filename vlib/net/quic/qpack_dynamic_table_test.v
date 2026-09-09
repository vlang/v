module quic

fn test_qpack_entry_size_formula() {
	assert qpack_entry_size('a', 'bb') == 1 + 2 + 32
	assert qpack_entry_size('', '') == 32
}

fn test_dynamic_table_starts_empty_with_zero_capacity() {
	t := QpackDynamicTable{}
	assert t.capacity() == 0
	assert t.size() == 0
	assert t.insert_count() == 0
}

fn test_dynamic_table_insert_rejects_entry_larger_than_capacity() {
	mut t := QpackDynamicTable{}
	t.set_capacity(10)
	if _ := t.insert('name', 'value') {
		assert false, 'entry (name+value+32 > 10) must be rejected'
	}
}

fn test_dynamic_table_insert_evicts_oldest_to_fit() {
	mut t := QpackDynamicTable{}
	t.set_capacity(qpack_entry_size('a', '1') + qpack_entry_size('b', '1')) // room for exactly 2
	t.insert('a', '1') or { panic('${err}') }
	t.insert('b', '1') or { panic('${err}') }
	assert t.insert_count() == 2
	// Third insert must evict the oldest ("a") to make room.
	t.insert('c', '1') or { panic('${err}') }
	assert t.insert_count() == 3
	if _ := t.get(0) {
		assert false, 'entry 0 should have been evicted'
	}
	e1 := t.get(1) or { panic('${err}') }
	assert e1.name == 'b'
	e2 := t.get(2) or { panic('${err}') }
	assert e2.name == 'c'
}

fn test_dynamic_table_set_capacity_zero_clears_table() {
	mut t := QpackDynamicTable{}
	t.set_capacity(1000)
	t.insert('a', '1') or { panic('${err}') }
	t.insert('b', '1') or { panic('${err}') }
	assert t.size() > 0
	t.set_capacity(0)
	assert t.size() == 0
	// Evicted, not un-inserted: insert_count (and thus future absolute
	// indices) must not roll back.
	assert t.insert_count() == 2
}

fn test_dynamic_table_can_insert_blocked_by_unacknowledged_entry() {
	mut t := QpackDynamicTable{}
	t.set_capacity(qpack_entry_size('a', '1')) // room for exactly ONE entry
	abs := t.insert('a', '1') or { panic('${err}') }
	t.add_ref(abs) or { panic('${err}') }
	// known_received_count=0 means abs 0 isn't acknowledged yet, so even
	// though ref_count were later released, it still isn't acknowledged --
	// eviction must be refused either way while referenced.
	assert !t.can_insert('b', '1', 0)
}

fn test_dynamic_table_can_insert_blocked_by_unacknowledged_even_with_zero_refs() {
	mut t := QpackDynamicTable{}
	t.set_capacity(qpack_entry_size('a', '1'))
	t.insert('a', '1') or { panic('${err}') }
	// No references outstanding, but known_received_count=0 means this
	// entry (abs 0) has NOT been acknowledged yet (0 is not < 0).
	assert !t.can_insert('b', '1', 0)
}

fn test_dynamic_table_can_insert_allowed_once_acknowledged_and_unreferenced() {
	mut t := QpackDynamicTable{}
	t.set_capacity(qpack_entry_size('a', '1'))
	t.insert('a', '1') or { panic('${err}') }
	// known_received_count=1 means abs 0 (< 1) is acknowledged; no references.
	assert t.can_insert('b', '1', 1)
}

fn test_dynamic_table_duplicate_reinserts_without_evicting_source_if_room() {
	mut t := QpackDynamicTable{}
	t.set_capacity(3 * qpack_entry_size('a', '1'))
	t.insert('a', '1') or { panic('${err}') }
	new_abs := t.duplicate(0) or { panic('${err}') }
	assert new_abs == 1
	e := t.get(1) or { panic('${err}') }
	assert e.name == 'a'
	assert e.value == '1'
}

fn test_dynamic_table_resolve_relative_from_insert_count() {
	mut t := QpackDynamicTable{}
	t.set_capacity(1000)
	t.insert('a', '1') or { panic('${err}') } // abs 0
	t.insert('b', '1') or { panic('${err}') } // abs 1
	t.insert('c', '1') or { panic('${err}') } // abs 2, insert_count now 3
	// relative 0 = most recently inserted = abs 2.
	assert t.resolve_relative_from_insert_count(0)! == 2
	assert t.resolve_relative_from_insert_count(2)! == 0
	if _ := t.resolve_relative_from_insert_count(3) {
		assert false, 'relative index 3 is out of range (only 3 entries)'
	}
}

fn test_dynamic_table_resolve_relative_from_base_distinct_from_insert_count_context() {
	mut t := QpackDynamicTable{}
	t.set_capacity(1000)
	t.insert('a', '1') or { panic('${err}') } // abs 0
	t.insert('b', '1') or { panic('${err}') } // abs 1
	// Base = 1 (a field section encoded before "b" was inserted): relative
	// 0 = abs (base-1) = abs 0, NOT abs 1 (which resolve_relative_from_
	// insert_count(0) would give against the current insert_count of 2).
	assert t.resolve_relative_from_base(0, 1)! == 0
	assert t.resolve_relative_from_insert_count(0)! == 1
}

fn test_dynamic_table_resolve_relative_from_base_rejects_index_at_or_above_base() {
	mut t := QpackDynamicTable{}
	t.set_capacity(1000)
	t.insert('a', '1') or { panic('${err}') }
	if _ := t.resolve_relative_from_base(1, 1) {
		assert false, 'relative index 1 with base 1 has no valid target (would be abs -1)'
	}
}

fn test_dynamic_table_resolve_post_base() {
	mut t := QpackDynamicTable{}
	t.set_capacity(1000)
	t.insert('a', '1') or { panic('${err}') } // abs 0
	// base=0: post-base 0 = abs 0.
	assert t.resolve_post_base(0, 0)! == 0
	t.insert('b', '1') or { panic('${err}') } // abs 1
	assert t.resolve_post_base(1, 0)! == 1
}

fn test_dynamic_table_resolve_post_base_rejects_not_yet_inserted() {
	mut t := QpackDynamicTable{}
	t.set_capacity(1000)
	t.insert('a', '1') or { panic('${err}') } // insert_count now 1
	if _ := t.resolve_post_base(1, 0) {
		assert false, 'post-base 1 with base 0 references abs 1, which does not exist yet'
	}
}

fn test_dynamic_table_get_rejects_evicted_entry() {
	mut t := QpackDynamicTable{}
	t.set_capacity(qpack_entry_size('a', '1'))
	t.insert('a', '1') or { panic('${err}') }
	t.insert('b', '1') or { panic('${err}') } // evicts "a"
	if _ := t.get(0) {
		assert false, 'entry 0 was evicted'
	}
}

fn test_dynamic_table_release_ref_is_noop_below_zero() {
	mut t := QpackDynamicTable{}
	t.set_capacity(1000)
	abs := t.insert('a', '1') or { panic('${err}') }
	t.release_ref(abs) or { panic('${err}') } // no add_ref happened; must not panic or go negative
	e := t.get(abs) or { panic('${err}') }
	assert e.ref_count == 0
}

fn test_dynamic_table_find_exact_only_considers_entries_before_bound() {
	mut t := QpackDynamicTable{}
	t.set_capacity(1000)
	t.insert('a', '1') or { panic('${err}') } // abs 0
	t.insert('a', '1') or { panic('${err}') } // abs 1, duplicate name+value
	// before=1 should only see abs 0, not abs 1.
	found := t.find_exact('a', '1', 1) or { panic('expected a match below the bound') }
	assert found == 0
	if _ := t.find_exact('a', '1', 0) {
		assert false, 'before=0 should see no entries at all'
	}
}

fn test_dynamic_table_can_set_capacity_forbids_evicting_unacknowledged_entry() {
	mut t := QpackDynamicTable{}
	t.set_capacity(100)
	abs := t.insert('x', '1') or { panic('${err}') }
	t.add_ref(abs) or { panic('${err}') }
	// known_received_count=0 means abs 0 isn't acknowledged yet -- even with
	// no outstanding ref_count, it would still be non-evictable; ref_count=1
	// here makes it doubly so. Shrinking to 10 can't fit the 34-byte entry.
	assert !t.can_set_capacity(10, 0)
	assert t.can_set_capacity(100, 0) // no shrink needed -- always safe

	assert t.can_set_capacity(1000, 0) // growth -- always safe
}

fn test_dynamic_table_can_set_capacity_allows_evicting_acknowledged_unreferenced_entry() {
	mut t := QpackDynamicTable{}
	t.set_capacity(100)
	t.insert('x', '1') or { panic('${err}') }
	// known_received_count=1 acknowledges abs 0, and it was never add_ref'd.
	assert t.can_set_capacity(10, 1)
}

fn test_dynamic_table_can_set_capacity_rejects_negative_capacity_even_when_table_empty() {
	// Fresh-eyes /vreview finding on can_set_capacity itself (2026-08-19): the
	// eviction-simulation loop's tail case (ran out of entries before `freed`
	// reached what's needed) must return false, mirroring can_insert's own
	// `return freed >= needed` -- not an unconditional `return true`. An empty
	// table proves this without even needing entries: over = 0 - (-5) = 5,
	// nothing to evict (loop body never runs since entries.len == 0), so
	// freed(0) >= over(5) is false. Unreachable via the only current caller
	// (QpackEncoder.set_capacity, which never passes a negative value), but
	// this is a `pub` function and its correctness shouldn't depend on that.
	t := QpackDynamicTable{}
	assert !t.can_set_capacity(-5, 0)
}
