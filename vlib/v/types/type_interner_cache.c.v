module types

// Entries are immutable after atomic publication. Their storage follows the
// canonical table; promotion clears the cache before releasing a stage arena.
@[direct_array_access; inline]
fn (i &TypeInterner) recent_canonical_type(t &Type) ?&CanonicalTypeEntry {
	slot := int(canonical_intern_cache_key(t) & 4095)
	entry := unsafe { &CanonicalTypeEntry(C.atomic_load_ptr(&i.recent[slot])) }
	if !isnil(entry) && semantic_types_equal(entry.typ, t) {
		return entry
	}
	return none
}

@[direct_array_access; inline]
fn (i &TypeInterner) remember_canonical_type(t &Type, entry &CanonicalTypeEntry) {
	slot := int(canonical_intern_cache_key(t) & 4095)
	unsafe {
		C.atomic_store_ptr(&i.recent[slot], entry)
	}
}

fn (i &TypeInterner) clear_recent_canonical_types() {
	for slot in 0 .. i.recent.len {
		unsafe {
			C.atomic_store_ptr(&i.recent[slot], nil)
		}
	}
}

// This forms a cache key without recursively hashing children. The owned entry is
// still compared structurally, so borrowed or mutated inputs cannot produce a hit
// merely because their addresses, lengths, or nominal spellings match.
@[inline]
fn canonical_intern_cache_key(t &Type) u64 {
	mut key := match t {
		Void { u64(1) }
		Unknown { u64(2) ^ (u64(t.reason.len) << 8) }
		Primitive { u64(3) ^ (u64(t.props) << 16) ^ (u64(t.size) << 8) }
		String { u64(4) }
		Char { u64(5) }
		Rune { u64(6) }
		ISize { u64(7) }
		USize { u64(8) }
		Nil { u64(9) }
		None { u64(10) }
		Array { u64(11) ^ canonical_intern_child_key(t.elem_type) }
		ArrayFixed { u64(12) ^ canonical_intern_child_key(t.elem_type) ^ (u64(t.len) << 8) }
		Channel { u64(13) ^ canonical_intern_child_key(t.elem_type) ^ (u64(t.is_mut) << 8) }
		Map {
			u64(14) ^ canonical_intern_child_key(t.key_type) ^ (canonical_intern_child_key(t.value_type) << 7)
		}
		Pointer { u64(15) ^ canonical_intern_child_key(t.base_type) }
		FnType {
			u64(16) ^ (u64(t.params.data) >> 3) ^ (u64(t.params.len) << 8) ^
				(u64(voidptr(t.return_type)) << 7) ^ u64(t.is_variadic)
		}
		OptionType { u64(17) ^ canonical_intern_child_key(t.base_type) }
		ResultType { u64(18) ^ canonical_intern_child_key(t.base_type) }
		Struct { u64(19) ^ (u64(t.name_id) << 8) }
		Interface { u64(20) ^ (u64(t.name_id) << 8) }
		Enum { u64(21) ^ (u64(t.name_id) << 8) ^ u64(t.is_flag) }
		SumType { u64(22) ^ (u64(t.name_id) << 8) }
		Alias { u64(23) ^ (u64(t.name_id) << 8) ^ canonical_intern_child_key(t.base_type) }
		MultiReturn { u64(24) ^ (u64(t.types.data) >> 3) ^ (u64(t.types.len) << 8) }
	}
	key = (key ^ (key >> 32)) * u64(0x9e3779b97f4a7c15)
	return key ^ (key >> 32)
}

// Scalar and nominal children have cheap content identities even when their
// temporary Type wrappers occupy different addresses. Other children stay shallow.
@[inline]
fn canonical_intern_child_key(t &Type) u64 {
	return match t {
		Primitive { u64(3) ^ (u64(t.props) << 16) ^ (u64(t.size) << 8) }
		Void { u64(1) }
		String { u64(4) }
		Char { u64(5) }
		Rune { u64(6) }
		ISize { u64(7) }
		USize { u64(8) }
		Nil { u64(9) }
		None { u64(10) }
		Struct { u64(19) ^ (u64(t.name_id) << 8) }
		Interface { u64(20) ^ (u64(t.name_id) << 8) }
		Enum { u64(21) ^ (u64(t.name_id) << 8) ^ u64(t.is_flag) }
		SumType { u64(22) ^ (u64(t.name_id) << 8) }
		else { u64(voidptr(t)) >> 3 }
	}
}
