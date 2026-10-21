module types

// Entries are immutable after atomic publication. Their storage follows the
// canonical table; promotion clears the cache before releasing a stage arena.
@[direct_array_access; inline]
fn (i &TypeInterner) recent_canonical_type(t &Type) ?&CanonicalTypeEntry {
	slot := int(canonical_intern_cache_key(t) & 4095)
	entry := unsafe { &CanonicalTypeEntry(C.atomic_load_ptr(voidptr(&i.recent[slot]))) }
	if !isnil(entry) && semantic_types_equal(entry.typ, t) {
		return entry
	}
	return none
}

@[direct_array_access; inline]
fn (i &TypeInterner) remember_canonical_type(t &Type, entry &CanonicalTypeEntry) {
	slot := int(canonical_intern_cache_key(t) & 4095)
	C.atomic_store_ptr(voidptr(&i.recent[slot]), entry)
}

fn (i &TypeInterner) clear_recent_canonical_types() {
	for slot in 0 .. i.recent.len {
		C.atomic_store_ptr(voidptr(&i.recent[slot]), unsafe { nil })
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
		Array { u64(11) ^ (u64(voidptr(t.elem_type)) >> 3) }
		ArrayFixed { u64(12) ^ (u64(voidptr(t.elem_type)) >> 3) ^ (u64(t.len) << 8) }
		Channel { u64(13) ^ (u64(voidptr(t.elem_type)) >> 3) ^ (u64(t.is_mut) << 8) }
		Map { u64(14) ^ (u64(voidptr(t.key_type)) >> 3) ^ (u64(voidptr(t.value_type)) << 7) }
		Pointer { u64(15) ^ (u64(voidptr(t.base_type)) >> 3) }
		FnType {
			u64(16) ^ (u64(t.params.data) >> 3) ^ (u64(t.params.len) << 8) ^
				(u64(voidptr(t.return_type)) << 7) ^ u64(t.is_variadic)
		}
		OptionType { u64(17) ^ (u64(voidptr(t.base_type)) >> 3) }
		ResultType { u64(18) ^ (u64(voidptr(t.base_type)) >> 3) }
		Struct { u64(19) ^ (u64(t.name_id) << 8) }
		Interface { u64(20) ^ (u64(t.name_id) << 8) }
		Enum { u64(21) ^ (u64(t.name_id) << 8) ^ u64(t.is_flag) }
		SumType { u64(22) ^ (u64(t.name_id) << 8) }
		Alias { u64(23) ^ (u64(t.name_id) << 8) ^ (u64(voidptr(t.base_type)) >> 3) }
		MultiReturn { u64(24) ^ (u64(t.types.data) >> 3) ^ (u64(t.types.len) << 8) }
	}
	key = (key ^ (key >> 32)) * u64(0x9e3779b97f4a7c15)
	return key ^ (key >> 32)
}
