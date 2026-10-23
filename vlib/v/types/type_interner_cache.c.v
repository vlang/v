module types

// Cache misses return nil without constructing an Option. Entries are immutable
// after atomic publication. Their storage follows the
// canonical table; promotion clears the cache before releasing a stage arena.
@[direct_array_access; inline]
fn (i &TypeInterner) recent_canonical_type(t &Type) &CanonicalTypeEntry {
	if t is Primitive && u32(t.props) < 32 {
		// Properties and size are the complete primitive identity. Each slot
		// contains an immutable owned entry and needs no structural comparison.
		slot := (int(t.props) << 8) | int(t.size)
		entry := unsafe { &CanonicalTypeEntry(C.atomic_load_ptr(&i.primitives[slot])) }
		if !isnil(entry) {
			return entry
		}
		return unsafe { nil }
	}
	slot := int(canonical_intern_cache_key(t) & 4095)
	entry := unsafe { &CanonicalTypeEntry(C.atomic_load_ptr(&i.recent[slot])) }
	if !isnil(entry) && semantic_types_equal(entry.typ, t) {
		return entry
	}
	return unsafe { nil }
}

@[direct_array_access; inline]
fn (i &TypeInterner) remember_canonical_type(t &Type, entry &CanonicalTypeEntry) {
	if t is Primitive && u32(t.props) < 32 {
		slot := (int(t.props) << 8) | int(t.size)
		unsafe {
			C.atomic_store_ptr(&i.primitives[slot], entry)
		}
		return
	}
	slot := int(canonical_intern_cache_key(t) & 4095)
	unsafe {
		C.atomic_store_ptr(&i.recent[slot], entry)
	}
}

fn (i &TypeInterner) clear_recent_canonical_types() {
	for slot in 0 .. i.primitives.len {
		unsafe {
			C.atomic_store_ptr(&i.primitives[slot], nil)
		}
	}
	for slot in 0 .. i.recent.len {
		unsafe {
			C.atomic_store_ptr(&i.recent[slot], nil)
			C.atomic_store_ptr(&i.identities[slot], nil)
		}
	}
}

// This forms a cache key without recursively hashing children. The owned entry is
// still compared structurally, so borrowed or mutated inputs cannot produce a hit
// merely because their addresses, lengths, or nominal spellings match. Signature
// keys sample their contents so separately allocated parameter arrays can hit.
// Keep the common scalar and nominal cases small enough to inline at cache probes.
@[inline]
fn canonical_intern_cache_key(t &Type) u64 {
	mut key := u64(0)
	if t is Primitive {
		key = u64(3) ^ (u64(t.props) << 16) ^ (u64(t.size) << 8)
	} else if t is Struct {
		key = u64(19) ^ (u64(t.name_id) << 8)
	} else {
		return canonical_intern_cache_key_composite(t)
	}
	key = (key ^ (key >> 32)) * u64(0x9e3779b97f4a7c15)
	return key ^ (key >> 32)
}

@[direct_array_access]
fn canonical_intern_cache_key_composite(t &Type) u64 {
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
			mut signature := u64(16) ^ (u64(t.params.len) << 8) ^
				(canonical_intern_child_key(t.return_type) << 7) ^ u64(t.is_variadic)
			if t.params.len > 0 {
				first := t.params[0]
				last := t.params[t.params.len - 1]
				signature ^= (canonical_intern_child_key(first.typ) << 11) ^
					(canonical_intern_child_key(last.typ) << 19) ^
					(u64(first.is_mut) << 5) ^ (u64(last.is_mut) << 6)
			}
			signature
		}
		OptionType { u64(17) ^ canonical_intern_child_key(t.base_type) }
		ResultType { u64(18) ^ canonical_intern_child_key(t.base_type) }
		Struct { u64(19) ^ (u64(t.name_id) << 8) }
		Interface { u64(20) ^ (u64(t.name_id) << 8) }
		Enum { u64(21) ^ (u64(t.name_id) << 8) ^ u64(t.is_flag) }
		SumType { u64(22) ^ (u64(t.name_id) << 8) }
		Alias { u64(23) ^ (u64(t.name_id) << 8) ^ canonical_intern_child_key(t.base_type) }
		MultiReturn {
			mut signature := u64(24) ^ (u64(t.types.len) << 8)
			if t.types.len > 0 {
				signature ^= (canonical_intern_child_key(t.types[0]) << 11) ^
					(canonical_intern_child_key(t.types[t.types.len - 1]) << 19)
			}
			signature
		}
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

// Only canonical owned pointers are published here. Borrowed values still need
// structural verification, even when their address has appeared before.
@[direct_array_access; inline]
fn (i &TypeInterner) canonical_identity(t &Type) &CanonicalTypeEntry {
	slot := int((usize(t) >> 4) & 4095)
	entry := unsafe { &CanonicalTypeEntry(C.atomic_load_ptr(&i.identities[slot])) }
	if !isnil(entry) && voidptr(entry.typ) == voidptr(t) {
		return entry
	}
	return unsafe { nil }
}

@[direct_array_access; inline]
fn (i &TypeInterner) remember_canonical_identity(entry &CanonicalTypeEntry) {
	slot := int((usize(entry.typ) >> 4) & 4095)
	unsafe {
		C.atomic_store_ptr(&i.identities[slot], entry)
	}
}
