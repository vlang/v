module types

fn test_interner_owns_nested_function_parameters() {
	mut params := [FnParam{ typ: &Type(int_) }]
	mut interner := new_type_interner()
	original := Type(FnType{
		params:      params
		return_type: &Type(string_)
	})
	id, canonical := interner.canonicalize(original)
	assert isnil(interner.canonical_identity(original))
	entry := interner.canonical_identity(canonical)
	assert !isnil(entry)
	assert entry.id == id
	same_id, same := interner.canonicalize(canonical)
	assert same_id == id
	assert voidptr(same) == voidptr(canonical)
	params[0] = FnParam{ typ: &Type(bool_), is_mut: true }
	assert canonical.name() == 'fn(int) string', canonical.name()
	changed_id, changed := interner.canonicalize(original)
	assert changed_id != id
	assert changed is FnType
	assert changed.params[0].is_mut
	assert semantic_types_equal(changed.params[0].typ, Type(bool_))
	canonical_count := interner.len()
	stable := Type(FnType{
		params:      [FnParam{ typ: &Type(int_) }]
		return_type: &Type(string_)
	})
	interner.clear_recent_canonical_types()
	found := interner.probe(stable) or { panic('canonical payload changed after insertion') }
	assert semantic_types_equal(found, stable)
	probed := interner.recent_canonical_type(stable)
	assert !isnil(probed)
	assert probed.id == id
	interner.clear_recent_canonical_types()
	frozen := interner.probe_frozen(stable) or { panic('frozen probe missed an owned type') }
	assert voidptr(frozen) == voidptr(found)
	frozen_entry := interner.recent_canonical_type(stable)
	assert !isnil(frozen_entry)
	assert frozen_entry.id == id
	new_id, _ := interner.canonicalize(stable)
	assert new_id == id
	assert interner.len() == canonical_count
}

fn interner_owned_payload_reader(interner &TypeInterner, expected Type) {
	for _ in 0 .. 5000 {
		found := interner.probe(expected) or { panic('immutable canonical payload disappeared') }
		assert semantic_types_equal(found, expected)
	}
}

fn test_interner_readers_do_not_share_caller_mutation() {
	$if prealloc {
		return
	}
	mut params := [FnParam{ typ: &Type(int_) }]
	mut interner := new_type_interner()
	interner.canonicalize(Type(FnType{ params: params, return_type: &Type(string_) }))
	canonical_count := interner.len()
	expected := Type(FnType{ params: [FnParam{ typ: &Type(int_) }], return_type: &Type(string_) })
	first := spawn interner_owned_payload_reader(interner, expected)
	second := spawn interner_owned_payload_reader(interner, expected)
	for n in 0 .. 5000 {
		params[0] = FnParam{ typ: if n % 2 == 0 { &Type(bool_) } else { &Type(int_) }, is_mut: n % 2 == 0 }
	}
	first.wait()
	second.wait()
	assert interner.len() == canonical_count
}

fn test_recent_signature_lookup_accepts_new_arrays_and_verifies_unsampled_parameters() {
	mut interner := new_type_interner()
	canonical_id, canonical := interner.canonicalize(Type(FnType{
		params:      [FnParam{ typ: &Type(int_) }, FnParam{ typ: &Type(bool_) },
			FnParam{ typ: &Type(string_) }]
		return_type: &Type(void_)
	}))
	mut params := [FnParam{ typ: &Type(int_) }, FnParam{ typ: &Type(bool_) },
		FnParam{ typ: &Type(string_) }]
	query := Type(FnType{ params: params, return_type: &Type(void_) })
	entry := interner.recent_canonical_type(query)
	assert !isnil(entry)
	assert entry.id == canonical_id
	params[1] = FnParam{ typ: &Type(u8_), is_mut: true }
	assert isnil(interner.recent_canonical_type(query))
	assert canonical.name() == 'fn(int, bool, string)'
	multi_id, multi := interner.canonicalize(Type(MultiReturn{
		types: [Type(int_), Type(bool_), Type(string_)]
	}))
	mut values := [Type(int_), Type(bool_), Type(string_)]
	multi_query := Type(MultiReturn{ types: values })
	multi_entry := interner.recent_canonical_type(multi_query)
	assert !isnil(multi_entry)
	assert multi_entry.id == multi_id
	values[1] = Type(u8_)
	assert isnil(interner.recent_canonical_type(multi_query))
	assert multi.name() == '(int, bool, string)'
}

fn test_recent_primitive_cache_preserves_all_identity_bits_and_clear() {
	mut interner := new_type_interner()
	mut ids := []TypeId{}
	for bits in 0 .. 32 {
		for size in [0, 8, 16, 32, 64, 128, 255] {
			typ := Type(Primitive{ props: unsafe { Properties(bits) }, size: u8(size) })
			id, _ := interner.canonicalize(typ)
			assert id !in ids
			ids << id
		}
	}
	mut index := 0
	for bits in 0 .. 32 {
		for size in [0, 8, 16, 32, 64, 128, 255] {
			typ := Type(Primitive{ props: unsafe { Properties(bits) }, size: u8(size) })
			entry := interner.recent_canonical_type(typ)
			assert !isnil(entry)
			assert entry.id == ids[index]
			assert semantic_types_equal(entry.typ, typ)
			index++
		}
	}
	interner.clear_recent_canonical_types()
	input := Type(int_)
	assert isnil(interner.recent_canonical_type(input))
	found := interner.probe(input) or { panic('cleared primitive was lost') }
	assert semantic_types_equal(found, input)
	entry := interner.recent_canonical_type(input)
	assert !isnil(entry)
	assert voidptr(entry.typ) == voidptr(found)
}
