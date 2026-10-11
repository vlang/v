module types

fn test_interner_owns_nested_function_parameters() {
	mut params := [FnParam{ typ: &Type(int_) }]
	mut interner := new_type_interner()
	original := Type(FnType{
		params:      params
		return_type: &Type(string_)
	})
	id, canonical := interner.canonicalize(original)
	canonical_count := interner.len()
	params[0] = FnParam{ typ: &Type(bool_), is_mut: true }
	assert canonical.name() == 'fn(int) string', canonical.name()
	stable := Type(FnType{
		params:      [FnParam{ typ: &Type(int_) }]
		return_type: &Type(string_)
	})
	found := interner.probe(stable) or { panic('canonical payload changed after insertion') }
	assert semantic_types_equal(found, stable)
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
