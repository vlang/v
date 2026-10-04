module types

fn test_scope_contains_tracks_visible_bindings_and_reuse() {
	mut parent := new_scope(unsafe { nil })
	parent.insert('inherited', unknown_type('unresolved binding'))
	parent.insert('shadowed', Type(string_))
	mut scope := new_scope(parent)
	for fast_lookup in [false, true] {
		scope.reset(parent)
		scope.fast_lookup = fast_lookup
		assert !scope.contains('')
		assert !scope.contains('missing')
		assert scope.contains('inherited'.clone())
		scope.insert('shadowed', Type(int_))
		assert scope.contains('shadowed'.clone())
		// Exercise direct-cache collisions and bloom-filter false positives.
		for i in 0 .. 100 {
			scope.insert('item_${i}', Type(bool_))
		}
		for i in 0 .. 100 {
			assert scope.contains('item_${i}'.clone())
			assert !scope.contains('absent_${i}')
		}
		mut nested := new_scope(scope)
		assert nested.contains('item_99')
		assert nested.contains('inherited')
		scope.reset(parent)
		assert !nested.contains('item_99')
		assert nested.contains('shadowed')
		assert scope.lookup('shadowed')? == Type(string_)
		scope.insert('replacement', Type(int_))
		assert nested.contains('replacement')
		assert !nested.contains('item_99')
	}
}

fn test_scope_lookup_keeps_binding_identity_for_copied_names_and_collisions() {
	mut parent := new_scope(unsafe { nil })
	parent.insert('value', Type(string_))
	mut scope := new_scope(parent)
	scope.fast_lookup = true
	original := 'value'.clone()
	owner := scope.insert_with_owner(original, Type(int_))
	copy := original.clone()
	assert voidptr(original.str) != voidptr(copy.str)
	assert scope.lookup(copy)? == Type(int_)
	assert scope.nearest_binding_owned_by(copy, owner)
	assert scope.lookup_owner(copy)?.storage_key() == owner.storage_key()

	// More bindings than cache slots exercise collisions and map fallback.
	for i in 0 .. 100 {
		scope.insert('item_${i}', Type(bool_))
	}
	for i in 0 .. 100 {
		assert scope.lookup('item_${i}')? == Type(bool_)
	}
	assert scope.lookup(copy)? == Type(int_)
	updated := scope.insert_with_owner(copy, Type(bool_))
	assert updated.storage_key() == owner.storage_key()
	assert scope.lookup(original)? == Type(bool_)

	scope.reset(parent)
	assert scope.lookup(original)? == Type(string_)
	assert !scope.nearest_binding_owned_by(original, owner)
	replacement := scope.insert_with_owner(copy, Type(int_))
	assert replacement.storage_key() != owner.storage_key()
	assert scope.lookup(original)? == Type(int_)
	assert scope.nearest_binding_owned_by(original, replacement)
}
