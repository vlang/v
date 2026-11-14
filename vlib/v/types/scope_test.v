module types

import v.flat

fn test_scope_contains_tracks_visible_bindings_and_reuse() {
	mut parent := new_scope(unsafe { nil })
	parent.insert_id(flat.source_type_id('inherited'), 'inherited', unknown_type('unresolved binding'))
	parent.insert_id(flat.source_type_id('shadowed'), 'shadowed', Type(string_))
	mut scope := new_scope(parent)
	for fast_lookup in [false, true] {
		scope.reset(parent)
		scope.fast_lookup = fast_lookup
		assert !scope.contains_id(flat.source_type_id(''))
		assert !scope.contains_id(flat.source_type_id('missing'))
		assert scope.contains_id(flat.source_type_id('inherited'.clone()))
		scope.insert_id(flat.source_type_id('shadowed'), 'shadowed', Type(int_))
		assert scope.contains_id(flat.source_type_id('shadowed'.clone()))
		// Exercise direct-cache collisions and bloom-filter false positives.
		for i in 0 .. 100 {
			scope.insert_id(flat.source_type_id('item_${i}'), 'item_${i}', Type(bool_))
		}
		for i in 0 .. 100 {
			assert scope.contains_id(flat.source_type_id('item_${i}'.clone()))
			assert !scope.contains_id(flat.source_type_id('absent_${i}'))
		}
		mut nested := new_scope(scope)
		assert nested.contains_id(flat.source_type_id('item_99'))
		assert nested.contains_id(flat.source_type_id('inherited'))
		scope.reset(parent)
		assert !nested.contains_id(flat.source_type_id('item_99'))
		assert nested.contains_id(flat.source_type_id('shadowed'))
		assert scope.lookup_id(flat.source_type_id('shadowed'))? == Type(string_)
		scope.insert_id(flat.source_type_id('replacement'), 'replacement', Type(int_))
		assert nested.contains_id(flat.source_type_id('replacement'))
		assert !nested.contains_id(flat.source_type_id('item_99'))
	}
}

fn test_scope_lookup_keeps_binding_identity_for_copied_names_and_collisions() {
	mut parent := new_scope(unsafe { nil })
	parent.insert_id(flat.source_type_id('value'), 'value', Type(string_))
	mut scope := new_scope(parent)
	scope.fast_lookup = true
	original := 'value'.clone()
	owner := scope.insert_with_owner_id(flat.source_type_id(original), original, Type(int_))
	copy := original.clone()
	assert voidptr(original.str) != voidptr(copy.str)
	assert scope.lookup_id(flat.source_type_id(copy))? == Type(int_)
	assert scope.nearest_binding_owned_by_id(flat.source_type_id(copy), owner)
	assert scope.lookup_owner_id(flat.source_type_id(copy))?.storage_key() == owner.storage_key()

	// More bindings than cache slots exercise collisions and map fallback.
	for i in 0 .. 100 {
		scope.insert_id(flat.source_type_id('item_${i}'), 'item_${i}', Type(bool_))
	}
	for i in 0 .. 100 {
		assert scope.lookup_id(flat.source_type_id('item_${i}'))? == Type(bool_)
	}
	assert scope.lookup_id(flat.source_type_id(copy))? == Type(int_)
	updated := scope.insert_with_owner_id(flat.source_type_id(copy), copy, Type(bool_))
	assert updated.storage_key() == owner.storage_key()
	assert scope.lookup_id(flat.source_type_id(original))? == Type(bool_)

	scope.reset(parent)
	assert scope.lookup_id(flat.source_type_id(original))? == Type(string_)
	assert !scope.nearest_binding_owned_by_id(flat.source_type_id(original), owner)
	replacement := scope.insert_with_owner_id(flat.source_type_id(copy), copy, Type(int_))
	assert replacement.storage_key() != owner.storage_key()
	assert scope.lookup_id(flat.source_type_id(original))? == Type(int_)
	assert scope.nearest_binding_owned_by_id(flat.source_type_id(original), replacement)
}
