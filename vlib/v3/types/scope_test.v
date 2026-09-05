module types

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
