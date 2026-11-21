module types

import v.flat

fn test_scope_owner_bound_type_tracks_updates_and_reuse() {
	mut scope := new_scope(unsafe { nil })
	assert ScopeBindingOwner{}.bound_type() == none
	owner := scope.insert_with_owner_id(flat.source_type_id('value'), 'value', Type(int_))
	assert owner.bound_type()? == Type(int_)
	updated := scope.insert_with_owner_id(flat.source_type_id('value'.clone()), 'value'.clone(), Type(string_))
	assert updated.bound_type()? == Type(string_)
	$if ownership ? {
		assert owner.bound_type() == none
	} $else {
		assert owner.bound_type()? == Type(string_)
	}
	for i in 0 .. 100 {
		scope.insert_id(flat.source_type_id('item_${i}'), 'item_${i}', Type(bool_))
	}
	assert updated.bound_type()? == Type(string_)
	scope.reset(unsafe { nil })
	scope.insert_id(flat.source_type_id('value'), 'value', Type(bool_))
	assert updated.bound_type() == none
}
