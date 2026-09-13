module types

fn test_cached_name_promotes_header_with_parent_owned_bytes() {
	$if prealloc {
		value := 'parent.name'.clone()
		scope := unsafe { prealloc_scope_begin() }
		name := cached_name(value)
		assert unsafe { prealloc_scope_owns(scope, name) }
		assert !unsafe { prealloc_scope_owns(scope, name.value.str) }
		unsafe { prealloc_scope_leave(scope) }
		promoted := promote_cached_name(name, scope)
		assert !unsafe { prealloc_scope_owns(scope, promoted) }
		assert !unsafe { prealloc_scope_owns(scope, promoted.value.str) }
		unsafe { prealloc_scope_free_after(scope) }
		assert promoted.value == 'parent.name'
	}
}

fn test_cached_name_promotes_scoped_bytes_and_preserves_parent_names() {
	$if prealloc {
		parent := cached_name('parent.name'.clone())
		scope := unsafe { prealloc_scope_begin() }
		value := 'worker.name'.clone()
		state := unsafe { prealloc_scope_suspend(scope) }
		name := cached_name(value)
		unsafe { prealloc_scope_resume(scope, state) }
		assert !unsafe { prealloc_scope_owns(scope, name) }
		assert unsafe { prealloc_scope_owns(scope, name.value.str) }
		unsafe { prealloc_scope_leave(scope) }
		promoted := promote_cached_name(name, scope)
		assert promote_cached_name(parent, scope) == parent
		assert !unsafe { prealloc_scope_owns(scope, promoted) }
		assert !unsafe { prealloc_scope_owns(scope, promoted.value.str) }
		unsafe { prealloc_scope_free_after(scope) }
		assert promoted.value == 'worker.name'
		assert parent.value == 'parent.name'
	}
}
