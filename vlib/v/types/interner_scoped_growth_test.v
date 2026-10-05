module types

fn test_interner_owns_edges_from_a_temporary_arena() {
	$if prealloc {
		mut interner := new_type_interner()
		scope := unsafe { prealloc_scope_begin() }
		child := &Type(Struct{ name: 'temporary.Record'.clone() })
		borrowed := Type(Pointer{ base_type: child })
		unsafe { prealloc_scope_leave(scope) }
		_, owned := interner.canonicalize(borrowed)
		assert owned is Pointer
		assert !unsafe { prealloc_scope_owns(scope, owned.base_type) }
		assert owned.base_type is Struct
		assert !unsafe { prealloc_scope_owns(scope, owned.base_type.name.str) }
		unsafe { prealloc_scope_free_after(scope) }
		assert owned.name() == '&temporary.Record'
		_, canonical := interner.canonicalize(Type(Struct{ name: 'temporary.Record' }))
		assert voidptr(owned.base_type) == voidptr(canonical)
	}
}

fn test_type_interner_promotion_preserves_canonical_edges() {
	$if prealloc {
		mut interner := new_type_interner()
		scope := unsafe { prealloc_scope_begin() }
		child := &Type(Struct{ name: 'scoped.Record'.clone() })
		parent_id, _ := interner.canonicalize(Type(Pointer{ base_type: child }))
		unsafe { prealloc_scope_leave(scope) }
		interner.promote_from(0, scope)
		unsafe { prealloc_scope_free_after(scope) }
		parent := interner.types[int(parent_id)]
		assert parent is Pointer
		_, canonical := interner.canonicalize(Type(Struct{ name: 'scoped.Record' }))
		assert voidptr(parent.base_type) == voidptr(canonical)
		assert parent.name() == '&scoped.Record'
	}
}

fn test_type_interner_promotes_scoped_slice_growth() {
	$if prealloc {
		mut interner := new_type_interner()
		interner.reserve(4)
		scope := unsafe { prealloc_scope_begin() }
		for i in 0 .. 256 {
			id, _ := interner.canonicalize(Type(Unknown{
				reason: 'scoped_type_${i}'
			}))
			interner.name(id)
		}
		assert unsafe { prealloc_scope_owns(scope, interner.types.data) }
		assert unsafe { prealloc_scope_owns(scope, interner.names.data) }
		unsafe { prealloc_scope_leave(scope) }

		interner.promote_from(0, scope)
		assert !unsafe { prealloc_scope_owns(scope, interner.types.data) }
		assert !unsafe { prealloc_scope_owns(scope, interner.names.data) }
		unsafe { prealloc_scope_free_after(scope) }

		id, canonical := interner.canonicalize(Type(Unknown{
			reason: 'scoped_type_128'
		}))
		assert id == TypeId(128)
		assert canonical is Unknown
		assert canonical.reason == 'scoped_type_128'
	}
}

fn test_symbol_interner_promotes_scoped_slice_growth() {
	$if prealloc {
		mut interner := new_symbol_interner()
		interner.reserve(4)
		scope := unsafe { prealloc_scope_begin() }
		for i in 0 .. 256 {
			interner.intern('scoped_symbol_${i}')
		}
		assert unsafe { prealloc_scope_owns(scope, interner.names.data) }
		unsafe { prealloc_scope_leave(scope) }

		interner.promote_from(0, scope)
		assert !unsafe { prealloc_scope_owns(scope, interner.names.data) }
		unsafe { prealloc_scope_free_after(scope) }

		id, canonical := interner.intern('scoped_symbol_128')
		assert id == SymbolId(129)
		assert canonical == 'scoped_symbol_128'
		assert interner.name(id) == canonical
	}
}
