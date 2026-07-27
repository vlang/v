module types

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
