// vtest vflags: -prealloc -gc none
module types

import v.flat

fn test_type_interner_owns_payloads_from_a_left_worker_scope() {
	$if prealloc {
		mut interner := new_type_interner()
		scope := unsafe { prealloc_scope_begin() }
		borrowed := Type(FnType{
			params:      [Type(ArrayFixed{
				elem_type: Type(Struct{
					name_id: flat.source_type_id('C.WorkerPayload'.clone())
				})
				len_expr:  'worker_count'.clone()
			})]
			params_mut:  [true]
			return_type: Type(Alias{
				name_id:   flat.source_type_id('WorkerResult'.clone())
				base_type: Type(Struct{
					name_id: flat.source_type_id('C.WorkerResult'.clone())
				})
			})
		})
		unsafe { prealloc_scope_leave(scope) }
		// Canonical types must own payloads borrowed from a caller's arena before
		// that arena is released.
		id, canonical := interner.canonicalize(borrowed)
		assert canonical is FnType
		assert !unsafe { prealloc_scope_owns(scope, canonical.params.data) }
		assert !unsafe { prealloc_scope_owns(scope, canonical.params_mut.data) }
		param := canonical.params[0]
		assert param is ArrayFixed
		assert !unsafe { prealloc_scope_owns(scope, param.len_expr.str) }
		elem := param.elem_type
		assert elem is Struct
		assert !unsafe { prealloc_scope_owns(scope, elem.name().str) }
		result := canonical.return_type
		assert result is Alias
		assert !unsafe { prealloc_scope_owns(scope, result.name().str) }
		unsafe { prealloc_scope_free_after(scope) }

		expected := Type(FnType{
			params:      [Type(ArrayFixed{
				elem_type: Type(Struct{
					name_id: flat.source_type_id('C.WorkerPayload')
				})
				len_expr:  'worker_count'
			})]
			params_mut:  [true]
			return_type: Type(Alias{
				name_id:   flat.source_type_id('WorkerResult')
				base_type: Type(Struct{
					name_id: flat.source_type_id('C.WorkerResult')
				})
			})
		})
		second_id, second := interner.canonicalize(expected)
		assert second_id == id
		assert semantic_types_equal(second, expected)
		assert interner.source_type_id(id) == expected.source_type_id()
		probed := interner.probe(expected) or { panic('published type is missing') }
		assert semantic_types_equal(probed, expected)
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
			interner.source_type_id(id)
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
