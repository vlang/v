// vtest vflags: -prealloc -gc none
module types

import v.flat

fn test_repeated_scalar_interning_after_scope_promotion() {
	$if prealloc {
		mut interner := new_type_interner()
		input := Type(int_)
		scope := unsafe { prealloc_scope_begin() }
		id, scoped := interner.canonicalize(input)
		assert unsafe { prealloc_scope_owns(scope, scoped) }
		unsafe { prealloc_scope_leave(scope) }
		interner.promote_from(0, scope)
		promoted_id, promoted := interner.canonicalize(input)
		assert promoted_id == id
		assert voidptr(promoted) == voidptr(interner.types[int(id)])
		assert !unsafe { prealloc_scope_owns(scope, promoted) }
		unsafe { prealloc_scope_free_after(scope) }
		again_id, again := interner.canonicalize(input)
		assert again_id == id
		assert voidptr(again) == voidptr(promoted)
		assert semantic_types_equal(again, input)
	}
}

fn test_interner_owns_edges_from_a_temporary_arena() {
	$if prealloc {
		mut interner := new_type_interner()
		scope := unsafe { prealloc_scope_begin() }
		child := &Type(Struct{ name_id: flat.source_type_id('temporary.Record') })
		borrowed := Type(Pointer{ base_type: child })
		unsafe { prealloc_scope_leave(scope) }
		_, owned := interner.canonicalize(borrowed)
		assert owned is Pointer
		assert !unsafe { prealloc_scope_owns(scope, owned.base_type) }
		assert owned.base_type is Struct
		assert !unsafe { prealloc_scope_owns(scope, owned.base_type.name().str) }
		unsafe { prealloc_scope_free_after(scope) }
		assert owned.name() == '&temporary.Record'
		_, canonical := interner.canonicalize(Type(Struct{ name_id: flat.source_type_id('temporary.Record') }))
		assert voidptr(owned.base_type) == voidptr(canonical)
	}
}

fn test_type_interner_promotion_preserves_canonical_edges() {
	$if prealloc {
		mut interner := new_type_interner()
		scope := unsafe { prealloc_scope_begin() }
		child := &Type(Struct{ name_id: flat.source_type_id('scoped.Record') })
		parent_id, _ := interner.canonicalize(Type(Pointer{ base_type: child }))
		unsafe { prealloc_scope_leave(scope) }
		interner.promote_from(0, scope)
		unsafe { prealloc_scope_free_after(scope) }
		parent := interner.types[int(parent_id)]
		assert parent is Pointer
		_, canonical := interner.canonicalize(Type(Struct{ name_id: flat.source_type_id('scoped.Record') }))
		assert voidptr(parent.base_type) == voidptr(canonical)
		assert parent.name() == '&scoped.Record'
	}
}

fn test_type_interner_owns_payloads_from_a_left_worker_scope() {
	$if prealloc {
		mut interner := new_type_interner()
		scope := unsafe { prealloc_scope_begin() }
		borrowed := Type(FnType{
			params:      [FnParam{
				typ:    &Type(ArrayFixed{
					elem_type: &Type(Struct{
						name_id: flat.source_type_id('C.WorkerPayload'.clone())
					})
					len_expr:  'worker_count'.clone()
				})
				is_mut: true
			}]
			return_type: &Type(Alias{
				name_id:   flat.source_type_id('WorkerResult'.clone())
				base_type: &Type(Struct{
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
		param := canonical.params[0].typ
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
			params:      [FnParam{
				typ:    &Type(ArrayFixed{
					elem_type: &Type(Struct{
						name_id: flat.source_type_id('C.WorkerPayload')
					})
					len_expr:  'worker_count'
				})
				is_mut: true
			}]
			return_type: &Type(Alias{
				name_id:   flat.source_type_id('WorkerResult')
				base_type: &Type(Struct{
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
