// vtest vflags: -prealloc -gc none
module types

import v.flat

fn test_pass2_promotion_retains_scoped_variant_payloads() {
	$if prealloc {
		name_id := flat.source_type_id('ScopedType')
		mut values := []Type{len: 24, init: builtin_void_type}
		scope := unsafe { prealloc_scope_begin() }
		values[0] = Type(Void{})
		values[1] = Type(Unknown{ reason: 'scoped unknown' })
		values[2] = Type(Primitive{ props: .integer, size: 32 })
		values[3] = Type(String{})
		values[4] = Type(Char{})
		values[5] = Type(Rune{})
		values[6] = Type(ISize{})
		values[7] = Type(USize{})
		values[8] = Type(Nil{})
		values[9] = Type(None{})
		values[10] = Type(Array{ elem_type: &builtin_int_type })
		values[11] = Type(ArrayFixed{ elem_type: &builtin_int_type, len: 2 })
		values[12] = Type(Channel{ elem_type: &builtin_int_type })
		values[13] = Type(Map{ key_type: &builtin_string_type, value_type: &builtin_int_type })
		values[14] = Type(Pointer{ base_type: &builtin_int_type })
		values[15] = Type(FnType{ return_type: &builtin_int_type })
		values[16] = Type(OptionType{ base_type: &builtin_int_type })
		values[17] = Type(ResultType{ base_type: &builtin_int_type })
		values[18] = Type(Struct{ name_id: name_id })
		values[19] = Type(Interface{ name_id: name_id })
		values[20] = Type(Enum{ name_id: name_id })
		values[21] = Type(SumType{ name_id: name_id })
		values[22] = Type(Alias{ name_id: name_id, base_type: &builtin_int_type })
		values[23] = Type(MultiReturn{ types: [builtin_int_type, builtin_string_type] })
		unsafe { prealloc_scope_leave(scope) }
		mut expected := []string{}
		mut promoted := []Type{}
		mut promotion := Pass2TypePromotion{}
		for value in values {
			expected << value.name()
			owned := promotion.promote(scope, value)
			assert !type_refers_to_scope(scope, owned)
			// Check storage directly too: the old scope query missed boxed payloads.
			if owned is Primitive {
				assert !unsafe { prealloc_scope_owns(scope, &owned.props) }
			}
			if owned is Struct {
				assert !unsafe { prealloc_scope_owns(scope, &owned.name_id) }
			}
			if owned is Pointer {
				assert !unsafe { prealloc_scope_owns(scope, &owned.base_type) }
			}
			promoted << owned
		}
		unsafe { prealloc_scope_free_after(scope) }
		for i, value in promoted {
			assert value.name() == expected[i]
		}
	}
}

// A stable descriptor can live outside a lane while its nested metadata lives inside it.
fn test_pass2_promotion_copies_lane_lists_of_a_descriptor_outside_the_lane() {
	$if prealloc {
		scope := unsafe { prealloc_scope_begin() }
		types := [Type(builtin_int_type), Type(Unknown{
			reason: 'lane reason'.clone()
		})]
		state := unsafe { prealloc_scope_suspend(scope) }
		multi := new_type_reference(Type(MultiReturn{ types: types }))
		unsafe { prealloc_scope_resume(scope, state) }
		unsafe { prealloc_scope_leave(scope) }
		assert !unsafe { prealloc_scope_owns(scope, multi) }
		assert unsafe { prealloc_scope_owns(scope, types.data) }
		assert type_refers_to_scope(scope, multi)
		mut promotion := &Pass2TypePromotion{}
		promoted := promotion.promote(scope, *multi)
		assert !type_refers_to_scope(scope, promoted)
		unsafe { prealloc_scope_free_after(scope) }
		if promoted is MultiReturn {
			assert promoted.types.len == 2
			first := promoted.types[0]
			second := promoted.types[1]
			assert first is Primitive
			if second is Unknown {
				assert second.reason == 'lane reason'
			} else {
				assert false, 'second type: ${second.type_name()}'
			}
		} else {
			assert false, 'promoted type: ${promoted.type_name()}'
		}
	}
}

fn test_pass2_promotion_finds_lane_storage_nested_in_outside_descriptors() {
	$if prealloc {
		scope := unsafe { prealloc_scope_begin() }
		params := [FnParam{ typ: &builtin_int_type }]
		mutable_params := [FnParam{ typ: &builtin_int_type, is_mut: true }]
		len_expr := 'lane_len'.clone()
		state := unsafe { prealloc_scope_suspend(scope) }
		fn_ptr := new_type_reference(Type(Pointer{
			base_type: new_type_reference(Type(FnType{
				params:      params
				return_type: &builtin_int_type
			}))
		}))
		mut_fn := new_type_reference(Type(FnType{
			params:      mutable_params
			return_type: &builtin_int_type
		}))
		fixed := new_type_reference(Type(OptionType{
			base_type: new_type_reference(Type(ArrayFixed{
				elem_type: &builtin_int_type
				len:       4
				len_expr:  len_expr
			}))
		}))
		outside := new_type_reference(Type(Map{
			key_type:   &builtin_string_type
			value_type: new_type_reference(Type(Array{ elem_type: &builtin_int_type }))
		}))
		unsafe { prealloc_scope_resume(scope, state) }
		unsafe { prealloc_scope_leave(scope) }
		for typ in [fn_ptr, mut_fn, fixed, outside] {
			assert !unsafe { prealloc_scope_owns(scope, typ) }
		}
		assert type_refers_to_scope(scope, fn_ptr)
		assert type_refers_to_scope(scope, mut_fn)
		assert type_refers_to_scope(scope, fixed)
		assert !type_refers_to_scope(scope, outside)
		mut promotion := &Pass2TypePromotion{}
		promoted_fn_ptr := promotion.promote(scope, *fn_ptr)
		promoted_mut_fn := promotion.promote(scope, *mut_fn)
		promoted_fixed := promotion.promote(scope, *fixed)
		promoted_outside := promotion.promote(scope, *outside)
		assert !type_refers_to_scope(scope, promoted_fn_ptr)
		assert !type_refers_to_scope(scope, promoted_mut_fn)
		assert !type_refers_to_scope(scope, promoted_fixed)
		// A graph with no lane storage keeps its actual descriptor references.
		assert semantic_types_equal(promoted_outside, outside)
		assert promoted_outside is Map
		assert outside is Map
		assert voidptr(promoted_outside.key_type) == voidptr(outside.key_type)
		assert voidptr(promoted_outside.value_type) == voidptr(outside.value_type)
		unsafe { prealloc_scope_free_after(scope) }
		assert promoted_fn_ptr.name() == '&fn(int) int'
		if promoted_fixed is OptionType {
			base := promoted_fixed.base_type
			if base is ArrayFixed {
				assert base.len == 4
				assert base.len_expr == 'lane_len'
			} else {
				assert false, 'option base type: ${base.type_name()}'
			}
		} else {
			assert false, 'promoted type: ${promoted_fixed.type_name()}'
		}
		if promoted_mut_fn is FnType {
			assert promoted_mut_fn.params.len == 1
			assert promoted_mut_fn.params[0].is_mut
			assert promoted_mut_fn.params[0].typ is Primitive
		} else {
			assert false, 'promoted type: ${promoted_mut_fn.type_name()}'
		}
	}
}

fn test_pass2_scope_query_terminates_on_cycles_and_still_visits_lane_edges() {
	$if prealloc {
		mut cycle := new_type_reference(Type(Void{}))
		// The fixture owns this descriptor; its test back edge requires a pointer write.
		unsafe { *cycle = Type(Map{ key_type: &builtin_int_type, value_type: cycle }) }
		scope := unsafe { prealloc_scope_begin() }
		lane_child := new_type_reference(Type(Unknown{ reason: 'lane child'.clone() }))
		unsafe { prealloc_scope_leave(scope) }
		assert !type_refers_to_scope(scope, cycle)
		mut promotion := Pass2TypePromotion{}
		kept := promotion.promote(scope, *cycle)
		assert kept is Map
		assert voidptr(kept.value_type) == voidptr(cycle)
		// The cyclic edge is visited before the other pending child.
		unsafe { *cycle = Type(Map{ key_type: lane_child, value_type: cycle }) }
		assert type_refers_to_scope(scope, cycle)
		// Break the test cycle and remove its lane reference before releasing the arena.
		unsafe { *cycle = Type(Void{}) }
		unsafe { prealloc_scope_free_after(scope) }
	}
}

fn test_pass2_promotion_follows_parameter_and_return_references_from_outside_lists() {
	$if prealloc {
		scope := unsafe { prealloc_scope_begin() }
		child := new_type_reference(Type(Unknown{ reason: 'lane edge'.clone() }))
		state := unsafe { prealloc_scope_suspend(scope) }
		parameter_root := new_type_reference(Type(FnType{
			params:      [FnParam{ typ: child }]
			return_type: &builtin_int_type
		}))
		return_root := new_type_reference(Type(FnType{ return_type: child }))
		// Retain the whole sum references before the parameter descriptor is smartcast.
		mut roots := []&Type{cap: 2}
		roots << parameter_root
		roots << return_root
		unsafe { prealloc_scope_resume(scope, state) }
		unsafe { prealloc_scope_leave(scope) }
		assert parameter_root is FnType
		assert !unsafe { prealloc_scope_owns(scope, parameter_root.params.data) }
		mut promotion := Pass2TypePromotion{}
		mut promoted := []Type{}
		for root in roots {
			assert !unsafe { prealloc_scope_owns(scope, root) }
			assert type_refers_to_scope(scope, root)
			owned := promotion.promote(scope, *root)
			assert !type_refers_to_scope(scope, owned)
			promoted << owned
		}
		unsafe { prealloc_scope_free_after(scope) }
		for i, typ in promoted {
			assert typ is FnType
			edge := if i == 0 { typ.params[0].typ } else { typ.return_type }
			assert edge is Unknown
			assert edge.reason == 'lane edge'
		}
	}
}
