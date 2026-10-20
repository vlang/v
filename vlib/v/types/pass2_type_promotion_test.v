// vtest vflags: -prealloc -gc none
module types

fn scope_owns_type_payload(scope voidptr, typ Type) bool {
	w0, w1, _ := type_value_words(&typ)
	return unsafe { prealloc_scope_owns(scope, voidptr(usize(w0))) }
		|| unsafe { prealloc_scope_owns(scope, voidptr(usize(w1))) }
}

// A FastC-built compiler boxes sum payloads outside V's arenas. The lists and
// strings of a type made in a pass-2 lane still come from the lane's arena, so
// they must be copied out of it although the payload box is not in it.
fn test_pass2_promotion_copies_lane_lists_of_a_payload_boxed_outside_the_lane() {
	$if prealloc {
		scope := unsafe { prealloc_scope_begin() }
		types := [Type(builtin_int_type), Type(Unknown{
			reason: 'lane reason'.clone()
		})]
		// Box the payload outside the lane's arena, as FastC does.
		state := unsafe { prealloc_scope_suspend(scope) }
		multi := Type(MultiReturn{
			types: types
		})
		unsafe { prealloc_scope_resume(scope, state) }
		unsafe { prealloc_scope_leave(scope) }
		assert !scope_owns_type_payload(scope, multi)
		assert unsafe { prealloc_scope_owns(scope, types.data) }
		assert type_refers_to_scope(scope, multi)
		mut promotion := &Pass2TypePromotion{}
		promoted := promotion.promote(scope, multi)
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

fn test_pass2_promotion_finds_lane_storage_nested_in_outside_boxes() {
	$if prealloc {
		scope := unsafe { prealloc_scope_begin() }
		params := [Type(builtin_int_type)]
		params_mut := [true]
		len_expr := 'lane_len'.clone()
		state := unsafe { prealloc_scope_suspend(scope) }
		fn_ptr := Type(Pointer{
			base_type: Type(FnType{
				params:      params
				return_type: Type(builtin_int_type)
			})
		})
		mut_fn := Type(FnType{
			params_mut:  params_mut
			return_type: Type(builtin_int_type)
		})
		fixed := Type(OptionType{
			base_type: Type(ArrayFixed{
				elem_type: Type(builtin_int_type)
				len:       4
				len_expr:  len_expr
			})
		})
		outside := Type(Map{
			key_type:   Type(string_)
			value_type: Type(Array{
				elem_type: Type(builtin_int_type)
			})
		})
		unsafe { prealloc_scope_resume(scope, state) }
		unsafe { prealloc_scope_leave(scope) }
		for typ in [fn_ptr, mut_fn, fixed, outside] {
			assert !scope_owns_type_payload(scope, typ)
		}
		assert type_refers_to_scope(scope, fn_ptr)
		assert type_refers_to_scope(scope, mut_fn)
		assert type_refers_to_scope(scope, fixed)
		assert !type_refers_to_scope(scope, outside)
		mut promotion := &Pass2TypePromotion{}
		promoted_fn_ptr := promotion.promote(scope, fn_ptr)
		promoted_mut_fn := promotion.promote(scope, mut_fn)
		promoted_fixed := promotion.promote(scope, fixed)
		promoted_outside := promotion.promote(scope, outside)
		assert !type_refers_to_scope(scope, promoted_fn_ptr)
		assert !type_refers_to_scope(scope, promoted_mut_fn)
		assert !type_refers_to_scope(scope, promoted_fixed)
		// A type with nothing in the lane's arena is kept as it is.
		kept_w0, kept_w1, _ := type_value_words(&promoted_outside)
		outside_w0, outside_w1, _ := type_value_words(&outside)
		assert kept_w0 == outside_w0 && kept_w1 == outside_w1
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
			assert promoted_mut_fn.params_mut == [true]
		} else {
			assert false, 'promoted type: ${promoted_mut_fn.type_name()}'
		}
	}
}
