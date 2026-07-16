module types

import v3.flat

struct SignatureDenseArrayLayoutForTest {
	key_bytes   int
	value_bytes int
mut:
	cap         int
	len         int
	deletes     u32
	all_deleted &u8 = unsafe { nil }
	keys        &u8 = unsafe { nil }
	values      &u8 = unsafe { nil }
}

struct SignatureMapLayoutForTest {
	key_bytes   int
	value_bytes int
mut:
	even_index      u32
	cached_hashbits u8
	shift           u8
	key_values      SignatureDenseArrayLayoutForTest
	metas           &u32 = unsafe { nil }
	extra_metas     u32
	has_string_keys bool
	hash_fn         voidptr
	key_eq_fn       voidptr
	clone_fn        voidptr
	free_fn         voidptr
pub mut:
	len int
}

fn signature_map_storage_owned_by_scope(scope voidptr, layout &SignatureMapLayoutForTest) bool {
	$if prealloc {
		return unsafe { prealloc_scope_owns(scope, layout.key_values.keys) }
			|| unsafe { prealloc_scope_owns(scope, layout.key_values.values) }
			|| unsafe { prealloc_scope_owns(scope, layout.metas) }
	} $else {
		return false
	}
}

fn test_rebuild_scoped_transform_signature_maps_after_growth() {
	$if prealloc {
		a := flat.FlatAst.new()
		mut tc := TypeChecker.new(&a)
		mut names := []string{cap: 4096}
		for i in 0 .. 4096 {
			names << 'generated_signature_${i}'
		}

		scope := unsafe { prealloc_scope_begin() }
		for name in names {
			tc.fn_ret_types[name] = Type(int_)
			tc.fn_param_types[name] = []Type{}
			tc.fn_variadic[name] = false
			tc.specialized_generic_fns[name] = true
		}
		ret_scoped := unsafe { &SignatureMapLayoutForTest(&tc.fn_ret_types) }
		params_scoped := unsafe { &SignatureMapLayoutForTest(&tc.fn_param_types) }
		variadic_scoped := unsafe { &SignatureMapLayoutForTest(&tc.fn_variadic) }
		specialized_scoped := unsafe { &SignatureMapLayoutForTest(&tc.specialized_generic_fns) }
		assert signature_map_storage_owned_by_scope(scope, ret_scoped)
		assert signature_map_storage_owned_by_scope(scope, params_scoped)
		assert signature_map_storage_owned_by_scope(scope, variadic_scoped)
		assert signature_map_storage_owned_by_scope(scope, specialized_scoped)

		unsafe { prealloc_scope_leave(scope) }
		tc.rebuild_scoped_transform_signature_maps()
		ret_rebuilt := unsafe { &SignatureMapLayoutForTest(&tc.fn_ret_types) }
		params_rebuilt := unsafe { &SignatureMapLayoutForTest(&tc.fn_param_types) }
		variadic_rebuilt := unsafe { &SignatureMapLayoutForTest(&tc.fn_variadic) }
		specialized_rebuilt := unsafe { &SignatureMapLayoutForTest(&tc.specialized_generic_fns) }
		assert !signature_map_storage_owned_by_scope(scope, ret_rebuilt)
		assert !signature_map_storage_owned_by_scope(scope, params_rebuilt)
		assert !signature_map_storage_owned_by_scope(scope, variadic_rebuilt)
		assert !signature_map_storage_owned_by_scope(scope, specialized_rebuilt)
		unsafe { prealloc_scope_free_after(scope) }

		for idx in [0, 2048, 4095] {
			name := names[idx]
			assert tc.fn_ret_types[name] or { Type(void_) } is Primitive
			assert name in tc.fn_param_types
			assert !tc.fn_variadic[name]
			assert tc.specialized_generic_fns[name]
		}
	}
}
