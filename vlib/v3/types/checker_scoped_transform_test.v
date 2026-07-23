module types

import v3.flat

fn test_power_assignment_reads_index_lhs() {
	assert assignment_op_reads_lhs(.power_assign)
}

fn test_stable_type_index_keeps_custom_types_above_builtin_range() {
	assert stable_interface_type_id_hash('BwoZ') & ~(0xff << 16) == 19
	type_id := stable_type_index('BwoZ')
	assert type_id > 65535
	assert type_id != 19
	assert type_id & (0xff << 16) == 0
}

fn test_stable_type_indexes_resolve_custom_type_collisions() {
	assert stable_type_index('ULz') == stable_type_index('AAbA')
	assert stable_type_index('main.Uc') == stable_type_index('main.ACRB')
	indexes := stable_type_indexes(['ULz', 'AAbA', 'main.Uc', 'main.ACRB'])
	reversed := stable_type_indexes(['main.ACRB', 'main.Uc', 'AAbA', 'ULz'])
	assert indexes == reversed
	assert indexes['ULz'] != indexes['AAbA']
	assert indexes['main.Uc'] != indexes['main.ACRB']
	for _, type_idx in indexes {
		assert type_idx > 65535
		assert type_idx & (0xff << 16) == 0
	}
}

fn test_const_int_power_string_respects_unary_minus_precedence() {
	a := flat.FlatAst.new()
	tc := TypeChecker.new(&a)
	negative_power := tc.const_int_value('-2 ** 2', []string{}) or { panic(err) }
	parenthesized_base := tc.const_int_value('(-2) ** 2', []string{}) or { panic(err) }
	nested_power := tc.const_int_value('-2 ** 2 ** 3', []string{}) or { panic(err) }
	assert negative_power == -4
	assert parenthesized_base == 4
	assert nested_power == -256
}

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
		scope := unsafe { prealloc_scope_begin() }
		for i in 0 .. 4096 {
			name := 'generated_signature_${i}'
			tc.fn_ret_types[name] = Type(Struct{
				name: 'GeneratedResult_${i}'
			})
			tc.fn_param_types[name] = [
				Type(Struct{
					name: 'GeneratedParam_${i}'
				}),
			]
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
			name := 'generated_signature_${idx}'
			ret := tc.fn_ret_types[name] or { Type(void_) }
			assert ret is Struct
			assert ret.name == 'GeneratedResult_${idx}'
			params := tc.fn_param_types[name] or { []Type{} }
			assert params.len == 1
			param := params[0]
			assert param is Struct
			assert param.name == 'GeneratedParam_${idx}'
			assert !tc.fn_variadic[name]
			assert tc.specialized_generic_fns[name]
		}
	}
}

fn test_type_qualification_preserves_channel_and_thread_wrappers() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.cur_module = 'worker'

	assert tc.qualify_type_text('chan bool') == 'chan bool'
	assert tc.qualify_type_text('[]chan bool') == '[]chan bool'
	assert tc.qualify_type_text('thread int') == 'thread int'

	channel_type := tc.parse_resolution_type('chan bool')
	if channel_type is Channel {
		assert channel_type.elem_type is Primitive
	} else {
		assert false, 'expected channel type, got `${channel_type.name()}`'
	}
}
