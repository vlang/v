module types

import v.flat

fn test_power_assignment_reads_index_lhs() {
	assert assignment_op_reads_lhs(.power_assign)
}

fn test_stable_type_index_keeps_custom_types_above_builtin_range() {
	assert stable_interface_type_id_hash('BwoZ') & ~(0xff << 16) == 19
	type_id := stable_type_index(flat.source_type_id('BwoZ'))
	assert type_id > 65535
	assert type_id != 19
	assert type_id & (0xff << 16) == 0
}

fn test_stable_type_indexes_resolve_custom_type_collisions() {
	assert stable_type_index(flat.source_type_id('ULz')) == stable_type_index(flat.source_type_id('AAbA'))
	assert stable_type_index(flat.source_type_id('main.Uc')) == stable_type_index(flat.source_type_id('main.ACRB'))
	indexes := stable_type_indexes(flat.TypeTextList.from_names(['ULz', 'AAbA', 'main.Uc', 'main.ACRB']).ids)
	reversed := stable_type_indexes(flat.TypeTextList.from_names(['main.ACRB', 'main.Uc', 'AAbA',
		'ULz']).ids)
	assert indexes == reversed
	assert indexes[flat.source_type_id('ULz')] != indexes[flat.source_type_id('AAbA')]
	assert indexes[flat.source_type_id('main.Uc')] != indexes[flat.source_type_id('main.ACRB')]
	for _, type_idx in indexes {
		assert type_idx > 65535
		assert type_idx & (0xff << 16) == 0
	}
}

fn test_stable_type_indexes_extend_without_renumbering_existing_types() {
	assert stable_type_index(flat.source_type_id('Box[Dxw]')) == stable_type_index(flat.source_type_id('Box[Kdd]'))
	mut indexes := stable_type_indexes(flat.TypeTextList.from_names(['Existing']).ids)
	existing_idx := indexes[flat.source_type_id('Existing')]
	extend_stable_type_indexes(mut indexes, flat.TypeTextList.from_names(['Box[Kdd]', 'Box[Dxw]']).ids)
	assert indexes[flat.source_type_id('Existing')] == existing_idx
	assert indexes[flat.source_type_id('Box[Dxw]')] != indexes[flat.source_type_id('Box[Kdd]')]
	for _, type_idx in indexes {
		assert type_idx > 65535
		assert type_idx & (0xff << 16) == 0
	}
}

fn test_stable_type_indexes_resolve_boxed_container_collisions() {
	assert stable_type_index(flat.source_type_id('[]main.AQVA')) == stable_type_index(flat.source_type_id('[]main.CFGS'))
	assert stable_type_index(flat.source_type_id('[]Aaxtc')) == stable_type_index(flat.source_type_id('[]Abddb'))
	indexes := stable_type_indexes(flat.TypeTextList.from_names(['[]main.AQVA', '[]main.CFGS',
		'[]Aaxtc', '[]Abddb']).ids)
	assert indexes[flat.source_type_id('[]main.AQVA')] != indexes[flat.source_type_id('[]main.CFGS')]
	assert indexes[flat.source_type_id('[]Aaxtc')] != indexes[flat.source_type_id('[]Abddb')]
}

fn test_stable_type_indexes_sort_raw_spellings_before_trimming() {
	padded := flat.source_type_id(' ULz ')
	clean := flat.source_type_id('ULz')
	other := flat.source_type_id('AAbA')
	empty := flat.source_type_id(' \t ')
	indexes := stable_type_indexes([padded, other, clean, empty, u32(0)])
	reversed := stable_type_indexes([u32(0), empty, clean, other, padded])
	assert indexes == reversed
	assert indexes.len == 2
	assert indexes[clean] == stable_type_index(clean)
	assert indexes[other] != indexes[clean]
	mut extended := stable_type_indexes([other])
	existing := extended[other]
	extend_stable_type_indexes(mut extended, [padded, clean, empty])
	assert extended[other] == existing
	assert extended.len == 2
	assert extended[clean] != extended[other]
}

fn test_const_int_power_string_respects_unary_minus_precedence() {
	a := flat.FlatAst.new()
	tc := TypeChecker.new(&a)
	negative_power := tc.const_int_value('-2 ** 2', []string{}) or { panic('constant evaluation failed') }
	parenthesized_base := tc.const_int_value('(-2) ** 2', []string{}) or { panic('constant evaluation failed') }
	nested_power := tc.const_int_value('-2 ** 2 ** 3', []string{}) or { panic('constant evaluation failed') }
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

struct SignatureMapDataLayoutForTest {
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
	count int
}

struct SignatureMapLayoutForTest {
mut:
	data &SignatureMapDataLayoutForTest = unsafe { nil }
}

fn signature_map_storage_owned_by_scope(scope voidptr, layout &SignatureMapDataLayoutForTest) bool {
	$if prealloc {
		return unsafe { prealloc_scope_owns(scope, layout.key_values.keys) }
			|| unsafe { prealloc_scope_owns(scope, layout.key_values.values) }
			|| unsafe { prealloc_scope_owns(scope, layout.metas) }
	} $else {
		return false
	}
}

fn test_discard_transform_signature_changes_resets_fork_publication_state() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.transform_signature_maps_shared = true
	tc.ensure_private_transform_signatures()
	tc.register_generated_fn_param_types('main.generated', [Type(int_)])
	assert tc.transform_signatures_changed()
	assert tc.transform_signature_names_log == ['main.generated']

	tc.discard_transform_signature_changes()
	assert !tc.transform_signatures_changed()
	assert tc.transform_signature_names_log.len == 0
}

fn test_rebuild_scoped_transform_signatures_and_suffix_index_after_growth() {
	$if prealloc {
		a := flat.FlatAst.new()
		mut tc := TypeChecker.new(&a)
		tc.ensure_private_transform_signatures()
		scope := unsafe { prealloc_scope_begin() }
		// Model a transform that leaves enough scoped map capacity for the
		// subsequent suffix rebuild to reuse unless it explicitly replaces the map.
		tc.receiver_method_suffix_index.reserve(65536)
		for i in 0 .. 4096 {
			name := 'generated.Box_${i}.open'
			tc.fn_ret_types[name] = Type(Struct{
				name_id: flat.source_type_id('GeneratedResult_${i}')
			})
			tc.register_generated_fn_param_types(name, [
				Type(Struct{
					name_id: flat.source_type_id('GeneratedParam_${i}')
				}),
			])
			tc.fn_variadic[name] = false
			tc.specialized_generic_fns[name] = true
		}
		ret_scoped := unsafe { (&SignatureMapLayoutForTest(&tc.fn_ret_types)).data }
		params_scoped := unsafe { (&SignatureMapLayoutForTest(&tc.fn_param_types)).data }
		variadic_scoped := unsafe { (&SignatureMapLayoutForTest(&tc.fn_variadic)).data }
		specialized_scoped := unsafe {
			(&SignatureMapLayoutForTest(&tc.specialized_generic_fns)).data
		}
		suffix_scoped := unsafe { (&SignatureMapLayoutForTest(&tc.receiver_method_suffix_index)).data }
		assert signature_map_storage_owned_by_scope(scope, ret_scoped)
		assert signature_map_storage_owned_by_scope(scope, params_scoped)
		assert signature_map_storage_owned_by_scope(scope, variadic_scoped)
		assert signature_map_storage_owned_by_scope(scope, specialized_scoped)
		assert signature_map_storage_owned_by_scope(scope, suffix_scoped)

		unsafe { prealloc_scope_leave(scope) }
		tc.rebuild_scoped_transform_signature_maps()
		tc.rebuild_fn_param_suffix_index()
		assert tc.transform_signature_names_log.len == 0
		ret_rebuilt := unsafe { (&SignatureMapLayoutForTest(&tc.fn_ret_types)).data }
		params_rebuilt := unsafe { (&SignatureMapLayoutForTest(&tc.fn_param_types)).data }
		variadic_rebuilt := unsafe { (&SignatureMapLayoutForTest(&tc.fn_variadic)).data }
		specialized_rebuilt := unsafe {
			(&SignatureMapLayoutForTest(&tc.specialized_generic_fns)).data
		}
		suffix_rebuilt := unsafe { (&SignatureMapLayoutForTest(&tc.receiver_method_suffix_index)).data }
		assert !signature_map_storage_owned_by_scope(scope, ret_rebuilt)
		assert !signature_map_storage_owned_by_scope(scope, params_rebuilt)
		assert !signature_map_storage_owned_by_scope(scope, variadic_rebuilt)
		assert !signature_map_storage_owned_by_scope(scope, specialized_rebuilt)
		assert !signature_map_storage_owned_by_scope(scope, suffix_rebuilt)
		unsafe { prealloc_scope_free_after(scope) }
		tc.register_generated_fn_param_types('generated.after_scope', [Type(int_)])
		assert tc.transform_signature_names_log == ['generated.after_scope']

		for idx in [0, 2048, 4095] {
			name := 'generated.Box_${idx}.open'
			ret := tc.fn_ret_types[name] or { Type(void_) }
			assert ret is Struct
			assert ret.name() == 'GeneratedResult_${idx}'
			params := tc.fn_param_types[name] or { []Type{} }
			assert params.len == 1
			param := params[0]
			assert param is Struct
			assert param.name() == 'GeneratedParam_${idx}'
			suffix_params := tc.fn_param_types_for_name('Box_${idx}.open')
			assert suffix_params.len == 1
			suffix_param := suffix_params[0]
			assert suffix_param is Struct
			assert suffix_param.name() == 'GeneratedParam_${idx}'
			assert !tc.fn_variadic[name]
			assert tc.specialized_generic_fns[name]
		}
	}
}

fn test_parallel_transform_fork_owns_mutable_signature_maps() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.fn_ret_types['existing'] = Type(string_)
	mut transform_fork := tc.fork_for_parallel_transform(&a)
	transform_fork.ensure_private_transform_signatures()
	transform_fork.fn_ret_types['generated'] = Type(bool_)
	transform_fork.register_generated_fn_param_types('generated', [Type(int_)])
	transform_fork.fn_variadic['generated'] = false
	transform_fork.specialized_generic_fns['generated'] = true
	assert transform_fork.transform_signature_names_log == ['generated']

	assert 'generated' !in tc.fn_ret_types
	assert 'generated' !in tc.fn_param_types
	assert 'generated' !in tc.fn_variadic
	assert 'generated' !in tc.specialized_generic_fns
	existing := tc.fn_ret_types['existing'] or { Type(void_) }
	assert existing is String
}

fn test_type_qualification_preserves_channel_and_thread_wrappers() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.cur_module = 'worker'

	assert flat.source_type_name(tc.qualify_type_text(flat.source_type_id('chan bool'))) == 'chan bool'
	assert flat.source_type_name(tc.qualify_type_text(flat.source_type_id('[]chan bool'))) == '[]chan bool'
	assert flat.source_type_name(tc.qualify_type_text(flat.source_type_id('thread int'))) == 'thread int'

	channel_type := tc.parse_resolution_type(flat.source_type_id('chan bool'))
	if channel_type is Channel {
		assert channel_type.elem_type is Primitive
	} else {
		assert false, 'expected channel type, got `${channel_type.name()}`'
	}
}
