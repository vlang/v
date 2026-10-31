module types

import os
import v.flat
import v.parser
import v.pref
import v.token

fn test_void_pointer_queries_preserve_nominal_and_alias_compatibility() {
	void_type := new_type_reference(Type(void_))
	integer_type := new_type_reference(Type(int_))
	void_pointer := new_type_reference(Type(Pointer{ base_type: void_type }))
	void_name := flat.source_type_void
	voidptr_name := flat.source_type_voidptr
	for typ in [Type(Pointer{ base_type: void_type }),
		Type(Alias{ name_id: flat.source_type_id('PointerAlias'), base_type: void_pointer }),
		Type(Struct{ name_id: voidptr_name }),
		Type(Alias{ name_id: flat.source_type_id('&void'), base_type: integer_type }),
		Type(Pointer{ base_type: new_type_reference(Type(Struct{ name_id: void_name })) }),
		Type(Pointer{ base_type: new_type_reference(Type(Alias{ name_id: void_name, base_type: integer_type })) }),
		Type(Pointer{ base_type: new_type_reference(Type(Alias{ name_id: flat.source_type_id('VoidAlias'), base_type: void_type })) })] {
		assert fn_param_is_voidptr_type(typ), typ.name()
	}
	for typ in [Type(int_), Type(string_), Type(void_), Type(Pointer{ base_type: integer_type }),
		Type(Pointer{ base_type: void_pointer }),
		Type(Pointer{ base_type: new_type_reference(Type(Alias{ name_id: voidptr_name, base_type: integer_type })) }),
		Type(Array{ elem_type: void_pointer }),
		Type(FnType{ params: [FnParam{ typ: void_pointer }], return_type: void_type }),
		Type(OptionType{ base_type: void_pointer })] {
		assert !fn_param_is_voidptr_type(typ), typ.name()
	}
}

fn test_integer_argument_queries_keep_named_leaves_and_reject_containers() {
	integer_type := new_type_reference(Type(int_))
	for typ in [Type(int_), Type(i64_), Type(u128_), Type(isize_), Type(rune_), Type(Primitive{}),
		Type(Primitive{ props: .unsigned, size: 16 }),
		Type(Alias{ name_id: flat.source_type_id('Count'), base_type: integer_type }),
		Type(Struct{ name_id: flat.source_type_int }), Type(Enum{ name_id: flat.source_type_u8 })] {
		assert call_arg_integer_type(typ), typ.name()
	}
	for typ in [Type(bool_), Type(f64_), Type(string_), Type(char_), Type(void_),
		Type(Pointer{ base_type: integer_type }), Type(Array{ elem_type: integer_type }),
		Type(FnType{ params: [FnParam{ typ: integer_type }], return_type: integer_type }),
		Type(Struct{ name_id: flat.source_type_id('Count') })] {
		assert !call_arg_integer_type(typ), typ.name()
	}
}

fn test_source_queries_follow_recollection_and_checker_views() {
	path := os.join_path(os.vtmp_dir(), 'source_query_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	mut initial_ast := flat.FlatAst.new()
	mut tc := TypeChecker.new(&initial_ast)
	for value in ['42', '987654321'] {
		source := 'module main\nfn main() { _ = ${value} }\n'
		os.write_file(path, source)!
		mut p := parser.Parser.new(pref.new_preferences())
		mut a := p.parse_file(path)
		assert p.diagnostics.len == 0
		tc.collect(a)
		view := tc.fork_for_parallel_check()
		mut found := false
		for i, node in a.nodes {
			if node.kind == .int_literal {
				assert tc.source_text_for_node(flat.NodeId(i)) == value
				assert view.source_text_for_node(flat.NodeId(i)) == value
				found = true
			}
		}
		assert found
	}
}

fn test_current_function_name_tracks_module_and_declaration_changes() {
	mut a := flat.FlatAst.new()
	id := a.add_node(flat.Node{ kind: .fn_decl, value: 'run' })
	mut tc := TypeChecker.new(&a)
	tc.fn_context.node_id = int(id)
	for mod in ['one', 'two', 'one'] {
		tc.cur_module = mod
		assert (tc.current_checked_fn_qname() or { panic('missing function') }) == '${mod}.run'
		assert (tc.current_checked_fn_qname() or { panic('missing function') }) == '${mod}.run'
	}
	a.nodes[int(id)].value = 'renamed'
	assert (tc.current_checked_fn_qname() or { panic('missing function') }) == 'one.renamed'
	tc.cur_module = 'main'
	assert (tc.current_checked_fn_qname() or { panic('missing function') }) == 'renamed'
}

fn test_call_metadata_keeps_print_parameters_separate_from_declarations() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	for name in ['println', 'eprint', 'panic', 'echo'] {
		tc.fn_param_types[name] = [Type(string_)]
		tc.fn_ret_types[name] = builtin_void_type
	}
	for _ in 0 .. 3 {
		for name in ['println', 'eprint', 'panic', 'echo'] {
			info := tc.call_info(name, false)
			assert info.params_known
			assert info.params.len == 1
			if name == 'echo' {
				assert info.params[0] is String
			} else {
				assert info.params[0] is Unknown
			}
			assert tc.fn_param_types[name][0] is String
		}
	}
}

fn test_noreturn_suffix_matching_keeps_component_boundaries() {
	mut a := flat.FlatAst.new()
	a.noreturn_fns['pkg.fail'] = true
	a.noreturn_fns['pkg.ignored'] = false
	mut tc := TypeChecker.new(&a)
	for name in ['pkg.fail', 'outer.pkg.fail', 'outer.inner.pkg.fail'] {
		assert tc.name_never_returns(name), name
	}
	for name in ['notpkg.fail', 'outer.notpkg.fail', 'outer.pkg.fail_extra', 'outer.pkg.ignored'] {
		assert !tc.name_never_returns(name), name
	}
}

fn test_integer_overflow_uses_the_full_span_of_generated_literals() {
	mut a := flat.FlatAst.new()
	mut files := token.FileSet.new()
	source := '18446744073709551616'
	a.source_files[1] = files.add_file('overflow.v', source.len)
	id := a.add_node(flat.Node{
		kind:  .int_literal
		value: '1'
		pos:   token.new_span(1, 0, source.len)
	})
	mut tc := TypeChecker.new(&a)
	tc.source_texts_by_file['overflow.v'] = source
	tc.check_untyped_integer_literal_overflow(id)
	assert tc.errors.len == 1
	assert tc.errors[0].msg == 'integer literal ${source} overflows int'
}

fn test_scalar_loop_post_preserves_body_pointer_aliases_with_and_without_cached_types() {
	mut a := flat.FlatAst.new()
	counter := a.add_val(.ident, 'counter')
	post_start := a.begin_children()
	a.add_child(counter)
	post := a.add_node(flat.Node{
		kind:           .postfix
		op:             .inc
		children_start: post_start
		children_count: 1
	})
	loop_start := a.begin_children()
	a.add_child(flat.empty_node)
	a.add_child(flat.empty_node)
	a.add_child(post)
	loop_id := a.add_node(flat.Node{
		kind:           .for_stmt
		value:          'c_style'
		children_start: loop_start
		children_count: 3
	})
	mut tc := TypeChecker.new(&a)
	tc.disable_explicit_mutability = true
	tc.cur_scope.insert('counter', Type(int_))
	base := {
		'pointer': ['before']
	}
	incoming := {
		'pointer':    ['after']
		'body_local': ['new']
	}
	for cached in [false, true] {
		if cached {
			tc.register_synth_type(counter, Type(int_))
		} else {
			tc.invalidate_checked_expr_type(int(counter))
		}
		tc.fn_context.pointer_binding_value_keys = clone_pointer_binding_value_keys(incoming)
		mut result := tc.pointer_binding_state_after_loop_post(*a.node(loop_id), incoming,
			base, base, false, map[string]Type{})
		assert result == incoming
		assert tc.fn_context.pointer_binding_value_keys == incoming
		assert tc.errors.len == 0
		result['pointer'] = ['changed']
		assert incoming['pointer'] == ['after']
	}
}

fn test_c_varargs_macro_recognition_keeps_the_source_callee() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	for base in ['C', 'other'] {
		for name in ['va_arg', 'va_start', 'va_end', 'va_copy', 'ordinary'] {
			base_id := a.add_val(.ident, base)
			selector_start := a.begin_children()
			a.add_child(base_id)
			selector := a.add_node(flat.Node{ kind: .selector, value: name, children_start: selector_start, children_count: 1 })
			call_start := a.begin_children()
			a.add_child(selector)
			call := a.add_node(flat.Node{ kind: .call, children_start: call_start, children_count: 1 })
			expected := if base == 'C' && name != 'ordinary' { 'C.${name}' } else { '' }
			assert tc.c_va_macro_call_name(*a.node(call)) == expected
		}
	}
}

fn test_noreturn_suffix_queries_preserve_exact_membership_and_qualified_suffixes() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	assert tc.name_never_returns('panic')
	assert !tc.name_never_returns('module.ordinary')
	a.noreturn_fns['ignored'] = true
	a.noreturn_fns['pkg.disabled'] = false
	a.noreturn_fns['pkg.fail'] = true
	a.noreturn_fns['.fail'] = true
	for name in ['pkg.fail', 'outer.pkg.fail', 'outer.inner.pkg.fail', 'pkg.disabled', 'outer..fail'] {
		assert tc.name_never_returns(name), name
	}
	for name in ['outer.ignored', 'outer.pkg.disabled', 'unrelated.fail', 'notpkg.fail'] {
		assert !tc.name_never_returns(name), name
	}
}

fn test_receiver_suffix_membership_tracks_escaped_and_generated_methods_and_views() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.method_suffix_prescreen = true
	tc.top_level_idx = [i32(0)]
	tc.register_generated_fn_param_types('Item.@match', []Type{})
	assert tc.selected_file_method_suffix_known('@match')
	assert tc.selected_file_method_suffix_known('match')
	assert !tc.selected_file_method_suffix_known('missing')
	view := tc.fork_for_parallel_check()
	assert view.selected_file_method_suffix_known('match')
	tc.register_generated_fn_param_types('Item.generated', []Type{})
	assert tc.selected_file_method_suffix_known('generated')
	tc.fn_param_types.delete('Item.@match')
	tc.rebuild_fn_param_suffix_index()
	assert !tc.selected_file_method_suffix_known('match')
	assert tc.selected_file_method_suffix_known('generated')
}

fn test_import_alias_negative_filter_keeps_collisions_rebindings_and_file_context() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.cur_file = 'first.v'
	tc.register_file_import('os', 'os')
	assert (tc.resolve_import_alias('os') or { '' }) == 'os'
	// Last bytes differing by 64 still collide in the conservative name mask.
	assert scope_name_bit('os') == scope_name_bit('o3')
	assert tc.resolve_import_alias('o3') == none
	assert tc.resolve_import_alias('absent') == none
	tc.register_file_import('o3', 'other.module')
	assert (tc.resolve_import_alias('o3') or { '' }) == 'other.module'
	tc.register_file_import('os', 'replacement')
	assert (tc.resolve_import_alias('os') or { '' }) == 'replacement'
	tc.cur_file = 'second.v'
	tc.register_file_import('strings', 'strings')
	assert tc.resolve_import_alias('os') == none
	assert (tc.resolve_import_alias('strings') or { '' }) == 'strings'
	tc.cur_file = 'first.v'
	assert (tc.resolve_import_alias('os') or { '' }) == 'replacement'
	mut info := tc.file_import_info()
	info.imports.delete('os')
	assert tc.resolve_import_alias('os') == none
}

fn test_import_alias_filter_accepts_unindexed_hand_built_views() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.cur_file = 'manual.v'
	tc.file_imports_by_file[tc.cur_file] = &FileImportInfo{
		imports: {
			'alias': 'original.module'
		}
	}
	assert (tc.resolve_import_alias('alias') or { '' }) == 'original.module'
	tc.register_file_import('new_alias', 'new.module')
	assert (tc.resolve_import_alias('new_alias') or { '' }) == 'new.module'
}

fn test_branch_snapshots_keep_map_and_array_storage_independent() {
	mut source := map[string][]string{}
	for i in 0 .. 200 {
		source['key${i}'] = ['value${i}']
	}
	for i in 0 .. 180 {
		source.delete('key${i}')
	}
	mut snapshot := clone_pointer_binding_value_keys(source)
	assert snapshot == source
	assert snapshot.keys() == source.keys()
	snapshot['key180'][0] = 'changed'
	snapshot['key181'] << 'appended'
	snapshot.delete('key182')
	snapshot['new'] = ['new']
	assert source['key180'] == ['value180']
	assert source['key181'] == ['value181']
	assert 'key182' in source
	assert 'new' !in source
	source.clear()
	mut empty_snapshot := clone_pointer_binding_value_keys(source)
	empty_snapshot['new'] = ['new']
	assert source.len == 0

	owner := ScopeBindingOwner{ index: 1, name: 'original' }
	mut owners := {
		'key': [owner]
	}
	mut owner_snapshot := clone_scope_binding_owner_map(owners)
	owner_snapshot['key'][0] = ScopeBindingOwner{ index: 2, name: 'changed' }
	owner_snapshot['key'] << owner
	assert owners['key'] == [owner]
}

fn test_pointer_binding_merge_preserves_order_fallbacks_and_independent_values() {
	mut base := {
		'a': ['one']
		'b': ['two']
	}
	mut left := {
		'a': ['left', 'left']
		'c': ['three']
	}
	mut right := {
		'a': ['right', 'left']
		'd': ['four']
	}
	mut merged := merge_pointer_binding_value_states([left, right], base)
	assert merged.keys() == ['a', 'b', 'c', 'd']
	assert merged['a'] == ['left', 'right']
	assert merged['b'] == ['two']
	assert merged['c'] == ['three', 'c']
	assert merged['d'] == ['d', 'four']
	merged['a'][0] = 'changed'
	merged['b'] << 'new'
	assert base['b'] == ['two']
	assert left['a'] == ['left', 'left']
	assert right['a'] == ['right', 'left']
}

fn test_pointer_binding_branch_snapshots_isolate_replacement_and_invalidation() {
	mut source := {
		'left':  ['first', 'second']
		'right': ['third']
	}
	mut snapshot := snapshot_pointer_binding_value_keys(source)
	source['left'] = ['changed']
	source.delete('right')
	source['new'] = ['new']
	assert snapshot['left'] == ['first', 'second']
	assert snapshot['right'] == ['third']
	assert 'new' !in snapshot
	snapshot['left'] = ['other']
	assert source['left'] == ['changed']
	mut tc := TypeChecker{}
	tc.fn_context.pointer_binding_value_keys = snapshot
	tc.invalidate_all_pointer_binding_aliases()
	assert source['left'] == ['changed']
}

fn selfhost_smartcast_index(mut a flat.FlatAst) flat.NodeId {
	base := a.add_val(.ident, 'object')
	field_start := a.begin_children()
	a.add_child(base)
	field := a.add_node(flat.Node{
		kind:           .selector
		value:          'member'
		children_start: field_start
		children_count: 1
	})
	index := a.add_val(.int_literal, '7')
	index_start := a.begin_children()
	a.add_child(field)
	a.add_child(index)
	return a.add_node(flat.Node{
		kind:           .index
		children_start: index_start
		children_count: 2
	})
}

@[unsafe]
fn test_empty_lexical_smartcast_filter_preserves_dynamic_types_and_range() {
	mut a := flat.FlatAst.new()
	id := selfhost_smartcast_index(mut a)
	mut tc := TypeChecker.new(&a)
	tc.build_direct_parent_index(&a)
	tc.smartcast_key_filter = &SmartcastKeyFilter{}
	tc.smartcast_key_filter.arm(0, a.nodes.len - 1)
	assert tc.smartcast_type(id) == none
	assert tc.smartcast_key_filter.ready
	assert tc.type_cache.lexical_smartcast_miss(int(id))
	tc.smartcasts['object.member[7]'] = Type(int_)
	assert tc.smartcast_type(id)? == Type(int_)
	tc.smartcasts.delete('object.member[7]')
	assert tc.smartcast_type(id) == none
	assert !tc.lexical_smartcasts_unused(-1)
	assert !tc.lexical_smartcasts_unused(a.nodes.len)
	// Reusing the filter for another body must discard its earlier empty state.
	type_id := a.add_val(.ident, 'int')
	condition_start := a.begin_children()
	a.add_child(id)
	a.add_child(type_id)
	a.add_node(flat.Node{
		kind:           .is_expr
		children_start: condition_start
		children_count: 2
	})
	tc.smartcast_key_filter.arm(0, a.nodes.len - 1)
	assert !tc.lexical_smartcasts_unused(int(id))
	assert !tc.lexical_smartcast_key_unused(int(id), 'object.member[7]')
	tc.smartcast_key_filter = unsafe { nil }
	assert !tc.lexical_smartcasts_unused(int(id))
}

fn test_empty_lexical_smartcast_filter_avoids_composite_key_allocations() {
	$if prealloc && prealloc_stats ? {
		mut a := flat.FlatAst.new()
		id := selfhost_smartcast_index(mut a)
		mut tc := TypeChecker.new(&a)
		tc.build_direct_parent_index(&a)
		tc.lexical_smartcast_misses = []bool{len: a.nodes.len}
		tc.smartcast_key_filter = &SmartcastKeyFilter{}
		tc.smartcast_key_filter.arm(0, a.nodes.len - 1)
		assert tc.smartcast_type(id) == none
		before := prealloc_stats_snapshot()
		assert before.enabled
		for _ in 0 .. 4096 {
			// Force a cold per-node lookup without reallocating its cache page.
			tc.type_cache.set_lexical_smartcast_state(int(id), 0)
			tc.lexical_smartcast_misses[int(id)] = false
			assert tc.smartcast_type(id) == none
		}
		after := prealloc_stats_snapshot()
		assert after.allocation_count == before.allocation_count
		assert after.allocated_bytes == before.allocated_bytes
	}
}

fn test_small_lexical_smartcast_filter_matches_indexed_and_selector_keys() {
	mut a := flat.FlatAst.new()
	id := selfhost_smartcast_index(mut a)
	mut tc := TypeChecker.new(&a)
	tc.smartcast_key_filter = &SmartcastKeyFilter{
		lo:    0
		hi:    a.nodes.len - 1
		ready: true
		keys:  ['other.member[7]', 'object.member[70]', 'object.member']
	}
	assert tc.lexical_smartcasts_unused(int(id))
	assert !tc.expr_key_matches(id, 'object.member[7x]')
	assert !tc.expr_key_matches(id, 'object.member[7')
	assert !tc.expr_key_matches(id, '[7]')
	assert !tc.expr_key_matches(id, '')
	tc.smartcast_key_filter.add('object.member[7]')
	assert !tc.lexical_smartcasts_unused(int(id))
	for i in 0 .. smartcast_key_filter_linear + 1 {
		tc.smartcast_key_filter.add('another${i}')
	}
	assert tc.smartcast_key_filter.index.len > 0
	assert !tc.lexical_smartcasts_unused(int(id))
}

fn test_small_lexical_smartcast_filter_avoids_unrelated_index_key_allocations() {
	$if prealloc && prealloc_stats ? {
		mut a := flat.FlatAst.new()
		id := selfhost_smartcast_index(mut a)
		mut tc := TypeChecker.new(&a)
		tc.build_direct_parent_index(&a)
		tc.lexical_smartcast_misses = []bool{len: a.nodes.len}
		tc.smartcast_key_filter = &SmartcastKeyFilter{
			lo:    0
			hi:    a.nodes.len - 1
			ready: true
			keys:  ['different', 'object.other[7]', 'object.member[70]']
		}
		assert tc.smartcast_type(id) == none
		before := prealloc_stats_snapshot()
		assert before.enabled
		for _ in 0 .. 4096 {
			tc.type_cache.set_lexical_smartcast_state(int(id), 0)
			tc.lexical_smartcast_misses[int(id)] = false
			assert tc.smartcast_type(id) == none
		}
		after := prealloc_stats_snapshot()
		assert after.allocation_count == before.allocation_count
		assert after.allocated_bytes == before.allocated_bytes
	}
}
