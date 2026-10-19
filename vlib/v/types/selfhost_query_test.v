module types

import os
import v.flat
import v.parser
import v.pref
import v.token

fn test_void_pointer_queries_preserve_nominal_and_alias_compatibility() {
	void_name := flat.source_type_void
	voidptr_name := flat.source_type_voidptr
	for typ in [Type(Pointer{ base_type: void_ }),
		Type(Alias{ name_id: flat.source_type_id('PointerAlias'), base_type: Pointer{ base_type: void_ } }),
		Type(Struct{ name_id: voidptr_name }),
		Type(Alias{ name_id: flat.source_type_id('&void'), base_type: int_ }),
		Type(Pointer{ base_type: Struct{ name_id: void_name } }),
		Type(Pointer{ base_type: Alias{ name_id: void_name, base_type: int_ } }),
		Type(Pointer{ base_type: Alias{ name_id: flat.source_type_id('VoidAlias'), base_type: void_ } })] {
		assert fn_param_is_voidptr_type(typ), typ.name()
	}
	for typ in [Type(int_), Type(string_), Type(void_), Type(Pointer{ base_type: int_ }),
		Type(Pointer{ base_type: Pointer{ base_type: void_ } }),
		Type(Pointer{ base_type: Alias{ name_id: voidptr_name, base_type: int_ } }),
		Type(Array{ elem_type: Pointer{ base_type: void_ } }),
		Type(FnType{ params: [Type(Pointer{ base_type: void_ })], return_type: void_ }),
		Type(OptionType{ base_type: Pointer{ base_type: void_ } })] {
		assert !fn_param_is_voidptr_type(typ), typ.name()
	}
}

fn test_integer_argument_queries_keep_named_leaves_and_reject_containers() {
	for typ in [Type(int_), Type(i64_), Type(u128_), Type(isize_), Type(rune_), Type(Primitive{}),
		Type(Primitive{ props: .unsigned, size: 16 }),
		Type(Alias{ name_id: flat.source_type_id('Count'), base_type: int_ }),
		Type(Struct{ name_id: flat.source_type_int }), Type(Enum{ name_id: flat.source_type_u8 })] {
		assert call_arg_integer_type(typ), typ.name()
	}
	for typ in [Type(bool_), Type(f64_), Type(string_), Type(char_), Type(void_),
		Type(Pointer{ base_type: int_ }), Type(Array{ elem_type: int_ }),
		Type(FnType{ params: [Type(int_)], return_type: int_ }),
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
	// Equal first bytes and lengths collide in the conservative name mask.
	assert scope_name_bit('os') == scope_name_bit('ox')
	assert tc.resolve_import_alias('ox') == none
	assert tc.resolve_import_alias('absent') == none
	tc.register_file_import('ox', 'other.module')
	assert (tc.resolve_import_alias('ox') or { '' }) == 'other.module'
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
