module markused

import v.flat
import v.types
import v.token

fn test_explicit_generic_factory_return_type_retains_receiver_methods() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.fn_generic_params['gates.make_gate'] = ['U']
	tc.fn_ret_types['gates.make_gate'] = types.Type(types.Pointer{
		base_type: types.Type(types.Struct{ name: 'gates.Gate[U]' })
	})
	arg := a.add_val(.ident, 'T')
	for imported in [false, true] {
		base := if imported {
			module_id := a.add_val(.ident, 'g')
			call_helper_node(mut a, flat.Node{ kind: .selector, value: 'make_gate' }, [module_id])
		} else {
			a.add_val(.ident, 'make_gate')
		}
		indexed := call_helper_node(mut a, flat.Node{ kind: .index }, [base, arg])
		call := call_helper_node(mut a, flat.Node{ kind: .call }, [indexed])
		collector := CallCollector{ a: &a, tc: &tc }
		imports := {
			'g': 'gates'
		}
		assert collector.top_level_call_return_type_name(call, 'gates', imports, map[string]bool{},
			map[string]string{}, false) == 'gates.Gate[T]'
		shadowed := if imported { 'g' } else { 'make_gate' }
		assert collector.top_level_call_return_type_name(call, 'gates', imports, {
			shadowed: true
		},
			map[string]string{}, false) == ''
	}
}

fn test_explicit_generic_factory_substitution_covers_nested_type_forms() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.fn_generic_params['gates.make_gate'] = ['U']
	base := a.add_val(.ident, 'make_gate')
	arg := a.add_val(.ident, 'T')
	indexed := call_helper_node(mut a, flat.Node{ kind: .index }, [base, arg])
	call := call_helper_node(mut a, flat.Node{ kind: .call }, [indexed])
	forms := [
		['U', 'T'],
		['[]U', '[]T'],
		['[4]U', '[4]T'],
		['[2][3]U', '[2][3]T'],
		['map[string]U', 'map[string]T'],
		['map[U][]U', 'map[T][]T'],
		['&U', '&T'],
		['?U', '?T'],
		['!U', '!T'],
		['(U, []U)', '(T, []T)'],
		['fn (U) U', 'fn(T) T'],
		['fn (mut U) ![]U', 'fn(mut T) ![]T'],
		['pkg.Box[U]', 'pkg.Box[T]'],
		['chan U', 'chan T'],
		['thread U', 'thread T'],
		['shared U', 'shared T'],
		['atomic U', 'atomic T'],
		['pkg.U', 'pkg.U'],
		['User', 'User'],
	]
	for form in forms {
		tc.fn_ret_types['gates.make_gate'] = types.Type(types.Struct{
			name: 'gates.Gate[${form[0]}]'
		})
		collector := CallCollector{ a: &a, tc: &tc }
		assert collector.top_level_call_return_type_name(call, 'gates', map[string]string{},
			map[string]bool{}, map[string]string{}, false) == 'gates.Gate[${form[1]}]'
	}
}

fn test_generic_factory_inference_uses_call_site_shadowing() {
	for imported in [false, true] {
		for placement in ['before', 'after', 'nested'] {
			mut a := flat.FlatAst.new()
			mut tc := types.TypeChecker.new(&a)
			tc.fn_generic_params['gates.make_gate'] = ['U']
			tc.fn_ret_types['gates.make_gate'] = types.Type(types.Pointer{
				base_type: types.Type(types.Struct{ name: 'gates.Gate[U]' })
			})
			shadow_name := if imported { 'g' } else { 'make_gate' }
			shadow_lhs := a.add_val(.ident, shadow_name)
			shadow_rhs := a.add_val(.int_literal, '1')
			shadow := call_helper_node(mut a, flat.Node{ kind: .decl_assign }, [
				shadow_lhs,
				shadow_rhs,
			])
			base := if imported {
				module_id := a.add_val(.ident, 'g')
				call_helper_node(mut a, flat.Node{ kind: .selector, value: 'make_gate' }, [module_id])
			} else {
				a.add_val(.ident, 'make_gate')
			}
			arg := a.add_val(.ident, 'T')
			indexed := call_helper_node(mut a, flat.Node{ kind: .index }, [base, arg])
			call := call_helper_node(mut a, flat.Node{ kind: .call }, [indexed])
			lhs := a.add_val(.ident, 'gate')
			decl := call_helper_node(mut a, flat.Node{ kind: .decl_assign }, [lhs, call])
			stmts := match placement {
				'before' { [shadow, decl] }
				'after' { [decl, shadow] }
				else { [call_helper_node(mut a, flat.Node{ kind: .block }, [shadow]), decl] }
			}
			body := call_helper_node(mut a, flat.Node{ kind: .block }, stmts)
			fn_id := call_helper_node(mut a, flat.Node{ kind: .fn_decl, value: 'use_gate' }, [body])
			collector := CallCollector{ a: &a, tc: &tc }
			_, local_types, _ := collector.local_value_info(a.node(fn_id), 'gates', {
				'g': 'gates'
			})
			assert (local_types['gate'] or { '' }) == if placement == 'before' {
				''
			} else {
				'gates.Gate[T]'
			}
		}
	}
}

fn test_checker_selected_generic_selector_factory_return_type() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.parallel_check_sparse = true
	arg := a.add_val(.ident, 'T')
	for imported_static in [false, true] {
		base := a.add_val(.ident, if imported_static { 'alias' } else { 'builder' })
		receiver := if imported_static {
			call_helper_node(mut a, flat.Node{ kind: .selector, value: 'Gate' }, [base])
		} else {
			base
		}
		factory := call_helper_node(mut a, flat.Node{ kind: .selector, value: 'make' }, [
			receiver,
		])
		indexed := call_helper_node(mut a, flat.Node{ kind: .index }, [factory, arg])
		call := call_helper_node(mut a, flat.Node{ kind: .call }, [indexed])
		resolved := if imported_static { 'gates.Gate.make' } else { 'gates.Builder.make' }
		tc.fn_generic_params[resolved] = ['U']
		tc.fn_ret_types[resolved] = types.Type(types.Pointer{
			base_type: types.Type(types.Struct{ name: 'gates.Gate[U]' })
		})
		tc.sparse_resolved_call_names[int(call)] = resolved
		collector := CallCollector{ a: &a, tc: &tc }
		assert collector.top_level_call_return_type_name(call, 'main', {
			'alias': 'gates'
		}, {
			'builder': true
		}, {
			'builder': 'gates.Builder'
		}, false) == 'gates.Gate[T]'
	}
}

fn call_helper_node(mut a flat.FlatAst, node flat.Node, children []flat.NodeId) flat.NodeId {
	start := a.children.len
	for child in children {
		a.add_child(child)
	}
	return a.add_node(flat.Node{
		...node
		children_start: start
		children_count: children.len
	})
}

fn test_array_literal_constructors_follow_cgen_lowering() {
	// Cgen lowers runtime array literals to constructors after markused, and
	// literal-output programs do not seed them, so the literal itself must keep
	// them. Fixed arrays, `in` operands and fixed constant tables need none.
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.const_types['table'] = types.Type(types.ArrayFixed{
		elem_type: types.int_
		len:       2
	})
	tc.const_types['names'] = types.Type(types.Array{
		elem_type: types.string_
	})
	c := CallCollector{
		a:               &a
		tc:              &tc
		import_contexts: [map[string]string{}]
	}
	ctor := ['new_array_from_c_array']
	assert array_literal_root_calls(mut a, c, .fn_decl, '', array_literal_node(mut a, 2)) == ctor
	assert array_literal_root_calls(mut a, c, .fn_decl, '', array_literal_node(mut a, 1)) == [
		'new_array_from_c_array',
		'new_array_from_c_array_noscan',
	]
	assert array_literal_root_calls(mut a, c, .fn_decl, '', array_literal_node(mut a, 0)).len == 0
	fixed := call_helper_node(mut a, flat.Node{ kind: .postfix, op: .not }, [
		array_literal_node(mut a, 2),
	])
	assert array_literal_root_calls(mut a, c, .fn_decl, '', fixed).len == 0
	// The elements of a fixed array can still be runtime arrays.
	fixed_of_arrays := call_helper_node(mut a, flat.Node{ kind: .postfix, op: .not }, [
		call_helper_node(mut a, flat.Node{ kind: .array_literal }, [
			array_literal_node(mut a, 2),
		]),
	])
	assert array_literal_root_calls(mut a, c, .fn_decl, '', fixed_of_arrays) == ctor
	in_expr := call_helper_node(mut a, flat.Node{ kind: .in_expr }, [
		a.add_val(.string_literal, 'a'),
		array_literal_node(mut a, 2),
	])
	assert array_literal_root_calls(mut a, c, .fn_decl, '', in_expr).len == 0
	not_in := call_helper_node(mut a, flat.Node{ kind: .prefix, op: .not }, [in_expr])
	assert array_literal_root_calls(mut a, c, .fn_decl, '', not_in).len == 0
	assert array_literal_root_calls(mut a, c, .const_field, 'table', array_literal_node(mut a,
		2)).len == 0
	assert array_literal_root_calls(mut a, c, .const_field, 'names', array_literal_node(mut a,
		2)) == ctor
}

fn array_literal_node(mut a flat.FlatAst, len int) flat.NodeId {
	elem := a.add_val(.string_literal, 'a')
	return call_helper_node(mut a, flat.Node{ kind: .array_literal }, []flat.NodeId{len: len, init: elem})
}

fn array_literal_root_calls(mut a flat.FlatAst, c &CallCollector, kind flat.NodeKind, name string, expr flat.NodeId) []string {
	root := call_helper_node(mut a, flat.Node{ kind: kind, value: name }, [expr])
	mut calls := []string{}
	c.collect_calls_with_locals(a.node(root), 'main', map[string]string{}, '', '', map[string]bool{},
		map[string]string{}, map[int]bool{}, mut calls)
	return calls
}

fn test_literal_output_gate_preserves_file_index_fallbacks() {
	mut a := flat.FlatAst.new()
	callee := a.add_val(.ident, 'println')
	value := a.add_val(.string_literal, 'hello')
	call := call_helper_node(mut a, flat.Node{ kind: .call }, [callee, value])
	body := call_helper_node(mut a, flat.Node{ kind: .block }, [call])
	main_fn := call_helper_node(mut a, flat.Node{ kind: .fn_decl, value: 'main' }, [body])
	entry := call_helper_node(mut a, flat.Node{ kind: .file, value: 'main.v' }, [main_fn])
	helper := a.add_val(.fn_decl, 'helper')
	dependency := call_helper_node(mut a, flat.Node{ kind: .file, value: 'helper.v' }, [helper])
	for mode in 0 .. 3 {
		a.file_node_ids = match mode {
			0 { []i32{} }
			1 { [i32(entry), i32(dependency)] }
			else { [i32(dependency)] }
		}
		a.file_index_incomplete = mode == 2
		assert is_trivial_literal_output_program(&a, {
			'main.v': true
		})
		assert !is_trivial_literal_output_program(&a, {
			'helper.v': true
		})
		a.nodes[int(value)].kind = .int_literal
		assert !is_trivial_literal_output_program(&a, {
			'main.v': true
		})
		a.nodes[int(value)].kind = .string_literal
	}
}

fn test_join_path_helper_preserves_resolved_and_source_names() {
	mut a := flat.FlatAst.new()
	arg := a.add_node(flat.Node{ kind: .string_literal, value: 'part' })
	spread := call_helper_node(mut a, flat.Node{ kind: .prefix, value: '...' }, [arg])
	for base_name in ['', 'os', 'other'] {
		base := a.add_node(flat.Node{ kind: .ident, value: base_name })
		callee := if base_name == '' {
			a.add_node(flat.Node{ kind: .ident, value: 'join_path' })
		} else {
			call_helper_node(mut a, flat.Node{ kind: .selector, value: 'join_path' }, [
				base,
			])
		}
		for resolved in ['', 'os.join_path'] {
			for last_arg in [arg, spread] {
				id := call_helper_node(mut a, flat.Node{ kind: .call }, [callee, arg, last_arg])
				c := CallCollector{ a: &a }
				mut calls := []string{}
				c.collect_lowered_join_path_single(a.node(id), resolved, mut calls)
				if last_arg == arg && (base_name != 'other' || resolved != '') {
					assert calls == ['join_path_single', 'os.join_path_single']
				} else {
					assert calls.len == 0
				}
			}
		}
	}
}

fn test_omitted_parameter_defaults_skip_receiver_and_named_arguments() {
	mut a := flat.FlatAst.new()
	default_ident := a.add_node(flat.Node{ kind: .ident, value: 'default_level' })
	default_call := call_helper_node(mut a, flat.Node{ kind: .call }, [default_ident])
	field := call_helper_node(mut a, flat.Node{ kind: .field_decl, value: 'level', typ: 'int' }, [
		default_call,
	])
	config := call_helper_node(mut a, flat.Node{ kind: .struct_decl, value: 'Config' }, [
		field,
	])
	receiver := a.add_node(flat.Node{ kind: .param, value: 'self', typ: 'Receiver', op: .dot })
	data := a.add_node(flat.Node{ kind: .param, value: 'data', typ: 'int' })
	options := a.add_node(flat.Node{ kind: .param, value: 'options', typ: 'Config' })
	decl := call_helper_node(mut a, flat.Node{ kind: .fn_decl, value: 'consume' }, [
		receiver,
		data,
		options,
	])
	callee := a.add_node(flat.Node{ kind: .ident, value: 'consume' })
	arg := a.add_node(flat.Node{ kind: .int_literal, value: '1' })
	named := call_helper_node(mut a, flat.Node{ kind: .field_init, value: 'level' }, [
		arg,
	])
	mut tc := types.TypeChecker.new(&a)
	tc.fn_ret_types['default_level'] = types.Type(types.int_)
	c := CallCollector{
		a:               &a
		tc:              &tc
		fn_decls:        {
			'consume': FnDeclInfo{ node_id: decl }
		}
		struct_decls:    {
			'Config': StructDeclInfo{ node_id: config }
		}
		import_contexts: [map[string]string{}]
	}
	for last_arg in [named, arg] {
		id := call_helper_node(mut a, flat.Node{ kind: .call }, [callee, arg, last_arg])
		mut calls := []string{}
		c.collect_omitted_params_default_calls(a.node(id), 'consume', '', map[string]string{}, mut calls)
		assert ('default_level' in calls) == (last_arg == named)
	}
}

fn test_call_search_distinguishes_leaf_index_and_nested_call() {
	mut a := flat.FlatAst.new()
	ident := a.add_node(flat.Node{ kind: .ident, value: 'data' })
	index := call_helper_node(mut a, flat.Node{ kind: .index }, [ident, ident])
	call := call_helper_node(mut a, flat.Node{ kind: .call }, [ident])
	nested := call_helper_node(mut a, flat.Node{ kind: .selector, value: 'field' }, [
		call,
	])
	c := CallCollector{ a: &a }
	assert !c.expr_contains_call_or_index(ident)
	assert !c.expr_contains_call(ident)
	assert c.expr_contains_call_or_index(index)
	assert !c.expr_contains_call(index)
	assert c.expr_contains_call_or_index(nested)
	assert c.expr_contains_call(nested)
}

fn test_closure_runtime_import_marks_syntax_need() {
	mut a := flat.FlatAst.new()
	a.add_node(flat.Node{
		kind:  .import_decl
		value: 'closure'
		typ:   '__v3_builtin_closure_runtime'
	})
	assert markused_syntax_needs_closure_runtime(&a)
}

fn test_explicit_generic_factory_resolves_nested_import_aliases() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.fn_generic_params['gates.make_gate'] = ['U']
	tc.fn_ret_types['gates.make_gate'] = types.Type(types.Struct{ name: 'gates.Gate[U]' })
	imports := {
		'alias': 'actual.module'
		'other': 'other.module'
	}
	for form in ['alias.Payload', '[]alias.Payload', '[2]alias.Payload', 'Box[alias.Payload]',
		'alias.Box[other.Payload]', '&alias.Payload', '?alias.Payload', '!alias.Payload',
		'map[alias.Key][]other.Payload', 'fn (alias.Payload) other.Payload',
		'(alias.Payload, []other.Payload)', 'chan alias.Payload', 'thread alias.Payload',
		'actual.module.Payload', 'long_alias.Payload', 'T'] {
		base := a.add_val(.ident, 'make_gate')
		// Type text nodes retain the same spelling for every nested argument form.
		arg := a.add_val(.struct_decl, form)
		indexed := call_helper_node(mut a, flat.Node{ kind: .index }, [base, arg])
		call := call_helper_node(mut a, flat.Node{ kind: .call }, [indexed])
		collector := CallCollector{ a: &a, tc: &tc }
		expected := if form == 'long_alias.Payload' {
			form
		} else {
			form.replace('alias.', 'actual.module.').replace('other.', 'other.module.')
		}
		assert collector.top_level_call_return_type_name(call, 'gates', imports,
			map[string]bool{}, map[string]string{}, false) == 'gates.Gate[${expected}]'
	}
}

fn test_explicit_generic_factory_qualifies_caller_types_and_selective_imports() {
	mut a := flat.FlatAst.new()
	source_file := 'consumer/use.v'
	a.source_files[3] = &token.File{ name: source_file }
	mut tc := types.TypeChecker.new(&a)
	tc.fn_generic_params['gates.make_gate'] = ['U']
	tc.fn_ret_types['gates.make_gate'] = types.Type(types.Struct{ name: 'gates.Gate[U]' })
	for name in ['consumer.Payload', 'consumer.Box', 'items.Selected'] {
		tc.structs[name] = []types.StructField{}
	}
	tc.file_selective_imports[source_file + '\nSelected'] = ['items.Selected']
	tc.file_selective_imports['other.v\nOtherOnly'] = ['items.Selected']
	forms := {
		'Payload':               'consumer.Payload'
		'[]Payload':             '[]consumer.Payload'
		'[2]Payload':            '[2]consumer.Payload'
		'Box[Payload]':          'consumer.Box[consumer.Payload]'
		'Box[T]':                'consumer.Box[T]'
		'Selected':              'items.Selected'
		'Box[[]Selected]':       'consumer.Box[[]items.Selected]'
		'map[string]Payload':    'map[string]consumer.Payload'
		'fn (Payload) Selected': 'fn (consumer.Payload) items.Selected'
		'OtherOnly':             'OtherOnly'
		'T':                     'T'
		'int':                   'int'
	}
	for form, expected in forms {
		base := a.add_val(.ident, 'make_gate')
		arg := a.add_node(flat.Node{ kind: .struct_decl, value: form, pos: token.Pos{ id: 3 } })
		indexed := call_helper_node(mut a, flat.Node{ kind: .index }, [base, arg])
		collector := CallCollector{ a: &a, tc: &tc }
		assert collector.generic_factory_return_type_name(a.node(indexed), 'gates.make_gate',
			'consumer', map[string]string{}, false, '') == 'gates.Gate[${expected}]'
	}
}

fn test_generic_factory_return_substitutes_receiver_and_method_arguments_together() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.parallel_check_sparse = true
	for parts in [
		['gates.Builder[T]', 'gates.Builder[V]', 'W', 'gates.Gate[V, W]'],
		['gates.Builder[T]', '&gates.Builder[[]V]', 'W', 'gates.Gate[[]V, W]'],
		['gates.Builder[T]', 'gates.Builder[U]', 'string', 'gates.Gate[U, string]'],
		['gates.Builder[[]T]', 'gates.Builder[[]V]', 'W', 'gates.Gate[V, W]'],
		['gates.Builder[T]', 'gates.Builder[V]', 'V, W', 'gates.Gate[V, W]'],
	] {
		method := '${parts[0]}.make'
		tc.fn_generic_params[method] = ['U']
		tc.fn_ret_types[method] = types.Type(types.Struct{ name: 'gates.Gate[T, U]' })
		base := a.add_val(.ident, 'builder')
		selector := call_helper_node(mut a, flat.Node{ kind: .selector, value: 'make' }, [base])
		mut indexed_children := [selector]
		for argument in parts[2].split(', ') { indexed_children << a.add_val(.ident, argument) }
		indexed := call_helper_node(mut a, flat.Node{ kind: .index }, indexed_children)
		call := call_helper_node(mut a, flat.Node{ kind: .call }, [indexed])
		tc.sparse_resolved_call_names[int(call)] = method
		collector := CallCollector{ a: &a, tc: &tc }
		assert collector.top_level_call_return_type_name(call, 'main', map[string]string{}, {
			'builder': true
		}, {
			'builder': parts[1]
		}, false) == parts[3]
	}
}

fn test_generic_factory_return_locks_main_module_type_arguments() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.structs['Payload'] = []types.StructField{}
	tc.struct_modules['Payload'] = 'main'
	tc.structs['gates.Payload'] = []types.StructField{}
	tc.fn_generic_params['gates.make_gate'] = ['U']
	tc.fn_ret_types['gates.make_gate'] = types.Type(types.Struct{ name: 'gates.Gate[U]' })
	tc.fn_ret_types['gates.Gate[main.Payload].backward'] = types.Type(types.int_)
	base := a.add_val(.ident, 'make_gate')
	arg := a.add_val(.ident, 'Payload')
	indexed := call_helper_node(mut a, flat.Node{ kind: .index }, [base, arg])
	collector := CallCollector{ a: &a, tc: &tc }
	inferred := collector.generic_factory_return_type_name(a.node(indexed), 'gates.make_gate', 'main', map[string]string{}, false, '')
	assert inferred == 'gates.Gate[main.Payload]'
	assert collector.typed_receiver_method_name(inferred, 'backward', 'main')? == 'gates.Gate[main.Payload].backward'
}

fn test_generic_factory_return_keeps_noncolliding_main_type_spelling() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.structs['Payload'] = []types.StructField{}
	tc.structs['main.Payload'] = []types.StructField{}
	tc.struct_modules['Payload'] = 'main'
	tc.fn_generic_params['gates.make_gate'] = ['U']
	tc.fn_ret_types['gates.make_gate'] = types.Type(types.Struct{ name: 'gates.Gate[U]' })
	tc.fn_ret_types['gates.Gate[Payload].backward'] = types.Type(types.int_)
	base := a.add_val(.ident, 'make_gate')
	arg := a.add_val(.ident, 'Payload')
	indexed := call_helper_node(mut a, flat.Node{ kind: .index }, [base, arg])
	collector := CallCollector{ a: &a, tc: &tc }
	inferred := collector.generic_factory_return_type_name(a.node(indexed), 'gates.make_gate',
		'main', map[string]string{}, false, '')
	assert inferred == 'gates.Gate[Payload]'
	assert collector.typed_receiver_method_name(inferred, 'backward', 'main')? == 'gates.Gate[Payload].backward'
}

fn test_generic_factory_return_locks_main_alias_when_target_collides() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.type_aliases['Payload'] = 'Context'
	tc.type_alias_modules['Payload'] = 'main'
	tc.structs['Context'] = []types.StructField{}
	tc.struct_modules['Context'] = 'main'
	tc.structs['gates.Context'] = []types.StructField{}
	tc.fn_generic_params['gates.make_gate'] = ['U']
	tc.fn_ret_types['gates.make_gate'] = types.Type(types.Struct{ name: 'gates.Gate[U]' })
	tc.fn_ret_types['gates.Gate[main.Payload].backward'] = types.Type(types.int_)
	base := a.add_val(.ident, 'make_gate')
	arg := a.add_val(.ident, 'Payload')
	indexed := call_helper_node(mut a, flat.Node{ kind: .index }, [base, arg])
	collector := CallCollector{ a: &a, tc: &tc }
	inferred := collector.generic_factory_return_type_name(a.node(indexed), 'gates.make_gate',
		'main', map[string]string{}, false, '')
	assert inferred == 'gates.Gate[main.Payload]'
	assert collector.typed_receiver_method_name(inferred, 'backward', 'main')? == 'gates.Gate[main.Payload].backward'
}

fn test_promoted_generic_factory_return_substitutes_embedded_receiver() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.parallel_check_sparse = true
	tc.structs['gates.Builder'] = []types.StructField{}
	tc.struct_generic_params['gates.Builder'] = ['T']
	tc.structs['gates.Outer'] = [
		types.StructField{
			name:     'Builder'
			typ:      types.Type(types.Struct{ name: 'gates.Builder[V]' })
			is_embed: true
		},
	]
	tc.struct_generic_params['gates.Outer'] = ['V']
	method := 'gates.Builder[T].make'
	tc.fn_generic_params[method] = ['U']
	tc.fn_ret_types[method] = types.Type(types.Struct{ name: 'gates.Gate[T, U]' })
	tc.fn_ret_types['gates.Gate[int, string].backward'] = types.Type(types.int_)
	base := a.add_val(.ident, 'outer')
	selector := call_helper_node(mut a, flat.Node{ kind: .selector, value: 'make' }, [base])
	arg := a.add_val(.ident, 'string')
	indexed := call_helper_node(mut a, flat.Node{ kind: .index }, [selector, arg])
	call := call_helper_node(mut a, flat.Node{ kind: .call }, [indexed])
	tc.sparse_resolved_call_names[int(call)] = method
	collector := CallCollector{ a: &a, tc: &tc }
	inferred := collector.top_level_call_return_type_name(call, 'main', map[string]string{}, {
		'outer': true
	}, {
		'outer': 'gates.Outer[int]'
	}, false)
	assert inferred == 'gates.Gate[int, string]'
	assert collector.typed_receiver_method_name(inferred, 'backward', 'main')? == 'gates.Gate[int, string].backward'
}

fn test_receiver_only_generic_factory_return_substitutes_explicit_receiver_arg() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.parallel_check_sparse = true
	method := 'gates.Builder[T].make'
	tc.fn_ret_types[method] = types.Type(types.Struct{ name: 'gates.Gate[T]' })
	tc.fn_ret_types['gates.Gate[int].backward'] = types.Type(types.int_)
	base := a.add_val(.ident, 'builder')
	selector := call_helper_node(mut a, flat.Node{ kind: .selector, value: 'make' }, [base])
	arg := a.add_val(.ident, 'int')
	indexed := call_helper_node(mut a, flat.Node{ kind: .index }, [selector, arg])
	call := call_helper_node(mut a, flat.Node{ kind: .call }, [indexed])
	tc.sparse_resolved_call_names[int(call)] = method
	collector := CallCollector{ a: &a, tc: &tc }
	assert collector.generic_fn_name_is_known(method, 'main')
	inferred := collector.top_level_call_return_type_name(call, 'main', map[string]string{}, {
		'builder': true
	}, {
		'builder': 'gates.Builder[int]'
	}, false)
	assert inferred == 'gates.Gate[int]'
	assert collector.typed_receiver_method_name(inferred, 'backward', 'main')? == 'gates.Gate[int].backward'
	plain_call := call_helper_node(mut a, flat.Node{ kind: .call }, [selector])
	tc.sparse_resolved_call_names[int(plain_call)] = method
	plain_inferred := collector.top_level_call_return_type_name(plain_call, 'main',
		map[string]string{}, {
			'builder': true
		}, {
			'builder': 'gates.Builder[int]'
		}, false)
	assert plain_inferred == 'gates.Gate[int]'
	assert collector.typed_receiver_method_name(plain_inferred, 'backward', 'main')? == 'gates.Gate[int].backward'
}

fn test_generic_factory_return_uses_placeholder_signature_text() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.structs['Payload'] = []types.StructField{}
	tc.fn_generic_params['gates.identity'] = ['U']
	tc.fn_ret_types['Payload.backward'] = types.Type(types.int_)
	base := a.add_val(.ident, 'identity')
	arg := a.add_val(.ident, 'Payload')
	indexed := call_helper_node(mut a, flat.Node{ kind: .index }, [base, arg])
	unknown := types.Type(types.Unknown{ reason: 'generic U' })
	for form in ['U', '?U', '!U'] {
		tc.fn_ret_type_texts['gates.identity'] = form
		tc.fn_ret_types['gates.identity'] = match form[0] {
			`?` { types.Type(types.OptionType{ base_type: unknown }) }
			`!` { types.Type(types.ResultType{ base_type: unknown }) }
			else { unknown }
		}
		collector := CallCollector{ a: &a, tc: &tc }
		inferred := collector.generic_factory_return_type_name(a.node(indexed), 'gates.identity',
			'main', map[string]string{}, true, '')
		assert inferred == 'Payload'
		assert collector.typed_receiver_method_name(inferred, 'backward', 'main')? == 'Payload.backward'
		if form[0] in [`?`, `!`] {
			wrapped := collector.generic_factory_return_type_name(a.node(indexed), 'gates.identity',
				'main', map[string]string{}, false, '')
			assert wrapped == '${form[..1]}Payload'
		}
	}
}

fn test_generic_factory_return_uses_nested_placeholder_signature_text() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.fn_generic_params['gates.make'] = ['U']
	tc.fn_ret_types['[]ReceiverRequest.receiver_kind'] = types.Type(types.int_)
	base := a.add_val(.ident, 'make')
	arg := a.add_val(.ident, 'ReceiverRequest')
	indexed := call_helper_node(mut a, flat.Node{ kind: .index }, [base, arg])
	unknown := types.Type(types.Unknown{ reason: 'generic U' })
	array_unknown := types.Type(types.Array{ elem_type: unknown })
	for form in ['[]U', '?[]U', '![]U'] {
		tc.fn_ret_type_texts['gates.make'] = form
		tc.fn_ret_types['gates.make'] = match form[0] {
			`?` { types.Type(types.OptionType{ base_type: array_unknown }) }
			`!` { types.Type(types.ResultType{ base_type: array_unknown }) }
			else { array_unknown }
		}
		collector := CallCollector{ a: &a, tc: &tc }
		assert collector.fn_return_type_name('gates.make', true) == '[]unknown'
		assert collector.generic_factory_return_type_name(a.node(indexed), 'gates.make',
			'main', map[string]string{}, true, '') == '[]ReceiverRequest'
		assert collector.typed_receiver_method_name('[]ReceiverRequest', 'receiver_kind',
			'main')? == '[]ReceiverRequest.receiver_kind'
		if form[0] in [`?`, `!`] {
			wrapped := collector.generic_factory_return_type_name(a.node(indexed), 'gates.make',
				'main', map[string]string{}, false, '')
			assert wrapped == '${form[..1]}[]ReceiverRequest'
		}
	}
}

fn test_generic_factory_return_keeps_qualified_unknown_module_name() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.fn_generic_params['unknown_factory.make'] = ['U']
	tc.fn_ret_types['unknown_factory.make'] = types.Type(types.Struct{
		name: 'unknown_factory.Gate'
	})
	tc.fn_ret_type_texts['unknown_factory.make'] = 'Gate'
	tc.fn_ret_types['unknown_factory.Gate.backward'] = types.Type(types.int_)
	base := a.add_val(.ident, 'make')
	arg := a.add_val(.ident, 'int')
	indexed := call_helper_node(mut a, flat.Node{ kind: .index }, [base, arg])
	collector := CallCollector{ a: &a, tc: &tc }
	inferred := collector.generic_factory_return_type_name(a.node(indexed), 'unknown_factory.make',
		'main', map[string]string{}, false, '')
	assert inferred == 'unknown_factory.Gate'
	assert collector.typed_receiver_method_name(inferred, 'backward', 'main')? == 'unknown_factory.Gate.backward'
}

fn test_generic_factory_signature_text_uses_declaration_scope() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.fn_generic_params['gates.make'] = ['U']
	tc.fn_type_modules['gates.make'] = 'gates'
	tc.structs['gates.Handler'] = []types.StructField{}
	tc.structs['gates.Key'] = []types.StructField{}
	base := a.add_val(.ident, 'make')
	arg := a.add_val(.ident, 'int')
	indexed := call_helper_node(mut a, flat.Node{ kind: .index }, [base, arg])
	collector := CallCollector{ a: &a, tc: &tc }
	unknown := types.Type(types.Unknown{ reason: 'generic U' })
	tc.fn_ret_types['gates.make'] = types.Type(types.Alias{
		name:      'gates.Handler[U]'
		base_type: unknown
	})
	tc.fn_ret_type_texts['gates.make'] = 'Handler[U]'
	assert collector.generic_factory_return_type_name(a.node(indexed), 'gates.make', 'consumer',
		map[string]string{}, false, '') == 'gates.Handler[int]'
	tc.fn_ret_types['gates.make'] = types.Type(types.Map{
		key_type:   types.Type(types.Struct{ name: 'gates.Key' })
		value_type: unknown
	})
	tc.fn_ret_type_texts['gates.make'] = 'map[Key]U'
	assert collector.generic_factory_return_type_name(a.node(indexed), 'gates.make', 'consumer',
		map[string]string{}, false, '') == 'map[gates.Key]int'
}

fn test_generic_factory_signature_text_uses_declaration_imports() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	method := 'gates.make'
	tc.fn_generic_params[method] = ['U']
	tc.fn_ret_types[method] = types.Type(types.Alias{
		name:      'external.Handler[U]'
		base_type: types.Type(types.Unknown{ reason: 'generic U' })
	})
	tc.fn_ret_type_texts[method] = 'handler.Handler[U]'
	base := a.add_val(.ident, 'make')
	arg := a.add_val(.ident, 'int')
	indexed := call_helper_node(mut a, flat.Node{ kind: .index }, [base, arg])
	decl_id := a.add_node(flat.Node{ kind: .fn_decl, value: 'make' })
	collector := CallCollector{
		a:               &a
		tc:              &tc
		fn_decls:        {
			method: FnDeclInfo{ node_id: decl_id, module: 'gates', import_context: 1 }
		}
		import_contexts: [map[string]string{}, {
			'handler': 'external'
		}]
	}
	assert collector.generic_factory_return_type_name(a.node(indexed), method, 'consumer',
		map[string]string{}, false, '') == 'external.Handler[int]'
}

fn test_unindexed_generic_factory_return_infers_argument_type() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.parallel_check_sparse = true
	method := 'gates.make_gate'
	tc.fn_generic_params[method] = ['U']
	tc.fn_param_type_texts[method] = ['U']
	tc.fn_ret_types[method] = types.Type(types.Struct{ name: 'gates.Gate[U]' })
	tc.fn_ret_types['gates.Gate[T].backward'] = types.Type(types.int_)
	base := a.add_val(.ident, 'make_gate')
	value := a.add_val(.ident, 'value')
	call := call_helper_node(mut a, flat.Node{ kind: .call }, [base, value])
	tc.sparse_resolved_call_names[int(call)] = method
	collector := CallCollector{ a: &a, tc: &tc }
	inferred := collector.top_level_call_return_type_name(call, 'consumer', map[string]string{}, {
		'value': true
	}, {
		'value': 'T'
	}, false)
	assert inferred == 'gates.Gate[T]'
	assert collector.typed_receiver_method_name(inferred, 'backward', 'consumer')? == 'gates.Gate[T].backward'
}

fn test_unindexed_generic_factory_return_infers_nested_argument_types() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.parallel_check_sparse = true
	method := 'gates.make_gate'
	tc.fn_generic_params[method] = ['U']
	tc.fn_ret_types[method] = types.Type(types.Struct{ name: 'gates.Gate[U]' })
	tc.fn_ret_types['gates.Gate[T].backward'] = types.Type(types.int_)
	base := a.add_val(.ident, 'make_gate')
	value := a.add_val(.ident, 'value')
	call := call_helper_node(mut a, flat.Node{ kind: .call }, [base, value])
	tc.sparse_resolved_call_names[int(call)] = method
	collector := CallCollector{ a: &a, tc: &tc }
	for forms in [
		['Box[U]', 'Box[T]'],
		['[3]U', '[3]T'],
		['chan U', 'chan T'],
		['chan []U', 'chan []T'],
		['[]chan U', '[]chan T'],
		['[2]chan U', '[2]chan T'],
		['map[string]chan U', 'map[string]chan T'],
		['?chan U', '?chan T'],
		['&chan U', '&chan T'],
		['chan map[string][]U', 'chan map[string][]T'],
		['?U', 'T'],
		['...U', 'T'],
	] {
		tc.fn_param_type_texts[method] = [forms[0]]
		inferred := collector.top_level_call_return_type_name(call, 'consumer', map[string]string{}, {
			'value': true
		}, {
			'value': forms[1]
		}, false)
		assert inferred == 'gates.Gate[T]'
		assert collector.typed_receiver_method_name(inferred, 'backward', 'consumer')? == 'gates.Gate[T].backward'
	}
}

fn test_generic_factory_infers_semantic_channel_element_types() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.structs['Payload'] = []types.StructField{}
	for pattern in ['chan U', 'chan []U', '[]chan U', 'map[string]chan U', '?chan U', '&chan U',
		'chan ?U', 'chan map[string][]U'] {
		actual := tc.parse_type(pattern.replace('U', 'Payload'))
		mut inferred := map[string]string{}
		markused_infer_alias_generic_type(pattern, actual, ['U'], mut inferred)
		assert inferred['U'] == 'Payload', pattern
	}
	channel := types.Type(types.Channel{ elem_type: types.Type(types.Struct{ name: 'Payload' }) })
	for actual in [channel, types.Type(types.Pointer{ base_type: channel }),
		types.Type(types.Alias{ name: 'Mailbox', base_type: channel })] {
		mut inferred := map[string]string{}
		markused_infer_alias_generic_type('chan U', actual, ['U'], mut inferred)
		assert inferred['U'] == 'Payload', actual.name()
	}
	for actual in [
		types.Type(types.Channel{ elem_type: types.Type(types.Unknown{}) }),
		types.Type(types.Array{ elem_type: types.Type(types.Struct{ name: 'Payload' }) }),
	] {
		mut inferred := map[string]string{}
		markused_infer_alias_generic_type('chan U', actual, ['U'], mut inferred)
		assert inferred.len == 0
	}
}

fn test_unindexed_factory_infers_aliased_channel_element_type() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.parallel_check_sparse = true
	tc.structs['Payload'] = []types.StructField{}
	tc.type_aliases['Mailbox'] = 'chan Payload'
	tc.type_alias_modules['Mailbox'] = 'main'
	tc.type_aliases['MailboxRef'] = '&Mailbox'
	tc.type_alias_modules['MailboxRef'] = 'main'
	method := 'gates.make_gate'
	tc.fn_generic_params[method] = ['U']
	tc.fn_param_type_texts[method] = ['chan U']
	tc.fn_ret_types[method] = types.Type(types.Struct{ name: 'gates.Gate[U]' })
	tc.fn_ret_types['gates.Gate[Payload].backward'] = types.Type(types.int_)
	base := a.add_val(.ident, 'make_gate')
	value := a.add_val(.ident, 'value')
	call := call_helper_node(mut a, flat.Node{ kind: .call }, [base, value])
	tc.sparse_resolved_call_names[int(call)] = method
	collector := CallCollector{ a: &a, tc: &tc }
	for actual in ['Mailbox', '&Mailbox', 'MailboxRef'] {
		inferred := collector.top_level_call_return_type_name(call, 'main', map[string]string{}, {
			'value': true
		}, {
			'value': actual
		}, false)
		assert inferred == 'gates.Gate[Payload]', actual
		assert collector.typed_receiver_method_name(inferred, 'backward', 'main')? == 'gates.Gate[Payload].backward'
	}
}

fn test_channel_generic_reachability_inference() {
	mut a := flat.FlatAst.new()
	tc := types.TypeChecker.new(&a)
	actual := types.Type(types.Channel{ elem_type: types.Type(types.int_) })
	inferred := tc.infer_generic_reachability_type_args('gates.make_gate', 'chan U', actual,
		['U'])
	assert inferred['U'] == 'int'
}

fn test_unindexed_generic_factory_infers_spread_and_shared_arguments() {
	for pattern in ['...U', 'shared U'] {
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		tc.parallel_check_sparse = true
		method := 'gates.make_gate'
		tc.fn_generic_params[method] = ['U']
		tc.fn_param_type_texts[method] = [pattern]
		tc.fn_ret_types[method] = types.Type(types.Struct{ name: 'gates.Gate[U]' })
		callee := a.add_val(.ident, 'make_gate')
		value := a.add_val(.ident, 'value')
		arg := if pattern == '...U' {
			call_helper_node(mut a, flat.Node{ kind: .prefix, value: '...' }, [value])
		} else {
			value
		}
		call := call_helper_node(mut a, flat.Node{ kind: .call }, [callee, arg])
		tc.sparse_resolved_call_names[int(call)] = method
		collector := CallCollector{ a: &a, tc: &tc }
		actual := if pattern == '...U' { '[]T' } else { 'T' }
		inferred := collector.top_level_call_return_type_name(call, 'main', map[string]string{}, {
			'value': true
		}, {
			'value': actual
		}, false)
		assert inferred == 'gates.Gate[T]', pattern
	}
}

fn test_repeated_generic_parameter_keeps_first_alias_inference() {
	mut inferred := map[string]string{}
	for name in ['A', 'B'] {
		markused_infer_alias_generic_type('U', types.Type(types.Alias{
			name:      name
			base_type: types.Type(types.int_)
		}), ['U'], mut inferred)
	}
	assert inferred['U'] == 'A'
}

fn test_unindexed_generic_factory_uses_prior_local_type() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.parallel_check_sparse = true
	method := 'gates.make_gate'
	tc.fn_generic_params[method] = ['U']
	tc.fn_param_type_texts[method] = ['U']
	tc.fn_ret_types[method] = types.Type(types.Struct{ name: 'gates.Gate[U]' })
	tc.structs['Payload'] = []types.StructField{}
	payload_lhs := a.add_val(.ident, 'payload')
	payload_rhs := a.add_val(.struct_init, 'Payload')
	payload_decl := call_helper_node(mut a, flat.Node{ kind: .decl_assign }, [
		payload_lhs,
		payload_rhs,
	])
	gate_lhs := a.add_val(.ident, 'gate')
	callee := a.add_val(.ident, 'make_gate')
	payload_arg := a.add_val(.ident, 'payload')
	factory_call := call_helper_node(mut a, flat.Node{ kind: .call }, [callee, payload_arg])
	tc.sparse_resolved_call_names[int(factory_call)] = method
	gate_decl := call_helper_node(mut a, flat.Node{ kind: .decl_assign }, [gate_lhs, factory_call])
	body := call_helper_node(mut a, flat.Node{ kind: .block }, [payload_decl, gate_decl])
	fn_id := call_helper_node(mut a, flat.Node{ kind: .fn_decl, value: 'use_gate' }, [body])
	collector := CallCollector{
		a:            &a
		tc:           &tc
		struct_decls: {
			'Payload': StructDeclInfo{ module: 'main' }
		}
	}
	_, local_types, _ := collector.local_value_info(a.node(fn_id), 'main', map[string]string{})
	assert local_types['payload'] == 'Payload'
	assert local_types['gate'] == 'gates.Gate[Payload]'
}

fn test_generic_factory_uses_for_in_element_type() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.parallel_check_sparse = true
	method := 'gates.make_gate'
	tc.fn_generic_params[method] = ['U']
	tc.fn_param_type_texts[method] = ['U']
	tc.fn_ret_types[method] = types.Type(types.Struct{ name: 'gates.Gate[U]' })
	tc.fn_ret_types['gates.Gate[T].backward'] = types.Type(types.int_)
	param := a.add_node(flat.Node{ kind: .param, value: 'values', typ: '[]T' })
	loop_value := a.add_val(.ident, 'value')
	container := a.add_val(.ident, 'values')
	gate_lhs := a.add_val(.ident, 'gate')
	callee := a.add_val(.ident, 'make_gate')
	value_arg := a.add_val(.ident, 'value')
	factory_call := call_helper_node(mut a, flat.Node{ kind: .call }, [callee, value_arg])
	tc.sparse_resolved_call_names[int(factory_call)] = method
	gate_decl := call_helper_node(mut a, flat.Node{ kind: .decl_assign }, [gate_lhs, factory_call])
	gate_use := a.add_val(.ident, 'gate')
	backward := call_helper_node(mut a, flat.Node{ kind: .selector, value: 'backward' }, [
		gate_use,
	])
	backward_call := call_helper_node(mut a, flat.Node{ kind: .call }, [backward])
	loop_body := call_helper_node(mut a, flat.Node{ kind: .block }, [gate_decl, backward_call])
	loop := call_helper_node(mut a, flat.Node{ kind: .for_in_stmt, value: '0' }, [
		loop_value,
		flat.empty_node,
		container,
		loop_body,
	])
	body := call_helper_node(mut a, flat.Node{ kind: .block }, [loop])
	fn_id := call_helper_node(mut a, flat.Node{ kind: .fn_decl, value: 'use_gates' }, [
		param,
		body,
	])
	collector := CallCollector{ a: &a, tc: &tc, import_contexts: [map[string]string{}] }
	_, _, ident_types := collector.local_value_info(a.node(fn_id), 'main', map[string]string{})
	assert ident_types[int(value_arg)] == 'T'
	assert ident_types[int(gate_use)] == 'gates.Gate[T]'
	assert 'gates.Gate[T].backward' in collector.collect_body(a.node(fn_id), 'main',
		map[string]string{}).calls
}

fn test_generic_factory_uses_custom_iterator_element_type() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.parallel_check_sparse = true
	factory := 'gates.make_gate'
	tc.fn_generic_params[factory] = ['U']
	tc.fn_param_type_texts[factory] = ['U']
	tc.fn_ret_types[factory] = types.Type(types.Struct{ name: 'gates.Gate[U]' })
	tc.fn_ret_types['gates.Gate[T].backward'] = types.Type(types.int_)
	tc.struct_generic_params['Iter'] = ['T']
	tc.fn_ret_types['Iter[T].next'] = types.Type(types.OptionType{
		base_type: types.Type(types.Struct{ name: 'T' })
	})
	param := a.add_node(flat.Node{ kind: .param, value: 'iter', typ: 'Iter[T]' })
	loop_value := a.add_val(.ident, 'value')
	container := a.add_val(.ident, 'iter')
	callee := a.add_val(.ident, 'make_gate')
	value_arg := a.add_val(.ident, 'value')
	factory_call := call_helper_node(mut a, flat.Node{ kind: .call }, [callee, value_arg])
	tc.sparse_resolved_call_names[int(factory_call)] = factory
	gate_lhs := a.add_val(.ident, 'gate')
	gate_decl := call_helper_node(mut a, flat.Node{ kind: .decl_assign }, [gate_lhs, factory_call])
	gate_use := a.add_val(.ident, 'gate')
	backward := call_helper_node(mut a, flat.Node{ kind: .selector, value: 'backward' }, [
		gate_use,
	])
	backward_call := call_helper_node(mut a, flat.Node{ kind: .call }, [backward])
	loop_body := call_helper_node(mut a, flat.Node{ kind: .block }, [gate_decl, backward_call])
	loop := call_helper_node(mut a, flat.Node{ kind: .for_in_stmt, value: '3' }, [
		loop_value,
		flat.empty_node,
		container,
		loop_body,
	])
	body := call_helper_node(mut a, flat.Node{ kind: .block }, [loop])
	fn_id := call_helper_node(mut a, flat.Node{ kind: .fn_decl, value: 'use_gate' }, [
		param,
		body,
	])
	collector := CallCollector{ a: &a, tc: &tc, import_contexts: [map[string]string{}] }
	_, _, ident_types := collector.local_value_info(a.node(fn_id), 'main', map[string]string{})
	assert ident_types[int(value_arg)] == 'T'
	assert ident_types[int(gate_use)] == 'gates.Gate[T]'
	assert 'gates.Gate[T].backward' in collector.collect_body(a.node(fn_id), 'main',
		map[string]string{}).calls
}

fn test_generic_factory_uses_pipe_lambda_array_element_type() {
	for callback in ['map', 'filter', 'any', 'all', 'count'] {
		assert_generic_factory_pipe_lambda_array_callback(callback)
	}
}

fn assert_generic_factory_pipe_lambda_array_callback(callback string) {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.parallel_check_sparse = true
	method := 'gates.make_gate'
	tc.fn_generic_params[method] = ['U']
	tc.fn_param_type_texts[method] = ['U']
	tc.fn_ret_types[method] = types.Type(types.Struct{ name: 'gates.Gate[U]' })
	tc.fn_ret_types['gates.Gate[T].backward'] = types.Type(types.int_)
	param := a.add_node(flat.Node{ kind: .param, value: 'values', typ: '[]T' })
	values := a.add_val(.ident, 'values')
	array_selector := call_helper_node(mut a, flat.Node{ kind: .selector, value: callback }, [
		values,
	])
	lambda_param := a.add_val(.ident, 'value')
	gate_lhs := a.add_val(.ident, 'gate')
	callee := a.add_val(.ident, 'make_gate')
	value_arg := a.add_val(.ident, 'value')
	factory_call := call_helper_node(mut a, flat.Node{ kind: .call }, [callee, value_arg])
	tc.sparse_resolved_call_names[int(factory_call)] = method
	gate_decl := call_helper_node(mut a, flat.Node{ kind: .decl_assign }, [gate_lhs, factory_call])
	gate_use := a.add_val(.ident, 'gate')
	backward := call_helper_node(mut a, flat.Node{ kind: .selector, value: 'backward' }, [
		gate_use,
	])
	backward_call := call_helper_node(mut a, flat.Node{ kind: .call }, [backward])
	lambda_body := call_helper_node(mut a, flat.Node{ kind: .block }, [gate_decl, backward_call])
	lambda := call_helper_node(mut a, flat.Node{ kind: .lambda_expr }, [lambda_param, lambda_body])
	array_call := call_helper_node(mut a, flat.Node{ kind: .call }, [array_selector, lambda])
	body := call_helper_node(mut a, flat.Node{ kind: .block }, [array_call])
	fn_id := call_helper_node(mut a, flat.Node{ kind: .fn_decl, value: 'use_gates' }, [
		param,
		body,
	])
	collector := CallCollector{ a: &a, tc: &tc, import_contexts: [map[string]string{}] }
	_, _, ident_types := collector.local_value_info(a.node(fn_id), 'main', map[string]string{})
	assert ident_types[int(value_arg)] == 'T'
	assert ident_types[int(gate_use)] == 'gates.Gate[T]'
	assert 'gates.Gate[T].backward' in collector.collect_body(a.node(fn_id), 'main',
		map[string]string{}).calls
}

fn test_for_in_map_registers_key_and_value_types() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	key := a.add_val(.ident, 'key')
	value := a.add_val(.ident, 'value')
	container := a.add_val(.ident, 'items')
	loop := call_helper_node(mut a, flat.Node{ kind: .for_in_stmt, value: '3' }, [key, value,
		container])
	collector := CallCollector{ a: &a, tc: &tc }
	mut names := {
		'items': true
	}
	mut type_names := {
		'items': 'map[string]T'
	}
	collector.register_top_level_for_in_vars(a.node(loop), 'main', map[string]string{},
		mut names, mut type_names)
	assert type_names['key'] == 'string'
	assert type_names['value'] == 'T'
}

fn test_for_in_binding_does_not_change_iterable_type_during_inference() {
	for has_second in [false, true] {
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		key := a.add_val(.ident, 'items')
		value := if has_second { a.add_val(.ident, 'value') } else { flat.empty_node }
		container := a.add_val(.ident, 'items')
		loop := call_helper_node(mut a, flat.Node{ kind: .for_in_stmt, value: '3' }, [
			key,
			value,
			container,
		])
		collector := CallCollector{ a: &a, tc: &tc }
		mut names := {
			'items': true
		}
		mut types_by_name := {
			'items': 'map[string]T'
		}
		collector.register_top_level_for_in_vars(a.node(loop), 'main', map[string]string{},
			mut names, mut types_by_name)
		if has_second {
			assert types_by_name['items'] == 'string'
			assert types_by_name['value'] == 'T'
		} else {
			assert types_by_name['items'] == 'T'
		}
	}
}

fn test_for_in_reference_binding_preserves_pointer_type() {
	for mode in ['plain', 'mut', 'borrow', 'pointer'] {
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		value := a.add_val(.ident, 'value')
		values := a.add_val(.ident, 'values')
		container := if mode == 'borrow' {
			call_helper_node(mut a, flat.Node{ kind: .prefix, op: .amp }, [values])
		} else {
			values
		}
		loop := call_helper_node(mut a, flat.Node{
			kind:  .for_in_stmt
			value: '3'
			op:    if mode == 'mut' { .amp } else { .none }
		}, [value, flat.empty_node, container])
		collector := CallCollector{ a: &a, tc: &tc }
		mut names := {
			'values': true
		}
		mut types_by_name := {
			'values': if mode == 'pointer' { '&[]T' } else { '[]T' }
		}
		collector.register_top_level_for_in_vars(a.node(loop), 'main', map[string]string{},
			mut names, mut types_by_name)
		expected := if mode == 'plain' { 'T' } else { '&T' }
		assert types_by_name['value'] == expected, mode
	}
}

fn test_generic_factory_multi_return_decl_uses_component_type() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.parallel_check_sparse = true
	method := 'gates.make_pair'
	tc.fn_generic_params[method] = ['U']
	tc.fn_ret_types[method] = types.Type(types.MultiReturn{
		types: [types.Type(types.Struct{ name: 'gates.Gate[U]' }), types.Type(types.int_)]
	})
	tc.fn_ret_types['gates.Gate[T].backward'] = types.Type(types.int_)
	callee := a.add_val(.ident, 'make_pair')
	type_arg := a.add_val(.ident, 'T')
	indexed := call_helper_node(mut a, flat.Node{ kind: .index }, [callee, type_arg])
	factory_call := call_helper_node(mut a, flat.Node{ kind: .call }, [indexed])
	tc.sparse_resolved_call_names[int(factory_call)] = method
	gate_lhs := a.add_val(.ident, 'gate')
	count_lhs := a.add_val(.ident, 'count')
	decl := call_helper_node(mut a, flat.Node{ kind: .decl_assign, value: '2' }, [
		gate_lhs,
		factory_call,
		count_lhs,
	])
	gate_use := a.add_val(.ident, 'gate')
	backward := call_helper_node(mut a, flat.Node{ kind: .selector, value: 'backward' }, [
		gate_use,
	])
	backward_call := call_helper_node(mut a, flat.Node{ kind: .call }, [backward])
	body := call_helper_node(mut a, flat.Node{ kind: .block }, [decl, backward_call])
	fn_id := call_helper_node(mut a, flat.Node{ kind: .fn_decl, value: 'use_pair' }, [body])
	collector := CallCollector{ a: &a, tc: &tc, import_contexts: [map[string]string{}] }
	_, local_types, ident_types := collector.local_value_info(a.node(fn_id), 'main',
		map[string]string{})
	assert local_types['gate'] == 'gates.Gate[T]'
	assert local_types['count'] == 'int'
	assert ident_types[int(gate_use)] == 'gates.Gate[T]'
	assert 'gates.Gate[T].backward' in collector.collect_body(a.node(fn_id), 'main',
		map[string]string{}).calls
}

fn test_local_type_inference_preserves_leaf_rhs_and_closure_scope() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	param := a.add_node(flat.Node{ kind: .param, value: 'item', typ: 'A' })
	copy_lhs := a.add_val(.ident, 'copy')
	copy_rhs := a.add_val(.ident, 'item')
	copy_decl := call_helper_node(mut a, flat.Node{ kind: .decl_assign }, [copy_lhs, copy_rhs])
	closure_param := a.add_node(flat.Node{ kind: .param, value: 'item', typ: 'B' })
	closure_use := a.add_val(.ident, 'item')
	closure_body := call_helper_node(mut a, flat.Node{ kind: .block }, [closure_use])
	closure := call_helper_node(mut a, flat.Node{ kind: .fn_literal }, [closure_param, closure_body])
	callback_lhs := a.add_val(.ident, 'callback')
	callback_decl := call_helper_node(mut a, flat.Node{ kind: .decl_assign }, [
		callback_lhs,
		closure,
	])
	outer_use := a.add_val(.ident, 'item')
	copy_use := a.add_val(.ident, 'copy')
	body := call_helper_node(mut a, flat.Node{ kind: .block }, [copy_decl, callback_decl, outer_use,
		copy_use])
	fn_id := call_helper_node(mut a, flat.Node{ kind: .fn_decl, value: 'use_item' }, [
		param,
		body,
	])
	collector := CallCollector{ a: &a, tc: &tc }
	_, local_types, ident_types := collector.local_value_info(a.node(fn_id), 'main',
		map[string]string{})
	assert local_types['item'] == 'A'
	assert local_types['copy'] == 'A'
	assert ident_types[int(copy_rhs)] == 'A'
	assert ident_types[int(closure_use)] == 'B'
	assert ident_types[int(outer_use)] == 'A'
	assert ident_types[int(copy_use)] == 'A'
}

fn test_local_type_inference_preserves_simultaneous_shadowed_bindings() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	first_param := a.add_node(flat.Node{ kind: .param, value: 'first', typ: 'A' })
	second_param := a.add_node(flat.Node{ kind: .param, value: 'second', typ: 'B' })
	first_lhs := a.add_val(.ident, 'first')
	first_rhs := a.add_val(.ident, 'second')
	second_lhs := a.add_val(.ident, 'second')
	second_rhs := a.add_val(.ident, 'first')
	decl := call_helper_node(mut a, flat.Node{ kind: .decl_assign, value: '2' }, [
		first_lhs,
		first_rhs,
		second_lhs,
		second_rhs,
	])
	inner_first := a.add_val(.ident, 'first')
	inner_second := a.add_val(.ident, 'second')
	inner := call_helper_node(mut a, flat.Node{ kind: .block }, [decl, inner_first, inner_second])
	outer_first := a.add_val(.ident, 'first')
	outer_second := a.add_val(.ident, 'second')
	body := call_helper_node(mut a, flat.Node{ kind: .block }, [inner, outer_first, outer_second])
	fn_id := call_helper_node(mut a, flat.Node{ kind: .fn_decl, value: 'swap_item_types' }, [
		first_param,
		second_param,
		body,
	])
	collector := CallCollector{ a: &a, tc: &tc }
	_, local_types, ident_types := collector.local_value_info(a.node(fn_id), 'main',
		map[string]string{})
	assert local_types['first'] == 'A'
	assert local_types['second'] == 'B'
	assert ident_types[int(first_rhs)] == 'B'
	assert ident_types[int(second_rhs)] == 'A'
	assert ident_types[int(inner_first)] == 'B'
	assert ident_types[int(inner_second)] == 'A'
	assert ident_types[int(outer_first)] == 'A'
	assert ident_types[int(outer_second)] == 'B'
}

fn test_nested_local_type_does_not_replace_outer_binding() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.fn_ret_types['A.method'] = types.Type(types.int_)
	tc.fn_ret_types['B.method'] = types.Type(types.int_)
	tc.fn_ret_types['C.method'] = types.Type(types.int_)
	outer_lhs := a.add_val(.ident, 'item')
	outer_rhs := a.add_val(.struct_init, 'A')
	outer_decl := call_helper_node(mut a, flat.Node{ kind: .decl_assign }, [outer_lhs, outer_rhs])
	inner_lhs := a.add_val(.ident, 'item')
	inner_rhs := a.add_val(.struct_init, 'B')
	inner_decl := call_helper_node(mut a, flat.Node{ kind: .decl_assign }, [inner_lhs, inner_rhs])
	inner_use := a.add_val(.ident, 'item')
	inner_method := call_helper_node(mut a, flat.Node{ kind: .selector, value: 'method' }, [
		inner_use,
	])
	inner_call := call_helper_node(mut a, flat.Node{ kind: .call }, [inner_method])
	inner_block := call_helper_node(mut a, flat.Node{ kind: .block }, [inner_decl, inner_call])
	closure_param := a.add_node(flat.Node{ kind: .param, value: 'item', typ: 'C' })
	closure_use := a.add_val(.ident, 'item')
	closure_method := call_helper_node(mut a, flat.Node{ kind: .selector, value: 'method' }, [
		closure_use,
	])
	closure_call := call_helper_node(mut a, flat.Node{ kind: .call }, [closure_method])
	closure_body := call_helper_node(mut a, flat.Node{ kind: .block }, [closure_call])
	closure := call_helper_node(mut a, flat.Node{ kind: .fn_literal }, [closure_param, closure_body])
	arm_lhs := a.add_val(.ident, 'item')
	arm_rhs := a.add_val(.struct_init, 'B')
	arm_decl := call_helper_node(mut a, flat.Node{ kind: .decl_assign }, [arm_lhs, arm_rhs])
	first_arm := call_helper_node(mut a, flat.Node{ kind: .match_branch }, [arm_decl])
	later_arm_use := a.add_val(.ident, 'item')
	later_arm_method := call_helper_node(mut a, flat.Node{ kind: .selector, value: 'method' }, [
		later_arm_use,
	])
	later_arm_call := call_helper_node(mut a, flat.Node{ kind: .call }, [later_arm_method])
	second_arm := call_helper_node(mut a, flat.Node{ kind: .match_branch }, [later_arm_call])
	match_stmt := call_helper_node(mut a, flat.Node{ kind: .match_stmt }, [first_arm, second_arm])
	select_lhs := a.add_val(.ident, 'item')
	select_rhs := a.add_val(.struct_init, 'B')
	select_decl := call_helper_node(mut a, flat.Node{ kind: .decl_assign }, [
		select_lhs,
		select_rhs,
	])
	first_select_arm := call_helper_node(mut a, flat.Node{ kind: .select_branch }, [select_decl])
	later_select_use := a.add_val(.ident, 'item')
	later_select_method := call_helper_node(mut a, flat.Node{ kind: .selector, value: 'method' }, [
		later_select_use,
	])
	later_select_call := call_helper_node(mut a, flat.Node{ kind: .call }, [later_select_method])
	second_select_arm := call_helper_node(mut a, flat.Node{ kind: .select_branch }, [
		later_select_call,
	])
	select_stmt := call_helper_node(mut a, flat.Node{ kind: .select_stmt }, [
		first_select_arm,
		second_select_arm,
	])
	outer_use := a.add_val(.ident, 'item')
	outer_method := call_helper_node(mut a, flat.Node{ kind: .selector, value: 'method' }, [
		outer_use,
	])
	outer_call := call_helper_node(mut a, flat.Node{ kind: .call }, [outer_method])
	body := call_helper_node(mut a, flat.Node{ kind: .block }, [outer_decl, inner_block, closure,
		match_stmt, select_stmt, outer_call])
	fn_id := call_helper_node(mut a, flat.Node{ kind: .fn_decl, value: 'use_item' }, [body])
	collector := CallCollector{
		a:               &a
		tc:              &tc
		struct_decls:    {
			'A': StructDeclInfo{ module: 'main' }
			'B': StructDeclInfo{ module: 'main' }
			'C': StructDeclInfo{ module: 'main' }
		}
		import_contexts: [map[string]string{}]
	}
	local_values, local_types, ident_types := collector.local_value_info(a.node(fn_id), 'main',
		map[string]string{})
	assert local_types['item'] == 'A'
	assert ident_types[int(inner_use)] == 'B'
	assert ident_types[int(closure_use)] == 'C'
	assert ident_types[int(later_arm_use)] == 'A'
	assert ident_types[int(later_select_use)] == 'A'
	assert ident_types[int(outer_use)] == 'A'
	scoped := CallCollector{
		...collector
		local_ident_types: ident_types
	}
	assert scoped.top_level_receiver_type_name(inner_use, 'main', map[string]string{},
		local_values, local_types) == 'B'
	assert scoped.top_level_receiver_type_name(outer_use, 'main', map[string]string{},
		local_values, local_types) == 'A'
	assert scoped.top_level_receiver_type_name(later_arm_use, 'main', map[string]string{},
		local_values, local_types) == 'A'
	assert scoped.top_level_receiver_type_name(later_select_use, 'main', map[string]string{},
		local_values, local_types) == 'A'
	calls := collector.collect_body(a.node(fn_id), 'main', map[string]string{}).calls
	assert 'A.method' in calls
	assert 'B.method' in calls
	assert 'C.method' in calls
}

fn test_unindexed_generic_factory_return_infers_callback_result() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.parallel_check_sparse = true
	method := 'gates.make_gate'
	tc.fn_generic_params[method] = ['U']
	tc.fn_param_type_texts[method] = ['fn () U']
	tc.fn_ret_types[method] = types.Type(types.Struct{ name: 'gates.Gate[U]' })
	tc.fn_ret_types['gates.Gate[T].backward'] = types.Type(types.int_)
	base := a.add_val(.ident, 'make_gate')
	callback := a.add_val(.ident, 'callback')
	call := call_helper_node(mut a, flat.Node{ kind: .call }, [base, callback])
	tc.sparse_resolved_call_names[int(call)] = method
	collector := CallCollector{ a: &a, tc: &tc }
	inferred := collector.top_level_call_return_type_name(call, 'consumer', map[string]string{}, {
		'callback': true
	}, {
		'callback': 'fn () T'
	}, false)
	assert inferred == 'gates.Gate[T]'
	assert collector.typed_receiver_method_name(inferred, 'backward', 'consumer')? == 'gates.Gate[T].backward'
}

fn test_unindexed_generic_factory_return_infers_interface_implementer() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.parallel_check_sparse = true
	method := 'gates.make_gate'
	tc.fn_generic_params[method] = ['U']
	tc.fn_param_type_texts[method] = ['Iterable[U]']
	tc.fn_ret_types[method] = types.Type(types.Struct{ name: 'gates.Gate[U]' })
	tc.fn_ret_types['gates.Gate[int].backward'] = types.Type(types.int_)
	tc.interface_names['Iterable'] = true
	tc.interface_generic_params['Iterable'] = ['U']
	tc.interface_fields['Iterable'] = [types.StructField{
		name: 'item'
		typ:  types.Type(types.Struct{ name: 'U' })
	}]
	tc.struct_generic_params['List'] = ['T']
	tc.structs['List'] = [types.StructField{
		name: 'item'
		typ:  types.Type(types.Struct{ name: 'T' })
	}]
	base := a.add_val(.ident, 'make_gate')
	source := a.add_val(.ident, 'source')
	call := call_helper_node(mut a, flat.Node{ kind: .call }, [base, source])
	tc.sparse_resolved_call_names[int(call)] = method
	collector := CallCollector{ a: &a, tc: &tc }
	inferred := collector.top_level_call_return_type_name(call, 'consumer', map[string]string{}, {
		'source': true
	}, {
		'source': 'List[int]'
	}, false)
	assert inferred == 'gates.Gate[int]'
	assert collector.typed_receiver_method_name(inferred, 'backward', 'consumer')? == 'gates.Gate[int].backward'
}

fn test_unindexed_generic_factory_infers_short_struct_fields_together() {
	mut mismatches := []string{}
	for pattern in ['P', '[]P', '[2]P', 'map[string]P', 'Box[P]', 'fn () P', 'fn (P) P', '?P',
		'&P'] {
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		tc.parallel_check_sparse = true
		method := 'gates.make_gate'
		tc.fn_generic_params[method] = ['U']
		tc.fn_param_type_texts[method] = ['Params[U]']
		tc.fn_ret_types[method] = types.Type(types.Struct{ name: 'gates.Gate[U]' })
		tc.fn_ret_types['gates.Gate[T].backward'] = types.Type(types.int_)
		tc.struct_generic_params['gates.Params'] = ['P']
		count_decl := a.add_node(flat.Node{ kind: .field_decl, value: 'count', typ: 'int' })
		items_decl := a.add_node(flat.Node{ kind: .field_decl, value: 'items', typ: pattern })
		struct_id := call_helper_node(mut a, flat.Node{ kind: .struct_decl, value: 'Params' }, [
			count_decl,
			items_decl,
		])
		fn_id := a.add_val(.fn_decl, 'make_gate')
		callee := a.add_val(.ident, 'make_gate')
		count_value := a.add_val(.int_literal, '1')
		count := call_helper_node(mut a, flat.Node{ kind: .field_init, value: 'count' }, [
			count_value,
		])
		items_value := a.add_val(.ident, 'values')
		items := call_helper_node(mut a, flat.Node{ kind: .field_init, value: 'items' }, [
			items_value,
		])
		call := call_helper_node(mut a, flat.Node{ kind: .call }, [callee, count, items])
		tc.sparse_resolved_call_names[int(call)] = method
		collector := CallCollector{
			a:               &a
			tc:              &tc
			fn_decls:        {
				method: FnDeclInfo{ node_id: fn_id, module: 'gates' }
			}
			struct_decls:    {
				'gates.Params': StructDeclInfo{ node_id: struct_id, module: 'gates' }
			}
			import_contexts: [map[string]string{}]
		}
		inferred := collector.top_level_call_return_type_name(call, 'consumer', map[string]string{}, {
			'values': true
		}, {
			'values': pattern.replace('P', 'T')
		}, false)
		if inferred != 'gates.Gate[T]' {
			mismatches << '${pattern}: ${inferred}'
		} else {
			assert collector.typed_receiver_method_name(inferred, 'backward', 'consumer')? == 'gates.Gate[T].backward'
		}
	}
	assert mismatches.len == 0, mismatches.str()
}

fn test_inferred_factory_callback_arguments_keep_caller_generic_names() {
	mut mismatches := []string{}
	for caller in ['T', 'U'] {
		for pattern in ['fn () U', 'fn (U) int', 'fn (U) U', 'fn (fn () U) int', 'fn () []U'] {
			mut a := flat.FlatAst.new()
			mut tc := types.TypeChecker.new(&a)
			tc.parallel_check_sparse = true
			method := 'gates.make_gate'
			tc.fn_generic_params[method] = ['U']
			tc.fn_param_type_texts[method] = [pattern]
			tc.fn_ret_types[method] = types.Type(types.Struct{ name: 'gates.Gate[U]' })
			expected := 'gates.Gate[${caller}]'
			tc.fn_ret_types['${expected}.backward'] = types.Type(types.int_)
			base := a.add_val(.ident, 'make_gate')
			value := a.add_val(.ident, 'value')
			call := call_helper_node(mut a, flat.Node{ kind: .call }, [base, value])
			tc.sparse_resolved_call_names[int(call)] = method
			collector := CallCollector{ a: &a, tc: &tc }
			inferred := collector.top_level_call_return_type_name(call, 'consumer', map[string]string{}, {
				'value': true
			}, {
				'value': pattern.replace('U', caller)
			}, false)
			if inferred != expected {
				mismatches << '${pattern} / ${caller}: ${inferred}'
			} else {
				assert collector.typed_receiver_method_name(inferred, 'backward', 'consumer')? == '${expected}.backward'
			}
		}
	}
	assert mismatches.len == 0, mismatches.str()
}

fn test_inferred_factory_interface_arguments_keep_caller_generic_names() {
	mut mismatches := []string{}
	for pattern in ['Iterable[U]', 'iter.Iterable[U]'] {
		for caller in ['int', 'T', 'U'] {
			mut a := flat.FlatAst.new()
			mut tc := types.TypeChecker.new(&a)
			tc.parallel_check_sparse = true
			tc.cur_module = 'consumer'
			tc.structs['consumer.Iterable'] = []types.StructField{}
			tc.structs['iter.Iterable'] = []types.StructField{}
			method := 'gates.make_gate'
			tc.fn_type_modules[method] = 'gates'
			tc.fn_type_files[method] = 'gates.v'
			tc.file_modules['gates.v'] = 'gates'
			tc.file_imports['gates.v\niter'] = 'gates'
			tc.fn_generic_params[method] = ['U']
			tc.fn_param_type_texts[method] = [pattern]
			tc.fn_ret_types[method] = types.Type(types.Struct{ name: 'gates.Gate[U]' })
			expected := 'gates.Gate[${caller}]'
			tc.fn_ret_types['${expected}.backward'] = types.Type(types.int_)
			tc.interface_names['gates.Iterable'] = true
			tc.interface_generic_params['gates.Iterable'] = ['E']
			tc.interface_fields['gates.Iterable'] = [types.StructField{
				name: 'item'
				typ:  types.Type(types.Struct{ name: 'E' })
			}]
			tc.struct_generic_params['consumer.List'] = ['E']
			tc.structs['consumer.List'] = [types.StructField{
				name: 'item'
				typ:  types.Type(types.Struct{ name: 'E' })
			}]
			base := a.add_val(.ident, 'make_gate')
			value := a.add_val(.ident, 'value')
			call := call_helper_node(mut a, flat.Node{ kind: .call }, [base, value])
			tc.sparse_resolved_call_names[int(call)] = method
			collector := CallCollector{ a: &a, tc: &tc }
			inferred := collector.top_level_call_return_type_name(call, 'consumer', map[string]string{}, {
				'value': true
			}, {
				'value': 'consumer.List[${caller}]'
			}, false)
			assert tc.cur_module == 'consumer'
			if inferred != expected {
				mismatches << '${pattern} / ${caller}: ${inferred}'
			} else {
				assert collector.typed_receiver_method_name(inferred, 'backward', 'consumer')? == '${expected}.backward'
			}
		}
	}
	assert mismatches.len == 0, mismatches.str()
}

fn test_factory_local_inference_preserves_lexical_bindings() {
	for scope in ['block', 'closure', 'parameter', 'lambda', 'if', 'loop'] {
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		tc.parallel_check_sparse = true
		factory := 'gates.make_gate'
		tc.fn_generic_params[factory] = ['U']
		tc.fn_param_type_texts[factory] = ['U']
		tc.fn_ret_types[factory] = types.Type(types.Struct{ name: 'gates.Gate[U]' })
		for name in ['A', 'B'] {
			tc.structs[name] = []types.StructField{}
			tc.fn_ret_types['${name}.method'] = types.Type(types.int_)
			tc.fn_ret_types['gates.Gate[${name}].backward'] = types.Type(types.int_)
		}
		outer_lhs := a.add_val(.ident, 'item')
		outer_rhs := a.add_val(.struct_init, 'A')
		outer_decl := call_helper_node(mut a, flat.Node{ kind: .decl_assign }, [
			outer_lhs,
			outer_rhs,
		])
		inner_lhs := a.add_val(.ident, 'item')
		inner_rhs := a.add_val(.struct_init, 'B')
		inner_decl := call_helper_node(mut a, flat.Node{ kind: .decl_assign }, [
			inner_lhs,
			inner_rhs,
		])
		inner_call := call_helper_method_call(mut a, 'item', 'method')
		inner_body := call_helper_node(mut a, flat.Node{ kind: .block },
			if scope == 'parameter' { [inner_call] } else { [inner_decl, inner_call] })
		nested := match scope {
			'closure' { call_helper_node(mut a, flat.Node{ kind: .fn_literal }, [inner_body]) }
			'parameter' {
				param := a.add_node(flat.Node{ kind: .param, value: 'item', typ: 'B' })
				call_helper_node(mut a, flat.Node{ kind: .fn_literal }, [param, inner_body])
			}
			'lambda' {
				param := a.add_node(flat.Node{ kind: .ident, value: 'item', typ: 'B' })
				call_helper_node(mut a, flat.Node{ kind: .lambda_expr }, [param, inner_call])
			}
			'if' {
				cond := a.add_val(.bool_literal, 'true')
				call_helper_node(mut a, flat.Node{ kind: .if_expr }, [cond, inner_body])
			}
			'loop' { call_helper_node(mut a, flat.Node{ kind: .for_stmt }, [inner_body]) }
			else { inner_body }
		}
		outer_call := call_helper_method_call(mut a, 'item', 'method')
		callee := a.add_val(.ident, 'make_gate')
		argument := a.add_val(.ident, 'item')
		factory_call := call_helper_node(mut a, flat.Node{ kind: .call }, [callee, argument])
		tc.sparse_resolved_call_names[int(factory_call)] = factory
		gate_lhs := a.add_val(.ident, 'gate')
		gate_decl := call_helper_node(mut a, flat.Node{ kind: .decl_assign }, [
			gate_lhs,
			factory_call,
		])
		gate_call := call_helper_method_call(mut a, 'gate', 'backward')
		body := call_helper_node(mut a, flat.Node{ kind: .block }, [outer_decl, nested, outer_call,
			gate_decl, gate_call])
		fn_id := call_helper_node(mut a, flat.Node{ kind: .fn_decl, value: 'use_gate' }, [body])
		collector := CallCollector{
			a:               &a
			tc:              &tc
			import_contexts: [map[string]string{}]
			struct_decls:    {
				'A': StructDeclInfo{ module: 'main' }
				'B': StructDeclInfo{ module: 'main' }
			}
		}
		mut calls := []string{}
		collector.collect_calls(a.node(fn_id), 'main', map[string]string{}, '', '', mut calls)
		assert 'A.method' in calls, '${scope}: ${calls}'
		assert 'B.method' in calls, '${scope}: ${calls}'
		assert 'gates.Gate[A].backward' in calls, '${scope}: ${calls}'
		assert 'gates.Gate[B].backward' !in calls, '${scope}: ${calls}'
	}
}

fn call_helper_method_call(mut a flat.FlatAst, receiver string, method string) flat.NodeId {
	base := a.add_val(.ident, receiver)
	selector := call_helper_node(mut a, flat.Node{ kind: .selector, value: method }, [base])
	return call_helper_node(mut a, flat.Node{ kind: .call }, [selector])
}

fn test_unindexed_factory_infers_variadic_caller_types() {
	for actual in ['T', '[]T', '[2]T', 'map[string][]T', '&T'] {
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		tc.parallel_check_sparse = true
		method := 'gates.make_gate'
		tc.fn_generic_params[method] = ['U']
		tc.fn_param_type_texts[method] = ['...U']
		tc.fn_ret_types[method] = types.Type(types.Struct{ name: 'gates.Gate[U]' })
		expected := 'gates.Gate[${actual}]'
		tc.fn_ret_types['${expected}.backward'] = types.Type(types.int_)
		base := a.add_val(.ident, 'make_gate')
		value := a.add_val(.ident, 'value')
		call := call_helper_node(mut a, flat.Node{ kind: .call }, [base, value])
		tc.sparse_resolved_call_names[int(call)] = method
		collector := CallCollector{ a: &a, tc: &tc }
		inferred := collector.top_level_call_return_type_name(call, 'consumer', map[string]string{}, {
			'value': true
		}, {
			'value': actual
		}, false)
		assert inferred == expected, actual
		assert collector.typed_receiver_method_name(inferred, 'backward', 'consumer')? == '${expected}.backward'
	}
}

fn test_factory_argument_inference_handles_spreads_modifiers_and_container_aliases() {
	mut failures := []string{}
	for form in [
		['...U', '[]T', 'T', 'spread'],
		['...U', '[2]T', 'T', 'spread'],
		['...U', '[][]T', '[]T', 'spread'],
		['...[]U', '[][]T', 'T', 'spread'],
		['...[]U', '[2][]T', 'T', 'spread'],
		['...U', 'Ints', 'int', 'spread'],
		['...U', 'Pair', 'int', 'spread'],
		['shared U', 'T', 'T', 'plain'],
		['shared []U', '[]T', 'T', 'plain'],
		['mut U', 'T', 'T', 'plain'],
		['[]U', 'Ints', 'int', 'plain'],
		['[2]U', 'Pair', 'int', 'plain'],
		['map[string]U', 'Counts', 'int', 'plain'],
		['[]U', '&Ints', 'int', 'plain'],
		['[]U', 'Numbers', 'Number', 'plain'],
		['...U', 'Numbers', 'Number', 'spread'],
		['U', 'Ints', 'Ints', 'plain'],
		['...U', 'Ints', 'Ints', 'plain'],
	] {
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		tc.parallel_check_sparse = true
		for name, actual in {
			'Ints':    '[]int'
			'Pair':    '[2]int'
			'Counts':  'map[string]int'
			'Number':  'int'
			'Numbers': '[]Number'
		} {
			tc.type_aliases[name] = actual
			tc.type_alias_modules[name] = 'main'
		}
		method := 'gates.make_gate'
		tc.fn_generic_params[method] = ['U']
		tc.fn_param_type_texts[method] = [form[0]]
		tc.fn_ret_types[method] = types.Type(types.Struct{ name: 'gates.Gate[U]' })
		expected := 'gates.Gate[${form[2]}]'
		tc.fn_ret_types['${expected}.backward'] = types.Type(types.int_)
		base := a.add_val(.ident, 'make_gate')
		value := a.add_val(.ident, 'value')
		arg := if form[3] == 'spread' {
			call_helper_node(mut a, flat.Node{ kind: .prefix, value: '...' }, [value])
		} else {
			value
		}
		call := call_helper_node(mut a, flat.Node{ kind: .call }, [base, arg])
		tc.sparse_resolved_call_names[int(call)] = method
		collector := CallCollector{ a: &a, tc: &tc }
		inferred := collector.top_level_call_return_type_name(call, 'main', map[string]string{}, {
			'value': true
		}, {
			'value': form[1]
		}, false)
		if inferred != expected {
			failures << '${form}: got ${inferred}, wanted ${expected}'
		} else {
			assert collector.typed_receiver_method_name(inferred, 'backward', 'main')? == '${expected}.backward'
		}
	}
	assert failures.len == 0, failures.join('\n')
}

fn test_factory_calls_used_directly_as_receivers_keep_specialized_methods() {
	for explicit in [false, true] {
		for cached_placeholder in [false, true] {
			mut a := flat.FlatAst.new()
			mut tc := types.TypeChecker.new(&a)
			tc.parallel_check_sparse = true
			method := 'gates.make_gate'
			tc.fn_generic_params[method] = ['U']
			tc.fn_param_type_texts[method] = ['U']
			tc.fn_ret_types[method] = types.Type(types.Struct{ name: 'gates.Gate[U]' })
			tc.fn_ret_types['gates.Gate[T].backward'] = types.Type(types.int_)
			param := a.add_node(flat.Node{ kind: .param, value: 'value', typ: 'T' })
			base := a.add_val(.ident, 'make_gate')
			callee := if explicit {
				type_arg := a.add_val(.ident, 'T')
				call_helper_node(mut a, flat.Node{ kind: .index }, [base, type_arg])
			} else {
				base
			}
			value := a.add_val(.ident, 'value')
			factory := call_helper_node(mut a, flat.Node{ kind: .call }, [callee, value])
			tc.sparse_resolved_call_names[int(factory)] = method
			if cached_placeholder {
				tc.sparse_expr_type_values[int(factory)] = types.Type(types.Struct{ name: 'gates.Gate[U]' })
			}
			selector := call_helper_node(mut a, flat.Node{ kind: .selector, value: 'backward' }, [factory])
			method_call := call_helper_node(mut a, flat.Node{ kind: .call }, [selector])
			body := call_helper_node(mut a, flat.Node{ kind: .block }, [method_call])
			fn_id := call_helper_node(mut a, flat.Node{ kind: .fn_decl, value: 'use_gate' }, [
				param,
				body,
			])
			collector := CallCollector{ a: &a, tc: &tc, import_contexts: [map[string]string{}] }
			assert collector.top_level_receiver_type_name(factory, 'main', map[string]string{}, {
				'value': true
			}, {
				'value': 'T'
			}) == 'gates.Gate[T]', 'explicit=${explicit}, cached_placeholder=${cached_placeholder}'
			assert 'gates.Gate[T].backward' in collector.collect_body(a.node(fn_id), 'main', map[string]string{}).calls, 'explicit=${explicit}, cached_placeholder=${cached_placeholder}'
		}
	}
}

fn test_factory_loop_bindings_use_iterator_next_and_checked_variable_types() {
	for form in [
		['Iter[T]', 'T', 'signature'],
		['&Iter[T]', 'T', 'signature'],
		['Iter[[]T]', '[]T', 'signature'],
		['IteratorAlias', 'int', 'signature'],
		['OpaqueIterator', 'Payload', 'checked'],
	] {
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		tc.parallel_check_sparse = true
		tc.structs['Iter'] = []types.StructField{}
		tc.struct_generic_params['Iter'] = ['U']
		tc.fn_ret_types['Iter[U].next'] = types.Type(types.OptionType{ base_type: types.Type(types.Struct{ name: 'U' }) })
		tc.fn_ret_type_texts['Iter[U].next'] = '?U'
		tc.fn_param_type_texts['Iter[U].next'] = ['&Iter[U]']
		tc.type_aliases['IteratorAlias'] = 'Iter[int]'
		tc.type_alias_modules['IteratorAlias'] = 'main'
		factory := 'gates.make_gate'
		tc.fn_generic_params[factory] = ['U']
		tc.fn_param_type_texts[factory] = ['U']
		tc.fn_ret_types[factory] = types.Type(types.Struct{ name: 'gates.Gate[U]' })
		expected := 'gates.Gate[${form[1]}]'
		tc.fn_ret_types['${expected}.backward'] = types.Type(types.int_)
		param := a.add_node(flat.Node{ kind: .param, value: 'iter', typ: form[0] })
		binding := a.add_val(.ident, 'value')
		container := a.add_val(.ident, 'iter')
		value_arg := a.add_val(.ident, 'value')
		callee := a.add_val(.ident, 'make_gate')
		factory_call := call_helper_node(mut a, flat.Node{ kind: .call }, [callee, value_arg])
		tc.sparse_resolved_call_names[int(factory_call)] = factory
		lhs := a.add_val(.ident, 'gate')
		decl := call_helper_node(mut a, flat.Node{ kind: .decl_assign }, [lhs, factory_call])
		method_call := call_helper_method_call(mut a, 'gate', 'backward')
		loop := call_helper_node(mut a, flat.Node{ kind: .for_in_stmt, value: '3' }, [
			binding,
			flat.empty_node,
			container,
			decl,
			method_call,
		])
		body := call_helper_node(mut a, flat.Node{ kind: .block }, [loop])
		fn_id := call_helper_node(mut a, flat.Node{ kind: .fn_decl, value: 'use_gate' }, [
			param,
			body,
		])
		if form[2] == 'checked' {
			tc.sparse_expr_type_values[int(binding)] = types.Type(types.Struct{ name: 'Payload' })
		}
		collector := CallCollector{ a: &a, tc: &tc, import_contexts: [map[string]string{}] }
		_, _, ident_types := collector.local_value_info(a.node(fn_id), 'main', map[string]string{})
		assert ident_types[int(value_arg)] == form[1], form.str()
		assert '${expected}.backward' in collector.collect_body(a.node(fn_id), 'main', map[string]string{}).calls, form.str()
	}
}

fn test_factory_loop_bindings_resolve_the_iterable_before_shadowing_it() {
	for form in [
		['map[string]T', 'string', 'T'],
		['[]T', 'int', 'T'],
		['[2]T', 'int', 'T'],
		['string', 'int', 'u8'],
	] {
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		tc.parallel_check_sparse = true
		key := a.add_val(.ident, 'items')
		value := a.add_val(.ident, 'value')
		container := a.add_val(.ident, 'items')
		loop := call_helper_node(mut a, flat.Node{ kind: .for_in_stmt, value: '3' }, [
			key,
			value,
			container,
		])
		collector := CallCollector{ a: &a, tc: &tc }
		mut values := {
			'items': true
		}
		mut local_types := {
			'items': form[0]
		}
		collector.register_top_level_for_in_vars(a.node(loop), 'main', map[string]string{}, mut values, mut local_types)
		assert local_types['items'] == form[1], form.str()
		assert local_types['value'] == form[2], form.str()
	}
}

fn test_factory_local_inference_uses_loop_bindings() {
	for container_type, value_type in {
		'[]T':            'T'
		'[2]T':           'T'
		'map[string]T':   'T'
		'&[]T':           '&T'
		'[]map[string]T': 'map[string]T'
		'string':         'u8'
	} {
		for use_key in [false, true] {
			mut a := flat.FlatAst.new()
			mut tc := types.TypeChecker.new(&a)
			tc.parallel_check_sparse = true
			factory := 'gates.make_gate'
			tc.fn_generic_params[factory] = ['U']
			tc.fn_param_type_texts[factory] = ['U']
			tc.fn_ret_types[factory] = types.Type(types.Struct{ name: 'gates.Gate[U]' })
			expected := if use_key {
				if container_type.starts_with('map[') { 'string' } else { 'int' }
			} else {
				value_type
			}
			tc.fn_ret_types['gates.Gate[${expected}].backward'] = types.Type(types.int_)
			param := a.add_node(flat.Node{ kind: .param, value: 'items', typ: container_type })
			key := a.add_val(.ident, 'key')
			value := a.add_val(.ident, 'value')
			container := a.add_val(.ident, 'items')
			callee := a.add_val(.ident, 'make_gate')
			arg := a.add_val(.ident, if use_key { 'key' } else { 'value' })
			call := call_helper_node(mut a, flat.Node{ kind: .call }, [callee, arg])
			tc.sparse_resolved_call_names[int(call)] = factory
			lhs := a.add_val(.ident, 'gate')
			decl := call_helper_node(mut a, flat.Node{ kind: .decl_assign }, [lhs, call])
			method := call_helper_method_call(mut a, 'gate', 'backward')
			loop := call_helper_node(mut a, flat.Node{ kind: .for_in_stmt, value: '3' }, [
				key,
				value,
				container,
				decl,
				method,
			])
			body := call_helper_node(mut a, flat.Node{ kind: .block }, [loop])
			fn_id := call_helper_node(mut a, flat.Node{ kind: .fn_decl, value: 'use_gate' }, [
				param,
				body,
			])
			collector := CallCollector{ a: &a, tc: &tc, import_contexts: [map[string]string{}] }
			mut calls := []string{}
			collector.collect_calls(a.node(fn_id), 'main', map[string]string{}, '', '', mut calls)
			assert 'gates.Gate[${expected}].backward' in calls, '${container_type}, key=${use_key}: ${calls}'
		}
	}
}

fn test_factory_local_inference_decomposes_multi_return_declarations() {
	for discarded in ['', 'first', 'last'] {
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		tc.parallel_check_sparse = true
		factory := 'gates.make_pair'
		tc.fn_generic_params[factory] = ['U']
		tc.fn_ret_types[factory] = types.Type(types.MultiReturn{
			types: [
				types.Type(types.Struct{ name: 'gates.Gate[U]' }),
				types.Type(types.int_),
				types.Type(types.Struct{ name: 'gates.Gate[[]U]' }),
			]
		})
		for typ in ['T', '[]T'] {
			tc.fn_ret_types['gates.Gate[${typ}].backward'] = types.Type(types.int_)
		}
		base := a.add_val(.ident, 'make_pair')
		type_arg := a.add_val(.ident, 'T')
		callee := call_helper_node(mut a, flat.Node{ kind: .index }, [base, type_arg])
		call := call_helper_node(mut a, flat.Node{ kind: .call }, [callee])
		tc.sparse_resolved_call_names[int(call)] = factory
		first := a.add_val(.ident, if discarded == 'first' { '_' } else { 'first' })
		ignored := a.add_val(.ident, '_')
		last := a.add_val(.ident, if discarded == 'last' { '_' } else { 'last' })
		decl := call_helper_node(mut a, flat.Node{ kind: .decl_assign, value: '3' }, [
			first,
			call,
			ignored,
			last,
		])
		mut statements := [decl]
		if discarded != 'first' {
			statements << call_helper_method_call(mut a, 'first', 'backward')
		}
		if discarded != 'last' { statements << call_helper_method_call(mut a, 'last', 'backward') }
		body := call_helper_node(mut a, flat.Node{ kind: .block }, statements)
		fn_id := call_helper_node(mut a, flat.Node{ kind: .fn_decl, value: 'use_gate' }, [body])
		collector := CallCollector{ a: &a, tc: &tc, import_contexts: [map[string]string{}] }
		mut calls := []string{}
		collector.collect_calls(a.node(fn_id), 'main', map[string]string{}, '', '', mut calls)
		if discarded != 'first' {
			assert 'gates.Gate[T].backward' in calls, calls.str()
		}
		if discarded != 'last' {
			assert 'gates.Gate[[]T].backward' in calls, calls.str()
		}
	}
}

fn test_factory_local_inference_preserves_range_integer_types() {
	mut mismatches := []string{}
	for bound_type in ['int', 'i64', 'u64'] {
		for literals in ['lower', 'upper', 'both', 'constant'] {
			for range_node in [false, true] {
				mut a := flat.FlatAst.new()
				mut tc := types.TypeChecker.new(&a)
				tc.parallel_check_sparse = true
				factory := 'gates.make_gate'
				tc.fn_generic_params[factory] = ['U']
				tc.fn_param_type_texts[factory] = ['U']
				tc.fn_ret_types[factory] = types.Type(types.Struct{ name: 'gates.Gate[U]' })
				expected := if literals == 'both' { 'int' } else { bound_type }
				tc.fn_ret_types['gates.Gate[${expected}].backward'] = types.Type(types.int_)
				low_param := a.add_node(flat.Node{ kind: .param, value: 'low', typ: bound_type })
				high_param := a.add_node(flat.Node{ kind: .param, value: 'high', typ: bound_type })
				zero := a.add_val(.int_literal, '0')
				tc.const_exprs['zero'] = zero
				low := if literals == 'constant' {
					a.add_val(.ident, 'zero')
				} else if literals in ['lower', 'both'] {
					zero
				} else {
					a.add_val(.ident, 'low')
				}
				high := if literals in ['upper', 'both'] {
					a.add_val(.int_literal, '10')
				} else {
					a.add_val(.ident, 'high')
				}
				key := a.add_val(.ident, 'i')
				callee := a.add_val(.ident, 'make_gate')
				arg := a.add_val(.ident, 'i')
				call := call_helper_node(mut a, flat.Node{ kind: .call }, [callee, arg])
				tc.sparse_resolved_call_names[int(call)] = factory
				lhs := a.add_val(.ident, 'gate')
				decl := call_helper_node(mut a, flat.Node{ kind: .decl_assign }, [lhs, call])
				method := call_helper_method_call(mut a, 'gate', 'backward')
				mut children := [key, flat.empty_node]
				if range_node {
					children << call_helper_node(mut a, flat.Node{ kind: .range }, [
						low,
						high,
					])
				} else {
					children << [low, high]
				}
				children << [decl, method]
				loop := call_helper_node(mut a, flat.Node{
					kind:  .for_in_stmt
					value: if range_node {
						'3'
					} else {
						'4'
					}
				}, children)
				body := call_helper_node(mut a, flat.Node{ kind: .block }, [loop])
				fn_id := call_helper_node(mut a, flat.Node{ kind: .fn_decl, value: 'use_gate' }, [
					low_param,
					high_param,
					body,
				])
				collector := CallCollector{ a: &a, tc: &tc, import_contexts: [map[string]string{}] }
				mut calls := []string{}
				collector.collect_calls(a.node(fn_id), 'main', map[string]string{}, '', '', mut calls)
				if 'gates.Gate[${expected}].backward' !in calls {
					mismatches << '${bound_type}/${literals}/${range_node}: ${calls}'
				}
			}
		}
	}
	assert mismatches.len == 0, mismatches.str()
}

fn test_rhs_closure_type_bindings_restore_after_map_growth() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	outer_param := a.add_node(flat.Node{ kind: .param, value: 'item', typ: 'A' })
	mut closure_children := []flat.NodeId{}
	closure_children << a.add_node(flat.Node{ kind: .param, value: 'item', typ: 'B' })
	// Force the shared local-type table to grow while analyzing the RHS.
	for i in 0 .. 128 {
		closure_children << a.add_node(flat.Node{
			kind:  .param
			value: 'inner_${i}'
			typ:   'B'
		})
	}
	inner_item := a.add_val(.ident, 'item')
	inner_last := a.add_val(.ident, 'inner_127')
	closure_children << call_helper_node(mut a, flat.Node{ kind: .block }, [
		inner_item,
		inner_last,
	])
	closure := call_helper_node(mut a, flat.Node{ kind: .fn_literal }, closure_children)
	callback_lhs := a.add_val(.ident, 'callback')
	callback_decl := call_helper_node(mut a, flat.Node{ kind: .decl_assign }, [
		callback_lhs,
		closure,
	])
	copy_lhs := a.add_val(.ident, 'copy')
	copy_rhs := a.add_val(.ident, 'item')
	copy_decl := call_helper_node(mut a, flat.Node{ kind: .decl_assign }, [copy_lhs, copy_rhs])
	outer_item := a.add_val(.ident, 'item')
	outer_last := a.add_val(.ident, 'inner_127')
	body := call_helper_node(mut a, flat.Node{ kind: .block }, [callback_decl, copy_decl, outer_item,
		outer_last])
	fn_id := call_helper_node(mut a, flat.Node{ kind: .fn_decl, value: 'use_item' }, [
		outer_param,
		body,
	])
	collector := CallCollector{ a: &a, tc: &tc }
	_, local_types, ident_types := collector.local_value_info(a.node(fn_id), 'main',
		map[string]string{})
	assert local_types['item'] == 'A'
	assert local_types['copy'] == 'A'
	assert 'inner_127' !in local_types
	assert ident_types[int(inner_item)] == 'B'
	assert ident_types[int(inner_last)] == 'B'
	assert ident_types[int(copy_rhs)] == 'A'
	assert ident_types[int(outer_item)] == 'A'
	assert int(outer_last) !in ident_types
}

fn test_initializer_without_local_bindings_keeps_calls_refs_and_generics() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.parallel_check_sparse = true
	tc.fn_generic_params['factory'] = ['T']
	tc.fn_ret_types['factory'] = types.Type(types.int_)
	callee := a.add_val(.ident, 'factory')
	arg := a.add_val(.ident, 'payload')
	call := call_helper_node(mut a, flat.Node{ kind: .call }, [callee, arg])
	tc.sparse_resolved_call_names[int(call)] = 'factory'
	field := call_helper_node(mut a, flat.Node{ kind: .field_decl, value: 'value' }, [call])
	collector := CallCollector{
		a:              &a
		tc:             &tc
		fn_decls:       {
			'factory': FnDeclInfo{ node_id: field }
		}
		fn_suffixes:    {
			'factory': true
		}
		const_decls:    {
			'payload': ConstDeclInfo{ expr_id: arg }
		}
		const_suffixes: {
			'payload': true
		}
	}
	names, has_binders := markused_local_value_names(&a, a.node(field))
	assert names.len == 0 && !has_binders
	_, type_names, ident_types, visible := collector.local_value_info_with_visibility(a.node(field), 'main', map[string]string{})
	assert type_names.len == 0 && ident_types.len == 0 && visible.len == 0
	mut calls := []string{}
	assert collector.collect_calls_with_generic_usage(a.node(field), 'main', map[string]string{}, '', '', mut calls)
	assert calls == ['factory', 'factory']
	mut refs := []string{}
	collector.collect_initializer_refs(a.node(field), 'main', map[string]string{}, mut refs)
	assert refs == ['payload']
}

fn test_local_analysis_keeps_for_in_bindings_when_name_scan_is_empty() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.parallel_check_sparse = true
	value := a.add_val(.ident, 'value')
	tc.sparse_expr_type_values[int(value)] = types.Type(types.Struct{ name: 'Payload' })
	container := a.add_val(.ident, 'items')
	use_value := a.add_val(.ident, 'value')
	body := call_helper_node(mut a, flat.Node{ kind: .block }, [use_value])
	loop := call_helper_node(mut a, flat.Node{ kind: .for_in_stmt, value: '0' }, [
		value,
		flat.empty_node,
		container,
		body,
	])
	outer := call_helper_node(mut a, flat.Node{ kind: .block }, [loop])
	fn_id := call_helper_node(mut a, flat.Node{ kind: .fn_decl, value: 'use_values' }, [outer])
	collector := CallCollector{ a: &a, tc: &tc }
	names, has_binders := markused_local_value_names(&a, a.node(fn_id))
	assert names.len == 0 && has_binders
	_, type_names, ident_types := collector.local_value_info(a.node(fn_id), 'main', map[string]string{})
	assert type_names.len == 0
	assert ident_types[int(use_value)] == 'Payload'
}

fn test_reused_local_visibility_keeps_shadowed_callback_and_constant_edges() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	before := a.add_val(.ident, 'callback')
	before_const := a.add_val(.ident, 'answer')
	lhs := a.add_val(.ident, 'callback')
	rhs := a.add_val(.int_literal, '1')
	decl := call_helper_node(mut a, flat.Node{ kind: .decl_assign }, [lhs, rhs])
	inner := a.add_val(.ident, 'callback')
	const_lhs := a.add_val(.ident, 'answer')
	const_rhs := a.add_val(.int_literal, '2')
	const_decl := call_helper_node(mut a, flat.Node{ kind: .decl_assign }, [const_lhs, const_rhs])
	inner_const := a.add_val(.ident, 'answer')
	block := call_helper_node(mut a, flat.Node{ kind: .block }, [decl, inner, const_decl, inner_const])
	lambda_param := a.add_node(flat.Node{ kind: .ident, value: 'callback', typ: 'int' })
	lambda_use := a.add_val(.ident, 'callback')
	lambda_body := call_helper_node(mut a, flat.Node{ kind: .block }, [lambda_use])
	lambda := call_helper_node(mut a, flat.Node{ kind: .lambda_expr }, [lambda_param, lambda_body])
	after := a.add_val(.ident, 'callback')
	after_const := a.add_val(.ident, 'answer')
	body := call_helper_node(mut a, flat.Node{ kind: .block }, [before, before_const, block, lambda,
		after, after_const])
	fn_id := call_helper_node(mut a, flat.Node{ kind: .fn_decl, value: 'use_callbacks' }, [body])
	collector := CallCollector{
		a:               &a
		tc:              &tc
		fn_decls:        {
			'callback': FnDeclInfo{ node_id: fn_id }
		}
		fn_suffixes:     {
			'callback': true
		}
		const_decls:     {
			'answer': ConstDeclInfo{ expr_id: const_rhs }
		}
		const_suffixes:  {
			'answer': true
		}
		import_contexts: [map[string]string{}]
	}
	names, _, _, visible := collector.local_value_info_with_visibility(a.node(fn_id), 'main', map[string]string{})
	assert collector.local_values_need_visibility(names, 'main', map[string]string{})
	assert int(inner) in visible && int(inner_const) in visible && int(lambda_use) in visible
	assert int(before) !in visible && int(before_const) !in visible
	assert int(after) !in visible && int(after_const) !in visible
	result := collector.collect_body(a.node(fn_id), 'main', map[string]string{})
	// Preserve the prior path, which separately recomputed visibility after
	// inference. The lambda's parameter declarator is also visited by the
	// fallback function-value walk before that binding applies to its body.
	old_names, old_types, old_ident_types := collector.local_value_info(a.node(fn_id), 'main', map[string]string{})
	old_visible := markused_visible_local_idents(&a, a.node(fn_id), old_names)
	assert visible == old_visible
	control := CallCollector{
		...collector
		local_ident_visibility:     old_visible
		local_ident_types:          old_ident_types
		has_local_ident_visibility: true
	}
	mut old_calls := []string{}
	control.collect_calls_with_locals(a.node(fn_id), 'main', map[string]string{}, '', '', old_names, old_types, old_visible, mut old_calls)
	assert result.calls == old_calls
	assert result.calls == ['callback', 'callback', 'callback']
	mut param_calls := []string{}
	control.collect_fn_value_ident(lambda_param, 'callback', 'main', map[string]string{}, false, mut param_calls)
	assert param_calls == ['callback']
	assert result.refs == ['answer']
	assert !result.uses_generics
	mut refs := []string{}
	collector.collect_initializer_refs(a.node(fn_id), 'main', map[string]string{}, mut refs)
	assert refs == result.refs
}
