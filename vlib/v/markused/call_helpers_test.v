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
			_, local_types := collector.local_value_info(a.node(fn_id), 'gates', {
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
