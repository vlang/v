module transform

import os
import v.flat
import v.parser
import v.pref
import v.types

fn test_generic_declaration_index_preserves_appended_templates_and_erasure() {
	path := os.join_path(os.vtmp_dir(), 'generic_decl_index_${os.getpid()}.v')
	os.write_file(path, 'module source\nstruct Box[T] { value T }\ntype Choice[T] = T | string\n')!
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	assert tc.top_level_idx.len > 0
	a.add_val(.file, 'appended.v')
	a.add_val(.module_decl, 'later')
	mut appended := flat.Node{ kind: .struct_decl, value: 'Added' }
	appended.set_generic_params(['T'])
	added := a.add_node(appended)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	indexed_structs := t.collect_generic_struct_decls()
	indexed_sums := t.collect_generic_sum_decls()
	assert 'source.Box' in indexed_structs
	assert indexed_structs['source.Box'].file == path
	assert indexed_structs['later.Added'].id == added
	assert indexed_structs['later.Added'].file == 'appended.v'
	assert indexed_sums['source.Choice'].file == path
	tc.top_level_idx.clear()
	assert t.collect_generic_struct_decls() == indexed_structs
	assert t.collect_generic_sum_decls() == indexed_sums
	t.set_node(int(added), flat.Node{})
	assert 'later.Added' !in t.collect_generic_struct_decls()
}

fn test_node_context_cache_growth_preserves_ids_and_initializes_new_slots() {
	mut cache := []u16{}
	grow_node_context_cache(mut cache, 3, 8)
	cache[0] = 7
	cache[2] = 9
	grow_node_context_cache(mut cache, 6, 8)
	assert cache == [u16(7), 0, 9, 0, 0, 0]
	cache[5] = 11
	grow_node_context_cache(mut cache, 12, 16)
	assert cache[..6] == [u16(7), 0, 9, 0, 0, 11]
	assert cache[6..] == [u16(0), 0, 0, 0, 0, 0]
	grow_node_context_cache(mut cache, 2, 16)
	assert cache.len == 12
	assert cache[5] == 11
}

fn test_specialized_fn_signature_types_are_recorded_once() {
	mut a := flat.FlatAst.new()
	param_id := a.add_node(flat.Node{
		kind:  .param
		value: 'value'
		typ:   flat.source_type_id('T')
	})
	children_start := a.children.len
	a.children << param_id
	mut fn_node := flat.Node{
		kind:           .fn_decl
		value:          'identity'
		typ:            flat.source_type_id('T')
		children_start: children_start
		children_count: 1
	}
	fn_node.set_generic_params(['T'])
	fn_id := a.add_node(fn_node)
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	decl := GenericFnDecl{
		id:     fn_id
		node:   fn_node
		module: 'main'
		file:   'main.v'
		key:    'identity'
	}

	t.request_generic_fn_specialization(decl, flat.TypeTextList.from_names(['int']).ids)
	assert t.monomorph_signature_types.len == 2
	t.register_specialized_fn_signature_value(decl, specialized_generic_fn_value(fn_node.value, flat.TypeTextList.from_names([
		'int',
	]).ids), flat.TypeTextList.from_names(['int']).ids)

	assert t.monomorph_signature_types.len == 2
	assert t.monomorph_signature_spec_keys.len == 1
}

fn test_comptime_loop_type_metadata_survives_generic_specialization() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cloning_comptime_for_depth = 1
	t.cloning_comptime_for_vars = ['field']

	assert t.subst_comptime_type_operand('field.typ', flat.TypeTextList.from_names(['Address']).ids) == 'field.typ'
	assert t.subst_comptime_type_operand('field.unaliased_typ', flat.TypeTextList.from_names(['Address']).ids) == 'field.unaliased_typ'
	assert t.subst_comptime_type_operand('field.unaliased_typ.payload_type', flat.TypeTextList.from_names([
		'Address',
	]).ids) == 'field.unaliased_typ.payload_type'
	t.cur_module = 'veb'
	assert t.subst_comptime_type_operand("'Middleware'", flat.TypeTextList.from_names(['App']).ids) == "'Middleware'"
	assert t.subst_comptime_type_condition("field.name == 'Middleware'", flat.TypeTextList.from_names([
		'App',
	]).ids) == "field.name == 'Middleware'"
}

fn test_comptime_condition_substitution_folds_type_condition_beside_target_flag() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.active_generic_params = ['T']

	assert t.subst_comptime_type_condition('windows && T is int', flat.TypeTextList.from_names(['string']).ids) == 'false'
	assert t.subst_comptime_type_condition('windows && T is int', flat.TypeTextList.from_names(['int']).ids) == 'windows'
	assert t.subst_comptime_type_condition('windows && T.indirections != 0', flat.TypeTextList.from_names(['int']).ids) == 'false'
	assert t.subst_comptime_type_condition('windows && T.indirections != 0', flat.TypeTextList.from_names(['&int']).ids) == 'windows'
}

fn test_generic_unresolved_type_detects_multi_return_placeholders() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.generic_arg_is_unresolved(flat.source_type_id('(T, T)'))
	assert t.generic_arg_is_unresolved(flat.source_type_id('([]T, map[string]T)'))
	assert !t.generic_arg_is_unresolved(flat.source_type_id('(f64, f64)'))
}

fn test_collision_locked_main_struct_is_a_concrete_generic_argument() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.structs['T'] = StructInfo{ name: flat.source_type_id('T'), module: 'main' }
	t.structs['S'] = StructInfo{ name: flat.source_type_id('S'), module: 'foreign' }
	t.cur_module = 'foreign'

	assert !t.generic_arg_is_unresolved(flat.source_type_id('main.T'))
	assert !t.generic_arg_is_unresolved(flat.source_type_id('[]main.T'))
	assert t.explicit_generic_arg_is_known_type('main.T', 'foreign')
	assert t.explicit_generic_arg_is_known_type('[]main.T', 'foreign')
	assert t.generic_arg_is_unresolved(flat.source_type_id('main.S'))
	assert !t.explicit_generic_arg_is_known_type('main.S', 'foreign')
	assert t.generic_arg_is_unresolved(flat.source_type_id('main.U'))
	assert !t.explicit_generic_arg_is_known_type('main.U', 'foreign')

	tc.imports['main'] = 'other'
	assert !t.is_known_concrete_type_name('main.T')
	assert !t.explicit_generic_arg_is_known_type('main.T', 'foreign')
}

fn test_declared_receiver_parameters_use_the_receiver_module() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.struct_generic_params['Box'] = ['A']
	tc.struct_generic_params['foreign.Box'] = ['T']
	tc.struct_generic_params['foreign.Pair'] = ['K', 'V']
	t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.declared_generic_receiver_param_names(flat.Node{ kind: .fn_decl, value: 'Box[T].get' },
		'foreign') == ['T']
	assert t.declared_generic_receiver_param_names(flat.Node{ kind: .fn_decl, value: 'Box[A].get' },
		'foreign').len == 0
	assert t.declared_generic_receiver_param_names(flat.Node{ kind: .fn_decl, value: 'Box[int].get' },
		'foreign').len == 0
	assert t.declared_generic_receiver_param_names(flat.Node{ kind: .fn_decl, value: 'Pair[K, int].get' },
		'foreign') == ['K']
}

fn test_generic_field_type_substitutes_fixed_array_length_expr() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.struct_generic_params['PaddedSlot'] = ['T']
	t := new_transformer(mut a, &tc, map[string]bool{})

	assert flat.source_type_name(t.normalize_field_type(flat.source_type_id('[32 - sizeof(T)]u8'), flat.source_type_id('PaddedSlot[int]'))) ==
		'[32 - sizeof(int)]u8'
}

fn test_zero_value_normalizes_generic_alias_but_preserves_generic_struct() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.type_aliases.set_id('Values', flat.source_type_id('[]T'))
	tc.type_alias_generic_params['Values'] = ['T']
	tc.structs['Box'] = []types.StructField{}
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	alias_zero_id := t.zero_value_for_type(flat.source_type_id('Values[int]'))
	alias_zero := t.a.nodes[int(alias_zero_id)]
	assert alias_zero.kind == .array_init
	assert alias_zero.value == 'int'
	assert alias_zero.typ_text() == '[]int'

	struct_zero_id := t.zero_value_for_type(flat.source_type_id('Box[int]'))
	struct_zero := t.a.nodes[int(struct_zero_id)]
	assert struct_zero.kind == .struct_init
	assert struct_zero.value == 'Box[int]'
	assert struct_zero.typ_text() == 'Box[int]'
}

fn test_explicit_generic_fn_value_candidates_resolve_selective_import() {
	mut a := flat.FlatAst.new()
	base_id := a.add_node(flat.Node{
		kind:  .ident
		value: 'id'
	})
	index_id := a.add_node(flat.Node{
		kind: .index
	})
	mut tc := types.TypeChecker.new(&a)
	file_name := '/tmp/main.v'
	tc.file_selective_imports[file_import_key(file_name, 'id')] = ['lib.id']
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	// The node context caches are sized together; a node without a file id
	// takes its context from its module entry.
	t.node_module_map_cache = []u16{len: a.nodes.len}
	t.node_file_map_cache = []u16{len: a.nodes.len}
	t.node_file_map_cache[int(index_id)] = t.node_context_text_id(file_name)

	candidates := t.explicit_generic_fn_value_decl_candidates(index_id, base_id, a.nodes[int(base_id)], 'main')
	assert candidates[0] == 'lib.id'
	assert 'id' in candidates
}

fn test_specialized_signature_qualifies_selective_import_in_wrappers() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	file_name := '/tmp/main.v'
	tc.structs['payloads.Payload'] = []types.StructField{}
	tc.file_selective_imports[file_import_key(file_name, 'Payload')] = ['payloads.Payload']
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	decl := GenericFnDecl{ file: file_name }
	for prefix in ['atomic ', 'chan ', 'thread ', '?[]'] {
		assert flat.source_type_name(t.qualify_specialized_signature_type_text(flat.source_type_id('${prefix}Payload'), decl)) == '${prefix}payloads.Payload'
	}
}

fn test_explicit_generic_arg_requires_a_known_type() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.explicit_generic_arg_is_known_type('int', 'main')
	assert t.explicit_generic_arg_is_known_type('[]string', 'main')
	assert t.explicit_generic_arg_is_known_type('map[string]int', 'main')
	assert !t.explicit_generic_arg_is_known_type('i', 'builtin')
}

fn test_materialized_generic_struct_fields_preserve_plain_alias_arguments() {
	mut a := flat.FlatAst.new()
	mut values_alias := flat.Node{
		kind:  .type_decl
		value: 'Values'
		typ:   flat.source_type_id('[]T')
	}
	values_alias.set_generic_params(['T'])
	a.add_node(values_alias)

	value_field := a.add_node(flat.Node{
		kind:  .field_decl
		value: 'value'
		typ:   flat.source_type_id('T')
	})
	values_field := a.add_node(flat.Node{
		kind:  .field_decl
		value: 'values'
		typ:   flat.source_type_id('Values[T]')
	})
	children_start := a.children.len
	a.children << value_field
	a.children << values_field
	mut box_decl := flat.Node{
		kind:           .struct_decl
		value:          'Box'
		children_start: children_start
		children_count: 2
	}
	box_decl.set_generic_params(['T'])
	box_id := a.add_node(box_decl)

	mut tc := types.TypeChecker.new(&a)
	tc.cur_module = 'main'
	tc.type_aliases.set_id('UserId', flat.source_type_int)
	tc.type_aliases.set_id('Values', flat.source_type_id('[]T'))
	tc.type_alias_generic_params['Values'] = ['T']
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_module = 'main'
	t.materialize_generic_struct_spec(flat.source_type_id('Box[UserId]'), GenericStructDecl{
		id:     box_id
		node:   box_decl
		module: 'main'
		key:    'Box'
	})

	fields := tc.structs['Box[UserId]'] or {
		assert false, 'missing materialized Box[UserId] fields'
		return
	}
	assert fields.len == 2
	assert fields[0].typ is types.Alias
	assert fields[0].typ.name() == 'UserId'
	assert fields[1].typ is types.Array
	assert fields[1].typ.name() == '[]int'
}

fn test_materialized_imported_generic_struct_preserves_locked_main_generic_argument() {
	mut a := flat.FlatAst.new()
	value_field := a.add_node(flat.Node{
		kind:  .field_decl
		value: 'value'
		typ:   flat.source_type_id('T')
	})
	children_start := a.children.len
	a.children << value_field
	mut result_decl := flat.Node{
		kind:           .struct_decl
		value:          'StructKeyDecodeResult'
		children_start: children_start
		children_count: 1
	}
	result_decl.set_generic_params(['T'])
	result_id := a.add_node(result_decl)

	mut tc := types.TypeChecker.new(&a)
	tc.struct_generic_params['StructType'] = ['T']
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.materialize_generic_struct_spec(flat.source_type_id('json2.StructKeyDecodeResult[main.StructType[main.string]]'), GenericStructDecl{
		id:     result_id
		node:   result_decl
		module: 'json2'
		key:    'json2.StructKeyDecodeResult'
	})

	fields := tc.structs['json2.StructKeyDecodeResult[main.StructType[main.string]]'] or {
		assert false, 'missing materialized result fields'
		return
	}
	assert fields[0].typ.name() == 'StructType[string]'
}

fn test_flattened_generic_struct_types_materialize_from_recorded_args() {
	mut a := flat.FlatAst.new()
	mut arc_decl := flat.Node{
		kind:  .struct_decl
		value: 'Arc'
	}
	arc_decl.set_generic_params(['T'])
	arc_id := a.add_node(arc_decl)
	mut tc := types.TypeChecker.new(&a)
	tc.sum_types.set('ResourceSum', ['Resource', 'int'])
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.record_generic_specialization_args_in_module('Arc', 'arc', flat.TypeTextList.from_names(['ResourceSum']).ids)
	decls := {
		'arc.Arc': GenericStructDecl{
			id:     arc_id
			node:   arc_decl
			module: 'arc'
			key:    'arc.Arc'
		}
	}
	mut specs := map[u32]u32{}
	t.collect_generic_struct_spec_from_type(flat.source_type_id('arc.Arc_ResourceSum'), 'main', '', decls, mut specs)
	assert specs[flat.source_type_id('arc.Arc[ResourceSum]')] == flat.source_type_id('arc.Arc')
}

fn test_flattened_generic_struct_arg_canonicalizes_to_source_application() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.struct_generic_params['StructType'] = ['T']
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.record_generic_specialization_args_in_module('StructType', 'main', flat.TypeTextList.from_names(['int']).ids)

	assert t.tc.struct_generic_params['StructType'] == ['T']
	assert flat.TypeTextList{ ids: t.recorded_type_specialization_args(flat.source_type_id('StructType_int')) or { []u32{} } }.names() == [
		'int',
	]
	assert generic_specialized_type_matches_flat_name('StructType_int', 'StructType', [
		'int',
	])
	assert flat.source_type_name(t.generic_specialized_source_type_name(flat.source_type_id('StructType_int')) or { u32(0) }) == 'StructType[int]'
	assert flat.source_type_name(t.canonical_generic_specialization_arg(flat.source_type_id('StructType_int'))) == 'StructType[int]'
	assert flat.source_type_name(t.generic_specialized_source_type_name(flat.source_type_id('&StructType_int')) or { u32(0) }) == '&StructType[int]'
	assert flat.source_type_name(t.canonical_generic_specialization_arg(flat.source_type_id('&StructType[int]'))) == '&StructType[int]'

	t.record_generic_specialization_args_in_module('StructType', 'main', flat.TypeTextList.from_names(['time.Time']).ids)
	assert c_name('StructType[time.Time]') == 'StructType_time__Time'
	assert flat.TypeTextList{ ids: t.recorded_type_specialization_args(flat.source_type_id('StructType_time.Time')) or { []u32{} } }.names() == [
		'time.Time',
	]
	assert flat.source_type_name(t.generic_specialized_source_type_name(flat.source_type_id('StructType_time.Time')) or { u32(0) }) == 'StructType[time.Time]'
	assert flat.source_type_name(t.canonical_generic_specialization_arg(flat.source_type_id('StructType_time.Time'))) == 'StructType[time.Time]'
}

fn test_short_fixed_array_generic_arg_canonicalizes_to_source_type() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	t := new_transformer(mut a, &tc, map[string]bool{})

	assert flat.source_type_name(t.canonical_generic_specialization_arg(flat.source_type_id('int_2'))) == '[2]int'
	assert flat.source_type_name(t.canonical_generic_specialization_arg(flat.source_type_id('int_3_2'))) == '[2][3]int'
	tc.type_aliases.set_id('int_2', flat.source_type_int)
	assert flat.source_type_name(t.canonical_generic_specialization_arg(flat.source_type_id('int_2'))) == 'int_2'
}

fn test_composite_generic_inference_preserves_nested_alias() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.type_aliases.set_id('Distance', flat.source_type_int)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.build_generic_alias_name_index()

	assert flat.source_type_name(t.generic_composite_inference_alias_type(flat.source_type_id('datatypes.LinkedList[Distance]'), 'datatypes')) == 'datatypes.LinkedList[Distance]'
}

fn test_composite_generic_inference_still_expands_direct_alias() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.type_aliases.set_id('Distances', flat.source_type_id('[]int'))
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.build_generic_alias_name_index()

	assert flat.source_type_name(t.generic_composite_inference_alias_type(flat.source_type_id('Distances'), 'main')) == '[]int'
}

fn test_concrete_generic_fn_alias_call_expands_multi_return_signature() {
	mut a := flat.FlatAst.new()
	callee_id := a.add_node(flat.Node{
		kind:  .ident
		value: 'make_splitter'
	})
	children_start := a.children.len
	a.children << callee_id
	call_id := a.add_node(flat.Node{
		kind:           .call
		typ:            flat.source_type_id('fn(I) (O, R)[string, string, string]')
		children_start: children_start
		children_count: 1
	})
	mut tc := types.TypeChecker.new(&a)
	tc.type_aliases.set_id('FnMultiReturn', flat.source_type_id('fn (I) (O, R)'))
	tc.type_alias_generic_params['FnMultiReturn'] = ['I', 'O', 'R']
	tc.fn_ret_types['make_splitter'] = types.Type(types.Alias{
		name_id:   flat.source_type_id('FnMultiReturn[string, string, string]')
		base_type: &types.Type(types.void_)
	})
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_module = 'main'

	concrete := flat.source_type_name(t.concrete_fn_alias_call_return_type(int(call_id), a.nodes[int(call_id)]) or {
		assert false, 'generic function type alias was not expanded'
		return
	})
	assert concrete == 'fn(string) (string, string)'
}

fn test_concrete_generic_fn_alias_call_expands_result_signature() {
	mut a := flat.FlatAst.new()
	callee_id := a.add_node(flat.Node{
		kind:  .ident
		value: 'literal'
	})
	children_start := a.children.len
	a.children << callee_id
	call_id := a.add_node(flat.Node{
		kind:           .call
		typ:            flat.source_type_id('fn(string) !ParseResult[T][string]')
		children_start: children_start
		children_count: 1
	})
	mut tc := types.TypeChecker.new(&a)
	tc.type_aliases.set_id('ParseFunction', flat.source_type_id('fn (string) !ParseResult[T]'))
	tc.type_alias_generic_params['ParseFunction'] = ['T']
	tc.fn_ret_types['literal'] = types.Type(types.Alias{
		name_id:   flat.source_type_id('ParseFunction[string]')
		base_type: &types.Type(types.void_)
	})
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_module = 'main'

	concrete := flat.source_type_name(t.concrete_fn_alias_call_return_type(int(call_id), a.nodes[int(call_id)]) or {
		assert false, 'generic result function type alias was not expanded'
		return
	})
	assert concrete == 'fn(string) !ParseResult[string]'
}

fn test_lock_colliding_main_generic_type_text_locks_args_behind_qualified_base() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	// The program (main) declares `Context` (bare-keyed); the `callee` module declares a
	// colliding `Context` of its own.
	t.structs['Context'] = StructInfo{}
	t.structs['callee.Context'] = StructInfo{}
	t.structs['MiddlewareOptions'] = StructInfo{}
	t.structs['callee.MiddlewareOptions'] = StructInfo{}
	tc.struct_generic_params['MiddlewareOptions'] = ['T']
	tc.struct_generic_params['callee.MiddlewareOptions'] = ['T']

	// A bare program type substituted into a specialization is locked to `main.`.
	assert flat.source_type_name(t.lock_colliding_main_generic_type_text(flat.source_type_id('Context'), 'callee')) == 'main.Context'
	// A qualified generic base with a bare program argument (`other.Box[Context]`) must
	// still lock the nested `Context`, not return early on the qualified spelling.
	assert flat.source_type_name(t.lock_colliding_main_generic_type_text(flat.source_type_id('other.Box[Context]'), 'callee')) == 'other.Box[main.Context]'
	// The same nested program type behind a map / fixed array is locked too.
	assert flat.source_type_name(t.lock_colliding_main_generic_type_text(flat.source_type_id('map[string]Context'), 'callee')) == 'map[string]main.Context'
	assert flat.source_type_name(t.lock_colliding_main_generic_type_text(flat.source_type_id('[3]Context'), 'callee')) == '[3]main.Context'
	assert flat.source_type_name(t.lock_colliding_main_generic_type_text(flat.source_type_id('(Context, []Context)'), 'callee')) == '(main.Context, []main.Context)'
	// A simple qualified type has no lockable bare component and is returned verbatim.
	assert flat.source_type_name(t.lock_colliding_main_generic_type_text(flat.source_type_id('veb.Context'), 'callee')) == 'veb.Context'
	// An already-qualified argument keeps its exact spelling (no rewrite / name desync).
	assert flat.source_type_name(t.lock_colliding_main_generic_type_text(flat.source_type_id('other.Box[user.LocalWriter]'), 'callee')) == 'other.Box[user.LocalWriter]'
	// No lock target: the callee module does not declare `Other`, so nothing changes.
	assert flat.source_type_name(t.lock_colliding_main_generic_type_text(flat.source_type_id('other.Box[Other]'), 'callee')) == 'other.Box[Other]'
	// A module-local generic base must stay local even when one of its concrete arguments
	// is a colliding main type. `callee.MiddlewareOptions` is not `main.MiddlewareOptions`.
	assert flat.source_type_name(t.lock_colliding_main_generic_type_text(flat.source_type_id('MiddlewareOptions[Context]'), 'callee')) == 'MiddlewareOptions[main.Context]'
	// When the generic base itself is an active caller type, caller provenance wins over the
	// otherwise ambiguous pair of main/callee generic declarations.
	t.structs['Box'] = StructInfo{}
	t.structs['callee.Box'] = StructInfo{}
	tc.struct_generic_params['Box'] = ['T']
	tc.struct_generic_params['callee.Box'] = ['T']
	t.active_specialization_main_types[flat.source_type_id('Box')] = true
	assert flat.source_type_name(t.lock_colliding_main_generic_type_text(flat.source_type_id('Box[string]'), 'callee')) == 'main.Box[string]'
	// A main type that is active in the specialization is locked even without a
	// callee homonym, since a different imported module can own the same short name.
	t.structs['Event'] = StructInfo{}
	t.structs['other.Event'] = StructInfo{}
	t.structs['string'] = StructInfo{
		module: 'builtin'
	}
	t.active_specialization_main_types[flat.source_type_id('Event')] = true
	assert flat.source_type_name(t.lock_colliding_main_generic_type_text(flat.source_type_id('Event'), 'callee')) == 'main.Event'
	t.active_specialization_main_types[flat.source_type_id('Context')] = true
	assert flat.source_type_name(t.lock_colliding_main_substitution_type_text(flat.source_type_id('fn (mut T) bool'), flat.source_type_id('fn (mut Context) bool'), 'callee', [
		'T',
	])) == 'fn (mut main.Context) bool'
	t.structs['Unique'] = StructInfo{}
	assert flat.source_type_name(t.lock_colliding_main_generic_type_text(flat.source_type_id('Unique'), 'callee')) == 'Unique'
	assert flat.source_type_name(t.canonical_generic_specialization_arg(flat.source_type_id('[]main.Event'))) == '[]Event'
	assert flat.source_type_name(t.canonical_generic_specialization_arg(flat.source_type_id('main.Box[main.Event]'))) == 'Box[Event]'
	assert flat.source_type_name(t.canonical_generic_specialization_arg(flat.source_type_id('[main.string]Box'))) == 'Box[string]'
	assert flat.source_type_name(t.canonical_generic_specialization_arg(flat.source_type_id('u8_3'))) == '[3]u8'
	assert strip_main_type_locks('domain.Type[main.Event]') == 'domain.Type[Event]'
	assert t.specialization_main_type_closure(flat.TypeTextList.from_names(['map[string]Event']).ids) == {
		flat.source_type_id('Event'): true
	}
	tc.type_aliases.set_id('AliasContext', flat.source_type_id('Context'))
	assert t.specialization_main_type_closure(flat.TypeTextList.from_names(['AliasContext']).ids) == {
		flat.source_type_id('AliasContext'): true
		flat.source_type_id('Context'):      true
	}
	t.active_specialization_main_types[flat.source_type_id('AliasContext')] = true
	assert flat.source_type_name(t.lock_colliding_main_generic_type_text(flat.source_type_id('AliasContext'), 'callee')) == 'main.AliasContext'
}

fn test_lock_colliding_main_generic_type_text_locks_program_aliases() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.structs['Cell'] = StructInfo{}
	t.structs['callee.Cell'] = StructInfo{
		module: 'callee'
	}
	tc.type_aliases.set_id('Cells', flat.source_type_id('[]Cell'))
	tc.type_alias_modules['Cells'] = 'main'
	tc.type_aliases.set_id('Ids', flat.source_type_id('[]int'))
	tc.type_alias_modules['Ids'] = 'main'
	// A program alias target is resolved where the alias is parsed, so an alias of a
	// colliding program type is locked even when it is not an active specialization type.
	assert flat.source_type_name(t.lock_colliding_main_generic_type_text(flat.source_type_id('Cells'), 'callee')) == 'main.Cells'
	assert flat.source_type_name(t.lock_colliding_main_generic_type_text(flat.source_type_id('map[string]Cells'), 'callee')) == 'map[string]main.Cells'
	// Without a colliding name or target the alias keeps its bare spelling.
	assert flat.source_type_name(t.lock_colliding_main_generic_type_text(flat.source_type_id('Ids'), 'callee')) == 'Ids'
	tc.type_aliases.set_id('callee.Ids', flat.source_type_id('[]u8'))
	tc.type_alias_modules['callee.Ids'] = 'callee'
	assert flat.source_type_name(t.lock_colliding_main_generic_type_text(flat.source_type_id('Ids'), 'callee')) == 'main.Ids'
	// Module aliases are already qualified.
	tc.type_aliases.set_id('other.Cells', flat.source_type_id('[]other.Cell'))
	tc.type_alias_modules['other.Cells'] = 'other'
	assert flat.source_type_name(t.lock_colliding_main_generic_type_text(flat.source_type_id('other.Cells'), 'callee')) == 'other.Cells'
}

fn test_nested_generic_components_keep_caller_type_provenance() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_module = 'callee'
	t.sum_types.set('Any', ['string', 'bool'])
	t.sum_types.set('callee.Any', ['string', 'int'])
	t.active_specialization_args = flat.TypeTextList.from_names(['[][2]Any'])
	t.active_specialization_main_types = t.specialization_main_type_closure(t.active_specialization_args.ids)
	for typ in ['[][2]Any', '[][]Any', '[]map[string]Any'] {
		id := a.add_node(flat.Node{ kind: .array_init, value: typ[2..], typ: flat.source_type_id(typ) })
		assert flat.source_type_name(t.generic_call_type_arg_name(id)) == typ
		assert flat.source_type_name(t.canonical_generic_specialization_arg(flat.source_type_id(typ))) == typ
	}
	for typ in ['[2]Any', 'map[string]Any', 'map[Any]int', '(int, [2]Any)',
		'other.Box[map[string][2]Any]', 'fn (map[string]Any) [2]Any', 'thread [2]Any'] {
		assert t.current_specialization_has_generic_arg(flat.source_type_id(typ)), typ
		assert t.specialization_main_type_closure(flat.TypeTextList.from_names([typ]).ids) == {
			flat.source_type_id('Any'): true
		}, typ
	}
	for typ in ['[2]Any', 'map[string]Any', 'map[Any]int', '(int, [2]Any)'] {
		assert t.substituted_type_belongs_to_main_generic(flat.source_type_id(typ)), typ
		assert flat.source_type_name(t.generic_inference_alias_target(flat.source_type_id(typ), 'callee')) == typ
		assert flat.source_type_name(t.generic_arg_for_decl_module(flat.source_type_id(typ), 'callee')) == typ
	}
	assert flat.source_type_name(t.generic_inference_alias_target(flat.source_type_id('other.Box[map[string]Any]'), 'callee')) == 'other.Box[map[string]Any]'
	assert flat.source_type_name(t.generic_arg_for_decl_module(flat.source_type_id('other.Box[[2]Any]'), 'callee')) == 'other.Box[[2]Any]'
	tc.type_aliases.set_id('other.Items', flat.source_type_id('other.Box[T]'))
	tc.type_alias_generic_params['other.Items'] = ['T']
	assert flat.source_type_name(t.generic_inference_alias_target(flat.source_type_id('other.Items[Any]'), 'callee')) == 'other.Items[Any]'
	t.structs['callee.Box'] = StructInfo{ module: 'callee' }
	tc.struct_generic_params['callee.Box'] = ['T']
	assert flat.source_type_name(t.generic_inference_alias_target(flat.source_type_id('Box[[2]Any]'), 'callee')) == 'callee.Box[[2]Any]'
	assert flat.source_type_name(t.generic_arg_for_decl_module(flat.source_type_id('Box[map[string]Any]'), 'callee')) == 'callee.Box[map[string]Any]'
	assert flat.source_type_name(t.generic_arg_for_call_and_decl_module(flat.source_type_id('Box[[2]Any]'), 'callee', 'other')) == 'callee.Box[[2]Any]'
	for typ in ['[2]callee.Any', 'map[string]callee.Any', '(int, callee.Any)', 'other.Box[callee.Any]',
		'map[string]int', '[2]int'] {
		assert !t.current_specialization_has_generic_arg(flat.source_type_id(typ)), typ
		assert !t.substituted_type_belongs_to_main_generic(flat.source_type_id(typ)), typ
	}
	for typ in ['?[]string', '?map[string]string', 'fn (?[]string) string'] {
		t.active_specialization_args = flat.TypeTextList.from_names([typ])
		assert flat.source_type_name(t.generic_inference_alias_target(flat.source_type_id(typ), 'callee')) == typ
		assert flat.source_type_name(t.generic_arg_for_decl_module(flat.source_type_id(typ), 'callee')) == typ
		assert flat.source_type_name(t.generic_arg_for_call_and_decl_module(flat.source_type_id(typ), 'callee', 'other')) == typ
	}
}

fn test_lock_colliding_main_substitution_keeps_decl_module_generic_base() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.structs['Arc'] = StructInfo{
		name:   flat.source_type_id('Arc')
		module: 'arc'
	}
	t.structs['arc.Arc'] = StructInfo{
		name:   flat.source_type_id('Arc')
		module: 'arc'
	}
	assert flat.source_type_name(t.lock_colliding_main_substitution_type_text(flat.source_type_id('Arc[T]'), flat.source_type_id('Arc[Resource]'), 'arc', [
		'T',
	])) == 'Arc[Resource]'
	assert flat.source_type_name(t.lock_colliding_main_substitution_type_text(flat.source_type_id('([]T, []T)'), flat.source_type_id('([]int, []int)'), 'arrays', [
		'T',
	])) == '([]int, []int)'
	t.structs['Context'] = StructInfo{}
	t.structs['arc.Context'] = StructInfo{}
	t.active_specialization_main_types[flat.source_type_id('Context')] = true
	assert flat.source_type_name(t.lock_colliding_main_substitution_type_text(flat.source_type_id('other.Box[map[other.Key]T]'), flat.source_type_id('other.Box[map[other.Key]Context]'), 'arc', [
		'T',
	])) == 'other.Box[map[other.Key]main.Context]'
	// A locked argument makes the spelling qualified, so the decl-module base must be too.
	assert flat.source_type_name(t.lock_colliding_main_substitution_type_text(flat.source_type_id('&Arc[T]'), flat.source_type_id('&Arc[Context]'), 'arc', [
		'T',
	])) == '&arc.Arc[main.Context]'
}

fn test_collision_locked_signature_keeps_interface_and_alias_declaration_modules() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.structs['Item'] = StructInfo{}
	t.structs['callee.Item'] = StructInfo{}
	t.active_specialization_main_types[flat.source_type_id('Item')] = true
	tc.interface_names['callee.Reader'] = true
	tc.interface_generic_params['callee.Reader'] = ['T']
	tc.type_aliases.set_id('callee.Mapper', flat.source_type_id('fn (T) int'))
	tc.type_alias_generic_params['callee.Mapper'] = ['T']
	decl := GenericFnDecl{ module: 'callee' }
	for base in ['Reader', 'Mapper'] {
		for prefix in ['', '&', '[]', '?'] {
			assert flat.source_type_name(t.specialized_signature_type_text(decl, flat.source_type_id('${prefix}${base}[T]'), flat.TypeTextList.from_names(['Item']).ids, ['T'])) == '${prefix}callee.${base}[main.Item]'
		}
		assert flat.source_type_name(t.specialized_signature_type_text(decl, flat.source_type_id('other.${base}[T]'), flat.TypeTextList.from_names(['Item']).ids, ['T'])) == 'other.${base}[main.Item]'
		assert flat.source_type_name(t.specialized_signature_type_text(decl, flat.source_type_id('${base}[T]'), flat.TypeTextList.from_names(['int']).ids, ['T'])).contains('main.') == false
	}
}

fn test_generic_fn_type_param_mode_payload_preserves_mutability() {
	assert generic_fn_type_param_mode_payload('mut Item') == 'mut Item'
	assert generic_fn_type_param_mode_payload('mut item Item') == 'mut Item'
	assert generic_fn_type_param_mode_payload('mut &Dog') == 'mut &Dog'
	assert generic_fn_type_param_mode_payload('item Item') == 'Item'
	assert subst_generic_fn_type_param_text('mut it T', flat.TypeTextList.from_names(['&Dog']).ids, ['T']) == 'mut it &Dog'
	assert flat.source_type_name(substitute_generic_type_text_with_params(flat.source_type_id('fn (mut it T)'), flat.TypeTextList.from_names(['&Dog']).ids, ['T'])) == 'fn(mut it &Dog)'

	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	assert flat.source_type_name(t.qualify_specialized_signature_type_text(flat.source_type_id('fn(mut &Dog)'), GenericFnDecl{})) == 'fn (mut &Dog)'

	assert flat.source_type_name(t.lock_colliding_main_substitution_type_text(flat.source_type_id('fn(mut it T)'), flat.source_type_id('fn(mut it &Dog)'), 'arc', [
		'T',
	])) == 'fn (mut &Dog)'
	assert flat.source_type_name(t.lock_colliding_main_substitution_type_text(flat.source_type_id('fn(mut it T)'), flat.source_type_id('fn(mut it Dog)'), 'arc', [
		'T',
	])) == 'fn (mut Dog)'
}

fn test_resolve_substituted_type_text_qualifies_local_generic_base() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.struct_generic_params['Node'] = ['T']
	tc.struct_generic_params['json2.Node'] = ['T']
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_module = 'json2'
	t.structs['Node'] = StructInfo{
		name:   flat.source_type_id('Node')
		module: 'main'
	}
	t.structs['json2.Node'] = StructInfo{
		name:   flat.source_type_id('Node')
		module: 'json2'
	}

	assert flat.source_type_name(t.resolve_substituted_type_text(flat.source_type_id('Node[json2.ValueInfo]'))) == 'json2.Node[json2.ValueInfo]'
	assert flat.source_type_name(t.resolve_substituted_type_text(flat.source_type_id('&Node[json2.ValueInfo]'))) == '&json2.Node[json2.ValueInfo]'
	t.active_specialization_main_types[flat.source_type_id('Node')] = true
	assert flat.source_type_name(t.resolve_substituted_type_text(flat.source_type_id('Node[int]'))) == 'Node[int]'
	assert flat.source_type_name(t.resolve_substituted_type_text(flat.source_type_id('main.Node'))) == 'main.Node'
}

fn test_imported_generic_alias_target_uses_declaration_module() {
	mut a := flat.FlatAst.new()
	mut inner_decl := flat.Node{
		kind:  .struct_decl
		value: 'Inner'
	}
	inner_decl.set_generic_params(['T'])
	inner_id := a.add_node(inner_decl)
	mut tc := types.TypeChecker.new(&a)
	tc.type_aliases.set_id('a.Box', flat.source_type_id('Inner[T]'))
	tc.type_alias_generic_params['a.Box'] = ['T']
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_module = 'main'
	t.structs['Inner'] = StructInfo{
		name:   flat.source_type_id('Inner')
		module: 'main'
	}
	t.structs['a.Inner'] = StructInfo{
		name:   flat.source_type_id('Inner')
		module: 'a'
	}

	assert flat.source_type_name(t.normalize_type_alias(flat.source_type_id('a.Box[int]'))) == 'a.Inner[int]'
	decls := {
		'a.Inner': GenericStructDecl{
			id:     inner_id
			node:   inner_decl
			module: 'a'
			key:    'a.Inner'
		}
	}
	mut specs := map[u32]u32{}
	t.collect_generic_struct_spec_from_type(flat.source_type_id('a.Box[int]'), 'main', '', decls, mut specs)
	assert specs[flat.source_type_id('a.Inner[int]')] == flat.source_type_id('a.Inner')
}

fn test_generic_method_decl_matches_embedded_receiver() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.embedded_fields[flat.source_type_id('Outer')] = [
		FieldInfo{
			name:        'Middle'
			typ:         flat.source_type_id('Middle')
			raw_typ:     flat.source_type_id('Middle')
			is_embedded: true
		},
	]
	t.embedded_fields[flat.source_type_id('Middle')] = [
		FieldInfo{
			name:        'Collector'
			typ:         flat.source_type_id('Collector[int]')
			raw_typ:     flat.source_type_id('Collector[int]')
			is_embedded: true
		},
	]
	decl := GenericFnDecl{
		node:   flat.Node{
			kind:  .fn_decl
			value: 'Collector[T].use'
		}
		module: 'main'
		key:    'Collector[T].use'
	}
	mut seen := map[u32]bool{}
	assert t.generic_decl_matches_embedded_receiver(flat.source_type_id('Outer'), decl, 'main', mut seen)
}

fn test_escaped_generic_method_indexes_unescaped_call_spelling() {
	mut a := flat.FlatAst.new()
	receiver_id := a.add_node(flat.Node{
		kind: .param
		typ:  flat.source_type_id('&Box[T]')
	})
	children_start := a.children.len
	a.children << receiver_id
	mut fn_decl := flat.Node{
		kind:           .fn_decl
		value:          'Box[T].@union'
		children_start: children_start
		children_count: 1
	}
	fn_decl.set_generic_params(['T'])
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.generic_fn_decls_cache['Box.@union'] = GenericFnDecl{
		node: fn_decl
		key:  'Box.@union'
	}
	t.build_generic_receiver_method_index()

	assert t.generic_fn_call_names['union']
	assert t.generic_receiver_methods_by_name['union'] == ['Box.@union']
	assert t.generic_receiver_decl_key('Box', 'union', t.generic_fn_decls_cache) == 'Box.@union'
	assert t.generic_plain_call_candidates('type', 'main') == ['type', '@type']
}

fn test_synthetic_generic_call_with_exact_identity_is_scannable() {
	mut a := flat.FlatAst.new()
	callee_id := a.add_node(flat.Node{
		kind:  .ident
		value: 'datatypes.LinkedList[map[string]int].str'
	})
	children_start := a.children.len
	a.children << callee_id
	call := flat.Node{
		kind:           .call
		children_start: children_start
		children_count: 1
	}
	mut tc := types.TypeChecker.new(&a)
	t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.synthetic_generic_call_has_exact_identity(call)
	a.nodes[int(callee_id)].value = 'orm.v_sql_create_table_T_Demo'
	assert t.synthetic_generic_call_has_exact_identity(call)
}

fn test_specialized_plain_generic_call_args_decode_top_level_array_suffix() {
	mut a := flat.FlatAst.new()
	callee_id := a.add_node(flat.Node{
		kind:  .ident
		value: 'json2__decode_T_Array_net__jsonrpc__Response'
	})
	children_start := a.children.len
	a.children << callee_id
	call := flat.Node{
		kind:           .call
		children_start: children_start
		children_count: 1
	}
	mut fn_decl := flat.Node{
		kind:  .fn_decl
		value: 'decode'
	}
	fn_decl.set_generic_params(['T'])
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	args := flat.TypeTextList{
		ids: t.specialized_plain_generic_call_args(call, GenericFnDecl{
			node:   fn_decl
			module: 'json2'
			key:    'json2.decode'
		}, 'net.jsonrpc') or {
			assert false, 'failed to decode specialized generic call arguments'
			return
		}
	}.names()
	assert args == ['[]net.jsonrpc.Response']
}

fn test_free_generic_map_suffix_preserves_qualified_value_type() {
	suffix := generic_type_full_suffixes(flat.TypeTextList.from_names(['map[string]binary.St']).ids)
	assert suffix == 'Map_string_binary__St'
	decoded := generic_type_arg_from_suffix_with_containers(suffix)
	assert decoded == 'map[string]binary.St'
}

// A synthesized node that hangs under no declaration has no module or file
// context, so a plain generic argument in its type text cannot be resolved when
// the spelling is materialized: `Middleware[Context]` was collected as written
// and the specialization rebased `Context` onto `main`, which does not declare
// it. cgen then emitted `typedef bool (*...)(main__Context*)` and the C compiler
// stopped with `unknown type name 'main__Context'` (RuoQi's veb middleware). A
// spelling that carries its own module, or that is only a builtin, stays valid.
fn test_contextless_generic_struct_spec_spelling_is_skipped() {
	ctx_file := os.join_path(os.temp_dir(), 'v3_ctx', 'main.v')
	mut a := flat.FlatAst.new()
	a.add_node(flat.Node{
		kind:  .file
		value: ctx_file
	})
	attached_id := a.add_node(flat.Node{
		kind:  .struct_init
		value: 'veb.Middleware[Ctx]'
		typ:   flat.source_type_id('veb.Middleware[Ctx]')
	})
	children_start := a.children.len
	a.children << attached_id
	fn_decl := flat.Node{
		kind:           .fn_decl
		value:          'run'
		children_start: children_start
		children_count: 1
	}
	a.add_node(fn_decl)
	a.add_node(flat.Node{
		kind:  .struct_init
		value: 'veb.Middleware[Context]'
		typ:   flat.source_type_id('veb.Middleware[Context]')
	})
	a.add_node(flat.Node{
		kind:  .struct_init
		value: 'veb.Middleware[model.Context]'
		typ:   flat.source_type_id('veb.Middleware[model.Context]')
	})
	mut params_node := flat.Node{
		kind:  .struct_init
		value: 'veb.Middleware'
		typ:   flat.source_type_id('veb.Middleware')
	}
	params_node.set_generic_params(['Scope'])
	a.add_node(params_node)
	mut tc := types.TypeChecker.new(&a)
	tc.file_modules[ctx_file] = 'main'
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	decls := {
		'veb.Middleware': GenericStructDecl{
			node:   fn_decl
			module: 'veb'
			key:    'veb.Middleware'
		}
	}
	mut specs := map[u32]u32{}
	t.collect_generic_struct_specs_range(decls, mut specs, 0, a.nodes.len)

	assert flat.source_type_id('veb.Middleware[Ctx]') in specs
	assert flat.source_type_id('veb.Middleware[model.Context]') in specs
	assert !(flat.source_type_id('veb.Middleware[Context]') in specs)
	assert !(flat.source_type_id('veb.Middleware[Scope]') in specs)

	assert type_text_has_unqualified_generic_arg(flat.source_type_id('veb.Middleware[Context]'))
	assert type_text_has_unqualified_generic_arg(flat.source_type_id('Map[string, Context]'))
	assert type_text_has_unqualified_generic_arg(flat.source_type_id('Map[string, []Context]'))
	assert type_text_has_unqualified_generic_arg(flat.source_type_id('Box[Array[Context]]'))
	assert type_text_has_unqualified_generic_arg(flat.source_type_id('outer.Cell[Box[Pair[int]]]'))
	assert type_text_has_unqualified_generic_arg(flat.source_type_id('outer.Cell[inner.Box[Pair[int]]]'))
	assert !type_text_has_unqualified_generic_arg(flat.source_type_id('outer.Cell[inner.Box[inner.Pair[int]]]'))
	assert type_text_has_unqualified_generic_arg(flat.source_type_id('Box[...Context]'))
	assert type_text_has_unqualified_generic_arg(flat.source_type_id('Box[[2]Context]'))
	assert !type_text_has_unqualified_generic_arg(flat.source_type_id('veb.Middleware[model.Context]'))
	assert !type_text_has_unqualified_generic_arg(flat.source_type_id('Map[string, []int]'))
	assert !type_text_has_unqualified_generic_arg(flat.source_type_id('Box[map[string]int]'))
	assert !type_text_has_unqualified_generic_arg(flat.source_type_id('map[string]int'))
	assert !type_text_has_unqualified_generic_arg(flat.source_type_id('Middleware'))
}

// The scoped (memory-bounded) monomorphize path is selected for every non-empty
// batch on purpose: one drain batch can discover thousands of nested
// specializations, and the regular path keeps every worker's scratch arena alive
// until the batch is merged (vlang/v#28564, 19.4 GB -> 7.4 GB on the veb + orm
// reproduction). Pin the boundaries so that a future cutoff change stays visible.
fn test_scoped_monomorphize_batch_selection_boundaries() {
	$if !v3_no_parallel ? {
		assert !should_use_scoped_monomorphize(0, 0)
		// One specialization is enough: the bounded path is not gated on AST size.
		assert should_use_scoped_monomorphize(0, 1)
		assert should_use_scoped_monomorphize(scoped_monomorph_node_threshold - 1, 1)
		assert should_use_scoped_monomorphize(scoped_monomorph_node_threshold, 1)
		assert should_use_scoped_monomorphize(0, scoped_monomorph_specs_threshold)
	}
}

// The monomorph cache outlives the worker arena that produced an entry: a scoped
// batch records a spec, releases its arena and a later pass re-seeds from this
// table (vlang/v#28489). Recording has to own key, module, declaration key and
// every argument, and a re-recorded key must keep the last copy instead of being
// skipped.
fn test_record_monomorph_cache_spec_replaces_existing_entry() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	t.record_monomorph_cache_spec('main.f_T_int', 'main.f', 'main', flat.TypeTextList.from_names(['int']).ids)
	t.record_monomorph_cache_spec('main.f_T_int', 'main.f', 'main', flat.TypeTextList.from_names(['iam.Token']).ids)
	spec := t.monomorph_cache_specs['main.f_T_int'] or {
		assert false, 'the re-recorded spec disappeared'
		return
	}
	assert spec.args_text() == ['iam.Token']
	assert spec.decl_key == 'main.f'
	assert spec.module == 'main'
}

fn test_serial_monomorph_batches_share_source_reflected_loop_index() {
	$if !v3_no_parallel ? {
		path := os.join_path(os.vtmp_dir(), 'monomorph_reflected_loop_${os.getpid()}.v')
		os.write_file(path, 'struct Item {
	items []string
}
fn count[T](value T) int {
	mut result := 0
	\$for field in T.fields {
		\$if field.typ is []string {
			for key, text in value.\$(field.name) {
				result += key + text.len
			}
		}
	}
	return result
}
fn main() {
	assert count[Item](Item{items: ["a", "bb"]}) == 4
}
') or { panic(err) }
		defer { os.rm(path) or {} }
		mut p := parser.Parser.new(pref.new_preferences())
		mut a := p.parse_file(path)
		mut tc := types.TypeChecker.new(a)
		tc.collect(a)
		tc.check_semantics()
		assert tc.errors.len == 0, tc.errors.str()
		mut t := new_transformer(mut a, &tc, {
			'main': true
		})
		t.prepare()
		decl := t.cached_generic_fn_decls()['count'] or { panic('missing count declaration') }
		args := ['Item']
		mut emitted := map[string]bool{}
		mut generated := []string{}
		assert t.run_scoped_monomorphize_specs([PendingGenericFnSpec{
			decl: decl
			args: flat.TypeTextList.from_names(args)
			key:  generic_fn_spec_key(decl.key, flat.TypeTextList.from_names(args).ids)
		}], mut emitted, mut generated)
		assert t.comptime_reflected_for_ready
		assert t.comptime_reflected_for_roles['key'] == 1
		assert t.comptime_reflected_for_roles['text'] == 2
		worker := t.fork_scoped_batch_worker(a, &tc)
		assert worker.comptime_reflected_for_ready
		assert worker.comptime_reflected_for_roles == t.comptime_reflected_for_roles
	}
}

fn test_generic_unresolved_function_type_checks_parameters_and_return() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	t := new_transformer(mut a, &tc, map[string]bool{})
	for typ in ['fn () T', 'fn (T) int', 'fn (value []T) string', 'fn (fn () T) int', 'Cell[fn () T]',
		'fn () unknown'] {
		assert t.generic_arg_is_unresolved(flat.source_type_id(typ)), typ
	}
	for typ in ['fn ()', 'fn () int', 'fn (string) int', 'Cell[fn () int]'] {
		assert !t.generic_arg_is_unresolved(flat.source_type_id(typ)), typ
	}
}

fn test_nested_generic_field_arguments_preserve_declaration_modules() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.struct_generic_params['outer.Cell'] = ['T']
	tc.struct_generic_params['inner.Box'] = ['T']
	tc.struct_generic_params['inner.Pair'] = ['T']
	t := new_transformer(mut a, &tc, map[string]bool{})
	assert flat.source_type_name(t.normalize_field_type(flat.source_type_id('T'), flat.source_type_id('outer.Cell[inner.Box[inner.Pair[int]]]'))) ==
		'inner.Box[inner.Pair[int]]'
	assert flat.source_type_name(t.normalize_field_type(flat.source_type_id('outer.Cell[inner.Box[inner.Pair[string]]]'), flat.source_type_id('inner.Owner'))) ==
		'outer.Cell[inner.Box[inner.Pair[string]]]'
}

fn test_nested_generic_arguments_resolve_each_component_in_source_scope() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.struct_generic_params['inner.Box'] = ['T']
	tc.struct_generic_params['inner.Pair'] = ['T']
	tc.structs['inner.Box'] = []types.StructField{}
	tc.structs['inner.Pair'] = []types.StructField{}
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	assert flat.TypeTextList{ ids: t.generic_struct_args_in_scope(flat.TypeTextList.from_names(['Box[Pair[int]]']).ids, 'inner', 'inner.v') }.names() ==
		['inner.Box[inner.Pair[int]]']
}

fn test_parameterized_typeof_display_preserves_source_and_caller_names() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.active_generic_params = ['T']
	for source, expected in {
		'Box[T]':       'Box[other.Payload]'
		'local.Box[T]': 'local.Box[other.Payload]'
		'&Box[T]':      '&Box[other.Payload]'
		'T':            'other.Payload'
	} {
		child := a.add_node(flat.Node{ kind: .ident, value: source })
		start := a.children.len
		a.children << child
		node := flat.Node{ kind: .typeof_expr, children_start: start, children_count: 1 }
		assert t.generic_comptime_typeof_display_name(node, flat.TypeTextList.from_names(['other.Payload']).ids,
			'other.Payload') == expected
	}
}

fn test_parameterized_typeof_value_display_preserves_source_names() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.active_generic_params = ['T']
	node := flat.Node{ kind: .typeof_expr, value: 'Box[T]' }
	assert t.generic_comptime_typeof_display_name(node, flat.TypeTextList.from_names(['other.Payload']).ids,
		'local.Box[other.Payload]') == 'Box[other.Payload]'
	marker := flat.Node{ kind: .typeof_expr, value: generic_type_name_marker('T') }
	assert t.generic_comptime_typeof_display_name(marker, flat.TypeTextList.from_names(['other.Payload']).ids,
		'other.Payload') == 'other.Payload'
}

fn test_monomorphize_after_transform_keeps_lowered_fn_bodies() {
	path := os.join_path(os.vtmp_dir(), 'monomorphize_lowered_bodies_${os.getpid()}.v')
	os.write_file(path, '@[heap]
struct Frame {
mut:
	name string
}

struct Tag {
mut:
	others []Frame
}

fn (mut tag Tag) add_frame(frame Frame) {
	tag.others << frame
}

fn fill[T](mut tag T) {
	mut cur := &Frame{}
	tag.add_frame(cur)
}

fn main() {
	mut tag := Tag{}
	fill[Tag](mut tag)
}
') or {
		panic(err)
	}
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_file(path)
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	// Markused names a method by its V spelling only.
	used := transform_with_used(mut a, &tc, {
		'main':          true
		'fill':          true
		'Tag.add_frame': true
	})
	assert !used['Tag__add_frame']
	mut add_frame_idx := -1
	for i, node in a.nodes {
		if node.kind == .fn_decl && node.value == 'Tag.add_frame' {
			add_frame_idx = i
		}
	}
	assert add_frame_idx >= 0
	lowered := a.nodes[add_frame_idx]
	mono_used, errors, _ := monomorphize_with_used_checked_config_scoped_cached(mut a, &tc,
		used, false, unsafe { nil }, []MonomorphCacheSpec{}, true)
	assert errors.len == 0, errors.str()
	// The call in `fill[Tag]` inserts the C spelling: that is not a new function.
	assert mono_used['Tag__add_frame']
	assert a.nodes[add_frame_idx].children_start == lowered.children_start
	assert a.nodes[add_frame_idx].children_count == lowered.children_count
}

fn test_late_fn_bodies_pass_lowers_a_body_once_while_monomorphizing() {
	path := os.join_path(os.vtmp_dir(), 'monomorphize_late_bodies_${os.getpid()}.v')
	os.write_file(path, 'struct Tag {
mut:
	count int
}

fn (mut tag Tag) bump() {
	tag.count++
}

fn main() {}
') or {
		panic(err)
	}
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_file(path)
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	used := transform_with_used(mut a, &tc, {
		'main': true
	})
	mut bump_idx := -1
	for i, node in a.nodes {
		if node.kind == .fn_decl && node.value == 'Tag.bump' {
			bump_idx = i
		}
	}
	assert bump_idx >= 0
	parsed := a.nodes[bump_idx]
	mut t := new_transformer(mut a, &tc, used)
	t.prepare()
	t.mark_used_fn_bodies_lowered()
	limit := t.a.nodes.len
	// Nothing named the method before: its first spelling gets its body lowered.
	t.transform_late_used_fn_bodies(['Tag__bump'], 0, 1, limit)
	lowered := t.a.nodes[bump_idx]
	assert lowered.children_start != parsed.children_start
	// Its other spelling, inserted in a later round, is not another function.
	t.transform_late_used_fn_bodies(['Tag.bump'], 0, 1, limit)
	assert t.a.nodes[bump_idx].children_start == lowered.children_start
}

fn test_scoped_late_fn_bodies_pass_records_lowered_bodies() {
	path := os.join_path(os.vtmp_dir(), 'monomorphize_scoped_late_bodies_${os.getpid()}.v')
	mut source := 'struct Tag {\nmut:\n\tcount int\n}\n'
	mut names := []string{}
	for i in 0 .. direct_late_transform_max_names + 1 {
		source += 'fn (mut tag Tag) bump${i}() { tag.count++ }\n'
		names << 'Tag__bump${i}'
	}
	source += 'fn main() {}\n'
	os.write_file(path, source) or { panic(err) }
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_file(path)
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	used := transform_with_used(mut a, &tc, {
		'main': true
	})
	mut t := new_transformer(mut a, &tc, used)
	t.prepare()
	t.mark_used_fn_bodies_lowered()
	// Monomorphization starts without the main transformer's body bookkeeping.
	assert t.transformed_fns.len == 0
	t.scope_parallel_workers = true
	t.retain_worker_results = true
	limit := t.a.nodes.len
	// More than the direct-pass limit routes these bodies through scoped batches.
	t.transform_late_used_fn_bodies(names, 0, names.len, limit)
	mut lowered := map[int]int{}
	for i in 0 .. limit {
		node := t.a.nodes[i]
		if node.kind == .fn_decl && node.value.starts_with('Tag.bump') {
			assert t.lowered_fn_bodies[i], node.value
			lowered[i] = node.children_start
		}
	}
	assert lowered.len == names.len
	// A later round discovers another spelling of a body that the batch lowered.
	t.transform_late_used_fn_bodies(['Tag.bump0'], 0, 1, limit)
	for idx, children_start in lowered {
		assert t.a.nodes[idx].children_start == children_start
	}
}

fn test_recorded_generic_arg_ids_preserve_ownership_and_invalidate_misses() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	// prepare allocates the miss cache; without it misses are not recorded.
	t.spec_args_misses = &SpecArgsMissCache{}
	key := flat.source_type_id('Record_int')
	assert t.recorded_type_specialization_args(key) == none
	assert t.spec_args_misses.misses[key]
	t.generic_specialization_args.set_ids('Record_int', [flat.source_type_int])
	mut args := t.recorded_type_specialization_args(key) or { panic('missing local arguments') }
	assert args == [flat.source_type_int]
	args[0] = flat.source_type_string
	assert t.recorded_type_specialization_args(key) or { []u32{} } == [flat.source_type_int]

	mut first_parent := flat.TypeTextListBindings{}
	first_parent.set_ids('First_int', [flat.source_type_int])
	t.generic_specialization_args_parent = &first_parent
	parent_key := flat.source_type_id('Second_string')
	assert t.recorded_type_specialization_args(parent_key) == none
	assert t.recorded_generic_specialization_args('Second_string') == none
	assert t.spec_args_misses.name_misses['Second_string']
	assert t.spec_args_misses.misses[parent_key]
	mut second_parent := flat.TypeTextListBindings{}
	second_parent.set_ids('Second_string', [flat.source_type_string])
	assert first_parent.ids.len == second_parent.ids.len
	t.generic_specialization_args_parent = &second_parent
	assert t.recorded_type_specialization_args(parent_key) or { []u32{} } == [flat.source_type_string]
	assert t.recorded_generic_specialization_args('Second_string') or { []u32{} } == [flat.source_type_string]
}

fn test_numeric_specialized_type_decoder_preserves_cached_argument_storage() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.struct_generic_params['Box'] = ['T']
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	// prepare allocates the decode cache; without it misses are not recorded.
	t.generic_spec_decode_cache = &TypeLookupCache{
		entries: map[u32]u32{}
		misses:  map[u32]bool{}
	}
	name := flat.source_type_id('Box_int')
	mut args := t.decode_single_generic_specialized_type_args(name) or { panic('missing decoded arguments') }
	assert args == [flat.source_type_int]
	args[0] = flat.source_type_string
	assert t.decode_single_generic_specialized_type_args(name) or { []u32{} } == [flat.source_type_int]
	assert t.generic_specialized_source_type_name(flat.source_type_id(' & Box_int ')) or { u32(0) } == flat.source_type_id('&Box[int]')
	missing := flat.source_type_id('NotMaterialized')
	assert t.decode_single_generic_specialized_type_args(missing) == none
	assert t.generic_spec_decode_cache.misses[missing]
}

fn test_void_pointer_staging_keeps_pointer_typed_nil_initializer() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	pointer_type := flat.source_type_voidptr
	decl_id := t.make_staging_value_decl('pointer_result', pointer_type)
	decl := t.a.node(decl_id)
	lhs := t.a.child_node(decl, 0)
	rhs := t.a.child_node(decl, 1)
	assert lhs.typ == pointer_type
	assert rhs.kind == .nil_literal
}
