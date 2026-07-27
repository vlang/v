module transform

import v3.flat
import v3.types

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
	t.node_file_map_cache = []string{len: a.nodes.len}
	t.node_file_map_cache[int(index_id)] = file_name

	candidates := t.explicit_generic_fn_value_decl_candidates(index_id, base_id,
		a.nodes[int(base_id)], 'main')
	assert candidates[0] == 'lib.id'
	assert 'id' in candidates
}

fn test_materialized_generic_struct_fields_preserve_plain_alias_arguments() {
	mut a := flat.FlatAst.new()
	mut values_alias := flat.Node{
		kind:  .type_decl
		value: 'Values'
		typ:   '[]T'
	}
	values_alias.set_generic_params(['T'])
	a.add_node(values_alias)

	value_field := a.add_node(flat.Node{
		kind:  .field_decl
		value: 'value'
		typ:   'T'
	})
	values_field := a.add_node(flat.Node{
		kind:  .field_decl
		value: 'values'
		typ:   'Values[T]'
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
	tc.type_aliases['UserId'] = 'int'
	tc.type_aliases['Values'] = '[]T'
	tc.type_alias_generic_params['Values'] = ['T']
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_module = 'main'
	t.materialize_generic_struct_spec('Box[UserId]', GenericStructDecl{
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

fn test_flattened_generic_struct_types_materialize_from_recorded_args() {
	mut a := flat.FlatAst.new()
	mut arc_decl := flat.Node{
		kind:  .struct_decl
		value: 'Arc'
	}
	arc_decl.set_generic_params(['T'])
	arc_id := a.add_node(arc_decl)
	mut tc := types.TypeChecker.new(&a)
	tc.sum_types['ResourceSum'] = ['Resource', 'int']
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.record_generic_specialization_args_in_module('Arc', 'arc', ['ResourceSum'])
	decls := {
		'arc.Arc': GenericStructDecl{
			id:     arc_id
			node:   arc_decl
			module: 'arc'
			key:    'arc.Arc'
		}
	}
	mut specs := map[string]string{}
	t.collect_generic_struct_spec_from_type('arc.Arc_ResourceSum', 'main', '', decls, mut specs)
	assert specs['arc.Arc[ResourceSum]'] == 'arc.Arc'
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
	assert t.lock_colliding_main_generic_type_text('Context', 'callee') == 'main.Context'
	// A qualified generic base with a bare program argument (`other.Box[Context]`) must
	// still lock the nested `Context`, not return early on the qualified spelling.
	assert t.lock_colliding_main_generic_type_text('other.Box[Context]', 'callee') == 'other.Box[main.Context]'
	// The same nested program type behind a map / fixed array is locked too.
	assert t.lock_colliding_main_generic_type_text('map[string]Context', 'callee') == 'map[string]main.Context'
	assert t.lock_colliding_main_generic_type_text('[3]Context', 'callee') == '[3]main.Context'
	assert t.lock_colliding_main_generic_type_text('(Context, []Context)', 'callee') == '(main.Context, []main.Context)'
	// A simple qualified type has no lockable bare component and is returned verbatim.
	assert t.lock_colliding_main_generic_type_text('veb.Context', 'callee') == 'veb.Context'
	// An already-qualified argument keeps its exact spelling (no rewrite / name desync).
	assert t.lock_colliding_main_generic_type_text('other.Box[user.LocalWriter]', 'callee') == 'other.Box[user.LocalWriter]'
	// No lock target: the callee module does not declare `Other`, so nothing changes.
	assert t.lock_colliding_main_generic_type_text('other.Box[Other]', 'callee') == 'other.Box[Other]'
	// A module-local generic base must stay local even when one of its concrete arguments
	// is a colliding main type. `callee.MiddlewareOptions` is not `main.MiddlewareOptions`.
	assert t.lock_colliding_main_generic_type_text('MiddlewareOptions[Context]', 'callee') == 'MiddlewareOptions[main.Context]'
	// A main type that is active in the specialization is locked even without a
	// callee homonym, since a different imported module can own the same short name.
	t.structs['Event'] = StructInfo{}
	t.structs['other.Event'] = StructInfo{}
	t.structs['string'] = StructInfo{
		module: 'builtin'
	}
	t.active_specialization_main_types['Event'] = true
	assert t.lock_colliding_main_generic_type_text('Event', 'callee') == 'main.Event'
	assert t.canonical_generic_specialization_arg('[]main.Event') == '[]Event'
	assert t.specialization_main_type_closure(['map[string]Event']) == {
		'Event': true
	}
}

fn test_lock_colliding_main_substitution_keeps_decl_module_generic_base() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.structs['Arc'] = StructInfo{
		name:   'Arc'
		module: 'arc'
	}
	t.structs['arc.Arc'] = StructInfo{
		name:   'Arc'
		module: 'arc'
	}
	assert t.lock_colliding_main_substitution_type_text('Arc[T]', 'Arc[Resource]', 'arc', [
		'T',
	]) == 'Arc[Resource]'
	assert t.lock_colliding_main_substitution_type_text('([]T, []T)', '([]int, []int)', 'arrays', [
		'T',
	]) == '([]int, []int)'
}

fn test_resolve_substituted_type_text_qualifies_local_generic_base() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.struct_generic_params['Node'] = ['T']
	tc.struct_generic_params['json2.Node'] = ['T']
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_module = 'json2'
	t.structs['Node'] = StructInfo{
		name:   'Node'
		module: 'main'
	}
	t.structs['json2.Node'] = StructInfo{
		name:   'Node'
		module: 'json2'
	}

	assert t.resolve_substituted_type_text('Node[json2.ValueInfo]') == 'json2.Node[json2.ValueInfo]'
	assert t.resolve_substituted_type_text('&Node[json2.ValueInfo]') == '&json2.Node[json2.ValueInfo]'
	t.active_specialization_main_types['Node'] = true
	assert t.resolve_substituted_type_text('Node[int]') == 'Node[int]'
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
	tc.type_aliases['a.Box'] = 'Inner[T]'
	tc.type_alias_generic_params['a.Box'] = ['T']
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_module = 'main'
	t.structs['Inner'] = StructInfo{
		name:   'Inner'
		module: 'main'
	}
	t.structs['a.Inner'] = StructInfo{
		name:   'Inner'
		module: 'a'
	}

	assert t.normalize_type_alias('a.Box[int]') == 'a.Inner[int]'
	decls := {
		'a.Inner': GenericStructDecl{
			id:     inner_id
			node:   inner_decl
			module: 'a'
			key:    'a.Inner'
		}
	}
	mut specs := map[string]string{}
	t.collect_generic_struct_spec_from_type('a.Box[int]', 'main', '', decls, mut specs)
	assert specs['a.Inner[int]'] == 'a.Inner'
}

fn test_generic_method_decl_matches_embedded_receiver() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.embedded_fields['Outer'] = [
		FieldInfo{
			name:        'Middle'
			typ:         'Middle'
			raw_typ:     'Middle'
			is_embedded: true
		},
	]
	t.embedded_fields['Middle'] = [
		FieldInfo{
			name:        'Collector'
			typ:         'Collector[int]'
			raw_typ:     'Collector[int]'
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
	mut seen := map[string]bool{}
	assert t.generic_decl_matches_embedded_receiver('Outer', decl, 'main', mut seen)
}
