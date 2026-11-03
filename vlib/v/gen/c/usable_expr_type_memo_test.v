module c

import v.flat
import v.types

fn test_usable_expr_type_uses_materialized_anonymous_storage() {
	for prefix in ['', '[]', '[][]'] {
		mut a := flat.FlatAst.new()
		id := a.add_node(flat.Node{
			kind:  .ident
			value: 'rows'
			typ:   flat.source_type_id('${prefix}Row')
		})
		mut tc := types.TypeChecker.new(&a)
		tc.structs[flat.source_type_id('Row')] = [types.StructField{ name: 'item', typ: types.Type(types.int_) }]
		tc.register_synth_type(id, tc.parse_type(flat.source_type_id('${prefix}struct')))
		mut g := FlatGen.new()
		g.a = &a
		g.tc = &tc
		assert g.usable_expr_type(id).name() == '${prefix}Row'
		// A concrete checked type keeps its authority over the textual annotation.
		tc.register_synth_type(id, tc.parse_type(flat.source_type_id('${prefix}string')))
		assert g.usable_expr_type(id).name() == '${prefix}string'
	}
}

fn test_usable_expr_type_memo_node_zero_starts_empty_and_reuses_its_result() {
	mut a := flat.FlatAst.new()
	id := a.add_val(.ident, 'value')
	assert id == 0
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.memo_usable_expr_types = true
	g.cur_param_types['value'] = types.Type(types.int_)
	g.begin_usable_expr_type_memo()
	assert *g.usable_expr_type(id) == types.Type(types.int_)
	// Change the uncached source to distinguish reuse from another resolution.
	g.cur_param_types['value'] = types.Type(types.bool_)
	assert *g.usable_expr_type(id) == types.Type(types.int_)
}

fn test_usable_expr_type_keeps_checked_container_alive_across_dense_slot_updates() {
	mut a := flat.FlatAst.new()
	value := a.add_val(.ident, 'rows')
	a.children << value
	parent := a.add_node(flat.Node{
		kind:           .paren
		children_start: 0
		children_count: 1
	})
	mut tc := types.TypeChecker.new(&a)
	tc.structs[flat.source_type_id('Payload')] = []types.StructField{}
	container := tc.parse_type(flat.source_type_id('[]Payload'))
	tc.register_synth_type(value, container)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.memo_usable_expr_types = true
	g.begin_usable_expr_type_memo()
	before := g.usable_expr_type(parent)
	assert *before == *container
	tc.register_synth_type(value, types.Type(types.String{}))
	// Growing the dense checker table must not invalidate a retained result.
	for i in 0 .. 1024 {
		id := a.add_val(.int_literal, i.str())
		tc.register_synth_type(id, types.Type(types.int_))
	}
	assert *before == *container
	assert *g.usable_expr_type(parent) == *container
	g.end_usable_expr_type_memo()
	g.begin_usable_expr_type_memo()
	assert *g.usable_expr_type(parent) == types.Type(types.String{})
}

fn test_usable_expr_type_memo_requires_the_exact_node_id_after_slot_collisions() {
	mut a := flat.FlatAst.new()
	a.nodes = []flat.Node{len: usable_expr_type_memo_slots + 1}
	a.nodes[0] = flat.Node{ kind: .ident, value: 'first' }
	a.nodes[usable_expr_type_memo_slots] = flat.Node{ kind: .ident, value: 'second' }
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.memo_usable_expr_types = true
	g.cur_param_types['first'] = types.Type(types.int_)
	g.cur_param_types['second'] = types.Type(types.String{})
	g.begin_usable_expr_type_memo()
	assert *g.usable_expr_type(flat.NodeId(0)) == types.Type(types.int_)
	assert *g.usable_expr_type(flat.NodeId(usable_expr_type_memo_slots)) == types.Type(types.String{})
	assert *g.usable_expr_type(flat.NodeId(0)) == types.Type(types.int_)
	assert *g.usable_expr_type(flat.NodeId(usable_expr_type_memo_slots)) == types.Type(types.String{})
}

fn test_usable_expr_type_memo_bypasses_completed_bodies_and_resets_for_the_next_body() {
	mut a := flat.FlatAst.new()
	id := a.add_val(.ident, 'value')
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.memo_usable_expr_types = true
	g.cur_param_types['value'] = types.Type(types.int_)
	g.begin_usable_expr_type_memo()
	assert *g.usable_expr_type(id) == types.Type(types.int_)
	g.end_usable_expr_type_memo()
	g.cur_param_types['value'] = types.Type(types.String{})
	assert *g.usable_expr_type(id) == types.Type(types.String{})
	g.begin_usable_expr_type_memo()
	assert *g.usable_expr_type(id) == types.Type(types.String{})
	g.cur_param_types['value'] = types.Type(types.bool_)
	assert *g.usable_expr_type(id) == types.Type(types.String{})
}

fn test_usable_expr_type_memo_rollover_does_not_revive_generation_one_entries() {
	mut a := flat.FlatAst.new()
	first := a.add_val(.ident, 'first')
	second := a.add_val(.ident, 'second')
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.memo_usable_expr_types = true
	g.cur_param_types['first'] = types.Type(types.int_)
	g.cur_param_types['second'] = types.Type(types.String{})
	g.begin_usable_expr_type_memo()
	assert *g.usable_expr_type(first) == types.Type(types.int_)
	assert *g.usable_expr_type(second) == types.Type(types.String{})
	g.end_usable_expr_type_memo()
	g.usable_expr_type_memo.generation = ~u32(0)
	g.cur_param_types['first'] = types.Type(types.bool_)
	g.cur_param_types['second'] = types.Type(types.int_)
	g.begin_usable_expr_type_memo()
	assert g.usable_expr_type_memo.generation == 1
	assert *g.usable_expr_type(first) == types.Type(types.bool_)
	assert *g.usable_expr_type(second) == types.Type(types.int_)
}

fn test_body_import_type_queries_keep_source_files_and_declaration_generations() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.cur_file = 'caller.v'
	tc.cur_module = 'main'
	tc.file_imports['first.v\nmodel'] = 'first.model'
	tc.file_imports['second.v\nmodel'] = 'second.model'
	tc.structs[flat.source_type_id('first.model.Item')] = []types.StructField{}
	tc.structs[flat.source_type_id('second.model.Item')] = []types.StructField{}
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.memo_usable_expr_types = true
	g.begin_usable_expr_type_memo()
	for _ in 0 .. 3 {
		for file in ['first.v', 'second.v'] {
			for typ in ['model.Item', '&model.Item', '[]model.Item', '?model.Item'] {
				cached := g.canonical_import_alias_type_in_file(flat.source_type_id(typ), file)
				assert cached == g.canonical_import_alias_type_in_file_uncached(flat.source_type_id(typ), file)
				assert cached.name().contains(file.all_before('.') + '.model.Item')
			}
		}
	}
	assert g.canonical_import_alias_type_in_file(flat.source_type_id('model.Item'), 'first.v') is types.Struct
	g.end_usable_expr_type_memo()
	// Another body can observe declarations registered after the previous one.
	tc.structs.delete(flat.source_type_id('first.model.Item'))
	tc.enum_names[flat.source_type_id('first.model.Item')] = true
	g.begin_usable_expr_type_memo()
	assert g.canonical_import_alias_type_in_file(flat.source_type_id('model.Item'), 'first.v') is types.Enum
	g.end_usable_expr_type_memo()
	g.usable_expr_type_memo.generation = ~u32(0)
	tc.enum_names.delete(flat.source_type_id('first.model.Item'))
	tc.structs[flat.source_type_id('first.model.Item')] = []types.StructField{}
	g.begin_usable_expr_type_memo()
	assert g.canonical_import_alias_type_in_file(flat.source_type_id('model.Item'), 'first.v') is types.Struct
}

fn test_body_import_type_queries_bypass_temporary_contexts() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.cur_file = 'caller.v'
	tc.cur_module = 'main'
	tc.structs[flat.source_type_id('Item')] = []types.StructField{}
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.memo_usable_expr_types = true
	type_id := flat.source_type_id('Item')
	g.begin_usable_expr_type_memo()
	assert g.canonical_import_alias_type_in_file(type_id, 'caller.v') is types.Struct
	tc.structs.delete(flat.source_type_id('Item'))
	tc.enum_names[flat.source_type_id('Item')] = true
	// Nested emission must resolve its own declarations without poisoning the body.
	g.const_init_depth = 1
	assert g.canonical_import_alias_type_in_file(type_id, 'caller.v') is types.Enum
	g.const_init_depth = 0
	g.struct_default_module = 'defaults'
	assert g.canonical_import_alias_type_in_file(type_id, 'caller.v') is types.Enum
	g.struct_default_module = ''
	tc.cur_module = 'other'
	assert g.canonical_import_alias_type_in_file(type_id, 'caller.v') is types.Enum
	tc.cur_module = 'main'
	tc.cur_file = 'other.v'
	assert g.canonical_import_alias_type_in_file(type_id, 'caller.v') is types.Enum
	tc.cur_file = 'caller.v'
	assert g.canonical_import_alias_type_in_file(type_id, 'caller.v') is types.Struct
	g.end_usable_expr_type_memo()
	assert g.canonical_import_alias_type_in_file(type_id, 'caller.v') is types.Enum
}

fn test_body_import_type_queries_preserve_colliding_spelling_ids() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.cur_file = 'caller.v'
	tc.cur_module = 'main'
	tc.structs[flat.source_type_id('CollisionStruct')] = []types.StructField{}
	first := flat.source_type_id('CollisionStruct')
	mask := u32(body_import_type_query_slots - 1)
	mut second := u32(0)
	for i in 0 .. body_import_type_query_slots * 2 {
		name := 'CollisionEnum${i}'
		id := flat.source_type_id(name)
		if id != first && (id & mask) == (first & mask) {
			second = id
			tc.enum_names[flat.source_type_id(name)] = true
			break
		}
	}
	assert second != 0
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.memo_usable_expr_types = true
	g.begin_usable_expr_type_memo()
	for _ in 0 .. 2 {
		assert g.canonical_import_alias_type_in_file(first, 'caller.v') is types.Struct
		assert g.canonical_import_alias_type_in_file(second, 'caller.v') is types.Enum
	}
}

fn test_body_resolve_reference_retains_memo_identity_and_resets_context() {
	mut a := flat.FlatAst.new()
	id := a.add_val(.ident, 'rows')
	mut tc := types.TypeChecker.new(&a)
	first := tc.parse_type(flat.source_type_id('[]int'))
	second := tc.parse_type(flat.source_type_id('[]string'))
	tc.cur_scope.insert('rows', *first)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.memo_usable_expr_types = true
	g.begin_usable_expr_type_memo()
	// Both value and reference entry points populate the same memo.
	assert g.body_resolve_type(id) == *first
	before := g.body_resolve_type_reference(id)
	assert voidptr(before) == voidptr(g.usable_expr_type_memo.resolve_values[int(id)])
	tc.cur_scope.insert('rows', *second)
	assert voidptr(g.body_resolve_type_reference(id)) == voidptr(before)
	g.const_init_depth = 1
	assert *g.body_resolve_type_reference(id) == *second
	g.const_init_depth = 0
	assert voidptr(g.body_resolve_type_reference(id)) == voidptr(before)
	g.end_usable_expr_type_memo()
	g.begin_usable_expr_type_memo()
	assert *g.body_resolve_type_reference(id) == *second
	assert *before == *first
	assert g.body_resolve_type(id) == *second
}

fn test_body_resolve_reference_does_not_freeze_unknown_results() {
	mut a := flat.FlatAst.new()
	id := a.add_val(.ident, 'later')
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.memo_usable_expr_types = true
	g.begin_usable_expr_type_memo()
	assert g.body_resolve_type_reference(id) is types.Unknown
	assert g.usable_expr_type_memo.resolve_gens[int(id)] == 0
	tc.cur_scope.insert('later', types.Type(types.int_))
	assert *g.body_resolve_type_reference(id) == types.Type(types.int_)
	assert g.usable_expr_type_memo.resolve_gens[int(id)] == g.usable_expr_type_memo.generation
}

fn test_body_resolve_reference_checks_the_full_id_on_collisions() {
	mut a := flat.FlatAst.new()
	a.nodes = []flat.Node{len: usable_expr_type_memo_slots + 1}
	a.nodes[0] = flat.Node{ kind: .int_literal, value: '1' }
	a.nodes[usable_expr_type_memo_slots] = flat.Node{ kind: .string_literal, value: 'second' }
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.memo_usable_expr_types = true
	g.begin_usable_expr_type_memo()
	for _ in 0 .. 2 {
		assert *g.body_resolve_type_reference(flat.NodeId(0)) == types.Type(types.int_)
		assert g.body_resolve_type_reference(flat.NodeId(usable_expr_type_memo_slots)) is types.String
	}
}

fn test_usable_expr_type_keeps_owned_container_child_references() {
	for canonical in [false, true] {
		for container_kind in 0 .. 4 {
			mut a := flat.FlatAst.new()
			value := a.add_val(.ident, 'value')
			a.children << value
			access := a.add_node(flat.Node{
				kind:           if container_kind == 0 { .prefix } else { .index }
				op:             .mul
				children_start: 0
				children_count: 1
			})
			mut tc := types.TypeChecker.new(&a)
			// A raw heap-owned child need not already have interner identity.
			child := types.new_type_reference(types.Type(types.String{}))
			container := match container_kind {
				0 { types.Type(types.Pointer{ base_type: child }) }
				1 { types.Type(types.Array{ elem_type: child }) }
				2 { types.Type(types.ArrayFixed{ elem_type: child, len: 3 }) }
				else {
					types.Type(types.Map{ key_type: types.new_type_reference(types.Type(types.int_)), value_type: child })
				}
			}
			owned := if canonical {
				tc.intern_type_reference(container)
			} else {
				types.new_type_reference(container)
			}
			expected := match owned {
				types.Pointer { owned.base_type }
				types.Array { owned.elem_type }
				types.ArrayFixed { owned.elem_type }
				types.Map { owned.value_type }
				else { panic('expected container') }
			}
			mut g := FlatGen.new()
			g.a = &a
			g.tc = &tc
			g.cur_param_types['value'] = owned
			g.memo_usable_expr_types = true
			g.begin_usable_expr_type_memo()
			before := g.usable_expr_type(access)
			assert voidptr(before) == voidptr(expected)
			assert *before == types.Type(types.String{})
			g.cur_param_types['value'] = tc.intern_type_reference(types.Type(types.Array{ elem_type: types.new_type_reference(types.Type(types.bool_)) }))
			for i in 0 .. 1024 {
				tc.intern_type_reference(types.Type(types.ArrayFixed{ elem_type: child, len: i + 1 }))
			}
			assert voidptr(g.usable_expr_type(access)) == voidptr(expected)
			assert *before == types.Type(types.String{})
			g.end_usable_expr_type_memo()
			g.begin_usable_expr_type_memo()
			if container_kind != 0 {
				assert *g.usable_expr_type(access) == types.Type(types.bool_)
			}
			assert *before == types.Type(types.String{})
		}
	}
}

fn test_usable_expr_type_shared_parameter_keeps_owned_value_reference() {
	mut a := flat.FlatAst.new()
	id := a.add_val(.ident, 'value')
	mut tc := types.TypeChecker.new(&a)
	child := types.new_type_reference(types.Type(types.String{}))
	parameter := types.new_type_reference(types.Type(types.Pointer{ base_type: child }))
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.cur_param_types['value'] = parameter
	g.push_scope()
	owner := tc.cur_scope.insert_with_owner('value', *parameter)
	g.declare_local_shared_storage(owner, true)
	g.memo_usable_expr_types = true
	g.begin_usable_expr_type_memo()
	before := g.usable_expr_type(id)
	assert voidptr(before) == voidptr(child)
	g.end_usable_expr_type_memo()
	g.declare_local_shared_storage(owner, false)
	g.begin_usable_expr_type_memo()
	assert voidptr(g.usable_expr_type(id)) == voidptr(parameter)
	assert *before == types.Type(types.String{})
	g.pop_scope()
}
