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
		tc.structs['Row'] = [types.StructField{ name: 'item', typ: types.Type(types.int_) }]
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
	mut g := FlatGen.new()
	g.a = &a
	g.memo_usable_expr_types = true
	g.cur_param_types['value'] = types.Type(types.int_)
	g.begin_usable_expr_type_memo()
	assert g.usable_expr_type(id) == types.Type(types.int_)
	// Change the uncached source to distinguish reuse from another resolution.
	g.cur_param_types['value'] = types.Type(types.bool_)
	assert g.usable_expr_type(id) == types.Type(types.int_)
}

fn test_usable_expr_type_memo_requires_the_exact_node_id_after_slot_collisions() {
	mut a := flat.FlatAst.new()
	a.nodes = []flat.Node{len: usable_expr_type_memo_slots + 1}
	a.nodes[0] = flat.Node{ kind: .ident, value: 'first' }
	a.nodes[usable_expr_type_memo_slots] = flat.Node{ kind: .ident, value: 'second' }
	mut g := FlatGen.new()
	g.a = &a
	g.memo_usable_expr_types = true
	g.cur_param_types['first'] = types.Type(types.int_)
	g.cur_param_types['second'] = types.Type(types.String{})
	g.begin_usable_expr_type_memo()
	assert g.usable_expr_type(flat.NodeId(0)) == types.Type(types.int_)
	assert g.usable_expr_type(flat.NodeId(usable_expr_type_memo_slots)) == types.Type(types.String{})
	assert g.usable_expr_type(flat.NodeId(0)) == types.Type(types.int_)
	assert g.usable_expr_type(flat.NodeId(usable_expr_type_memo_slots)) == types.Type(types.String{})
}

fn test_usable_expr_type_memo_bypasses_completed_bodies_and_resets_for_the_next_body() {
	mut a := flat.FlatAst.new()
	id := a.add_val(.ident, 'value')
	mut g := FlatGen.new()
	g.a = &a
	g.memo_usable_expr_types = true
	g.cur_param_types['value'] = types.Type(types.int_)
	g.begin_usable_expr_type_memo()
	assert g.usable_expr_type(id) == types.Type(types.int_)
	g.end_usable_expr_type_memo()
	g.cur_param_types['value'] = types.Type(types.String{})
	assert g.usable_expr_type(id) == types.Type(types.String{})
	g.begin_usable_expr_type_memo()
	assert g.usable_expr_type(id) == types.Type(types.String{})
	g.cur_param_types['value'] = types.Type(types.bool_)
	assert g.usable_expr_type(id) == types.Type(types.String{})
}

fn test_usable_expr_type_memo_rollover_does_not_revive_generation_one_entries() {
	mut a := flat.FlatAst.new()
	first := a.add_val(.ident, 'first')
	second := a.add_val(.ident, 'second')
	mut g := FlatGen.new()
	g.a = &a
	g.memo_usable_expr_types = true
	g.cur_param_types['first'] = types.Type(types.int_)
	g.cur_param_types['second'] = types.Type(types.String{})
	g.begin_usable_expr_type_memo()
	assert g.usable_expr_type(first) == types.Type(types.int_)
	assert g.usable_expr_type(second) == types.Type(types.String{})
	g.end_usable_expr_type_memo()
	g.usable_expr_type_memo.generation = ~u32(0)
	g.cur_param_types['first'] = types.Type(types.bool_)
	g.cur_param_types['second'] = types.Type(types.int_)
	g.begin_usable_expr_type_memo()
	assert g.usable_expr_type_memo.generation == 1
	assert g.usable_expr_type(first) == types.Type(types.bool_)
	assert g.usable_expr_type(second) == types.Type(types.int_)
}

fn test_body_import_type_queries_keep_source_files_and_declaration_generations() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.cur_file = 'caller.v'
	tc.cur_module = 'main'
	tc.file_imports['first.v\nmodel'] = 'first.model'
	tc.file_imports['second.v\nmodel'] = 'second.model'
	tc.structs['first.model.Item'] = []types.StructField{}
	tc.structs['second.model.Item'] = []types.StructField{}
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
	tc.structs.delete('first.model.Item')
	tc.enum_names['first.model.Item'] = true
	g.begin_usable_expr_type_memo()
	assert g.canonical_import_alias_type_in_file(flat.source_type_id('model.Item'), 'first.v') is types.Enum
	g.end_usable_expr_type_memo()
	g.usable_expr_type_memo.generation = ~u32(0)
	tc.enum_names.delete('first.model.Item')
	tc.structs['first.model.Item'] = []types.StructField{}
	g.begin_usable_expr_type_memo()
	assert g.canonical_import_alias_type_in_file(flat.source_type_id('model.Item'), 'first.v') is types.Struct
}

fn test_body_import_type_queries_bypass_temporary_contexts() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.cur_file = 'caller.v'
	tc.cur_module = 'main'
	tc.structs['Item'] = []types.StructField{}
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.memo_usable_expr_types = true
	type_id := flat.source_type_id('Item')
	g.begin_usable_expr_type_memo()
	assert g.canonical_import_alias_type_in_file(type_id, 'caller.v') is types.Struct
	tc.structs.delete('Item')
	tc.enum_names['Item'] = true
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
	tc.structs['CollisionStruct'] = []types.StructField{}
	first := flat.source_type_id('CollisionStruct')
	mask := u32(body_import_type_query_slots - 1)
	mut second := u32(0)
	for i in 0 .. body_import_type_query_slots * 2 {
		name := 'CollisionEnum${i}'
		id := flat.source_type_id(name)
		if id != first && (id & mask) == (first & mask) {
			second = id
			tc.enum_names[name] = true
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
