module transform

import v.flat
import v.types

fn test_match_type_patterns_use_original_spelling_ids() {
	mut a := flat.FlatAst.new()
	spelling := flat.source_type_id('First')
	pattern := a.add_node(flat.Node{
		kind:         .ident
		source_value: spelling
		typ:          flat.source_type_bool
	})
	legacy := a.add_node(flat.Node{ kind: .ident, value: 'Second' })
	mut tc := types.TypeChecker.new(&a)
	t := new_transformer(mut a, &tc, map[string]bool{})
	assert t.a.node(pattern).value == ''
	assert t.type_pattern_name(pattern) == spelling
	assert t.type_pattern_name(legacy) == flat.source_type_id('Second')
}

fn test_generic_source_value_substitution_preserves_operand_role() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.active_generic_params = ['T']
	t.cur_module = 'library'
	t.tc.cur_module = 'library'
	parameter := flat.source_type_id('T')
	cast := flat.Node{
		kind:         .cast_expr
		source_value: parameter
		typ:          flat.source_type_id('?T')
	}
	assert t.subst_node_source_value(cast, [flat.source_type_int]) == flat.source_type_int
	local := flat.source_type_id('local')
	assert t.subst_node_source_value(flat.Node{
		kind:         .sizeof_expr
		source_value: local
	}, [flat.source_type_int]) == local
	t.cloning_comptime_for_vars = ['variant']
	variant := flat.source_type_id('variant')
	assert t.subst_node_source_value(flat.Node{
		kind:         .is_expr
		source_value: variant
	}, [flat.source_type_int]) == variant
}

fn test_transform_source_value_writes_preserve_refined_type() {
	mut a := flat.FlatAst.new()
	id := a.add_node(flat.Node{
		kind:  .cast_expr
		value: 'T'
		typ:   flat.source_type_id('?T')
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.set_node_source_value(int(id), flat.source_type_int)
	assert t.a.node(id).source_value_id() == flat.source_type_int
	assert t.a.node(id).value == ''
	assert t.a.node(id).typ == flat.source_type_id('?T')
	t.base_write_intercept = true
	t.shared_base_nodes = t.a.nodes.len
	t.item_range_lo = t.a.nodes.len
	t.item_range_hi = t.a.nodes.len
	t.defer_oor_writes = true
	t.set_node_source_value(int(id), flat.source_type_string)
	assert t.a.node(id).source_value_id() == flat.source_type_int
	t.flush_deferred_base_writes()
	assert t.a.node(id).source_value_id() == flat.source_type_string
	assert t.a.node(id).typ == flat.source_type_id('?T')
}
