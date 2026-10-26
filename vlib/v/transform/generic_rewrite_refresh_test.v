module transform

import v.flat
import v.types

fn test_generic_refresh_revisits_existing_result_parents_after_late_rewrite() {
	for prefix in ['!', '?'] {
		mut a := flat.FlatAst.new()
		lhs := a.add_node(flat.Node{
			kind:  .ident
			value: 'value'
			typ:   flat.source_type_id('int')
		})
		callee := a.add_val(.ident, 'specialized')
		call_start := a.children.len
		a.children << callee
		call := a.add_node(flat.Node{
			kind:           .call
			typ:            flat.source_type_id('${prefix}int')
			children_start: call_start
			children_count: 1
		})
		or_start := a.children.len
		a.children << call
		or_expr := a.add_node(flat.Node{
			kind:           .or_expr
			typ:            flat.source_type_id('int')
			children_start: or_start
			children_count: 1
		})
		decl_start := a.children.len
		a.children << lhs
		a.children << or_expr
		decl := a.add_node(flat.Node{
			kind:           .decl_assign
			typ:            flat.source_type_id('int')
			children_start: decl_start
			children_count: 2
		})
		mut tc := types.TypeChecker.new(&a)
		tc.specialized_generic_fns['specialized'] = true
		mut t := new_transformer(mut a, &tc, map[string]bool{})
		assert !t.refresh_decl_assign_types_after_generic_rewrite()
		node_count := a.nodes.len
		// A later specialization can rewrite a call without appending its parents.
		t.set_node_typ(int(call), flat.source_type_id('${prefix}string'))
		assert t.refresh_decl_assign_types_after_generic_rewrite()
		assert a.nodes.len == node_count
		assert a.nodes[int(or_expr)].typ == flat.source_type_id('string')
		assert a.nodes[int(decl)].typ == flat.source_type_id('string')
		assert a.nodes[int(lhs)].typ == flat.source_type_id('string')
		assert t.var_type('value') == flat.source_type_id('string')
	}
}
