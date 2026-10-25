module transform

import v.flat
import v.types

fn test_numeric_multi_assign_targets_preserve_pointer_storage_rules() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	cases := [
		['mut_value', '&int', 'int', 'int'],
		['mut_pointer', '&&int', 'int', '&&int'],
		['mut_pointer_field', '&int', '&int', '&int'],
		['ordinary', '&int', 'int', '&int'],
		['heap_value', '&int', 'int', 'int'],
		['mut_padded', ' &int ', 'int', ' &int '],
		['mut_spaced_base', '& int', 'int', ' int'],
	]
	mut lhs_ids := []flat.NodeId{}
	for item in cases {
		lhs_ids << a.add_node(flat.Node{
			kind:  .ident
			value: item[0]
			typ:   flat.source_type_id(item[1])
		})
	}
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.heaped_amp_locals['heap_value'] = true
	for i, item in cases {
		t.mut_param_values[item[0]] = item[0].starts_with('mut_')
		id := lhs_ids[i]
		assert t.multi_assign_slot_target_type(id, a.nodes[int(id)], flat.source_type_id(item[2])) ==
			flat.source_type_id(item[3]), item[0]
	}
}

fn numeric_addressability_test_node(mut a flat.FlatAst, kind flat.NodeKind, children []flat.NodeId) flat.NodeId {
	start := a.children.len
	for child in children {
		a.add_child(child)
	}
	return a.add_node(flat.Node{
		kind:           kind
		children_start: start
		children_count: flat.child_count(children.len)
		value:          if kind == .selector { 'field' } else { '' }
	})
}

fn test_numeric_addressability_preserves_index_and_selector_whitespace_rules() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.type_aliases.set_id('PointerAlias', flat.source_type_id('&int'))
	tc.type_aliases.set_id('MapPointerAlias', flat.source_type_id('&map[string]int'))
	names := ['&int', '&&int', '&string', '&map[string]int', 'int', ' &int ', 'PointerAlias',
		'MapPointerAlias']
	indexed := [true, true, false, false, false, false, true, false]
	selected := [true, true, true, true, false, true, true, true]
	mut index_ids := []flat.NodeId{}
	mut selector_ids := []flat.NodeId{}
	for name in names {
		// A cast yields a temporary; addressability here must come from its pointer type.
		base := a.add_node(flat.Node{ kind: .cast_expr, typ: flat.source_type_id(name) })
		zero := a.add_val(.int_literal, '0')
		index_ids << numeric_addressability_test_node(mut a, .index, [base, zero])
		selector_ids << numeric_addressability_test_node(mut a, .selector, [base])
	}
	t := new_transformer(mut a, &tc, map[string]bool{})
	for i, name in names {
		assert t.expr_can_take_address(index_ids[i]) == indexed[i], name
		assert t.expr_can_take_address(selector_ids[i]) == selected[i], name
	}
}
