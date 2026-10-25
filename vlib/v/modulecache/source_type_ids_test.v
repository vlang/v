module modulecache

import v.flat
import v.types

fn test_cached_global_source_ids_preserve_lookup_order_and_header_syntax() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.file_scope.insert('value', types.Type(types.u8_))
	tc.file_scope.insert('mod.value', types.Type(types.u16_))
	tc.file_scope.insert('mod.inferred', tc.parse_resolution_type(flat.source_type_id('[3]u16')))
	tc.file_scope.insert('mod.unset', types.Type(types.none_))
	assert cached_global_source_type_id(&tc, 'mod', 'value') == flat.source_type_u8
	assert cached_global_source_type_id(&tc, 'mod', 'missing') == 0
	assert cached_global_source_type_id(&tc, 'mod', 'unset') == flat.source_type_voidptr

	fields := [a.add_node(flat.Node{
		kind:  .field_decl
		value: 'explicit'
		typ:   flat.source_type_id('[2]u8')
	}), a.add_node(flat.Node{
		kind:  .field_decl
		value: 'inferred'
	}), a.add_node(flat.Node{
		kind:  .field_decl
		value: 'unset'
	}), a.add_node(flat.Node{
		kind:  .field_decl
		value: 'missing'
	})]
	start := a.children.len
	a.children << fields
	node_id := a.add_node(flat.Node{
		kind:           .global_decl
		children_start: i32(start)
		children_count: flat.child_count(fields.len)
	})
	text := global_text(&a, &tc, 'mod', a.nodes[int(node_id)])
	assert text == '__global (\n\texplicit [2]u8\n\tinferred [3]u16\n\tunset voidptr\n\tmissing\n)'
}
