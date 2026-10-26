module types

import v.flat
import v.token

fn test_const_struct_inference_uses_original_operand_after_annotation_changes() {
	mut a := flat.FlatAst.new()
	original_id := flat.source_type_id('Original')
	resolved_id := flat.source_type_id('Resolved')
	literal_id := a.add_node(flat.Node{
		kind:         .struct_init
		source_value: original_id
		typ:          resolved_id
	})
	mut tc := TypeChecker.new(&a)
	tc.structs['Original'] = []StructField{}
	tc.structs['Resolved'] = []StructField{}
	inferred := tc.syntactic_const_element_type(literal_id)?
	assert inferred.source_type_id() == original_id
	assert a.node(literal_id).typ == resolved_id
}

fn test_import_usage_and_error_metadata_keep_interned_source_operands() {
	mut a := flat.FlatAst.new()
	import_id := a.add_node(flat.Node{
		kind:  .import_decl
		value: 'pkg'
		typ:   flat.source_type_id('pkg')
		pos:   token.new_pos(1, 0)
	})
	for kind in [flat.NodeKind.struct_init, .cast_expr, .map_init, .sizeof_expr, .ident] {
		spelling := if kind == .map_init { 'map[string]pkg.Item' } else { 'pkg.Item' }
		operand_id := a.add_node(flat.Node{
			kind:         kind
			source_value: flat.source_type_id(spelling)
			typ:          flat.source_type_int
			pos:          token.new_pos(1, 20)
		})
		mut tc := TypeChecker.new(&a)
		assert tc.import_is_used(import_id, *a.node(import_id), [int(operand_id)])
		diagnostic := tc.make_type_error(.unknown_type, 'unknown type', operand_id)
		assert diagnostic.node_value == spelling
	}
}
