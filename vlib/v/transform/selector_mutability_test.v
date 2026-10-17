module transform

import v.flat
import v.types

fn test_rebuilt_pointer_field_selector_preserves_mutable_argument() {
	for mutable in [false, true] {
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		mut t := new_transformer(mut a, &tc, map[string]bool{})
		t.set_var_type('holder', flat.source_type_id('&Holder'))
		t.structs['Holder'] = StructInfo{
			name:   flat.source_type_id('Holder')
			fields: [FieldInfo{ name: 'value', typ: flat.source_type_id('&int') }]
		}
		base := t.make_ident('holder')
		field := t.make_selector(base, 'value', flat.source_type_id('&int'))
		a.set_node_is_mut(field, mutable)
		lowered := t.transform_selector_expr(field, a.nodes[int(field)])
		assert a.nodes[int(lowered)].op == .arrow
		assert a.nodes[int(lowered)].is_mut == mutable
	}
}
