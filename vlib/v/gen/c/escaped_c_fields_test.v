module c

import v.flat
import v.types

fn test_escaped_c_field_names_follow_the_resolved_owner() {
	g := FlatGen.new()
	c_struct := types.Type(types.Struct{ name: 'C.EscapedFields' })
	alias := types.Type(types.Alias{ name: 'Fields', base_type: c_struct })
	pointer := types.Type(types.Pointer{ base_type: alias })
	pointer_alias := types.Type(types.Alias{ name: 'FieldsPtr', base_type: pointer })
	for owner in [c_struct, alias, pointer, pointer_alias] {
		assert g.field_c_name(owner, '@type') == 'type'
		assert g.field_c_name(owner, '@module') == 'module'
		assert g.field_c_name(owner, '@select') == 'select'
		assert g.field_c_name(owner, '_v_type') == '_v_type'
		assert g.field_c_name(owner, 'type') == 'type'
	}
}

fn test_escaped_v_field_names_keep_their_existing_spelling() {
	g := FlatGen.new()
	v_struct := types.Type(types.Struct{ name: 'EscapedFields' })
	alias := types.Type(types.Alias{ name: 'Fields', base_type: v_struct })
	pointer := types.Type(types.Pointer{ base_type: alias })
	for owner in [v_struct, alias, pointer] {
		for field in ['@type', '@struct', '@select', '_v_type', 'type'] {
			assert g.field_c_name(owner, field) == c_name(field)
		}
	}
}

fn test_escaped_c_initializer_field_names_resolve_aliases() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.structs['C.EscapedFields'] = []types.StructField{}
	tc.structs['EscapedFields'] = []types.StructField{}
	tc.type_aliases['Fields'] = 'C.EscapedFields'
	tc.type_alias_modules['Fields'] = 'main'
	tc.cur_module = 'main'
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	assert g.init_field_c_name('C.EscapedFields', '@type') == 'type'
	assert g.init_field_c_name('Fields', '@type') == 'type'
	assert g.init_field_c_name('EscapedFields', '@type') == '_v_type'
	assert g.init_field_c_name('EscapedFields', '@struct') == '_v_struct'
}
