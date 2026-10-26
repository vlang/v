module types

import v.flat

fn test_recursive_embeds_preserve_direct_fields_and_terminate_missing_lookup() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.structs['Left'] = [
		StructField{ name: 'Right', typ: Type(Struct{ name_id: flat.source_type_id('Right') }), is_embed: true },
		StructField{ name: 'value', typ: builtin_int_type },
	]
	tc.structs['Right'] = [
		StructField{ name: 'Left', typ: Type(Struct{ name_id: flat.source_type_id('Left') }), is_embed: true },
		StructField{ name: 'value', typ: builtin_string_type },
	]
	assert (tc.struct_field_type(flat.source_type_id('Left'), 'value') or { panic('missing direct field') }) == builtin_int_type
	assert (tc.struct_field_type(flat.source_type_id('Right'), 'value') or { panic('missing direct field') }) == builtin_string_type
	assert tc.struct_field_type(flat.source_type_id('Left'), 'missing') == none
	assert tc.struct_field_type(flat.source_type_id('Right'), 'missing') == none
}
