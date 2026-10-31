module types

import v.flat

fn test_scalar_source_id_classification_matches_exact_builtin_spellings() {
	for spelling in ['bool', 'char', 'i8', 'u8', 'i128', 'u128', 'uint', 'nil', 'none', 'voidptr',
		'array', 'map', 'unknown', '', ' int ', 'Int', 'builtin.int', '?int', '[]int'] {
		expected := is_builtin_type_name(spelling) && spelling != 'array' && spelling != 'map'
		assert scalar_builtin_type_name_id(flat.source_type_id(spelling)) == expected
	}
}

fn test_source_field_lookup_preserves_annotations_and_embedded_fallback() {
	mut a := flat.FlatAst.new()
	module_id := a.add_val(.module_decl, 'main')
	annotation := flat.source_type_id(' \tmap[string][]int\n')
	field := a.add_node(flat.Node{ kind: .field_decl, value: 'items', typ: annotation })
	embed := a.add_node(flat.Node{ kind: .field_decl, value: 'Embedded' })
	children := a.begin_children()
	a.add_child(field)
	a.add_child(embed)
	decl := a.add_node(flat.Node{
		kind:           .struct_decl
		value:          'Container'
		children_start: children
		children_count: 2
	})
	mut tc := TypeChecker.new(&a)
	tc.top_level_idx = [int(module_id), int(decl)]
	fields := tc.source_struct_field_decls(flat.source_type_id('main.Container'))
	assert fields.len == 2
	assert fields[0].name == 'items'
	assert fields[0].typ == annotation
	assert fields[1].name == 'Embedded'
	assert fields[1].typ == flat.source_type_id('Embedded')
}

fn test_literal_and_variadic_type_ids_keep_whitespace_and_wrapper_rules() {
	a := flat.FlatAst.new()
	tc := TypeChecker.new(&a)
	literal := flat.source_type_id(' \tBox[int]\n')
	parsed := flat.source_type_id('main.Box')
	assert tc.struct_init_field_lookup_name(literal, 0) == flat.source_type_id('Box[int]')
	assert tc.struct_init_field_lookup_name(literal, parsed) == flat.source_type_id('main.Box[int]')
	concrete := flat.source_type_id('main.Box[string]')
	assert tc.struct_init_field_lookup_name(literal, concrete) == concrete
	assert tc.struct_init_field_lookup_name(flat.source_type_id(' \t\n'), parsed) == parsed
	for spelling in ['pkg.Item', ' \tpkg.Item\n', '... pkg.Item ', '[] pkg.Item ', '...[]pkg.Item',
		'[2]pkg.Item'] {
		clean := spelling.trim_space()
		expected := if clean.starts_with('...') {
			clean[3..].trim_space()
		} else if clean.starts_with('[]') {
			clean[2..].trim_space()
		} else {
			clean
		}
		assert generic_variadic_elem_param_text(flat.source_type_id(spelling)) == flat.source_type_id(expected)
	}
}
