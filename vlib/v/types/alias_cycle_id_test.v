module types

import v.flat

fn test_alias_cycle_ids_keep_scope_precedence_and_depth_order() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	first := flat.source_type_id('scope.First')
	second := flat.source_type_id('Second')
	tc.type_aliases.set('scope.First', 'Second')
	tc.type_aliases.set('scope.Second', ' First \t')
	assert tc.alias_chain_reaches(first, second, 0)
	assert tc.alias_chain_reaches(first, flat.source_type_id('scope.Second'), 0)
	assert !tc.alias_chain_reaches(first, flat.source_type_id('other.Second'), 0)
	// Exact identity is checked before the recursion limit, after qualification.
	assert tc.alias_chain_reaches(first, flat.source_type_id('First'), tc.type_aliases.ids.len)
	assert !tc.alias_chain_reaches(first, second, tc.type_aliases.ids.len)
	// An existing short binding takes precedence over scoped fallback.
	tc.type_aliases.set_id('Second', flat.source_type_int)
	assert !tc.alias_chain_reaches(first, second, 0)
	assert tc.alias_chain_reaches(first, flat.source_type_id('scope.Second'), 0)
	// An unrelated cycle must still terminate at the same depth bound.
	tc.type_aliases.set('Left', 'Right')
	tc.type_aliases.set('Right', 'Left')
	assert !tc.alias_chain_reaches(first, flat.source_type_id('Left'), 0)
}

fn test_alias_cycle_ids_keep_diagnostic_and_replacement_order() {
	mut a := flat.FlatAst.new()
	container := a.add_node(flat.Node{
		kind:  .type_decl
		value: 'Recursive'
		typ:   flat.source_type_id('[]Recursive')
	})
	first := a.add_node(flat.Node{
		kind:  .type_decl
		value: 'First'
		typ:   flat.source_type_id('Second')
	})
	second := a.add_node(flat.Node{
		kind:  .type_decl
		value: 'Second'
		typ:   flat.source_type_id('First')
	})
	mut tc := TypeChecker.new(&a)
	tc.type_aliases.set('cycles.Recursive', '[]Recursive')
	tc.type_aliases.set('cycles.First', 'Second')
	tc.type_aliases.set('cycles.Second', 'First')
	tc.type_aliases.set_id('Other', flat.source_type_int)
	// A repeated declaration is ignored, but replacements wait until every
	// declaration has been checked so both aliases in the cycle are diagnosed.
	tc.check_alias_declaration_cycles_of([i32(container), i32(first), i32(first), i32(second)],
		'cycles')
	assert tc.errors.len == 3
	assert tc.errors[0].msg == 'recursive declarations of aliases are not allowed - the alias `Recursive` is used in the array'
	assert tc.errors[1].msg == 'alias `First` forms a cycle through `Second`'
	assert tc.errors[2].msg == 'alias `Second` forms a cycle through `First`'
	for name in ['Recursive', 'First', 'Second'] {
		assert tc.recursive_alias_names[flat.source_type_id(name)]
		assert tc.recursive_alias_names[flat.source_type_id('cycles.${name}')]
		assert tc.type_aliases.value_id('cycles.${name}') == flat.source_type_void
	}
	assert tc.type_aliases.value_id('Other') == flat.source_type_int
}
