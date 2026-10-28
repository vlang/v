module types

import v.flat

fn test_allocation_alignment_follows_value_edges() {
	mut a := flat.FlatAst.new()
	aligned_decl := a.add_node(flat.Node{ kind: .struct_decl, value: 'Aligned' })
	mut tc := TypeChecker.new(&a)
	tc.first_type_declaration_ids[flat.source_type_id('Aligned')] = int(aligned_decl)
	tc.declaration_attributes[int(aligned_decl)] = ['aligned: 64']
	tc.structs[flat.source_type_id('Aligned')] = []StructField{}
	tc.sum_types.set(flat.source_type_id('Choice'), ['Aligned', 'int'])
	tc.type_aliases.set(flat.source_type_id('Alias'), 'Choice')
	tc.structs[flat.source_type_id('Holder')] = [StructField{ name: 'value', typ: *tc.parse_type(flat.source_type_id('Choice')) }]
	tc.structs[flat.source_type_id('Box')] = [StructField{ name: 'value', typ: *tc.parse_type(flat.source_type_id('T')) }]
	tc.struct_generic_params[flat.source_type_id('Box')] = generic_param_ids(['T'])
	aligned := ['Aligned', 'Choice', 'Alias', 'Holder', 'Box[Choice]', '[2]Choice', '?Choice',
		'!Choice']
	ordinary := ['int', '&Choice', '[]Choice', 'map[string]Choice', 'fn () Choice']
	for name in aligned {
		assert tc.requires_aligned_allocation(tc.parse_type(flat.source_type_id(name))), name
	}
	for name in ordinary {
		assert !tc.requires_aligned_allocation(tc.parse_type(flat.source_type_id(name))), name
	}
}

fn test_allocation_alignment_cache_tracks_declaration_changes() {
	mut a := flat.FlatAst.new()
	decl := a.add_node(flat.Node{ kind: .struct_decl, value: 'Value' })
	mut tc := TypeChecker.new(&a)
	tc.structs[flat.source_type_id('Value')] = []StructField{}
	tc.first_type_declaration_ids[flat.source_type_id('Value')] = int(decl)
	value := tc.parse_type(flat.source_type_id('Value'))
	assert !tc.requires_aligned_allocation(value)
	tc.declaration_attributes[int(decl)] = ['aligned: 64']
	tc.clear_field_lookup_cache()
	assert tc.requires_aligned_allocation(value)
}

fn test_allocation_scan_follows_storage_and_generic_fields() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.structs[flat.source_type_id('Plain')] = [StructField{ name: 'value', typ: Type(int_) }]
	tc.structs[flat.source_type_id('Pair')] = [
		StructField{ name: 'left', typ: *tc.parse_type(flat.source_type_id('Plain')) },
		StructField{ name: 'right', typ: *tc.parse_type(flat.source_type_id('Plain')) },
	]
	tc.structs[flat.source_type_id('Box')] = [StructField{ name: 'value', typ: *tc.parse_type(flat.source_type_id('T')) }]
	tc.struct_generic_params[flat.source_type_id('Box')] = generic_param_ids(['T'])
	tc.structs[flat.source_type_id('Shared')] = tc.structs[flat.source_type_id('Plain')]
	tc.struct_shared_fields[flat.source_type_id('Shared')] = {
		'value': true
	}
	tc.sum_types.set(flat.source_type_id('Scalars'), ['Plain', 'int'])
	tc.sum_types.set(flat.source_type_id('References'), ['Plain', '&Plain'])
	scalars := ['int', 'Plain', 'Pair', 'Box[Plain]', 'Scalars', '[2]Plain', '?Plain']
	references := [
		'&Plain',
		'[]Plain',
		'map[string]Plain',
		'string',
		'!Plain',
		'References',
		'Box[string]',
		'Shared',
		'Unknown',
		'C.Unknown',
	]
	for name in scalars {
		assert !tc.allocation_layout(tc.parse_type(flat.source_type_id(name))).has(.scanned), name
	}
	for name in references {
		assert tc.allocation_layout(tc.parse_type(flat.source_type_id(name))).has(.scanned), name
	}
	tc.structs[flat.source_type_id('Plain')] = [StructField{ name: 'value', typ: Type(string_) }]
	tc.clear_field_lookup_cache()
	assert tc.allocation_layout(tc.parse_type(flat.source_type_id('Pair'))).has(.scanned)
}
