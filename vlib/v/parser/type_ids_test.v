module parser

import v.flat

fn test_vml_declared_type_ids_keep_exact_spelling_dispatch() {
	for type_id in [flat.source_type_f64, flat.source_type_f32, flat.source_type_int] {
		assert vml_property_use(VmlProperty{ expr: &VmlExpr{}, declared_type: type_id }) == .number
	}
	assert vml_property_use(VmlProperty{ expr: &VmlExpr{}, declared_type: flat.source_type_bool }) == .bool_
	assert vml_property_use(VmlProperty{ expr: &VmlExpr{}, declared_type: flat.source_type_string }) == .string_
	assert vml_property_use(VmlProperty{ expr: &VmlExpr{}, declared_type: flat.source_type_id('color') }) == .color
	// Explicit unknown or padded annotations override the name-based inference.
	for spelling in [' int ', 'pkg.int', 'unknown'] {
		assert vml_property_use(VmlProperty{ expr: &VmlExpr{}, name: 'width', declared_type: flat.source_type_id(spelling) }) == .raw
	}
	assert vml_property_use(VmlProperty{ expr: &VmlExpr{}, name: 'width' }) == .number
}

fn test_named_constraint_ids_preserve_sparse_parameter_order() {
	constraint := flat.source_type_id('pkg.Box[&Item]')
	assert named_constraints([u32(0), constraint]) == [u32(0), constraint]
	assert named_constraints([u32(0), u32(0)]).len == 0
}
