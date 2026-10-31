module parser

import v.flat

fn test_named_constraint_ids_preserve_sparse_parameter_order() {
	constraint := flat.source_type_id('pkg.Box[&Item]')
	assert named_constraints([u32(0), constraint]) == [u32(0), constraint]
	assert named_constraints([u32(0), u32(0)]).len == 0
}
