module types

import v.flat

fn test_mut_optional_generic_param_substitution_preserves_declared_pointer_shape() {
	a := flat.FlatAst.new()
	tc := TypeChecker.new(&a)
	open := mut_param_semantic_type(tc.parse_type(flat.source_type_id('&?T')))
	item := Type(Struct{ name_id: flat.source_type_id('Item') })
	arg := Type(Pointer{ base_type: new_type_reference(item) })
	assert tc.substitute_generic_param_type(open, flat.source_type_id('&?T'), [arg], generic_param_ids(['T'])).name() == '&?&Item'
	// An explicit payload reference, an absent annotation and a padded spelling
	// all keep the original declaration-prefix distinction.
	for spelling in ['&?&T', '', ' &?T', '&!T'] {
		assert tc.substitute_generic_param_type(open, flat.source_type_id(spelling), [arg], generic_param_ids(['T'])).name() == '&?&&Item'
	}
	assert tc.substitute_generic_param_type(tc.parse_type(flat.source_type_id('T')),
		flat.source_type_id('&?T'), [arg], generic_param_ids(['T'])).name() == '&Item'
}
