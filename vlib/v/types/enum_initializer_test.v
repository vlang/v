module types

import v.flat

fn test_enum_initializer_references_use_the_declaring_module_and_exact_name() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.enum_fields['models.Kind'] = ['struct', '@none', 'type', '@type']
	mut values := {
		'struct': 4
		'@none':  11
		'type':   17
		'@type':  23
	}
	expressions := map[string]flat.NodeId{}
	mut resolving := map[string]bool{}
	for field, expected in {
		'@struct': 4
		'none':    11
		'type':    17
		'@type':   23
	} {
		for enum_name in ['Kind', 'models.Kind'] {
			actual := tc.comptime_static_enum_field_ref_value(field, 'models', enum_name,
				mut values, expressions, mut resolving) or { -1 }
			assert actual == expected, field
		}
	}
}
