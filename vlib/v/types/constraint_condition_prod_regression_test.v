module types

import v.flat

fn test_constraint_condition_term_handles_float_group_and_type_list() {
	mut a := flat.FlatAst.new()
	tc := TypeChecker.new(&a)
	constraint := GenericConstraint{
		types: [Type(i8_), Type(f32_), Type(f64_)]
	}
	for term in ['A is $float', 'B in[f32, f64]'] {
		got := tc.constraint_condition_term_types(term, term[..1], constraint) or { panic('term not recognized: ${term}') }
		assert got.len == 2, '${term}: ${got}'
		assert got.any(it.source_type_id() == Type(f32_).source_type_id())
		assert got.any(it.source_type_id() == Type(f64_).source_type_id())
	}
}
