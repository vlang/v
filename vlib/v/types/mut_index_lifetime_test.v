module types

import v.flat

struct MutIndexLifetimeCase {
	base   Type
	result string
}

fn test_mutable_index_resolves_after_binding_unwrap() {
	cases := [
		MutIndexLifetimeCase{ base: builtin_string_type, result: 'u8' },
		MutIndexLifetimeCase{ base: Type(Array{ elem_type: &builtin_int_type }), result: 'int' },
		MutIndexLifetimeCase{ base: Type(OptionType{ base_type: &builtin_string_type }), result: '?u8' },
		MutIndexLifetimeCase{ base: Type(Pointer{ base_type: &builtin_int_type }), result: '&int' },
	]
	for case in cases {
		mut a := flat.FlatAst.new()
		value := a.add_val(.ident, 'value')
		position := a.add_val(.int_literal, '0')
		a.children << [value, position]
		index := a.add_node(flat.Node{ kind: .index, children_count: 2 })
		mut tc := TypeChecker.new(&a)
		tc.cur_scope = new_scope(tc.file_scope)
		raw := Type(Pointer{ base_type: new_type_reference(case.base) })
		owner := tc.cur_scope.insert_with_owner('value', raw)
		tc.fn_context.node_id = int(value)
		tc.fn_context.mut_param_base_types['value'] = case.base
		tc.fn_context.mut_param_owners['value'] = owner
		mut_base := tc.mut_param_expr_base(value, raw) or { panic('missing mutable binding') }
		assert mut_base.name() == case.base.name()
		actual := tc.resolve_index_type(*a.node(index)).name()
		assert actual == case.result, '${case.base.name()}: ${actual} != ${case.result}'
	}
}
