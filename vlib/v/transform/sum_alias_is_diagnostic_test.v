module transform

import v.flat
import v.types

fn test_sum_alias_is_resolution_preserves_non_sum_diagnostic() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.type_aliases.set_id(flat.source_type_id('First'), flat.source_type_int)
	tc.type_aliases.set_id(flat.source_type_id('Second'), flat.source_type_id('First'))
	tc.type_aliases.set_id(flat.source_type_id('Third'), flat.source_type_id('Second'))
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_module = 'main'
	t.validating_generic_spec = true
	t.set_var_type('value', flat.source_type_id('Third'))
	value := t.make_ident('value')
	start := a.children.len
	a.children << value
	check := a.add_node(flat.Node{
		kind:           .is_expr
		source_value:   flat.source_type_int
		children_start: start
		children_count: 1
		typ:            flat.source_type_bool
	})
	t.transform_is_expr(check, a.nodes[int(check)])
	assert t.monomorph_errors == [
		'`is` can only be used with sum type or interface values, not `First`',
	]
}
