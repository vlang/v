module transform

import v.flat
import v.types

fn test_match_common_type_resolves_import_alias_on_enum_cast() {
	mut a := flat.FlatAst.new()
	subject := a.add_val(.ident, 'k')
	condition := a.add_val(.int_literal, '1')
	zero := a.add_val(.int_literal, '0')
	cast_start := a.begin_children()
	a.add_child(zero)
	cast := a.add_node(flat.Node{
		kind:           .cast_expr
		value:          'colors.Second'
		children_start: cast_start
		children_count: 1
	})
	first_start := a.begin_children()
	a.add_child(condition)
	a.add_child(cast)
	first := a.add_node(flat.Node{
		kind:           .match_branch
		children_start: first_start
		children_count: 2
	})
	shorthand := a.add_val(.enum_val, 'blue')
	last_start := a.begin_children()
	a.add_child(shorthand)
	last := a.add_node(flat.Node{
		kind:           .match_branch
		value:          'else'
		children_start: last_start
		children_count: 1
	})
	match_start := a.begin_children()
	a.add_child(subject)
	a.add_child(first)
	a.add_child(last)
	node := flat.Node{
		kind:           .match_stmt
		children_start: match_start
		children_count: 3
	}
	mut tc := types.TypeChecker.new(&a)
	tc.file_imports[file_import_key('match.v', 'colors')] = 'match_colors'
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_file = 'match.v'
	t.enum_types[flat.source_type_id('match_colors.First')] = ['red', 'blue']
	t.enum_types[flat.source_type_id('match_colors.Second')] = ['red', 'blue']
	assert t.match_expr_common_value_type(node) == flat.source_type_id('match_colors.Second')
}
