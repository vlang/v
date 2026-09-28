module types

import os
import v.flat

fn test_return_alias_assertion_smartcast_stays_in_its_block() {
	mut a := flat.FlatAst.new()
	helper := a.add_val(.ident, 'helper')
	condition_start := a.begin_children()
	a.add_child(helper)
	condition := a.add_node(flat.Node{
		kind:           .is_expr
		value:          'int'
		children_start: condition_start
		children_count: 1
	})
	assert_start := a.begin_children()
	a.add_child(condition)
	assertion := a.add_node(flat.Node{
		kind:           .assert_stmt
		children_start: assert_start
		children_count: 1
	})
	block_start := a.begin_children()
	a.add_child(assertion)
	block := a.add_node(flat.Node{
		kind:           .block
		children_start: block_start
		children_count: 1
	})
	mut tc := TypeChecker.new(&a)
	tc.cur_scope.insert('helper', Type(int_))
	tc.smartcasts['outer'] = Type(string_)
	mut visiting := map[int]bool{}
	mut sources := []flat.NodeId{}
	tc.collect_returned_alias_sources_in_scope(block, map[string]flat.NodeId{}, mut visiting,
		mut sources)
	assert 'helper' !in tc.smartcasts
	assert tc.smartcasts['outer'] or { panic('missing outer smartcast') } == Type(string_)
}

fn test_flow_smartcast_callables_preserve_returned_aliases() {
	for index, body in [
		'assert helper is MapperA; return helper(values)',
		'for helper is MapperA { return helper(values) }; return values.clone()',
		'match helper { MapperA, MapperB { return helper(values) } else {} }; return values.clone()',
	] {
		path := os.join_path(os.vtmp_dir(), 'v3_return_alias_flow_${os.getpid()}_${index}.v')
		os.write_file(path, 'type MapperA = fn ([]int) []int
type MapperB = fn ([]int) []int
type MapperOrInt = MapperA | MapperB | int
fn helper(values []int) []int { return values.clone() }
fn passthrough(values []int) []int { return values }
fn nested(values []int, helper MapperOrInt) []int { ${body} }
fn main() { original := [1, 2]; mut alias := nested(original, MapperOrInt(MapperA(passthrough))); alias[0] = 9 }
')!
		defer { os.rm(path) or {} }
		result := os.execute('${os.quoted_path(@VEXE)} -new-compiler -check ${os.quoted_path(path)}')
		assert result.exit_code != 0, 'case ${index}: ${result.output}'
		assert result.output.contains('immutable'), 'case ${index}: ${result.output}'
	}
}
