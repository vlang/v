module types

import os
import v.flat

fn test_module_named_local_receiver_preserves_returned_alias() {
	root := os.join_path(os.vtmp_dir(), 'v3_return_alias_module_receiver_${os.getpid()}')
	module_dir := os.join_path(root, 'nested', 'foo')
	os.mkdir_all(module_dir)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'alias_scope' }")!
	os.write_file(os.join_path(module_dir, 'foo.v'), 'module foo
struct Passthrough {}
pub fn borrow(values []int) []int { return values.clone() }
fn (receiver Passthrough) borrow(values []int) []int { return values }
pub fn nested(values []int) []int { foo := Passthrough{}; return foo.borrow(values) }
')!
	os.write_file(os.join_path(root, 'main.v'), 'import nested.foo
fn main() { original := [1, 2]; mut alias := foo.nested(original); alias[0] = 9 }
')!
	result := os.exec([@VEXE, '-new-compiler', '-check', root])
	assert result.exit_code != 0, result.output
	assert result.output.contains('immutable'), result.output
}

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
		'is_mapper := helper is MapperA; if is_mapper { return helper(values) }; return values.clone()',
		'is_mapper := helper is MapperA; assert is_mapper; return helper(values)',
		'is_mapper, unused := helper is MapperA, 1; _ = unused; for is_mapper { return helper(values) }; return values.clone()',
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
		result := os.exec([@VEXE, '-new-compiler', '-check', path])
		assert result.exit_code != 0, 'case ${index}: ${result.output}'
		assert result.output.contains('immutable'), 'case ${index}: ${result.output}'
	}
}

fn test_for_in_header_handler_sees_outer_function() {
	path := os.join_path(os.vtmp_dir(), 'v3_return_alias_loop_header_${os.getpid()}.v')
	os.write_file(path, 'type Mapper = fn ([]int) []int
fn helper(values []int) []int { return values.clone() }
fn make_helpers() ![]Mapper { return error("no helpers") }
fn nested(values []int) []int {
	for helper in make_helpers() or { return helper(values) } { _ = helper }
	return values.clone()
}
fn main() { original := [1, 2]; mut fresh := nested(original); fresh[0] = 9 }
')!
	defer { os.rm(path) or {} }
	result := os.exec([@VEXE, '-new-compiler', '-check', path])
	assert result.exit_code == 0, result.output
}
