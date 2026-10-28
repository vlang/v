module types

import os
import v.flat

const attribute_narrowing_prefix = "@[attribute]
struct Route { path string }
@[attribute]
struct Get { path string }
@[attribute]
struct Limit { path int }
@[attribute]
struct MethodField { str string }
@[Route('/r')]
@[Get('/g')]
@[Limit(7)]
fn target() {}
@[Route('/r')]
@[Get('/g')]
fn uniform_target() {}
fn takes_string(value string) {}
fn takes_int(value int) {}
"

fn check_attribute_narrowing_source(body string) os.Result {
	root := os.join_path(os.vtmp_dir(), 'attribute_narrowing_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	path := os.join_path(root, 'main.v')
	os.write_file(path, attribute_narrowing_prefix + '\nfn main() {\n' + body + '\n}\n') or {
		panic(err)
	}
	return os.exec([@VEXE, '-check', path])
}

fn test_attribute_narrowing_checks_every_expression_shape() {
	for expression in [
		'x := attr.path\nprintln(x)',
		'mut x := ""\nx = attr.path\nprintln(x)',
		'assert attr.path == "/r"',
		'println("path=\${attr.path}")',
		'if attr.path == "/r" { println("found") }',
		'println(attr.path.len)',
	] {
		result := check_attribute_narrowing_source('$for attr in target.attributes {\n' + expression + '\n}')
		assert result.exit_code != 0, expression
		assert result.output.contains('attribute field `path` requires narrowing'), result.output
	}
	unknown := check_attribute_narrowing_source('$for attr in target.attributes {\nx := attr.missing\nprintln(x)\n}')
	assert unknown.exit_code != 0
	assert unknown.output.contains('unknown field `missing` from attr'), unknown.output
}

fn test_attribute_narrowing_tracks_boolean_branches_and_field_types() {
	result := check_attribute_narrowing_source('$for attr in target.attributes {
		println(attr.str())
		$if attr is Limit { takes_int(attr.path) }
		$if attr is Route || attr is Get { takes_string(attr.path) }
		$if attr.name == "Route" && attr is Route { takes_string(attr.path) }
		$if attr.has_arg && attr is Route { takes_string(attr.path) }
		$if !(attr is Route) {} $else { takes_string(attr.path) }
		$if attr is Route {
			$if attr is Get {} $else { takes_string(attr.path) }
		}
	}')
	assert result.exit_code == 0, result.output
}

fn test_attribute_narrowing_requires_a_schema_guard_for_uniform_fields() {
	for body in [
		'println(attr.path)',
		'$if attr.has_arg { println(attr.path) }',
		'$if attr.name == "Route" { println(attr.path) }',
		'$if true { println(attr.path) }',
		'$if attr !is Limit { println(attr.path) }',
		'$if attr is Route || attr.has_arg { println(attr.path) }',
	] {
		result := check_attribute_narrowing_source('$for attr in uniform_target.attributes {\n' + body + '\n}')
		assert result.exit_code != 0, body
		assert result.output.contains('attribute field `path` requires narrowing'), result.output
	}
}

fn test_attribute_narrowing_requires_every_possible_schema_to_agree() {
	for condition in ['attr is Route || attr is Limit', 'attr !is Get'] {
		result := check_attribute_narrowing_source('$for attr in target.attributes {\n$if ' + condition + ' { takes_string(attr.path) }\n}')
		assert result.exit_code != 0
		assert result.output.contains('attribute field `path`'), result.output
	}
	condition := check_attribute_narrowing_source('$for attr in target.attributes {\n$if attr is Route {\n$if attr.path == "/r" {}\n}\n}')
	assert condition.exit_code != 0
	assert condition.output.contains('unknown field `path` from attr'), condition.output
}

fn test_attribute_narrowing_scope_state_is_lazy_and_fork_safe() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	assert isnil(tc.comptime_attr_narrow_state)
	assert tc.comptime_attribute_union_member_type('path') == none
	parent_state := &ComptimeAttributeNarrowState{ names: ['', 'main.Route', 'main.Get'] }
	tc.comptime_attr_narrow_state = parent_state
	mut forked := tc.fork_program_view(&a, map[int][]SymbolId{})
	assert voidptr(forked.comptime_attr_narrow_state) == voidptr(parent_state)
	forked.comptime_attr_narrow_state = &ComptimeAttributeNarrowState{ names: ['main.Route'] }
	assert tc.comptime_attribute_narrowing_names() == ['', 'main.Route', 'main.Get']
	assert forked.comptime_attribute_narrowing_names() == ['main.Route']
	forked.comptime_attr_narrow_state = parent_state
	assert forked.comptime_attribute_narrowing_names() == tc.comptime_attribute_narrowing_names()
	item := ComptimeStaticValueCase{ attr_schema_id: flat.source_type_id('main.Route') }
	narrowed := ComptimeStaticValueCase{ ...item, attr_schema_narrowed: true }
	assert flat.source_type_name(item.attr_schema_id) == 'main.Route'
	assert narrowed.attr_schema_id == item.attr_schema_id
	assert !item.attr_schema_narrowed && narrowed.attr_schema_narrowed
	assert ComptimeStaticValueCase{}.attr_schema_id == 0
}
