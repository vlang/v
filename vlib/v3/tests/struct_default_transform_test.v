import os
import v3.flat
import v3.parser
import v3.pref
import v3.transform
import v3.types

// parse_and_transform reads parse and transform input for v3 tests.
fn parse_and_transform(source string) &flat.FlatAst {
	src := os.join_path(os.temp_dir(), 'v3_struct_default_transform_test.v')
	os.write_file(src, source) or { panic(err) }
	prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	mut a := p.parse_file(src)
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.annotate_types()
	transform.transform(mut a, &tc)
	return a
}

// transformed_struct_init supports transformed struct init handling for v3 tests.
fn transformed_struct_init(a &flat.FlatAst, fn_name string) flat.Node {
	for node in a.nodes {
		if node.kind != .fn_decl || node.value != fn_name {
			continue
		}
		for i in 0 .. node.children_count {
			stmt := a.child_node(&node, i)
			if stmt.kind != .decl_assign || stmt.children_count < 2 {
				continue
			}
			rhs := a.child_node(stmt, 1)
			if rhs.kind == .struct_init {
				return *rhs
			}
		}
	}
	assert false
	return flat.Node{}
}

// find_field resolves find field information for v3 tests.
fn find_field(a &flat.FlatAst, node flat.Node, name string) flat.Node {
	for i in 0 .. node.children_count {
		field := a.child_node(&node, i)
		if field.kind == .field_init && field.value == name {
			return *field
		}
	}
	assert false
	return flat.Node{}
}

// call_name updates call name state for v3 tests.
fn call_name(a &flat.FlatAst, call flat.Node) string {
	if call.kind != .call || call.children_count == 0 {
		return ''
	}
	return a.child_node(&call, 0).value
}

// test_transform_fills_missing_struct_defaults validates this v3 regression case.
fn test_transform_fills_missing_struct_defaults() {
	mut a := parse_and_transform("
struct Person {
	name string = 'v' + '3'
	age int = 42
}

fn main() {
	person := Person{age: 7}
}
")
	init := transformed_struct_init(a, 'main')
	assert init.value == 'Person'
	assert init.children_count == 2

	name_field := find_field(a, init, 'name')
	name_value := a.child_node(&name_field, 0)
	assert name_value.kind == .string_literal
	assert name_value.value == 'v3'

	age_field := find_field(a, init, 'age')
	age_value := a.child_node(&age_field, 0)
	assert age_value.kind == .int_literal
	assert age_value.value == '7'
}
