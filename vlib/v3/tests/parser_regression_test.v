import os
import v3.flat
import v3.parser
import v3.pref

// parse_parser_regression_source reads parse parser regression source input for v3 tests.
fn parse_parser_regression_source(name string, source string) &flat.FlatAst {
	src := os.join_path(os.temp_dir(), 'v3_${name}.v')
	os.write_file(src, source) or { panic(err) }
	mut prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	p.parse_into(src)
	return p.a
}

// interface_method_param_types supports interface method param types handling for v3 tests.
fn interface_method_param_types(a &flat.FlatAst, iface string, method string) []string {
	for node in a.nodes {
		if node.kind != .interface_decl || node.value != iface {
			continue
		}
		for i in 0 .. node.children_count {
			field := a.child_node(&node, i)
			if field.kind != .interface_field || field.value != method {
				continue
			}
			mut params := []string{}
			for j in 0 .. field.children_count {
				param := a.child_node(field, j)
				if param.kind == .param {
					params << param.typ
				}
			}
			return params
		}
	}
	return []string{}
}

// test_interface_method_generic_type_only_param_is_not_parsed_as_name
// validates this v3 regression case.
fn test_interface_method_generic_type_only_param_is_not_parsed_as_name() {
	a := parse_parser_regression_source('interface_generic_param',
		'const max_len = 16\nstruct Result[T] {}\nstruct Node {}\n\ninterface Sink {\n\tput(Result[int])\n\tappend(values []int)\n\tvisit(node &Node)\n\tread(buf [max_len]u8)\n}\n')
	assert interface_method_param_types(a, 'Sink', 'put') == ['Result[int]']
	assert interface_method_param_types(a, 'Sink', 'append') == ['[]int']
	assert interface_method_param_types(a, 'Sink', 'visit') == ['&Node']
	assert interface_method_param_types(a, 'Sink', 'read') == ['[max_len]u8']
}
