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

fn fn_decl_param_pairs(a &flat.FlatAst, kind flat.NodeKind, name string) []string {
	for node in a.nodes {
		if node.kind != kind || node.value != name {
			continue
		}
		mut pairs := []string{}
		for i in 0 .. node.children_count {
			param := a.child_node(&node, i)
			if param.kind == .param {
				pairs << '${param.value}:${param.typ}'
			}
		}
		return pairs
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

fn test_c_function_anonymous_params_are_parsed_as_types() {
	a := parse_parser_regression_source('c_anon_params',
		'struct T {}\nstruct C.FILE {}\nstruct C.Widget {}\nstruct C.Node {}\n\nfn C.anon(&C.FILE, voidptr, int, &&T, [4]&C.Widget, ?&C.Node, !&C.Node, fn (&C.Node) int) int\nfn C.named(stream &C.FILE, a, b int) int\nfn C.named_arrays(m [16]f32, r []rune) int\nfn C.variadic(...int) int\nfn JS.js_anon(JS.Number) JS.Number\nfn JS.js_named(x JS.Number) JS.Number\nfn JS.setInterval(any, int, ...any) int\nfn JS.console.dir(any, any)\nfn JS.named_any(x any) any\nfn ordinary(int) {}\n')
	assert fn_decl_param_pairs(a, .c_fn_decl, 'anon') == [
		':&C.FILE',
		':voidptr',
		':int',
		':&&T',
		':[4]&C.Widget',
		':?&C.Node',
		':!&C.Node',
		':fn(&C.Node) int',
	]
	assert fn_decl_param_pairs(a, .c_fn_decl, 'named') == [
		'stream:&C.FILE',
		'a:int',
		'b:int',
	]
	assert fn_decl_param_pairs(a, .c_fn_decl, 'named_arrays') == [
		'm:[16]f32',
		'r:[]rune',
	]
	assert fn_decl_param_pairs(a, .c_fn_decl, 'variadic') == [':...int']
	assert fn_decl_param_pairs(a, .c_fn_decl, 'js_anon') == [':JS.Number']
	assert fn_decl_param_pairs(a, .c_fn_decl, 'js_named') == ['x:JS.Number']
	assert fn_decl_param_pairs(a, .c_fn_decl, 'setInterval') == [
		':any',
		':int',
		':...any',
	]
	assert fn_decl_param_pairs(a, .c_fn_decl, 'console.dir') == [
		':any',
		':any',
	]
	assert fn_decl_param_pairs(a, .c_fn_decl, 'named_any') == ['x:any']
	assert fn_decl_param_pairs(a, .fn_decl, 'ordinary') == ['int:']
}

fn test_sql_identifier_calls_are_not_parsed_as_sql_expr() {
	a := parse_parser_regression_source('sql_identifier_call',
		'fn sql(x int) int {\n\treturn x + 1\n}\n\nfn main() {\n\tx := sql(2)\n\tsql := 1\n\t_ := sql + 2\n}\n')
	mut sql_expr_count := 0
	mut call_count := 0
	for node in a.nodes {
		if node.kind == .sql_expr {
			sql_expr_count++
		}
		if node.kind == .call {
			call_count++
		}
	}
	assert sql_expr_count == 0
	assert call_count == 1
}
