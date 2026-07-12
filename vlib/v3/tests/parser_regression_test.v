import os
import v3.flat
import v3.parser
import v3.pref
import v3.types

// parse_parser_regression_source reads parse parser regression source input for v3 tests.
fn parse_parser_regression_source(name string, source string) &flat.FlatAst {
	src := os.join_path(os.temp_dir(), 'v3_${name}.v')
	os.write_file(src, source) or { panic(err) }
	mut prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	p.parse_into(src)
	return p.a
}

fn parse_parser_regression_sources(name string, sources []string) &flat.FlatAst {
	mut prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	for i, source in sources {
		src := os.join_path(os.temp_dir(), 'v3_${name}_${i}.v')
		os.write_file(src, source) or { panic(err) }
		p.parse_into(src)
	}
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

fn struct_init_values(a &flat.FlatAst) []string {
	mut values := []string{}
	for node in a.nodes {
		if node.kind == .struct_init {
			values << node.value
		}
	}
	return values
}

fn cast_expr_values(a &flat.FlatAst) []string {
	mut values := []string{}
	for node in a.nodes {
		if node.kind == .cast_expr {
			values << node.value
		}
	}
	return values
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

fn test_lifetime_generic_suffixes_are_erased() {
	a := parse_parser_regression_source('lifetime_generic_suffixes',
		'struct IgnoreMatch {}\nstruct Match[T] {}\n\ninterface Matcher {\n\tmatched[^a](item Match[IgnoreMatch[^a]]) IgnoreMatch[^a]\n}\n\nfn use(item Match[IgnoreMatch[^a]]) {}\nfn after() int {\n\treturn 1\n}\n')
	assert interface_method_param_types(a, 'Matcher', 'matched') == [
		'Match[IgnoreMatch]',
	]
	assert fn_decl_param_pairs(a, .fn_decl, 'use') == ['item:Match[IgnoreMatch]']
	assert fn_decl_param_pairs(a, .fn_decl, 'after') == []
}

fn test_lifetime_generic_struct_init_suffixes_are_erased() {
	a := parse_parser_regression_source('lifetime_generic_struct_init_suffixes',
		'struct Candidate {}\nstruct Slot[T] {}\n\nfn make[^a]() {\n\t_ := Candidate[^a]{}\n\t_ := Slot[int, ^a]{}\n}\n')
	assert struct_init_values(a) == ['Candidate', 'Slot[int]']
}

fn test_c_pointer_cast_selector_parses_cast_before_selector() {
	a := parse_parser_regression_source('c_pointer_cast_selector', 'module main

@[typedef]
struct C.log__Logger {
mut:
	_object voidptr
}

fn object(logger &C.log__Logger) voidptr {
	return &C.log__Logger(logger)._object
}
')
	assert '&C.log__Logger' in cast_expr_values(a)
}

fn test_c_function_anonymous_params_are_parsed_as_types() {
	a := parse_parser_regression_source('c_anon_params',
		'struct T {}\nstruct C.FILE {}\nstruct C.Widget {}\nstruct C.Node {}\ntype MyHandle = voidptr\n\nfn C.anon(&C.FILE, voidptr, int, &&T, [4]&C.Widget, ?&C.Node, !&C.Node, fn (&C.Node) int) int\nfn C.custom(MyHandle, int, MyHandle) int\nfn C.lower(size_t, int, pthread_t) int\nfn C.named(stream &C.FILE, a, b int) int\nfn C.named_custom(handle MyHandle, a, b int) int\nfn C.named_arrays(m [16]f32, r []rune) int\nfn C.variadic(...int) int\nfn JS.js_anon(JS.Number) JS.Number\nfn JS.js_named(x JS.Number) JS.Number\nfn JS.setInterval(any, int, ...any) int\nfn JS.console.dir(any, any)\nfn JS.named_any(x any) any\nfn ordinary(int) {}\n')
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
	assert fn_decl_param_pairs(a, .c_fn_decl, 'custom') == [
		':MyHandle',
		':int',
		':MyHandle',
	]
	assert fn_decl_param_pairs(a, .c_fn_decl, 'lower') == [
		':size_t',
		':int',
		':pthread_t',
	]
	assert fn_decl_param_pairs(a, .c_fn_decl, 'named') == [
		'stream:&C.FILE',
		'a:int',
		'b:int',
	]
	assert fn_decl_param_pairs(a, .c_fn_decl, 'named_custom') == [
		'handle:MyHandle',
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

fn test_moduleless_file_does_not_inherit_previous_parser_module() {
	a := parse_parser_regression_sources('moduleless_export', [
		'module expmod\n\nfn helper() {}\n',
		"@[export: '1bad']\nfn lonely() {}\n",
	])
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.check_semantics()
	assert tc.errors.len == 1, tc.errors.str()
	assert tc.errors[0].msg.contains('for `lonely`'), tc.errors.str()
	assert !tc.errors[0].msg.contains('expmod.lonely'), tc.errors.str()
	assert fn_decl_param_pairs(a, .fn_decl, 'lonely') == []
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

fn test_statement_match_trailing_or_is_preserved() {
	a := parse_parser_regression_source('match_trailing_or_stmt',
		'fn f() !int {\n\treturn 1\n}\n\nfn main() {\n\tmatch f() {\n\t\t0, 1 {}\n\t\telse {}\n\t} or { 0 }\n}\n')
	mut found := false
	for node in a.nodes {
		if node.kind == .or_expr && node.children_count >= 1 {
			child := a.child_node(&node, 0)
			if child.kind == .match_stmt {
				found = true
			}
		}
	}
	assert found
}

fn test_local_generic_type_with_qualified_arg_resolves_base_before_qualification() {
	a := parse_parser_regression_source('local_generic_qualified_arg',
		'module main\n\nimport other\n\nfn main() {\n\tstruct Box[T] {}\n\tmut boxes := []Box[other.Thing]{}\n\tboxes << Box[other.Thing]{}\n}\n')
	mut local_decl_name := ''
	mut array_types := []string{}
	mut init_types := []string{}
	for node in a.nodes {
		match node.kind {
			.struct_decl {
				if node.value.starts_with('Box@local@') {
					local_decl_name = node.value
				}
			}
			.array_init {
				if node.value.contains('Box') {
					array_types << node.value
				}
			}
			.struct_init {
				if node.value.contains('Box') {
					init_types << node.value
				}
			}
			else {}
		}
	}
	assert local_decl_name.starts_with('Box@local@main')
	assert array_types == ['${local_decl_name}[other.Thing]']
	assert init_types == ['${local_decl_name}[other.Thing]']
}

fn test_local_type_generic_call_type_arg_is_resolved() {
	a := parse_parser_regression_source('local_generic_call_type_arg',
		'module main\n\nfn id[T](x T) T {\n\treturn x\n}\n\nfn main() {\n\tstruct Row {\n\t\tn int\n\t}\n\t_ := id[Row](Row{\n\t\tn: 1\n\t})\n}\n')
	mut local_row := ''
	mut call_type_args := []string{}
	mut init_types := []string{}
	for node in a.nodes {
		match node.kind {
			.struct_decl {
				if node.value.starts_with('Row@local@') {
					local_row = node.value
				}
			}
			.index {
				if node.children_count == 2 {
					base := a.child_node(&node, 0)
					arg := a.child_node(&node, 1)
					if base.kind == .ident && base.value == 'id' {
						call_type_args << arg.value
					}
				}
			}
			.struct_init {
				if node.value.contains('Row') {
					init_types << node.value
				}
			}
			else {}
		}
	}
	assert local_row.starts_with('Row@local@main')
	assert call_type_args == [local_row]
	assert init_types == [local_row]
}

fn test_uppercase_index_condition_before_block_is_not_struct_init() {
	a := parse_parser_regression_source('uppercase_index_condition_block',
		'const Foo = [true]\n\nfn main() {\n\tif Foo[0] {\n\t\tprintln("ok")\n\t}\n}\n')
	mut foo_struct_inits := []string{}
	mut foo_index_count := 0
	for node in a.nodes {
		match node.kind {
			.struct_init {
				if node.value == 'Foo' {
					foo_struct_inits << node.value
				}
			}
			.index {
				if node.children_count == 2 {
					base := a.child_node(&node, 0)
					if base.kind == .ident && base.value == 'Foo' {
						foo_index_count++
					}
				}
			}
			else {}
		}
	}
	assert foo_struct_inits == []
	assert foo_index_count == 1
}

fn test_uppercase_identifier_index_condition_before_block_is_not_struct_init() {
	a := parse_parser_regression_source('uppercase_identifier_index_condition_block',
		'const Foo = [true, false]\n\nfn main() {\n\tidx := 0\n\tif Foo[idx] {\n\t\tprintln("ok")\n\t}\n}\n')
	mut foo_struct_inits := []string{}
	mut foo_index_count := 0
	for node in a.nodes {
		match node.kind {
			.struct_init {
				if node.value == 'Foo' {
					foo_struct_inits << node.value
				}
			}
			.index {
				if node.children_count == 2 {
					base := a.child_node(&node, 0)
					if base.kind == .ident && base.value == 'Foo' {
						foo_index_count++
					}
				}
			}
			else {}
		}
	}
	assert foo_struct_inits == []
	assert foo_index_count == 1
}

fn test_local_sibling_types_are_predeclared_before_fields() {
	a := parse_parser_regression_source('local_sibling_struct_fields',
		'module main\n\nfn main() {\n\t_ := []struct {\n\t\tn int\n\t}{}\n\tstruct A {\n\t\tb &B\n\t}\n\tstruct B {\n\t\ta &A\n\t}\n}\n')
	mut local_a := ''
	mut local_b := ''
	for node in a.nodes {
		if node.kind == .struct_decl {
			if node.value.starts_with('A@local@main') {
				local_a = node.value
			}
			if node.value.starts_with('B@local@main') {
				local_b = node.value
			}
		}
	}
	mut a_fields := []string{}
	mut b_fields := []string{}
	for node in a.nodes {
		if node.kind != .struct_decl {
			continue
		}
		for i in 0 .. node.children_count {
			field := a.child_node(&node, i)
			if field.kind != .field_decl {
				continue
			}
			if node.value == local_a {
				a_fields << '${field.value}:${field.typ}'
			}
			if node.value == local_b {
				b_fields << '${field.value}:${field.typ}'
			}
		}
	}
	assert local_a.len > 0
	assert local_b.len > 0
	assert a_fields == ['b:&${local_b}']
	assert b_fields == ['a:&${local_a}']
}

fn test_local_type_scope_names_do_not_collapse_punctuation() {
	a := parse_parser_regression_source('local_scope_punctuation_collision',
		'module main\n\nstruct Foo {}\n\nfn (f Foo) bar() {\n\tstruct Row {\n\t\tmethod int\n\t}\n\t_ := Row{}\n}\n\nfn Foo_bar() {\n\tstruct Row {\n\t\tfunction int\n\t}\n\t_ := Row{}\n}\n')
	mut row_names := []string{}
	for node in a.nodes {
		if node.kind == .struct_decl && node.value.starts_with('Row@local@') {
			row_names << node.value
		}
	}
	assert row_names.len == 2
	assert row_names[0] != row_names[1]
}

fn test_multiline_keyword_infix_expressions_continue_after_semicolon() {
	a := parse_parser_regression_source('multiline_keyword_infix',
		'module main\n\nstruct Foo {}\n\nfn main() {\n\tvalue := Foo{}\n\tif value\n\t\tis Foo {}\n\txs := [1, 2]\n\tok := 1\n\t\tin xs\n\t_ := value\n\t\tas Foo\n\t_ = ok\n}\n')
	mut is_count := 0
	mut in_count := 0
	mut as_count := 0
	for node in a.nodes {
		match node.kind {
			.is_expr {
				is_count++
			}
			.in_expr {
				in_count++
			}
			.as_expr {
				as_count++
			}
			else {}
		}
	}
	assert is_count == 1
	assert in_count == 1
	assert as_count == 1
}

fn test_normalized_option_result_fixed_array_type_names_parse_as_wrapped_arrays() {
	mut a := flat.FlatAst.new()
	tc := types.TypeChecker.new(&a)
	opt := tc.parse_type('?int[2]')
	assert opt is types.OptionType
	if opt is types.OptionType {
		assert opt.base_type is types.ArrayFixed
		if opt.base_type is types.ArrayFixed {
			assert opt.base_type.elem_type.name() == 'int'
			assert opt.base_type.len == 2
		}
	}
	res := tc.parse_type('!Foo[3]')
	assert res is types.ResultType
	if res is types.ResultType {
		assert res.base_type is types.ArrayFixed
		if res.base_type is types.ArrayFixed {
			assert res.base_type.elem_type.name() == 'Foo'
			assert res.base_type.len == 3
		}
	}
}
