import os

const vexe = @VEXE
const vfmt_test_tdir = os.join_path(os.vtmp_dir(), 'vfmt_test_25005')

fn testsuite_begin() {
	os.rmdir_all(vfmt_test_tdir) or {}
	os.mkdir_all(vfmt_test_tdir)!
}

fn testsuite_end() {
	os.rmdir_all(vfmt_test_tdir) or {}
}

fn test_fmt_keeps_invalid_assert_source_unchanged() {
	source_path := os.join_path(vfmt_test_tdir, 'invalid_assert_message.v')
	original := "fn main() {\n\tassert false 'bye'\n}\n"
	os.write_file(source_path, original)!

	res := os.exec([vexe, 'fmt', '-w', source_path])

	assert res.exit_code != 0, res.output
	assert res.output.contains('unexpected string `bye`, expecting `,`'), res.output
	assert os.read_file(source_path)! == original
}

fn test_fmt_preferences_respect_vflags() {
	source_path := os.join_path(vfmt_test_tdir, 'vflags_backend_js.v')
	os.write_file(source_path, "fn main() {\n\tx := 'abc'.str\n}\n")!

	old_vflags := os.getenv('VFLAGS')
	defer {
		if old_vflags == '' {
			os.unsetenv('VFLAGS')
		} else {
			os.setenv('VFLAGS', old_vflags, true)
		}
	}

	os.unsetenv('VFLAGS')
	warmup_res := os.exec([vexe, 'fmt', '-help'])
	assert warmup_res.exit_code == 0, warmup_res.output
	c_res := os.exec([vexe, 'fmt', source_path])
	assert c_res.exit_code == 0, c_res.output
	assert c_res.output.contains("x := c'abc'"), c_res.output
	assert !c_res.output.contains("x := 'abc'.str"), c_res.output
	special_js_path := os.join_path(vfmt_test_tdir, 'v', 'gen', 'js', 'tests', 'js.v')
	os.mkdir_all(os.dir(special_js_path))!
	os.write_file(special_js_path, "fn main() {\n\tx := 'abc'.str\n}\n")!
	special_res := os.exec([vexe, 'fmt', special_js_path])
	assert special_res.exit_code == 0, special_res.output
	assert special_res.output.contains("x := 'abc'.str"), special_res.output
	assert !special_res.output.contains("x := c'abc'"), special_res.output

	for backend_flag in ['-b', '-backend'] {
		for backend in ['js', 'js_node', 'js_browser', 'js_freestanding'] {
			os.setenv('VFLAGS', '${backend_flag} ${backend}', true)
			res := os.exec([vexe, 'fmt', source_path])
			assert res.exit_code == 0, '${backend_flag} ${backend}: ${res.output}'
			assert res.output.contains("x := 'abc'.str"), '${backend_flag} ${backend}: ${res.output}'
			assert !res.output.contains("x := c'abc'"), '${backend_flag} ${backend}: ${res.output}'
		}
	}
	for backend in ['native', 'go', 'arm64', 'eval'] {
		os.unsetenv('VFLAGS')
		res := os.exec([vexe, '-b', '${backend}', 'fmt', source_path])
		assert res.exit_code == 0, '${backend}: ${res.output}'
		assert res.output.contains("x := c'abc'"), '${backend}: ${res.output}'
	}

	for backend_flag in ['-b', '-backend'] {
		os.setenv('VFLAGS', '${backend_flag} jss', true)
		vflags_res := os.exec([vexe, 'fmt', source_path])
		assert vflags_res.exit_code != 0, '${backend_flag} jss: ${vflags_res.output}'
		assert vflags_res.output.contains('Unknown V backend: jss'), vflags_res.output

		os.unsetenv('VFLAGS')
		cli_res :=
			os.exec([vexe, 'fmt', '${backend_flag}', 'jss', source_path])
		assert cli_res.exit_code != 0, '${backend_flag} jss: ${cli_res.output}'
		assert cli_res.output.contains('Unknown V backend: jss'), cli_res.output
	}
}

fn test_fmt_joined_backend_options() {
	source_path := os.join_path(vfmt_test_tdir, 'joined_backend.v')
	os.write_file(source_path, "fn main() { x := 'abc'.str }\n")!
	old_vflags := os.getenv('VFLAGS')
	defer {
		if old_vflags == '' {
			os.unsetenv('VFLAGS')
		} else {
			os.setenv('VFLAGS', old_vflags, true)
		}
	}
	for flag in ['-b', '-backend'] {
		for backend in ['js', 'js_browser', 'wasm', 'arm64', 'eval'] {
			option := '${flag}=${backend}'
			for placement in ['prefix', 'suffix', 'environment'] {
				os.unsetenv('VFLAGS')
				args := match placement {
					'prefix' { '${option} fmt' }
					'suffix' { 'fmt ${option}' }
					else {
						os.setenv('VFLAGS', option, true)
						'fmt'
					}
				}
				res := os.exec([vexe, ...(os.split_args(args) or { panic(err) }), source_path])
				assert res.exit_code == 0, '${placement} ${option}: ${res.output}'
				expected := if backend.starts_with('js') { "x := 'abc'.str" } else { "x := c'abc'" }
				assert res.output.contains(expected), '${placement} ${option}: ${res.output}'
			}
		}
		os.unsetenv('VFLAGS')
		invalid := os.exec([vexe, '${flag}' + '=jss', 'fmt', source_path])
		assert invalid.exit_code != 0, invalid.output
		assert invalid.output.contains('Unknown V backend: jss'), invalid.output
	}
}

fn test_fmt_accepts_semantic_duplicate_operator_and_recursive_alias() {
	for source in [
		'struct Number {}\nfn (n Number) + (other Number) Number { return n }\nfn (n Number) + (other Number) Number { return n }\n',
		'type Node = Node\n',
		'type Maybe = int | none\n',
		'interface T {}\n',
		'type Callback = fn (Callback)\n',
		'@[deprecated; deprecated] fn old() {}\n',
		'fn run(Value int) {}\n',
		'fn main() { $if myflag { println(1) } }\n',
		'fn main() { defer { defer {} } }\n',
		'fn main() { defer { return } }\n',
		'fn f() ! { defer { f()! } }\n',
		'fn f() ?int { defer { f()? }; return 1 }\n',
		'fn main() { println($res()) }\n',
		'struct Number {}\nfn (n Number) += (other Number) Number { return n }\n',
		'fn handle(int) {}\n',
		'fn main() { a := []int{init: 1}; _ = a }\n',
		'fn main() { select { else {} else {} } }\n',
		'fn main() { unsafe { unsafe { println(1) } } }\n',
	] {
		path := os.join_path(vfmt_test_tdir, 'semantic_editing_buffer.v')
		os.write_file(path, source)!
		res := os.exec([vexe, 'fmt', path])
		assert res.exit_code == 0, res.output
	}
}

fn test_fmt_preserves_assembly_compatibility() {
	for options in ['', '-b=wasm', '-b=arm64', '-b=eval'] {
		for assembly in ['asm arm64 intel { nop }', 'asm amd64 raw { "nop" }', 'asm goto arm64 { nop }'] {
			source := 'fn main() { ${assembly} }\n'
			res, formatted := run_vfmt_write('assembly_compatibility', source, options)
			assert res.exit_code == 0, res.output
			assert formatted.contains(assembly), formatted
			second, twice := run_vfmt_write('assembly_compatibility_twice', formatted, options)
			assert second.exit_code == 0, second.output
			assert twice == formatted
		}
	}
}

fn test_fmt_preserves_match_conditions() {
	source := 'fn f(value int) { if match value { 0 { true } else { false } } { println(value) } }\n'
	res, formatted := run_vfmt_write('match_condition', source, '')
	assert res.exit_code == 0, res.output
	assert formatted.contains('if match value {')
	assert formatted.contains('true') && formatted.contains('false')
	assert formatted.contains('println(value)')
	second, twice := run_vfmt_write('match_condition_twice', formatted, '')
	assert second.exit_code == 0, second.output
	assert twice == formatted
}

fn test_fmt_preserves_index_propagation() {
	for source in [
		'fn f(values []?int) ?int { return values[0]? }\n',
		'fn f(values map[string]?int) ?int { return values["key"]? }\n',
	] {
		res, formatted := run_vfmt_write('index_propagation', source, '')
		assert res.exit_code == 0, res.output
		assert formatted.contains(']?'), formatted
		second, twice := run_vfmt_write('index_propagation_twice', formatted, '')
		assert second.exit_code == 0, second.output
		assert twice == formatted
	}
}

fn test_fmt_preserves_multiple_attribute_groups() {
	for source, expected in {
		'@[inline] @[deprecated] fn f() {}\n':                        [
			'@[deprecated; inline]',
			'fn f() {}',
		]
		'@[inline]\n@[deprecated]\nfn f() {}\n':                      [
			'@[deprecated; inline]',
			'fn f() {}',
		]
		"struct Holder { value int @[required] @[json: 'value'] }\n": ['required', "json: 'value'",
			'value int']
	} {
		res, formatted := run_vfmt_write('multiple_attribute_groups', source, '')
		assert res.exit_code == 0, res.output
		for part in expected {
			assert formatted.contains(part), formatted
		}
		second, formatted_twice := run_vfmt_write('multiple_attribute_groups_twice', formatted, '')
		assert second.exit_code == 0, second.output
		assert formatted_twice == formatted
	}
	invalid := '@[inline]\n@[deprecated: ]\nfn f() {}\n'
	res, unchanged := run_vfmt_write('invalid_attribute_group', invalid, '')
	assert res.exit_code != 0, res.output
	assert unchanged == invalid
}

fn test_fmt_preserves_duplicate_deprecated_message_arguments() {
	source := "@[deprecated(msg: 'old', msg: 'new')] fn old() {}\n"
	res, formatted := run_vfmt_write('duplicate_deprecated_messages', source, '')
	assert res.exit_code == 0, res.output
	assert formatted.contains("@[deprecated(msg: 'old', msg: 'new')]")
	second, formatted_twice := run_vfmt_write('duplicate_deprecated_messages_twice', formatted, '')
	assert second.exit_code == 0, second.output
	assert formatted_twice == formatted
}

fn test_fmt_preserves_signature_and_comptime_semantic_errors() {
	for source, expected in {
		'fn f() { match sql { else {} } }\n':         'match sql'
		'fn f(sql int) {}\n':                         'fn f(sql int)'
		'fn f(value, sql int) {}\n':                  'fn f(value int, sql int)'
		'fn f[T](value T) [T] { return value }\n':    'fn f[T](value T) [T]'
		'fn f(xs ...int, y int) {}\n':                'fn f(xs ...int, y int)'
		'fn f[T]() { $for field in T.unknown {} }\n': '$for field in T.unknown'
	} {
		res, formatted := run_vfmt_write('semantic_signature', source, '')
		assert res.exit_code == 0, res.output
		assert formatted.contains(expected), formatted
		second, formatted_twice := run_vfmt_write('semantic_signature_twice', formatted, '')
		assert second.exit_code == 0, second.output
		assert formatted_twice == formatted
	}
	for suffix, prefix in {
		'c':  'JS'
		'js': 'C'
	} {
		res, formatted := run_vfmt_write('interop_placement.${suffix}', 'fn ${prefix}.alert()\n', '')
		assert res.exit_code == 0, res.output
		assert formatted.contains('fn ${prefix}.alert()'), formatted
		second, formatted_twice := run_vfmt_write('interop_placement_twice.${suffix}', formatted, '')
		assert second.exit_code == 0, second.output
		assert formatted_twice == formatted
	}
}

fn test_fmt_preserves_dynamic_query_data_blocks() {
	for options in ['', '-no-migrate-json2'] {
		for prefix in ['', 'sql '] {
			block := '${prefix}{
		name == req_name,
		if req_status == 1 {
			status == req_status
		},
		// Keep commas and conditions inside the query-data block.
		email == req_email
	}'
			source := 'fn main() {
	up_expr := ${block}
	_ := up_expr
}
'
			res, formatted := run_vfmt_write('dynamic_query_data', source, options)
			assert res.exit_code == 0, res.output
			assert formatted.contains('up_expr := ${block}'), formatted
			second, twice := run_vfmt_write('dynamic_query_data_twice', formatted, options)
			assert second.exit_code == 0, second.output
			assert twice == formatted
		}
		res, formatted := run_vfmt_write('empty_query_data', 'fn main() { up_expr := sql { }; _ := up_expr }\n', options)
		assert res.exit_code == 0, res.output
		assert formatted.contains('up_expr := sql { }'), formatted
		second, twice := run_vfmt_write('empty_query_data_twice', formatted, options)
		assert second.exit_code == 0, second.output
		assert twice == formatted
	}
}

fn test_fmt_preserves_sql_identifiers_in_control_headers_and_branches() {
	for source in [
		'fn f(sql bool) { if sql {}; for sql {}; match sql { else {} } }\n',
		'fn f(value int, sql int) { match value { sql {} else {} } }\n',
	] {
		res, formatted := run_vfmt_write('sql_identifier', source, '')
		assert res.exit_code == 0, res.output
		second, twice := run_vfmt_write('sql_identifier_twice', formatted, '')
		assert second.exit_code == 0, second.output
		assert twice == formatted
		for fragment in ['if sql', 'for sql', 'match sql'] {
			if source.contains(fragment) {
				assert formatted.contains(fragment), formatted
			}
		}
	}
}

fn test_fmt_preserves_deep_assignment_expressions() {
	mut expression := '1'
	for _ in 0 .. 101 {
		expression = '1 + (${expression})'
	}
	source := 'fn main() { value := ${expression}; _ = value }\n'
	res, formatted := run_vfmt_write('deep_assignment', source, '')
	assert res.exit_code == 0, res.output
	assert formatted.contains('value := ${expression}'), formatted
	second, twice := run_vfmt_write('deep_assignment_twice', formatted, '')
	assert second.exit_code == 0, second.output
	assert twice == formatted
}

fn test_fmt_preserves_bare_channel_types() {
	for source, expected in {
		'fn f(ch chan) {}\n':                   'fn f(ch chan)'
		'fn main() { ch := chan{}; _ = ch }\n': 'ch := chan{}'
		'struct Holder { ch chan }\n':          'ch chan'
		'fn f() chan { return chan{} }\n':      'fn f() chan'
	} {
		res, formatted := run_vfmt_write('bare_channel', source, '')
		assert res.exit_code == 0, res.output
		assert formatted.contains(expected), formatted
		second, twice := run_vfmt_write('bare_channel_twice', formatted, '')
		assert second.exit_code == 0, second.output
		assert twice == formatted
	}
}

fn test_fmt_preserves_interop_qualifiers_on_receiver_declarations() {
	for prefix, suffix in {
		'C':  'c'
		'JS': 'js'
	} {
		for method in ['foo', '@select', 'nested.foo'] {
			for body in ['', ' {}'] {
				signature := 'fn (value Example) ${prefix}.${method}()${body}'
				source := 'struct Example {}\n\n${signature}\n'
				res, formatted := run_vfmt_write('interop_receiver.${suffix}', source, '')
				assert res.exit_code == 0, res.output
				assert formatted.contains(signature), formatted
				second, formatted_twice := run_vfmt_write('interop_receiver_twice.${suffix}', formatted, '')
				assert second.exit_code == 0, second.output
				assert formatted_twice == formatted
			}
		}
	}
}

fn test_fmt_preserves_qualified_receiver_and_inclusive_range_syntax() {
	for source, expected in {
		'struct Example {}\nfn (value Example) Foo.bar() {}':  'fn (value Example) Foo.bar() {}'
		'struct Example {}\nfn (value Example) Foo.@select()': 'fn (value Example) Foo.@select()'
		'fn main() { for i in 0 ... 3 { println(i) } }':       'for i in 0 ... 3'
	} {
		res, formatted := run_vfmt_write('qualified_method_range', source + '\n', '')
		assert res.exit_code == 0, res.output
		assert formatted.contains(expected), formatted
		second, twice := run_vfmt_write('qualified_method_range_twice', formatted, '')
		assert second.exit_code == 0, second.output
		assert twice == formatted
	}
}

fn test_fmt_preserves_semantic_signature_and_collection_restrictions() {
	for source, expected in {
		'fn f(value array) {}':                               'value array'
		'fn f(value map) {}':                                 'value map'
		'fn main() { _ := [2]map{} }':                        '[2]map{}'
		'fn main() { asm amd64 raw raw { nop } }':            'asm amd64 raw raw { nop }'
		'fn main() { asm amd64 intel intel { nop } }':        'asm amd64 intel intel { nop }'
		'fn main() { asm amd64 { lock nop } }':               'asm amd64 { lock nop }'
		'struct Holder { value mut int }':                    'value mut int'
		'fn f() mut int { return 1 }':                        'fn f() mut int'
		'__global int int':                                   '__global int int'
		'fn main() { mut _ := 1 }':                           'mut _ := 1'
		'fn main() { shared _ := 1 }':                        'shared _ := 1'
		'fn main() { atomic _ := 1 }':                        'atomic _ := 1'
		'fn main() { mut x := 1; x = 2 @[freed; other] }':    '@[freed; other]'
		'fn main() { mut x := 1; x = 2 @[freed: 1] }':        '@[freed: 1]'
		'fn main() { println($res()) }':                      'println($res())'
		'struct Holder { values &[]int }':                    'values &[]int'
		'fn + (a int, b int) int { return a + b }':           'fn +('
		"@[export: 'f'] fn f[T](value T) {}":                 "@[export: 'f']"
		"@[export: 'f'] fn C.f()":                            'fn C.f()'
		'fn C.foo() { println(1) }':                          'fn C.foo() { println(1) }'
		'fn JS.foo() { println(1) }':                         'fn JS.foo() { println(1) }'
		'fn f(mut values ...int) {}':                         'mut values ...int'
		'fn f(shared values ...int) {}':                      'shared values ...int'
		'fn f(atomic values ...int) {}':                      'atomic values ...int'
		'type Callback = fn (mut ...int)':                    'fn (mut ...int)'
		'fn main() { values := []!int{}; _ = values }':       '[]!int{}'
		'fn main() { values := [2]!int{}; _ = values }':      '[2]!int{}'
		'fn main() { values := [..]!int[1, 2]; _ = values }': '[..]!int[1, 2]'
		'fn main() { values := chan !int{}; _ = values }':    'chan !int{}'
		'fn main() { values := [2]int{len: 2}; _ = values }': '[2]int{len: 2}'
	} {
		res, formatted := run_vfmt_write('semantic_types', source + '\n', '')
		assert res.exit_code == 0, res.output
		assert formatted.contains(expected), formatted
		second, formatted_twice := run_vfmt_write('semantic_types_twice', formatted, '')
		assert second.exit_code == 0, second.output
		assert formatted_twice == formatted
	}
}

fn test_fmt_uses_v3_formatter() {
	source_path := os.join_path(vfmt_test_tdir, 'v3_formatter.v')
	os.write_file(source_path, 'fn main(){println("v3")}\n')!

	res := os.exec([vexe, 'fmt', '-verbose', source_path])

	assert res.exit_code == 0, res.output
	assert res.output.contains('vfmt running v.gen.v over file:'), res.output
	assert res.output.contains("fn main() { println('v3') }"), res.output
}

fn test_fmt_checks_accept_legacy_formatted_source() {
	source_path := os.join_path(vfmt_test_tdir, 'legacy_formatter.v')
	source := '// Header\n\nmodule main\n'
	os.write_file(source_path, source)!

	format_res := os.exec([vexe, 'fmt', source_path])
	assert format_res.exit_code == 0, format_res.output
	assert format_res.output == source, format_res.output

	for check_args in ['-verify -inprocess', '-verify', '-c'] {
		res :=
			os.exec([vexe, 'fmt', ...(os.split_args(check_args) or { panic(err) }), source_path])
		assert res.exit_code == 0, '${check_args}: ${res.output}'
		assert os.read_file(source_path)! == source
	}
}

fn test_fmt_checks_accept_legacy_source_when_v3_parsing_fails() {
	source_path := os.join_path(vfmt_test_tdir, 'legacy_only_parser.v')
	source := 'fn render(path string) string {\n\treturn \$tmpl(path)\n}\n'
	os.write_file(source_path, source)!

	for check_args in ['-verify -inprocess', '-verify', '-c'] {
		res :=
			os.exec([vexe, 'fmt', ...(os.split_args(check_args) or { panic(err) }), source_path])
		assert res.exit_code == 0, '${check_args}: ${res.output}'
		assert os.read_file(source_path)! == source
	}
}

fn test_fmt_checks_continue_after_legacy_parse_errors() {
	v3_path := os.join_path(vfmt_test_tdir, 'v3_only_unformatted.v')
	later_path := os.join_path(vfmt_test_tdir, 'later_unformatted.v')
	os.write_file(v3_path, 'struct Owned implements IClone{\n\tvalue string\n}\n')!
	os.write_file(later_path, 'fn later(){println(1)}\n')!
	files := [v3_path, later_path]

	check_res := os.exec([vexe, 'fmt', '-c', ...files])
	assert check_res.exit_code == 2, check_res.output
	assert check_res.output.contains('v3_only_unformatted.v'), check_res.output
	assert check_res.output.contains('later_unformatted.v'), check_res.output

	noerror_res := os.exec([vexe, 'fmt', '-c', '-noerror', ...files])
	assert noerror_res.exit_code == 0, noerror_res.output
	assert noerror_res.output.contains('v3_only_unformatted.v'), noerror_res.output
	assert noerror_res.output.contains('later_unformatted.v'), noerror_res.output
}

fn test_fmt_debug_reports_v3_node_kinds() {
	source_path := os.join_path(vfmt_test_tdir, 'v3_formatter_debug.v')
	os.write_file(source_path, 'fn main() { println(1) }\n')!

	res := os.exec([vexe, 'fmt', '-debug', source_path])

	assert res.exit_code == 0, res.output
	assert res.output.contains('stmt fn_decl'), res.output
	assert res.output.contains('expr call'), res.output
}

fn run_vfmt_write(name string, source string, extra_args string) (os.Result, string) {
	source_path := os.join_path(vfmt_test_tdir, '${name}.v')
	os.write_file(source_path, source) or { panic(err) }
	res :=
		os.exec([vexe, 'fmt', '-w', '-verbose', ...(os.split_args(extra_args) or { panic(err) }),
			source_path])
	formatted := os.read_file(source_path) or { panic(err) }
	return res, formatted
}

fn test_fmt_preserves_comments_with_v3() {
	source := '// vfmt off\nfn main(){println("keep this") }\n// vfmt on\nfn format_me(){println("yes")}\n'
	res, formatted := run_vfmt_write('comments', source, '')

	assert res.exit_code == 0, res.output
	assert res.output.contains('vfmt running v.gen.v over file:'), res.output
	assert formatted.contains('// vfmt off')
	assert formatted.contains('// vfmt on')
	assert formatted.contains('println("keep this")')
	assert formatted.contains("fn format_me() { println('yes') }"), formatted
}

fn test_fmt_keeps_regular_comments_attached_with_v3() {
	source := '// docs\nfn main(){\n\tx := 1 // inline\n\t_ = x\n}\n'
	res, formatted := run_vfmt_write('regular_comments', source, '')

	assert res.exit_code == 0, res.output
	assert res.output.contains('vfmt running v.gen.v over file:'), res.output
	assert formatted.starts_with('// docs\nfn main() {')
	assert formatted.contains('x := 1 // inline')
}

fn test_fmt_keeps_comments_inside_construct_boundaries_with_v3() {
	source := 'struct Foo {
	value int
}

fn comments(base Foo, condition bool) {
	updated := Foo{
		// before
		...base // after
	}
	empty := Foo{
		// inside struct init
	}
	values := [
		// inside array
	]
	if condition {
		// inside branch
	} else {
		// inside else branch
	}
	_ = updated
	_ = empty
	_ = values
}

fn only_comments() {
	// abc
}
'
	res, formatted := run_vfmt_write('construct_comment_boundaries', source, '')

	assert res.exit_code == 0, res.output
	assert res.output.contains('vfmt running v.gen.v over file:'), res.output
	assert formatted == source, formatted
	second_res, formatted_twice :=
		run_vfmt_write('construct_comment_boundaries_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_preserves_compact_function_and_expression_bodies_with_v3() {
	source := 'fn empty() {}\n\nfn comment_only() {\n\t// keep inside\n}\n\nfn compact_expressions() {\n\t_ := if true { 1 } else { 2 }\n\t_ := match 10 {\n\t\t10 { 10 }\n\t\t5 {}\n\t\telse { 2 }\n\t}\n\tmatch 1 {\n\t\telse {\n\t\t\t// keep inside\n\t\t}\n\t}\n}\n'
	res, formatted := run_vfmt_write('compact_bodies', source, '')

	assert res.exit_code == 0, res.output
	assert formatted == source, formatted
	second_res, formatted_twice := run_vfmt_write('compact_bodies_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_keeps_trailing_array_comments_inside_literal_with_v3() {
	source := 'fn array_comments() {\n\t_ := [\n\t\t// before\n\t\t6,\n\t\t// after\n\t]\n\t_ := [\n\t\t7, // inline after\n\t]\n}\n'
	res, formatted := run_vfmt_write('trailing_array_comments', source, '')

	assert res.exit_code == 0, res.output
	assert formatted == source, formatted
	second_res, formatted_twice := run_vfmt_write('trailing_array_comments_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_keeps_trailing_block_and_struct_update_comments_inside_with_v3() {
	source := "struct Item {\n\tvalue int\n}\n\nfn comment_boundaries() {\n\t{\n\t\tprintln('first')\n\t\t// trailing block\n\t}\n\t{\n\t\tprintln('second')\n\n\t\t// trailing after blank\n\t}\n\titem := Item{}\n\t_ := Item{\n\t\t...item // inline spread\n\t\t// trailing spread\n\t}\n\t_ := Item{\n\t\t...item\n\t\tvalue: 1\n\t\t// trailing field\n\t}\n}\n"
	res, formatted := run_vfmt_write('trailing_block_update_comments', source, '')

	assert res.exit_code == 0, res.output
	assert formatted == source, formatted
	second_res, formatted_twice := run_vfmt_write('trailing_block_update_comments_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_preserves_unsafe_and_defer_source_layout_with_v3() {
	source := 'fn foo() {}\n\nfn block_layouts() {\n\tunsafe { 6 }\n\tunsafe {}\n\tunsafe {\n\t}\n\tx := unsafe {\n\t\t5\n\t}\n\ty := unsafe { 7 }\n\tdefer {}\n\tdefer { foo() }\n\tdefer {\n\t\tfoo()\n\t}\n\t_ = x\n\t_ = y\n}\n'
	res, formatted := run_vfmt_write('unsafe_defer_layout', source, '')

	assert res.exit_code == 0, res.output
	assert formatted == source, formatted
	second_res, formatted_twice := run_vfmt_write('unsafe_defer_layout_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_preserves_compact_empty_literals_and_declarations_with_v3() {
	source := 'interface Compact {}\n\nstruct Between {}\n\ninterface Expanded {\n}\n\nenum CompactEnum {}\n\nstruct Between2 {}\n\nenum ExpandedEnum {\n}\n\nfn literal_layouts() {\n\tcompact := fn (_s string) {}\n\texpanded := fn (_s string) {\n\t}\n\t_ = compact\n\t_ = expanded\n}\n'
	res, formatted := run_vfmt_write('compact_empty_literals_declarations', source, '')

	assert res.exit_code == 0, res.output
	assert formatted == source, formatted
	second_res, formatted_twice := run_vfmt_write('compact_empty_literals_declarations_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_preserves_loop_labels_debugger_and_enum_groups_with_v3() {
	source := 'enum Grouped {\n\taa = 1\n\tbbb\n\n\tcccc  = 5\n\tddddd = 10\n\n\t// final group\n\tee  = 20\n\tfff = 30\n}\n\nfn labelled_debugger() {\n\tL1: for {\n\t\t\$dbg;\n\t\tbreak L1\n\t}\n}\n'
	res, formatted := run_vfmt_write('loop_label_debugger_enum_groups', source, '')

	assert res.exit_code == 0, res.output
	assert formatted == source, formatted
	second_res, formatted_twice := run_vfmt_write('loop_label_debugger_enum_groups_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_preserves_or_block_layout_and_lock_comments_with_v3() {
	source := "fn block_boundaries() {\n\tempty_or_block() or {}\n\tempty_or_block() or {\n\t}\n\tfn_with_option() or { return }\n\tfn_with_option() or {\n\t\treturn\n\t}\n\tlock value {\n\t\tprintln('inside')\n\t\t// trailing lock\n\t}\n\tlock value {\n\t\t// comment only\n\t}\n}\n"
	res, formatted := run_vfmt_write('or_layout_lock_comments', source, '')

	assert res.exit_code == 0, res.output
	assert formatted == source, formatted
	second_res, formatted_twice := run_vfmt_write('or_layout_lock_comments_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_keeps_trailing_loop_comments_inside_body_with_v3() {
	source := "fn loop_comments(items []int) {\n\tfor {\n\t\tprintln('regular')\n\t\t// trailing regular\n\t}\n\tfor {\n\t\t// comment-only regular\n\t}\n\tfor i := 0; i < 1; i++ {\n\t\tprintln(i)\n\t\t// trailing C-style\n\t}\n\tfor item in items {\n\t\tprintln(item)\n\t\t// trailing for-in\n\t}\n\tfor _ in items {\n\t\t// comment-only for-in\n\t}\n}\n"
	res, formatted := run_vfmt_write('trailing_loop_comments', source, '')

	assert res.exit_code == 0, res.output
	assert formatted == source, formatted
	second_res, formatted_twice := run_vfmt_write('trailing_loop_comments_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_keeps_trailing_comptime_for_comments_inside_body_with_v3() {
	source := 'fn comptime_loop_comments[T]() {\n\t\$for field in T.fields {\n\t\tprintln(field.name)\n\t\t// trailing comptime loop\n\t}\n\t\$for method in T.methods {\n\t\t// comment-only comptime loop\n\t}\n}\n'
	res, formatted := run_vfmt_write('trailing_comptime_for_comments', source, '')

	assert res.exit_code == 0, res.output
	assert formatted == source, formatted
	second_res, formatted_twice := run_vfmt_write('trailing_comptime_for_comments_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_preserves_aggregate_member_blank_lines_with_v3() {
	source := 'struct Grouped {\n\ta int\n\n\tbb string\n\tcc bool\n}\n\ninterface Contract {\n\ta int\n\n\tbb string\n\n\tfirst()\n\tsecond()\n}\n'
	res, formatted := run_vfmt_write('aggregate_member_blank_lines', source, '')

	assert res.exit_code == 0, res.output
	assert formatted == source, formatted
	second_res, formatted_twice :=
		run_vfmt_write('aggregate_member_blank_lines_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_keeps_blank_lines_between_consecutive_enums_with_v3() {
	source := 'enum First {\n\tone\n}\n\nenum Second {\n\ttwo\n}\n'
	res, formatted := run_vfmt_write('consecutive_enum_blank_lines', source, '')

	assert res.exit_code == 0, res.output
	assert formatted == source, formatted
	second_res, formatted_twice :=
		run_vfmt_write('consecutive_enum_blank_lines_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_keeps_trailing_positional_struct_init_comments_inside_with_v3() {
	source := 'struct Pair {\n\tfirst  int\n\tsecond int\n}\n\nfn positional() {\n\t_ := Pair{\n\t\t1,\n\t\t2,\n\t\t// trailing positional\n\t}\n}\n'
	expected := 'struct Pair {\n\tfirst  int\n\tsecond int\n}\n\nfn positional() {\n\t_ := Pair{1, 2,\n\t\t// trailing positional\n\t}\n}\n'
	res, formatted := run_vfmt_write('trailing_positional_struct_init_comment', source, '')

	assert res.exit_code == 0, res.output
	assert formatted == expected, formatted
	second_res, formatted_twice := run_vfmt_write('trailing_positional_struct_init_comment_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_preserves_compact_struct_updates_with_v3() {
	source := 'struct Position {\n\tpos int\n\tlen int\n}\n\nfn compact(field Position, name_len int) {\n\t_ := Position{ ...field }\n\t_ := Position{ ...field, len: name_len }\n}\n'
	res, formatted := run_vfmt_write('compact_struct_updates', source, '')

	assert res.exit_code == 0, res.output
	assert formatted == source, formatted
	second_res, formatted_twice := run_vfmt_write('compact_struct_updates_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_expands_grouped_consts_and_keeps_trailing_global_comments_inside_with_v3() {
	source := 'const (\n\t// first docs\n\tfirst = 1\n\tsecond = 2\n)\n\npub const (\n\tthird = 3\n)\n\n__global (\n\tvalue = 4\n\t// trailing global\n)\n'
	expected := '// first docs\nconst first = 1\nconst second = 2\n\npub const third = 3\n\n__global (\n\tvalue = 4\n\t// trailing global\n)\n'
	res, formatted := run_vfmt_write('grouped_consts_trailing_global_comment', source, '')

	assert res.exit_code == 0, res.output
	assert formatted == expected, formatted
	second_res, formatted_twice := run_vfmt_write('grouped_consts_trailing_global_comment_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_keeps_trailing_array_initializer_comments_inside_with_v3() {
	source := 'fn f() {\n\ta := []int{len: 1\n\t\t/* trailing initializer */\n\t}\n\t_ = a\n}\n'
	res, formatted := run_vfmt_write('trailing_array_initializer_comment', source, '')

	assert res.exit_code == 0, res.output
	assert formatted == source, formatted
	second_res, formatted_twice := run_vfmt_write('trailing_array_initializer_comment_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_keeps_singleton_grouped_const_comments_before_declaration_with_v3() {
	source := 'const (\n\t// only docs\n\tonly = 1\n)\n'
	expected := '// only docs\nconst only = 1\n'
	res, formatted := run_vfmt_write('singleton_grouped_const_comment', source, '')

	assert res.exit_code == 0, res.output
	assert formatted == expected, formatted
	second_res, formatted_twice := run_vfmt_write('singleton_grouped_const_comment_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_preserves_declaration_attribute_groups_with_v3() {
	source := "@[deprecated: 'use bar() instead']\n@[foo: bar]\n@[if debug; inline]\nfn keep_attributes() {}\n\n@[deprecated(msg: 'use foo_v2() instead', after: '2026-06-01')]\n@[inline]\nfn call_syntax() {}\n\n@[inline]\n@[export: 'symbol']\n@[unsafe]\n@[tom: 'jerry']\nfn normalized() {}\n"
	expected := "@[deprecated: 'use bar() instead']\n@[foo: bar]\n@[if debug; inline]\nfn keep_attributes() {}\n\n@[deprecated(msg: 'use foo_v2() instead', after: '2026-06-01')]\n@[inline]\nfn call_syntax() {}\n\n@[export: 'symbol']\n@[tom: 'jerry']\n@[inline; unsafe]\nfn normalized() {}\n"
	res, formatted := run_vfmt_write('declaration_attribute_groups', source, '')

	assert res.exit_code == 0, res.output
	assert formatted == expected, formatted
	second_res, formatted_twice :=
		run_vfmt_write('declaration_attribute_groups_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_keeps_comptime_branch_and_selective_import_comments_inside_with_v3() {
	source := "import sample {\n\tOne,\n\tTwo,\n\t// trailing import\n}\n\n\$if linux {\n\tprintln('first')\n\t// trailing first\n} \$else \$if windows {\n\t// comment-only second\n} \$else {\n\tprintln('last')\n\t// trailing last\n}\n"
	res, formatted := run_vfmt_write('comptime_branch_selective_import_comments', source, '')

	assert res.exit_code == 0, res.output
	assert formatted == source, formatted
	second_res, formatted_twice := run_vfmt_write('comptime_branch_selective_import_comments_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_emits_comments_before_struct_init_field_names_with_v3() {
	source := 'struct Config {
	one int
	two int
}

fn config() Config {
	return Config{
		// before one
		one: 1 // after one

		// before two
		two: 2 // after two
	}
}
'
	res, formatted := run_vfmt_write('struct_init_field_comments', source, '')

	assert res.exit_code == 0, res.output
	assert formatted == source, formatted
	second_res, formatted_twice := run_vfmt_write('struct_init_field_comments_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_preserves_three_value_if_guard_bindings_with_v3() {
	source := "fn create() ?(int, string, bool) {
	return 5, 'value', true
}

fn check() {
	if r1, r2, r3 := create() {
		_ = r1
		_ = r2
		_ = r3
	}
}
"
	res, formatted := run_vfmt_write('three_value_if_guard', source, '')

	assert res.exit_code == 0, res.output
	assert formatted == source, formatted
}

fn test_fmt_retains_expanded_call_argument_layout_with_v3() {
	source := 'fn many(a int, b int, c int) int {
	return a + b + c
}

fn configure(config Config) {
}

fn calls() {
	x := many(
		1,
		// before two
		2, 3, // after three
	)
	configure(
		// before one
		one: 1 // after one
		two: 2
	)
	_ = x
}
'
	res, formatted := run_vfmt_write('expanded_call_arguments', source, '')

	assert res.exit_code == 0, res.output
	assert formatted == source, formatted
	second_res, formatted_twice := run_vfmt_write('expanded_call_arguments_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_expands_long_single_line_named_call_arguments_with_v3() {
	source := "fn calls() {\n\tbar_func(x: 'a very long content should cause vfmt to use multiple lines instead of one.', y: 123456789)\n}\n"
	expected := "fn calls() {\n\tbar_func(\n\t\tx: 'a very long content should cause vfmt to use multiple lines instead of one.'\n\t\ty: 123456789\n\t)\n}\n"
	res, formatted := run_vfmt_write('long_single_line_named_call_arguments', source, '')

	assert res.exit_code == 0, res.output
	assert formatted == expected, formatted
	second_res, formatted_twice := run_vfmt_write('long_single_line_named_call_arguments_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_removes_redundant_parentheses_with_v3() {
	source := 'fn predicate(char int) bool {\n\treturn (char >= 65 && char <= 90)\n}\n\nfn checks() {\n\tx := 3\n\t_ := &(((x)))\n\t_, _ := (((22 > 11))), (43 > 22)\n\t_ := ((10 + 11))\n\t_ := (cond1 && cond2) || (single_ident)\n\t_ := (\n\t\t// keep grouping\n\t\tx\n\t)\n\tassert (((((1 + 2) == 3))))\n\tassert (((true)))\n}\n'
	expected := 'fn predicate(char int) bool {\n\treturn char >= 65 && char <= 90\n}\n\nfn checks() {\n\tx := 3\n\t_ := &x\n\t_, _ := (22 > 11), (43 > 22)\n\t_ := (10 + 11)\n\t_ := (cond1 && cond2) || single_ident\n\t_ := (\n\t\t// keep grouping\n\t\tx\n\t)\n\tassert (1 + 2) == 3\n\tassert true\n}\n'
	res, formatted := run_vfmt_write('redundant_parentheses', source, '')

	assert res.exit_code == 0, res.output
	assert formatted == expected, formatted
	second_res, formatted_twice := run_vfmt_write('redundant_parentheses_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_emits_hash_directive_attributes_with_v3() {
	source := '@[use_once] #include "header.h"
@[custom_tag; use_once] #flag -I @VMODROOT/c
'
	expected := '@[use_once]
#include "header.h"

@[custom_tag; use_once]
#flag -I @VMODROOT/c
'
	res, formatted := run_vfmt_write('hash_directive_attributes', source, '')

	assert res.exit_code == 0, res.output
	assert formatted == expected, formatted
	second_res, formatted_twice := run_vfmt_write('hash_directive_attributes_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_preserves_boolean_compound_assignment_spelling_with_v3() {
	source := 'fn update() {
	mut flag := true
	flag ||= false
	flag &&= true
	mut flags := [true]
	flags[0] ||= false
	flags[0] &&= true
	mut bits := 1
	bits |= 2
	bits &= 1
}
'
	res, formatted := run_vfmt_write('boolean_compound_assignments', source, '')

	assert res.exit_code == 0, res.output
	assert formatted == source, formatted
	second_res, formatted_twice :=
		run_vfmt_write('boolean_compound_assignments_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_preserves_comment_only_files_with_v3() {
	source := '/*\nmodule acommentedmodule\n*/\n'
	res, formatted := run_vfmt_write('comment_only_file', source, '')

	assert res.exit_code == 0, res.output
	assert formatted == source, formatted
	second_res, formatted_twice := run_vfmt_write('comment_only_file_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_preserves_atomic_parameter_qualifiers_with_v3() {
	source := 'fn update(atomic value u64) {\n\t_ = value\n}\n'
	res, formatted := run_vfmt_write('atomic_parameter_qualifier', source, '')

	assert res.exit_code == 0, res.output
	assert formatted == source, formatted
	second_res, formatted_twice := run_vfmt_write('atomic_parameter_qualifier_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_preserves_comptime_if_with_v3() {
	source := "fn main(){\n\t\$if windows {\n\t\tprintln('windows')\n\t} \$else {\n\t\tprintln('other')\n\t}\n}\n"
	res, formatted := run_vfmt_write('comptime_if', source, '')

	assert res.exit_code == 0, res.output
	assert res.output.contains('vfmt running v.gen.v over file:'), res.output
	assert formatted.contains('$if windows')
	assert formatted.contains("println('windows')")
	assert formatted.contains("println('other')")
}

fn test_fmt_preserves_comptime_calls_and_pseudo_variables_with_v3() {
	source := "fn main(){\n\ta := \$embed_file('./missing.txt')\n\tb := \$env('VFMT_SECRET')\n\tc := \$tmpl('./missing.html')\n\tprintln(@FILE)\n\tprintln(@LINE)\n\tprintln(@VEXE)\n\t_ = a\n\t_ = b\n\t_ = c\n}\n"
	res, formatted := run_vfmt_write('comptime_calls', source, '')

	assert res.exit_code == 0, res.output
	assert formatted.contains("\$embed_file('./missing.txt')"), formatted
	assert formatted.contains("\$env('VFMT_SECRET')"), formatted
	assert formatted.contains("\$tmpl('./missing.html')"), formatted
	assert formatted.contains('println(@FILE)'), formatted
	assert formatted.contains('println(@LINE)'), formatted
	assert formatted.contains('println(@VEXE)'), formatted
}

fn test_fmt_preserves_comptime_method_shorthand_with_v3() {
	source := 'struct Dummy {}\n\nfn (d Dummy) sample(x int) int {\n\treturn x + 1\n}\n\nfn main() {\n\t\$for method in Dummy.methods {\n\t\tDummy{}.\$method(1)\n\t}\n}\n'
	res, formatted := run_vfmt_write('comptime_method_shorthand', source, '')

	assert res.exit_code == 0, res.output
	assert formatted.contains('Dummy{}.\$method(1)'), formatted
	assert !formatted.contains('Dummy{}.\$(method)(1)'), formatted
	second_res, formatted_twice := run_vfmt_write('comptime_method_shorthand_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_preserves_conditional_attributes_with_v3() {
	source := "@[if formatter_missing_flag ?]\nfn guarded(){\n\tprintln('kept')\n}\n"
	res, formatted := run_vfmt_write('conditional_attribute', source, '')

	assert res.exit_code == 0, res.output
	assert formatted.contains('@[if formatter_missing_flag ?]'), formatted
	assert formatted.contains("println('kept')"), formatted
}

fn test_fmt_preserves_volatile_fields_with_v3() {
	source := 'struct Counter {\n\tvolatile value u64\n}\n\nfn main() {\n\tmut volatile counter := u64(0)\n\t_ = counter\n}\n'
	res, formatted := run_vfmt_write('volatile_field', source, '')

	assert res.exit_code == 0, res.output
	assert formatted.contains('volatile value u64'), formatted
	assert formatted.contains('mut volatile counter := u64(0)'), formatted
	second_res, formatted_twice := run_vfmt_write('volatile_field_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_preserves_fixed_array_literal_prefixes_with_v3() {
	source := 'fn main() {\n\ta := [4]f32[1, 2, 3, 4]\n\tb := [..]f32[1, 2, 3, 4]\n\t_ = a\n\t_ = b\n}\n'
	res, formatted := run_vfmt_write('fixed_array_literal_prefixes', source, '')

	assert res.exit_code == 0, res.output
	assert formatted.contains('a := [4]f32[1, 2, 3, 4]'), formatted
	assert formatted.contains('b := [..]f32[1, 2, 3, 4]\n'), formatted
}

fn test_fmt_escapes_rune_literals_with_v3() {
	source := 'fn f() {\n\tprintln(`\\n`)\n\tprintln(`\\``)\n}\n'
	res, formatted := run_vfmt_write('rune_literal_escaping', source, '')

	assert res.exit_code == 0, res.output
	assert formatted == source, formatted
	second_res, formatted_twice := run_vfmt_write('rune_literal_escaping_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_preserves_and_wraps_array_layout_with_v3() {
	vertical := 'fn f() {
	values := [
		[1, 2],
		[3, 4],
	]
	_ = values
}
'
	vertical_res, vertical_formatted := run_vfmt_write('vertical_array_layout', vertical, '')
	assert vertical_res.exit_code == 0, vertical_res.output
	assert vertical_formatted == vertical, vertical_formatted

	wrapped_source := "const supported_platforms = ['windows', 'macos', 'linux', 'freebsd', 'openbsd', 'netbsd', 'dragonfly', 'android', 'js', 'solaris', 'haiku']\n"
	wrapped_res, wrapped := run_vfmt_write('wrapped_array_layout', wrapped_source, '')
	assert wrapped_res.exit_code == 0, wrapped_res.output
	assert wrapped.contains("'netbsd', 'dragonfly',\n\t'android', 'js'"), wrapped
	second_res, wrapped_twice := run_vfmt_write('wrapped_array_layout_twice', wrapped, '')
	assert second_res.exit_code == 0, second_res.output
	assert wrapped_twice == wrapped
}

fn test_fmt_preserves_multiline_map_layout_with_v3() {
	source := "numbers := {'one': 1, 'twentytwo': 22}\n"
	res, formatted := run_vfmt_write('multiline_map_layout', source, '')

	assert res.exit_code == 0, res.output
	assert formatted == "numbers := {\n\t'one':       1\n\t'twentytwo': 22\n}\n", formatted
	second_res, formatted_twice := run_vfmt_write('multiline_map_layout_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_preserves_closure_capture_qualifiers_with_v3() {
	source := 'fn consume[T](value T) {}\n\nfn main() {\n\tatomic counter := 0\n\tcallback := fn [mut value, atomic counter, shared state] () {}\n\tconsume[[]int]([]int{})\n\t_ = callback\n}\n'
	res, formatted := run_vfmt_write('closure_capture_qualifiers', source, '')

	assert res.exit_code == 0, res.output
	assert formatted.contains('atomic counter := 0'), formatted
	assert formatted.contains('fn [mut value, atomic counter, shared state] ()'), formatted
	assert formatted.contains('consume[[]int]([]int{})'), formatted
	second_res, formatted_twice := run_vfmt_write('closure_capture_qualifiers_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_places_shared_before_receiver_and_parameter_names_with_v3() {
	source := 'struct St {}\n\nfn (shared receiver St) use(shared value St) {}\n'
	res, formatted := run_vfmt_write('shared_receiver_and_parameter', source, '')

	assert res.exit_code == 0, res.output
	assert formatted.contains('fn (shared receiver St) use(shared value St)'), formatted
}

fn test_fmt_preserves_comptime_match_with_v3() {
	source := "\$match @OS {\n\t'linux' {\n\t\tconst platform = 'linux'\n\t}\n\t\$else {\n\t\tconst platform = 'other'\n\t}\n}\n\nfn main() {\n\tvalue := \$match @OS {\n\t\t'linux' { 'linux' }\n\t\t\$else { 'other' }\n\t}\n\t_ = value\n}\n"
	res, formatted := run_vfmt_write('comptime_match', source, '')

	assert res.exit_code == 0, res.output
	assert formatted.contains('\$match @OS'), formatted
	assert formatted.contains("const platform = 'linux'"), formatted
	assert formatted.contains("const platform = 'other'"), formatted
	assert formatted.count('\$match @OS') == 2, formatted
	second_res, formatted_twice := run_vfmt_write('comptime_match_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_preserves_inclusive_match_ranges_with_v3() {
	source := "fn classify(value int) string {\n\treturn match value {\n\t\t32...126 { 'printable' }\n\t\telse { 'other' }\n\t}\n}\n"
	res, formatted := run_vfmt_write('inclusive_match_range', source, '')

	assert res.exit_code == 0, res.output
	assert formatted.contains('32...126 {'), formatted
	assert !formatted.contains('32 .. 126'), formatted
	second_res, formatted_twice := run_vfmt_write('inclusive_match_range_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_preserves_lifetime_annotations_with_v3() {
	source := 'interface Reader {\n\tread[^a](value &^a string) &^a string\n}\n\nstruct Borrowed[^a, T] {\n\tvalue &^a T\n}\n\nfn Borrowed.new[^a, T](value &^a T) Borrowed[^a, T] {\n\treturn Borrowed[^a, T]{\n\t\tvalue: value\n\t}\n}\n\nfn (borrowed &^a Borrowed[^a, T]) get[^a]() &^a T {\n\treturn borrowed.value\n}\n'
	res, formatted := run_vfmt_write('lifetime_annotations', source, '')

	assert res.exit_code == 0, res.output
	assert formatted.contains('read[^a](value &^a string) &^a string'), formatted
	assert formatted.contains('struct Borrowed[^a, T]'), formatted
	assert formatted.contains('value &^a T'), formatted
	assert formatted.contains('fn Borrowed.new[^a, T](value &^a T) Borrowed[^a, T]'), formatted
	assert formatted.contains('return Borrowed[^a, T]{'), formatted
	assert formatted.contains('fn (borrowed &^a Borrowed[^a, T]) get[^a]() &^a T'), formatted
	second_res, formatted_twice := run_vfmt_write('lifetime_annotations_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_ignores_vfmt_directives_inside_strings_with_v3() {
	source := "fn main(){\n\toff := '// vfmt off'\n\ton := '// vfmt on'\n\tprintln(off + on)\n}\n\nfn format_me(){println('yes')}\n"
	res, formatted := run_vfmt_write('vfmt_directives_in_strings', source, '')

	assert res.exit_code == 0, res.output
	assert formatted.contains("off := '// vfmt off'"), formatted
	assert formatted.contains("on := '// vfmt on'"), formatted
	assert formatted.contains("fn format_me() { println('yes') }"), formatted
}

fn test_fmt_preserves_go_and_spawn_keywords_with_v3() {
	source := 'fn work() {}\n\nfn main() {\n\tgo work()\n\tspawn work()\n}\n'
	res, formatted := run_vfmt_write('go_and_spawn', source, '')

	assert res.exit_code == 0, res.output
	assert formatted.contains('go work()'), formatted
	assert formatted.contains('spawn work()'), formatted
}

fn test_fmt_preserves_legacy_dollar_builtins_with_v3() {
	source := "fn main() {\n\t// vfmt off\n\tn := 1\n\tassert \$typeof(n).name == 'int'\n\tassert \$sizeof(n) > 0\n\tassert !\$isreftype[int]()\n\tassert \$dump(n) == n\n\t// vfmt on\n}\n"
	res, formatted := run_vfmt_write('legacy_dollar_builtins', source, '')

	assert res.exit_code == 0, res.output
	assert formatted.contains('\$typeof(n).name'), formatted
	assert formatted.contains('\$sizeof(n)'), formatted
	assert formatted.contains('\$isreftype[int]()'), formatted
	assert formatted.contains('\$dump(n)'), formatted
	assert formatted.ends_with('\t// vfmt on\n}\n'), formatted
}

fn test_fmt_preserves_bodyless_function_prefixes_with_v3() {
	source := 'fn plain(value usize) usize\nfn C.c_call(value int) int\nfn JS.js_call(value int) int\n'
	res, formatted := run_vfmt_write('bodyless_function_prefixes', source, '')

	assert res.exit_code == 0, res.output
	assert formatted.contains('fn plain(value usize) usize'), formatted
	assert formatted.contains('fn C.c_call(value int) int'), formatted
	assert formatted.contains('fn JS.js_call(value int) int'), formatted
	assert !formatted.contains('fn C.plain'), formatted
}

fn test_fmt_preserves_mixed_lock_modes_with_v3() {
	source := 'struct St {}

fn f(shared a St, shared b St, shared c St) {
	rlock a; lock b; rlock c {
		println(1)
	}
}
'
	res, formatted := run_vfmt_write('mixed_lock_modes', source, '')

	assert res.exit_code == 0, res.output
	assert formatted.contains('lock b; rlock a, c {'), formatted
	second_res, formatted_twice := run_vfmt_write('mixed_lock_modes', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_avoids_json2_declaration_collisions_with_v3() {
	source := 'import json

const json2 = 1

fn f() {
	println(json.encode(1))
}
'
	res, formatted := run_vfmt_write('json2_declaration_collision', source, '')

	assert res.exit_code == 0, res.output
	assert formatted.contains('import json\n'), formatted
	assert formatted.contains('json.encode('), formatted
	assert !formatted.contains('import json2'), formatted

	fn_source := 'import json

fn json2() {}

fn f() {
	println(json.encode(1))
}
'
	fn_res, fn_formatted := run_vfmt_write('json2_function_collision', fn_source, '')
	assert fn_res.exit_code == 0, fn_res.output
	assert fn_formatted.contains('import json\n'), fn_formatted
	assert fn_formatted.contains('json.encode('), fn_formatted
	assert !fn_formatted.contains('import json2'), fn_formatted
}

fn test_fmt_avoids_json2_declaration_collisions_in_comptime_branches_with_v3() {
	declarations := {
		'const':    'const json2 = 1'
		'global':   '__global json2 = 1'
		'function': 'fn json2() {}'
	}
	for name, declaration in declarations {
		source := 'import json

\$if custom ? {
	${declaration}
}

fn f() {
	println(json.encode(1))
}
'
		res, formatted := run_vfmt_write('comptime_json2_${name}_collision', source, '')
		assert res.exit_code == 0, res.output
		assert formatted.contains('import json\n'), formatted
		assert formatted.contains('json.encode('), formatted
		assert !formatted.contains('import json2'), formatted
		second_res, formatted_twice := run_vfmt_write('comptime_json2_${name}_collision_twice', formatted, '')
		assert second_res.exit_code == 0, second_res.output
		assert formatted_twice == formatted
	}
}

fn test_fmt_skips_json2_migration_in_vfmt_disabled_regions_with_v3() {
	source := 'import json

fn f() {
	// vfmt off
	println(json.encode(1))
	// vfmt on
}
'
	res, formatted := run_vfmt_write('disabled_json_migration', source, '')

	assert res.exit_code == 0, res.output
	assert formatted.contains('import json\n'), formatted
	assert formatted.contains('json.encode(1)'), formatted
	assert !formatted.contains('import json2'), formatted
	assert !formatted.contains('json2.encode'), formatted
	second_res, formatted_twice := run_vfmt_write('disabled_json_migration_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_json_decode_migration_runs_and_skips_option_targets_with_v3() {
	// Migrated decode calls must still decode the same payloads.
	migrated_source := 'import json\n\nstruct Foo {\n\ta int\n}\n\nfn main() {\n\tlist := json.decode([]Foo, \'[{"a":1}]\') or { panic(err) }\n\tby_key := json.decode(map[string]Foo, \'{"k":{"a":2}}\') or { panic(err) }\n\tprintln(\'\${list[0].a} \${by_key[\'k\'].a}\')\n}\n'
	res, migrated := run_vfmt_write('json_decode_targets', migrated_source, '')
	assert res.exit_code == 0, res.output
	assert migrated.contains('json2.decode[[]Foo]('), migrated
	assert migrated.contains('json2.decode[map[string]Foo]('), migrated
	run_res := os.exec([vexe, 'run', os.join_path(vfmt_test_tdir, 'json_decode_targets.v')])
	assert run_res.exit_code == 0, run_res.output
	assert run_res.output.trim_space() == '1 2', run_res.output
	// V does not accept `?Foo` as a type argument, so `json.decode(?Foo, s)` has no
	// json2 counterpart, and the file keeps its legacy source.
	option_source := "import json\n\nstruct Foo {\n\ta int\n}\n\nfn main() {\n\tw := json.decode(?Foo, '{}') or { return }\n\tprintln(json.encode(w))\n}\n"
	option_res, option_formatted := run_vfmt_write('json_decode_option_target', option_source,
		'')
	assert option_res.exit_code == 0, option_res.output
	assert option_formatted == option_source
	// Without migration, formatting keeps the option type argument too.
	plain_res, plain_formatted := run_vfmt_write('json_decode_option_target_no_migrate',
		option_source, '-no-migrate-json2')
	assert plain_res.exit_code == 0, plain_res.output
	assert plain_formatted == option_source
}

fn test_fmt_json_decode_migration_of_value_targets_with_v3() {
	// A value target names its type with an empty initializer, which is dropped.
	source := 'import json\n\nstruct Foo {\n\ta int\n}\n\nfn main() {\n\tm := json.decode(map[string]int{}, \'{"a":1}\') or { return }\n\tl := json.decode([]Foo{}, \'[{"a":2}]\') or { return }\n\tf := json.decode(Foo{}, \'{"a":3}\') or { return }\n\tprintln(\'\${m[\'a\']} \${l[0].a} \${f.a}\')\n}\n'
	res, migrated := run_vfmt_write('json_decode_value_targets', source, '')
	assert res.exit_code == 0, res.output
	assert migrated.contains('json2.decode[map[string]int]('), migrated
	assert migrated.contains('json2.decode[[]Foo]('), migrated
	assert migrated.contains('json2.decode[Foo]('), migrated
	run_res := os.exec([vexe, 'run', os.join_path(vfmt_test_tdir, 'json_decode_value_targets.v')])
	assert run_res.exit_code == 0, run_res.output
	assert run_res.output.trim_space() == '1 2 3', run_res.output
	// A target with a non-empty initializer names no type to spell back.
	initialized := "import json\n\nfn main() {\n\tl := json.decode([]int{len: 2}, '[]') or { return }\n\tprintln(l)\n}\n"
	init_res, init_formatted := run_vfmt_write('json_decode_initialized_target', initialized,
		'')
	assert init_res.exit_code == 0, init_res.output
	assert init_formatted == initialized
}

fn test_fmt_skips_json2_migration_for_narrowed_sumtype_values_with_v3() {
	header := 'import json\n\nstruct Cat {\n\tname string\n}\n\nstruct Dog {\n\tname string\n}\n\ntype Animal = Cat | Dog\n\n'
	for name, body in {
		'if_is':       'fn f(x Animal) {\n\tif x is Cat {\n\t\tprintln(json.encode(x))\n\t}\n}\n'
		'assert_is':   'fn f(animals []Animal) {\n\tassert animals[0] is Cat\n\tprintln(json.encode(animals[0]))\n}\n'
		'match_arm':   'fn f(x Animal) {\n\tmatch x {\n\t\tCat { println(json.encode(x)) }\n\t\telse {}\n\t}\n}\n'
		'not_is_exit': 'fn f(x Animal) {\n\tif x !is Cat {\n\t\treturn\n\t}\n\tprintln(json.encode(x))\n}\n'
	} {
		source := header + body
		res, formatted := run_vfmt_write('narrowed_${name}', source, '')
		assert res.exit_code == 0, res.output
		assert formatted == source, name
	}
	// Encoding a value that is not narrowed is migrated as usual.
	plain := header + 'fn f(x Animal) {\n\tprintln(json.encode(x))\n}\n'
	_, migrated := run_vfmt_write('not_narrowed', plain, '')
	assert migrated.contains('json2.encode(x, escape_unicode: true, time_as_unix: true)'), migrated
}

fn test_fmt_skips_json2_migration_for_shadowed_selective_imports_with_v3() {
	header := 'import json { decode, encode }\n\nstruct User {\n\tname string\n}\n\n'
	for name, body in {
		'param':    "fn use(decode fn (string) string) string {\n\treturn decode('{}')\n}\n"
		'local':    "fn f() {\n\tdecode := fn (s string) string {\n\t\treturn s\n\t}\n\tprintln(decode('{}'))\n\tprintln(encode(User{}))\n}\n"
		'for_var':  "fn f() {\n\tfor decode in [fn (s string) string {\n\t\treturn s\n\t}] {\n\t\tprintln(decode('{}'))\n\t}\n}\n"
		'as_value': 'fn f() {\n\tg := encode[User]\n\t_ = g\n}\n'
	} {
		source := header + body
		res, formatted := run_vfmt_write('shadowed_selective_${name}_test', source, '')
		assert res.exit_code == 0, res.output
		assert formatted == source, name
	}
	// A legacy decode call without both arguments cannot be rewritten either.
	one_arg := "import json\n\nfn f() {\n\tprintln(json.decode('{}'))\n\tprintln(json.encode(1))\n}\n"
	res, formatted := run_vfmt_write('one_argument_decode', one_arg, '')
	assert res.exit_code == 0, res.output
	assert formatted == one_arg
}

fn test_fmt_keeps_json_module_found_through_path_flag_with_v3() {
	path_root := os.join_path(vfmt_test_tdir, 'json_path_root')
	os.mkdir_all(os.join_path(path_root, 'json'))!
	os.write_file(os.join_path(path_root, 'json', 'json.v'),
		"module json\n\npub fn encode[T](x T) string {\n\treturn 'path'\n}\n")!
	source := 'import json\n\nfn main() {\n\tprintln(json.encode(1))\n}\n'
	old_vflags := os.getenv('VFLAGS')
	os.setenv('VFLAGS', '-path ${path_root}|@vlib|@vmodules', true)
	res, formatted := run_vfmt_write('json_from_path_flag', source, '')
	if old_vflags == '' {
		os.unsetenv('VFLAGS')
	} else {
		os.setenv('VFLAGS', old_vflags, true)
	}
	assert res.exit_code == 0, res.output
	assert formatted == source
	// Without `-path`, the same import names the removed vlib module.
	_, migrated := run_vfmt_write('json_without_path_flag', source, '')
	assert migrated.contains('import json2\n'), migrated
}

fn test_fmt_stdin_keeps_the_callers_project_json_module_with_v3() {
	project_dir := os.join_path(vfmt_test_tdir, 'stdin_json_project')
	os.mkdir_all(os.join_path(project_dir, 'json'))!
	os.write_file(os.join_path(project_dir, 'json', 'json.v'),
		"module json\n\npub fn encode[T](x T) string {\n\treturn 'own'\n}\n")!
	source_path := os.join_path(project_dir, 'app.v')
	os.write_file(source_path, 'import json\n\nfn main() {\n\tprintln(json.encode(1))\n}\n')!
	// stdin is staged in a temporary folder, but its imports belong to the caller's
	// working directory, which has its own `json` module here.
	old_wd := os.getwd()
	os.chdir(project_dir)!
	mut input_process := os.new_process(vexe)
	input_process.set_args(['fmt'])
	input_process.set_redirect_stdio_merged()
	input_process.set_stdin_path(source_path)
	input_process.run()
	input_output := input_process.stdout_slurp()
	input_process.wait()
	res := os.Result{ exit_code: input_process.code, output: input_output }
	input_process.close()
	os.chdir(old_wd)!
	assert res.exit_code == 0, res.output
	assert res.output.contains('import json\n'), res.output
	assert !res.output.contains('json2'), res.output
}

fn test_fmt_json_lookup_with_path_flag_follows_the_compiler_with_v3() {
	module_source := "module json\n\npub fn encode[T](x T) string {\n\treturn 'own'\n}\n"
	source := 'import json\n\nfn main() {\n\tprintln(json.encode(1))\n}\n'
	root := os.join_path(vfmt_test_tdir, 'json_path_lookup')
	// A module in ~/.vmodules is not searched when `-path` leaves out `@vmodules`.
	vmodules := os.join_path(root, 'vmodules')
	os.mkdir_all(os.join_path(vmodules, 'json'))!
	os.write_file(os.join_path(vmodules, 'json', 'json.v'), module_source)!
	os.mkdir_all(os.join_path(root, 'app'))!
	app_file := os.join_path(root, 'app', 'main.v')
	os.write_file(app_file, source)!
	// A sibling of the importing file's directory is still found through `-path`.
	os.mkdir_all(os.join_path(root, 'project', 'src'))!
	os.mkdir_all(os.join_path(root, 'project', 'json'))!
	os.write_file(os.join_path(root, 'project', 'json', 'json.v'), module_source)!
	project_file := os.join_path(root, 'project', 'src', 'main.v')
	os.write_file(project_file, source)!
	old_vflags := os.getenv('VFLAGS')
	old_vmodules := os.getenv('VMODULES')
	os.setenv('VMODULES', vmodules, true)
	os.setenv('VFLAGS', '-path @vlib', true)
	app_res := os.exec([vexe, 'fmt', '-w', app_file])
	project_res := os.exec([vexe, 'fmt', '-w', project_file])
	os.unsetenv('VFLAGS')
	app_copy := os.join_path(root, 'app', 'copy.v')
	os.write_file(app_copy, source)!
	with_vmodules_res := os.exec([vexe, 'fmt', '${app_copy}'])
	for name, value in {
		'VFLAGS':   old_vflags
		'VMODULES': old_vmodules
	} {
		if value == '' {
			os.unsetenv(name)
		} else {
			os.setenv(name, value, true)
		}
	}
	assert app_res.exit_code == 0, app_res.output
	assert os.read_file(app_file)!.contains('import json2\n')
	assert project_res.exit_code == 0, project_res.output
	assert os.read_file(project_file)! == source
	// Without `-path`, ~/.vmodules is searched, so a file importing that module keeps it.
	assert with_vmodules_res.exit_code == 0, with_vmodules_res.output
	assert with_vmodules_res.output.contains('import json\n'), with_vmodules_res.output
	// A `json` folder above another `v.mod` project is not the imported module with
	// `-path` (the compiler's `module_dir_belongs_to_other_project`).
	outer := os.join_path(root, 'outer')
	os.mkdir_all(os.join_path(outer, 'app', 'src'))!
	os.mkdir_all(os.join_path(outer, 'json'))!
	os.write_file(os.join_path(outer, 'json', 'json.v'), module_source)!
	os.write_file(os.join_path(outer, 'app', 'v.mod'), "Module {\n\tname: 'app'\n}\n")!
	outer_file := os.join_path(outer, 'app', 'src', 'main.v')
	os.write_file(outer_file, source)!
	os.setenv('VFLAGS', '-path @vlib', true)
	outer_res := os.exec([vexe, 'fmt', outer_file])
	if old_vflags == '' {
		os.unsetenv('VFLAGS')
	} else {
		os.setenv('VFLAGS', old_vflags, true)
	}
	assert outer_res.exit_code == 0, outer_res.output
	assert outer_res.output.contains('import json2\n'), outer_res.output
}

fn test_fmt_keeps_project_owned_json_module_imports_with_v3() {
	source := "import json

fn test_own() {
	assert json.encode(1) == 'own 1'
}
"
	project_dir := os.join_path(vfmt_test_tdir, 'own_json_project')
	os.mkdir_all(os.join_path(project_dir, 'json'))!
	os.write_file(os.join_path(project_dir, 'json', 'json.v'),
		"module json\n\npub fn encode[T](x T) string {\n\treturn 'own \${x}'\n}\n")!
	for name in ['own_json_test.v', 'own_json_consumer.v'] {
		source_path := os.join_path(project_dir, name)
		os.write_file(source_path, source)!
		res := os.exec([vexe, 'fmt', '-w', source_path])
		assert res.exit_code == 0, res.output
		assert os.read_file(source_path)! == source
	}
	// Without a project `json` module, the import names the removed vlib module, and
	// test files are migrated like other code. A file with a local `json` binding
	// keeps the legacy source, since its method calls are not module calls.
	for name, local_source in {
		'local_var': 'json := Encoder{}\n\tassert json.encode() == 0'
		'param':     'f := fn (json Encoder) int {\n\t\treturn json.encode()\n\t}\n\tassert f(Encoder{}) == 0'
		'for_var':   'for json in [Encoder{}] {\n\t\tassert json.encode() == 0\n\t}'
	} {
		local_json := 'import json

struct Encoder {}

fn (e Encoder) encode() int {
	return 0
}

fn test_local() {
	${local_source}
}
'
		local_res, local_formatted := run_vfmt_write('local_json_${name}_test', local_json, '')
		assert local_res.exit_code == 0, local_res.output
		assert local_formatted == local_json
	}
	res, formatted := run_vfmt_write('removed_json_module_test', source, '')
	assert res.exit_code == 0, res.output
	assert formatted.contains('import json2\n'), formatted
	assert formatted.contains('json2.encode(1, escape_unicode: true, time_as_unix: true)'), formatted
}

fn test_fmt_keeps_comments_before_expanded_const_fields_with_v3() {
	source := 'const (
	// pi documents pi
	pi = 3.14
	// phi documents phi
	phi = 1.618
)
'
	res, formatted := run_vfmt_write('grouped_const_comments', source, '')

	assert res.exit_code == 0, res.output
	assert formatted.contains('// pi documents pi\nconst pi = 3.14'), formatted
	assert formatted.contains('// phi documents phi\nconst phi = 1.618'), formatted
	assert !formatted.contains('const ('), formatted
	assert !formatted.contains('pi =\n'), formatted
}

fn test_fmt_preserves_typed_map_entries_and_typeof_array_init_with_v3() {
	source := "fn f() {\n\tm := map[string]int{'a': 1}\n\tfixed := [1, 2, 3]!\n\tdyn := []typeof(fixed[0]){}\n\tprintln(m)\n\tprintln(dyn)\n}\n"
	res, formatted := run_vfmt_write('typed_map_and_typeof_array', source, '')

	assert res.exit_code == 0, res.output
	assert formatted.contains("map[string]int{\n\t\t'a': 1\n\t}"), formatted
	assert formatted.contains('[]typeof(fixed[0]){}'), formatted
	second_res, formatted_twice := run_vfmt_write('typed_map_and_typeof_array', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_rewrites_legacy_it_only_in_array_init_expression_with_v3() {
	source := 'fn f() {
	it := 3
	a := []int{len: it, cap: it + 1, init: it}
	println(a)
}
'
	res, formatted := run_vfmt_write('array_init_legacy_it_scope', source, '')

	assert res.exit_code == 0, res.output
	assert formatted.contains('[]int{len: it, cap: it + 1, init: index}'), formatted
	second_res, formatted_twice := run_vfmt_write('array_init_legacy_it_scope_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_preserves_multi_variable_c_style_loop_headers_with_v3() {
	source := 'fn f() {
	L4: for a, b := 0, 10; a < 4; a++, b-- {
		if a < 2 {
			continue L4
		}
		break L4
	}
}
'
	res, formatted := run_vfmt_write('multi_variable_c_style_loop', source, '')

	assert res.exit_code == 0, res.output
	assert formatted.contains('L4: for a, b := 0, 10; a < 4; a++, b-- {'), formatted
	assert !formatted.contains('\n\t{\n\t\tmut a, b := 0, 10'), formatted
	second_res, formatted_twice :=
		run_vfmt_write('multi_variable_c_style_loop_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_preserves_postfix_assignment_attributes_with_v3() {
	source := 'fn f() {
	x := [1, 2, 3] @[freed]
	unsafe {
		x.free()
	}
}
'
	res, formatted := run_vfmt_write('postfix_assignment_attribute', source, '')

	assert res.exit_code == 0, res.output
	assert formatted.contains('x := [1, 2, 3] @[freed]'), formatted
	second_res, formatted_twice :=
		run_vfmt_write('postfix_assignment_attribute_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_preserves_branch_prediction_builtins_with_v3() {
	source := 'fn f(value int) bool {
	if _likely_(value > 0) {
		return true
	}
	return _unlikely_(value < 0)
}
'
	res, formatted := run_vfmt_write('branch_prediction_builtins', source, '')

	assert res.exit_code == 0, res.output
	assert formatted.contains('if _likely_(value > 0) {'), formatted
	assert formatted.contains('return _unlikely_(value < 0)'), formatted
	second_res, formatted_twice := run_vfmt_write('branch_prediction_builtins_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_emits_mut_before_static_declarations_with_v3() {
	source := '@[unsafe]
fn next() int {
	mut static value := 1
	value++
	return value
}
'
	res, formatted := run_vfmt_write('mutable_static_declaration', source, '')

	assert res.exit_code == 0, res.output
	assert formatted.contains('mut static value := 1'), formatted
	assert !formatted.contains('static mut value'), formatted
	second_res, formatted_twice := run_vfmt_write('mutable_static_declaration_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_skips_json2_migration_for_aliased_qualifier_local_collisions_with_v3() {
	source := 'import json
import json2 as j2

struct Foo {}

fn already_new() {
	println(j2.encode(1))
}

fn encode_legacy() string {
	j2 := Foo{}
	return json.encode(j2)
}
'
	res, formatted := run_vfmt_write('aliased_json2_local_collision', source, '')

	assert res.exit_code == 0, res.output
	assert formatted.contains('import json\n'), formatted
	assert formatted.contains('import json2 as j2'), formatted
	assert formatted.contains('j2 := Foo{}'), formatted
	assert formatted.contains('return json.encode(j2)'), formatted
	assert !formatted.contains('return j2.encode(j2'), formatted
	second_res, formatted_twice :=
		run_vfmt_write('aliased_json2_local_collision_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_preserves_global_grouping_and_field_comments_with_v3() {
	source := '@[c_extern]
__global errno C.int

__global enabled = bool(true)

__global (
	// typed global docs
	typed int
	// initialized global docs
	initialized = int(2)
)
'
	res, formatted := run_vfmt_write('global_grouping_and_comments', source, '')

	assert res.exit_code == 0, res.output
	assert formatted.contains('@[c_extern]\n__global errno C.int'), formatted
	assert formatted.contains('__global enabled = bool(true)'), formatted
	assert formatted.contains('__global (\n\t// typed global docs\n\ttyped       int'), formatted
	assert formatted.contains('\t// initialized global docs\n\tinitialized = int(2)'), formatted
	assert !formatted.contains('__global (\n\terrno C.int'), formatted
	assert !formatted.contains('initialized =\n'), formatted
	second_res, formatted_twice :=
		run_vfmt_write('global_grouping_and_comments_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_preserves_js_string_prefixes_with_v3() {
	source_path := os.join_path(vfmt_test_tdir, 'js_string_prefixes.js.v')
	source := "fn f() {\n\ts := js'hello V'\n\tp := 'abc'.str\n\tassert s == js'hello V'\n\tassert p == 'abc'\n}\n"
	os.write_file(source_path, source)!

	res := os.exec([vexe, 'fmt', '-w', '-verbose', source_path])
	formatted := os.read_file(source_path)!
	assert res.exit_code == 0, res.output
	assert formatted.contains("s := js'hello V'"), formatted
	assert formatted.contains("s == js'hello V'"), formatted
	assert formatted.contains("p := 'abc'.str"), formatted

	second_res := os.exec([vexe, 'fmt', '-w', source_path])
	assert second_res.exit_code == 0, second_res.output
	assert os.read_file(source_path)! == formatted
}

fn test_fmt_preserves_global_struct_sections_with_v3() {
	source := 'struct State {
	local int
__global:
	shared_value int
}
'
	res, formatted := run_vfmt_write('global_struct_section', source, '')

	assert res.exit_code == 0, res.output
	assert formatted.contains('__global:\n\tshared_value int'), formatted
	assert !formatted.contains('pub mut:'), formatted
	second_res, formatted_twice := run_vfmt_write('global_struct_section', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_normalizes_legacy_const_decl_assign_with_v3() {
	res, formatted := run_vfmt_write('legacy_const_decl_assign', 'const answer := 42\n', '')

	assert res.exit_code == 0, res.output
	assert formatted == 'const answer = 42\n', formatted
}

fn test_fmt_honors_new_int_with_v3() {
	c_source := 'module main

fn C.abc(a int, b []int, foreign C.int, foreign_values []C.int) int

fn C.foreign(value C.int) C.int

fn abc(a int) int
'
	c_res, c_formatted := run_vfmt_write('new_int_c_decl', c_source, '-new_int')
	assert c_res.exit_code == 0, c_res.output
	assert c_formatted.contains('fn C.abc(a i32, b []i32, foreign C.int, foreign_values []C.int) i32'), c_formatted

	assert c_formatted.contains('fn C.foreign(value C.int) C.int'), c_formatted
	assert !c_formatted.contains('C.i32'), c_formatted
	assert c_formatted.contains('fn abc(a int) int'), c_formatted

	translated_source := '@[translated]
module translated

fn convert(value int) int {
	return int(value)
}
'
	translated_res, translated_formatted := run_vfmt_write('new_int_translated', translated_source, '-new_int')
	assert translated_res.exit_code == 0, translated_res.output
	assert translated_formatted.contains('fn convert(value i32) i32'), translated_formatted
	assert translated_formatted.contains('return i32(value)'), translated_formatted
}

fn test_fmt_preserves_function_exit_defer_with_v3() {
	source := "fn cleanup() {\n\tdefer\n\t(fn)\n\t{\n\t\tprintln('done')\n\t}\n\tprintln('after')\n}\n"
	expected := "fn cleanup() {\n\tdefer(fn) {\n\t\tprintln('done')\n\t}\n\tprintln('after')\n}\n"
	res, formatted := run_vfmt_write('function_exit_defer', source, '')

	assert res.exit_code == 0, res.output
	assert formatted == expected, formatted
	second_res, formatted_twice := run_vfmt_write('function_exit_defer', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_reescapes_control_bytes_with_v3() {
	source := "fn main() {\n\tprint('\\a\\b\\f\\v\\x01\\x1b\\x7f')\n}\n"
	res, formatted := run_vfmt_write('escaped_control_bytes', source, '')

	assert res.exit_code == 0, res.output
	assert formatted == source, formatted
	second_res, formatted_twice := run_vfmt_write('escaped_control_bytes', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_does_not_import_for_in_binders_with_v3() {
	source := 'struct Item {\n\tname string\n}\n\nfn scan(items []Item) {\n\tfor flag in items {\n\t\tprintln(flag.name)\n\t}\n}\n'
	res, formatted := run_vfmt_write('for_in_binder_import', source, '')

	assert res.exit_code == 0, res.output
	assert !formatted.contains('import flag'), formatted
	second_res, formatted_twice := run_vfmt_write('for_in_binder_import', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_resolves_implied_imports_in_selector_scope_with_v3() {
	source := 'struct Clock {\n\tnow int\n}\n\nfn shadow(time Clock) {\n\tprintln(time.now)\n}\n\nfn use_module() {\n\tprintln(time.now())\n}\n'
	res, formatted := run_vfmt_write('scope_aware_implied_import', source, '')

	assert res.exit_code == 0, res.output
	assert formatted.contains('import time'), formatted
	assert formatted.contains('fn shadow(time Clock)'), formatted
	assert formatted.contains('println(time.now)'), formatted
	assert formatted.contains('println(time.now())'), formatted
	second_res, formatted_twice := run_vfmt_write('scope_aware_implied_import_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_preserves_isreftype_spelling_with_v3() {
	source := 'fn check[T](value T) {\n\t_ = isreftype(T)\n\t_ = isreftype[T]()\n\t_ = isreftype(value)\n\t_ = isreftype(sizeof(T))\n}\n'
	res, formatted := run_vfmt_write('isreftype_spelling', source, '')

	assert res.exit_code == 0, res.output
	assert !formatted.contains('__v3_isreftype'), formatted
	assert formatted.contains('isreftype(T)'), formatted
	assert formatted.contains('isreftype[T]()'), formatted
	assert formatted.contains('isreftype(value)'), formatted
	assert formatted.contains('isreftype(sizeof(T))'), formatted
	second_res, formatted_twice := run_vfmt_write('isreftype_spelling_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_preserves_anonymous_aggregate_types_with_v3() {
	source := "struct Holder {\n\titem []struct { foo string }\n}\n\nfn accept(value struct{ foo string }) {\n\tprintln(value.foo)\n}\n\nfn main() {\n\taccept(struct { foo: 'ok' })\n}\n"
	res, formatted := run_vfmt_write('anonymous_aggregate_types', source, '')

	assert res.exit_code == 0, res.output
	assert !formatted.contains('AnonStruct_'), formatted
	assert formatted.contains('item []struct { foo string }'), formatted
	assert formatted.contains('value struct{ foo string }'), formatted
	assert formatted.contains("accept(struct { foo: 'ok' })"), formatted
	second_res, formatted_twice := run_vfmt_write('anonymous_aggregate_types', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_preserves_mutable_match_subjects_with_v3() {
	source := 'fn update(mut value int) {\n\tmatch mut value {\n\t\tint {\n\t\t\tvalue++\n\t\t}\n\t}\n}\n'
	res, formatted := run_vfmt_write('mutable_match_subject', source, '')

	assert res.exit_code == 0, res.output
	assert formatted.contains('match mut value {'), formatted
	second_res, formatted_twice := run_vfmt_write('mutable_match_subject', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_preserves_c_string_prefix_with_v3() {
	source := "fn main(){\n\tx := c' '\n\t_ = x\n}\n"
	res, formatted := run_vfmt_write('c_string', source, '')

	assert res.exit_code == 0, res.output
	assert res.output.contains('vfmt running v.gen.v over file:'), res.output
	assert formatted.contains("x := c' '")
}

fn test_fmt_preserves_raw_string_prefix_with_v3() {
	source := 'fn main(){\n\tx := r"raw \\ value"\n\t_ = x\n}\n'
	res, formatted := run_vfmt_write('raw_string', source, '')

	assert res.exit_code == 0, res.output
	assert formatted.contains('x := r"raw \\ value"')
}

fn test_fmt_accepts_inline_asm_with_v3() {
	source := 'fn main(){\n\tasm amd64 {\n\t\tnop\n\t}\n}\n'
	res, formatted := run_vfmt_write('inline_asm', source, '')

	assert res.exit_code == 0, res.output
	assert res.output.contains('vfmt running v.gen.v over file:'), res.output
	assert formatted.contains('asm amd64 {')
	assert formatted.contains('nop')
}

fn test_fmt_preserves_json_migration_options_with_v3() {
	source := 'import json\n\nfn main(){\n\tprintln(json.encode(1))\n}\n'
	migrate_res, migrated := run_vfmt_write('json_migrate', source, '')

	assert migrate_res.exit_code == 0, migrate_res.output
	assert migrate_res.output.contains('vfmt running v.gen.v over file:'), migrate_res.output
	assert migrated.contains('import json2')
	assert migrated.contains('json2.encode(')

	keep_res, kept := run_vfmt_write('json_keep', source, '-no-migrate-json2')
	assert keep_res.exit_code == 0, keep_res.output
	assert kept.contains('import json\n')
	assert kept.contains('json.encode(')
}

fn test_fmt_preserves_for_in_binder_mutability_with_v3() {
	source := 'fn loops(mut values []int) {
	for mut index, _ in values {
		index++
	}
	for index, mut value in values {
		value++
		_ = index
	}
}
'
	res, formatted := run_vfmt_write('for_in_binder_mutability', source, '')

	assert res.exit_code == 0, res.output
	assert formatted == source, formatted
	second_res, formatted_twice := run_vfmt_write('for_in_binder_mutability_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_preserves_function_parameter_comments_with_v3() {
	source := 'fn describe(
	// documents a
	a int // explains a
	// documents b
	b int // explains b
	// closes parameter list
) {
}
'
	expected := 'fn describe(
	// documents a
	a int, // explains a
	// documents b
	b int // explains b
	// closes parameter list
) {
}
'
	res, formatted := run_vfmt_write('function_parameter_comments', source, '')

	assert res.exit_code == 0, res.output
	assert formatted == expected, formatted
	second_res, formatted_twice :=
		run_vfmt_write('function_parameter_comments_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_preserves_selective_import_and_field_layout_with_v3() {
	source := 'import math { max, min }
import os {
	file_ext,
	user_os,
}

interface Hex {
	a     int
	ab    int
	abcde int
mut:
	aaaaaaaaaaaaaaaaaa string
	b                  f64
}

struct Hex2 {
	a     int
	ab    int
	abcde int
}
'
	res, formatted := run_vfmt_write('selective_import_and_field_layout', source, '')

	assert res.exit_code == 0, res.output
	assert formatted == source, formatted
	second_res, formatted_twice := run_vfmt_write('selective_import_and_field_layout_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_keeps_comments_inside_fn_literals_with_v3() {
	source := "fn main() {\n\tcomment_only := fn () {\n\t\t// only comment\n\t}\n\twith_statement := fn () {\n\t\tprintln('inside')\n\t\t// trailing comment\n\t}\n\t_ = comment_only\n\t_ = with_statement\n}\n"
	res, formatted := run_vfmt_write('fn_literal_comments', source, '')

	assert res.exit_code == 0, res.output
	assert formatted == source, formatted
	second_res, formatted_twice := run_vfmt_write('fn_literal_comments_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_keeps_trailing_select_comments_inside_body_with_v3() {
	source := 'fn receive(ch chan int) {
	mut n := 0
	select {
		n = <-ch {
			n++
		}
		// post comment
	}
	assert n >= 0
}
'
	res, formatted := run_vfmt_write('select_trailing_comment', source, '')

	assert res.exit_code == 0, res.output
	assert formatted == source, formatted
	second_res, formatted_twice := run_vfmt_write('select_trailing_comment_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_renders_comptime_if_expressions_with_v3() {
	source := 'const enable_debug = \$if debug { true } \$else { false }

fn enabled() bool {
	return \$if prod { false } \$else \$if debug { true } \$else { false }
}
'
	res, formatted := run_vfmt_write('comptime_if_expressions', source, '')

	assert res.exit_code == 0, res.output
	assert formatted == source, formatted
	assert !formatted.contains('/* comptime_if */'), formatted
	second_res, formatted_twice := run_vfmt_write('comptime_if_expressions_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_preserves_statement_gaps_and_interface_end_comments_with_v3() {
	source := "interface Speaker {\n\t// first\n\tspeak() string\n\t// last\n}\n\nfn spaced() {\n\tprintln('a')\n\tprintln('b')\n\n\tprintln('c')\n\n\tif true {\n\t\tprintln('d')\n\t}\n\n\tdump('e')\n}\n"
	res, formatted := run_vfmt_write('statement_gaps_and_interface_comment', source, '')

	assert res.exit_code == 0, res.output
	assert formatted == source, formatted
	second_res, formatted_twice := run_vfmt_write('statement_gaps_and_interface_comment_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_suppresses_parser_warnings_with_v3() {
	source := 'fn main() {
	value := []int
	_ = value
}
'
	res, formatted := run_vfmt_write('parser_warning', source, '')

	assert res.exit_code == 0, res.output
	assert !res.output.contains('warning:'), res.output
	assert formatted.contains('value := []int'), formatted
}

fn test_fmt_preserves_multiline_strings_and_trailing_struct_comments_with_v3() {
	source := "const text = 'first\nsecond\nthird'\n\nstruct User {\n\tname string\n\t// trailing one\n\t// trailing two\n}\n"
	res, formatted := run_vfmt_write('multiline_string_struct_comments', source, '')

	assert res.exit_code == 0, res.output
	assert formatted == source, formatted
	second_res, formatted_twice := run_vfmt_write('multiline_string_struct_comments_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_preserves_declaration_list_layout_with_v3() {
	input := 'fn wrapped(first_parameter string,\n\tsecond_parameter int, third_parameter bool) {\n\tprintln(first_parameter)\n}\n\ntype Long = FirstVeryLongVariant | SecondVeryLongVariant | ThirdVeryLongVariant | FourthVeryLongVariant | FifthVeryLongVariant\n\ntype Commented = First // first\n\t| Second\n\t// disabled\n\t| Third\n\nenum Code {\n\ta = 1\n\tlong_name = 2\n\t// trailing\n}\n'
	expected := 'fn wrapped(first_parameter string,\n\tsecond_parameter int, third_parameter bool) {\n\tprintln(first_parameter)\n}\n\ntype Long = FirstVeryLongVariant\n\t| SecondVeryLongVariant\n\t| ThirdVeryLongVariant\n\t| FourthVeryLongVariant\n\t| FifthVeryLongVariant\n\ntype Commented = First // first\n\t| Second\n\t// disabled\n\t| Third\n\nenum Code {\n\ta         = 1\n\tlong_name = 2\n\t// trailing\n}\n'
	res, formatted := run_vfmt_write('declaration_list_layout', input, '')

	assert res.exit_code == 0, res.output
	assert formatted == expected, formatted
	second_res, formatted_twice := run_vfmt_write('declaration_list_layout_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_demangles_function_local_aggregate_types_with_v3() {
	source := "fn local_types() {
	struct Tick {
		next  &Tick = unsafe { nil }
		value int
	}
	union Number {
		integer int
		decimal f64
	}
	struct Wrapper {
		Tick
		numbers map[string]Number
	}

	mut ticks := []Tick{}
	first := Tick{
		value: 1
	}
	ticks << Tick{
		...first
		value: 2
	}
	wrapper := Wrapper{
		Tick:    first
		numbers: {
			'one': Number{
				integer: 1
			}
		}
	}
	_ = ticks
	_ = wrapper
}
"
	res, formatted := run_vfmt_write('function_local_aggregate_types', source, '')

	assert res.exit_code == 0, res.output
	assert formatted == source, formatted
	assert !formatted.contains('@local@'), formatted
	second_res, formatted_twice := run_vfmt_write('function_local_aggregate_types_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_preserves_contextual_range_embed_and_position_restrictions() {
	for source, expected in {
		'fn main() { value := [2]field.typ{} }':                  '[2]field.typ{}'
		'fn f[T]() { if T is int { println(1) } }':               'if T is int'
		'struct Example { value field.typ }':                     'value field.typ'
		'struct Example { value pkg.typ }':                       'value pkg.typ'
		'fn f(a, mut b int) {}':                                  'fn f(a int, mut b int)'
		'fn f(a, mut b &int) {}':                                 'fn f(a &int, mut b &int)'
		'fn main() { for i := 0; i < 3; j := 1 { println(j) } }': 'j := 1'
		'fn main() {}\n#!/usr/bin/env -S v run':                  '#!/usr/bin/env -S v run'
		'fn f(x int) { match x { 0 .. 3 {} else {} } }':          '0 .. 3'
		'struct Holder { pkg.lower }':                            'pkg.lower'
	} {
		res, formatted := run_vfmt_write('match_range_embed', source + '\n', '')
		assert res.exit_code == 0, res.output
		assert formatted.contains(expected), formatted
		second, twice := run_vfmt_write('match_range_embed_twice', formatted, '')
		assert second.exit_code == 0, second.output
		assert twice == formatted
	}
}
