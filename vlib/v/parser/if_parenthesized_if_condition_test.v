module parser

import os
import v.pref

const parenthesized_if_condition_error = 'the condition of an `if` cannot start with a parenthesized `if` expression; assign it to a variable first'

fn parse_if_condition_source(name string, source string) []Diagnostic {
	path := os.join_path(os.vtmp_dir(), 'if_parenthesized_if_condition_${name}_${os.getpid()}.v')
	os.write_file(path, source) or { panic(err) }
	defer { os.rm(path) or {} }
	mut p := Parser.new(pref.new_preferences())
	p.parse_file(path)
	return p.diagnostics
}

fn test_if_condition_rejects_leading_parenthesized_if() {
	for cond in ['(if a { b } else { c })', '((if a { b } else { c }))', '(if a { 1 } else { 2 }) < 2',
		'(if a { b } else { c } && b)', '(\n\tif a {\n\t\tb\n\t} else {\n\t\tc\n\t})'] {
		for header in ['if', 'if b {} else if'] {
			diagnostics := parse_if_condition_source('rejected', 'fn f(a bool, b bool, c bool) { ${header} ${cond} {} }\n')
			assert diagnostics.len == 1, '${header} ${cond}: ${diagnostics}'
			assert diagnostics[0].message == parenthesized_if_condition_error, '${header} ${cond}: ${diagnostics}'
		}
	}
}

fn test_if_condition_error_points_at_nested_if() {
	diagnostics := parse_if_condition_source('position', 'fn f(a bool) {\n\tif ((if a { true } else { false })) {}\n}\n')
	assert diagnostics.len == 1, diagnostics.str()
	assert diagnostics[0].line == 2
	assert diagnostics[0].column == 7
}

fn test_if_condition_accepts_other_parenthesized_conditions() {
	for cond in ['(a)', '(a && b) || c', '(match a { true { b } else { c } })'] {
		diagnostics := parse_if_condition_source('accepted', 'fn f(a bool, b bool, c bool) { if ${cond} {} }\n')
		assert diagnostics.len == 0, '${cond}: ${diagnostics}'
	}
}

fn test_translated_if_condition_keeps_leading_parenthesized_if() {
	diagnostics := parse_if_condition_source('translated', '@[translated]\nmodule main\nfn f(a bool, b bool, c bool) { if (if a { b } else { c }) {} }\n')
	assert diagnostics.len == 0, diagnostics.str()
}
