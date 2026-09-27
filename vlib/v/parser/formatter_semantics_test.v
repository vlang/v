module parser

import os
import v.pref

fn test_formatter_preserves_syntax_without_semantic_diagnostics() {
	path := os.join_path(os.vtmp_dir(), 'formatter_semantics_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	cases := {
		'fn main() { callback := fn [missing] () {}; _ = callback }':         'undefined ident: `missing`'
		'interface Reader { read[T](value T) T }':                            'non-generic interface `Reader` cannot define a generic method'
		'fn loops(values []int) { for mut index, _ in values { index++ } }':  'index of array or key of map cannot be mutated'
		'fn loops() { for mut index in 0 .. 3 { index++ } }':                 'variable in range `for` cannot be mut'
		'fn loops() { for index, value in 0 .. 3 { _ = value } }':            'cannot declare index variable with range `for`'
		'interface Abc { fun(); fun() }':                                     'duplicate method `fun`'
		'fn loops(values []int) { val := 1; for val in values { _ = val } }': 'redefinition of value iteration variable `val`, use `for (val in array) {` if you want to check for a condition instead'
		'fn closure() { x := 1; callback := fn [x] (x int) {} }':             'the parameter name `x` conflicts with the captured value name'
	}
	for source, expected in cases {
		os.write_file(path, source + '\n')!
		mut compiler := Parser.new(pref.new_preferences())
		compiler.parse_file(path)
		assert compiler.diagnostics.any(it.message == expected), compiler.diagnostics.str()

		mut prefs := pref.new_preferences()
		prefs.is_fmt = true
		mut formatter := Parser.new(prefs)
		formatter.parse_file(path)
		assert formatter.diagnostics.len == 0, formatter.diagnostics.str()
	}
}

fn test_formatter_still_reports_invalid_syntax() {
	path := os.join_path(os.vtmp_dir(), 'formatter_invalid_syntax_${os.getpid()}.v')
	os.write_file(path, 'fn main() { value := }\n')!
	defer { os.rm(path) or {} }
	mut prefs := pref.new_preferences()
	prefs.is_fmt = true
	mut formatter := Parser.new(prefs)
	formatter.parse_file(path)
	assert formatter.diagnostics.len > 0
}
