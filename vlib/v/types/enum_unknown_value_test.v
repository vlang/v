module types

import os
import v.parser
import v.pref

fn check_enum_value_source(name string, body string) TypeChecker {
	path := os.join_path(os.vtmp_dir(), 'enum_unknown_value_${name}_${os.getpid()}.v')
	os.write_file(path, 'enum Color2 { red green }\nfn show(c Color2) {}\nfn main() { ${body} }\n') or { panic(err) }
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	_ = tc.check_semantics_opt(false)
	return tc
}

fn test_unknown_qualified_enum_value_is_rejected_in_calls_and_inferred_assignments() {
	tc := check_enum_value_source('qualified', 'show(Color2.blue) c := Color2.blue show(c)')
	errors := tc.errors.filter(it.msg == 'unknown enum field `blue` for `Color2`')
	assert errors.len == 2, tc.errors.str()
}

fn test_unknown_enum_value_is_rejected_in_match_patterns() {
	for i, pattern in ['.blue', 'Color2.blue'] {
		tc := check_enum_value_source('match_${i}', 'c := Color2.red match c { .red {} ${pattern} {} else {} }')
		assert tc.errors.any(it.msg == 'unknown enum field `blue` for `Color2`'), tc.errors.str()
	}
}

fn test_unknown_enum_shorthand_stays_rejected_outside_matches() {
	for i, body in ['show(.blue)', 'mut c := Color2.red c = .blue', 'c := Color2.red _ = c == .blue'] {
		tc := check_enum_value_source('shorthand_${i}', body)
		assert tc.errors.any(it.msg == 'unknown enum field `blue` for `Color2`'), tc.errors.str()
	}
}

fn test_declared_enum_values_remain_valid_in_qualified_and_shorthand_forms() {
	tc := check_enum_value_source('valid', 'show(Color2.green) show(.red) mut c := Color2.red c = .green _ = c == .green match c { .red {} Color2.green {} }')
	assert tc.errors.len == 0, tc.errors.str()
}
