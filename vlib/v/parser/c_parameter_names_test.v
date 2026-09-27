module parser

import os
import v.pref

fn test_c_function_parameters_preserve_c_names() {
	path := os.join_path(os.vtmp_dir(), 'c_parameter_names_${os.getpid()}.c.v')
	defer { os.rm(path) or {} }
	os.write_file(path, 'fn C.external_call(AP voidptr, N i32, B &f64) i32\n')!
	mut p := Parser.new(pref.new_preferences())
	p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
}

fn test_v_parameter_name_rule_is_semantic() {
	path := os.join_path(os.vtmp_dir(), 'v_parameter_names_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	os.write_file(path, 'fn f(Upper int) {}\n')!
	mut p := Parser.new(pref.new_preferences())
	p.parse_file(path)
	assert p.diagnostics.any(it.message == 'parameter name must not begin with upper case letter (`U`)')

	mut prefs := pref.new_preferences()
	prefs.is_fmt = true
	mut formatter := Parser.new(prefs)
	formatter.parse_file(path)
	assert formatter.diagnostics.len == 0, formatter.diagnostics.str()
}
