import os
import v3.parser
import v3.pref
import v3.types

const checker_print_tests_dir = os.dir(@FILE)
const checker_print_v3_dir = os.dir(checker_print_tests_dir)
const checker_print_v3_src = os.join_path(checker_print_v3_dir, 'v3.v')
const checker_print_vexe = @VEXE

// check_print_sources validates check print sources state for v3 tests.
fn check_print_sources(name string, files map[string]string) []types.TypeError {
	root := os.join_path(os.temp_dir(), 'v3_checker_print_${name}')
	if os.exists(root) {
		os.rmdir_all(root) or { panic(err) }
	}
	os.mkdir_all(root) or { panic(err) }
	mut paths := []string{}
	for rel, src in files {
		path := os.join_path(root, rel)
		os.mkdir_all(os.dir(path)) or { panic(err) }
		os.write_file(path, src) or { panic(err) }
		paths << path
	}
	prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	mut a := p.parse_files(paths)
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	for path in paths {
		tc.diagnostic_files[path] = true
	}
	tc.diagnose_unknown_calls = true
	tc.check_semantics()
	return tc.errors
}

// build_checker_print_v3_bin builds checker print v3 bin data for v3 tests.
fn build_checker_print_v3_bin(name string) string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_checker_print_${name}')
	build := os.execute('${checker_print_vexe} -o ${v3_bin} ${checker_print_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

// test_bare_println_allows_non_string_for_stringify_lowering validates this v3 regression case.
fn test_bare_println_allows_non_string_for_stringify_lowering() {
	errors := check_print_sources('bare_println', {
		'main.v': 'module main

fn println(s string) {}

fn main() {
	println(123)
}
'
	})
	assert errors.len == 0, errors.str()
}

// test_local_println_non_string_param_keeps_declared_arg_type validates this v3 regression case.
fn test_local_println_non_string_param_keeps_declared_arg_type() {
	errors := check_print_sources('local_println_int_param', {
		'main.v': 'module main

fn println(i int) {}

fn main() {
	println("x")
}
'
	})
	assert errors.len == 1, errors.str()
	assert errors[0].msg == 'cannot use `string` as argument 1 to `println`; expected `int`'
}

// test_qualified_println_keeps_declared_arg_type validates this v3 regression case.
fn test_qualified_println_keeps_declared_arg_type() {
	errors := check_print_sources('qualified_println', {
		'main.v':    'module main

import log

fn main() {
	log.println(123)
}
'
		'log/log.v': 'module log

pub fn println(s string) {}
'
	})
	assert errors.len == 1, errors.str()
	assert errors[0].msg == 'cannot use `int` as argument 1 to `log.println`; expected `string`'
}

// test_eprint_bool_compile_uses_stringification validates this v3 regression case.
fn test_eprint_bool_compile_uses_stringification() {
	v3_bin := build_checker_print_v3_bin('eprint_bool')
	src := os.join_path(os.temp_dir(), 'v3_checker_print_eprint_bool.v')
	bin := os.join_path(os.temp_dir(), 'v3_checker_print_eprint_bool')
	os.write_file(src, '
fn main() {
	eprint(true)
}
') or { panic(err) }
	compile := os.execute('${v3_bin} -o ${bin} ${src}')
	assert compile.exit_code == 0, compile.output
}
