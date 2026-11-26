import os
import v.parser
import v.pref
import v.tempname
import v.types

// check_print_sources validates check print sources state for v3 tests.
fn check_print_sources(name string, files map[string]string) []types.TypeError {
	root := os.join_path(os.vtmp_dir(), 'v3_checker_print_${name}_${tempname.unique_token()}')
	os.mkdir(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
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

fn test_bare_println_allows_explicit_multi_pointer_for_stringify_lowering() {
	errors := check_print_sources('bare_println_multi_pointer', {
		'main.v': 'module main

struct Item {}

fn println(s string) {}

fn main() {
	item := &Item{}
	println(&item)
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
	assert errors[0].msg == 'cannot use `string` as `int` in argument 1 to `println`'
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
	assert errors[0].msg == 'cannot use `int literal` as `string` in argument 1 to `log.println`'
}

// test_eprint_bool_compile_uses_stringification validates this v3 regression case.
fn test_eprint_bool_compile_uses_stringification() {
	root := os.join_path(os.vtmp_dir(), 'v3_checker_print_eprint_bool_${tempname.unique_token()}')
	os.mkdir(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	src := os.join_path(root, 'v3_checker_print_eprint_bool.v')
	bin := os.join_path(root, 'app')
	os.write_file(src, '
fn main() {
	eprint(true)
}
') or { panic(err) }
	compile := os.exec([@VEXE, '-o', bin, src])
	assert compile.exit_code == 0, compile.output
}
