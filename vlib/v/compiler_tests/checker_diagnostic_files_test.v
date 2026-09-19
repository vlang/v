import os
import v.parser
import v.pref
import v.types

fn check_diagnostic_project(name string, files map[string]string, diagnostic_rels []string) []types.TypeError {
	root := os.join_path(os.temp_dir(), 'v3_checker_diagnostic_files_${name}_${os.getpid()}')
	if os.exists(root) {
		os.rmdir_all(root) or { panic(err) }
	}
	os.mkdir_all(root) or { panic(err) }
	mut paths := []string{}
	mut rels := files.keys()
	rels.sort()
	for rel in rels {
		path := os.join_path(root, rel)
		os.mkdir_all(os.dir(path)) or { panic(err) }
		os.write_file(path, files[rel]) or { panic(err) }
		paths << path
	}
	builtin_path := os.join_path(root, 'builtin/ierror.v')
	os.mkdir_all(os.dir(builtin_path)) or { panic(err) }
	os.write_file(builtin_path, 'module builtin

pub interface IError {
	msg() string
	code() int
}
') or {
		panic(err)
	}
	prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	p.parse_file(builtin_path)
	p.a.user_code_start = p.a.nodes.len
	mut a := p.parse_files(paths)
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.diagnose_unknown_calls = true
	for rel in diagnostic_rels {
		tc.diagnostic_files[os.join_path(root, rel)] = true
	}
	tc.check_semantics()
	return tc.errors.clone()
}

const invalid_ierror_return_project = {
	'bad/bad.v': 'module bad

pub struct NotError {}

pub fn ok() int {
	return 1
}

pub fn fail() !int {
	return NotError{}
}
'
	'main.v':    'module main

import bad

fn main() {
	_ := bad.ok()
}
	'
}

const called_invalid_ierror_return_project = {
	'bad/bad.v': 'module bad

pub struct NotError {}

pub fn fail() !int {
	return NotError{}
}
'
	'main.v':    'module main

import bad

fn main() {
	_ := bad.fail() or { 0 }
}
'
}

const called_transitive_invalid_ierror_return_project = {
	'bad/bad.v': 'module bad

pub struct NotError {}

pub fn fail() !int {
	return NotError{}
}

pub fn unused_fail() !int {
	return NotError{}
}

pub fn wrapper() int {
	return fail() or { 0 }
}

pub fn outer_wrapper() int {
	return wrapper()
}
'
	'main.v':    'module main

import bad

fn main() {
	_ := bad.outer_wrapper()
}
'
}

const called_invalid_ierror_method_return_project = {
	'bad/bad.v': 'module bad

pub struct NotError {}

pub struct Worker {}

pub fn make() Worker {
	return Worker{}
}

pub fn (w Worker) fail() !int {
	return NotError{}
}
'
	'main.v':    'module main

import bad

fn main() {
	_ := bad.make().fail() or { 0 }
}
'
}

const called_invalid_ierror_local_method_return_project = {
	'bad/bad.v': 'module bad

pub struct NotError {}

pub struct Worker {}

pub fn make() Worker {
	return Worker{}
}

pub fn (w Worker) fail() !int {
	return NotError{}
}
'
	'main.v':    'module main

import bad

fn main() {
	b := bad.make()
	_ := b.fail() or { 0 }
}
'
}

const called_invalid_ierror_multi_return_local_method_project = {
	'bad/bad.v': 'module bad

pub struct NotError {}

pub struct Worker {}

pub fn make_dep() (int, Worker) {
	return 0, Worker{}
}

pub fn (w Worker) fail() !int {
	return NotError{}
}
'
	'main.v':    'module main

import bad

fn main() {
	_, dep := bad.make_dep()
	_ := dep.fail() or { 0 }
}
'
}

const called_invalid_ierror_for_in_local_method_project = {
	'bad/bad.v': 'module bad

pub struct NotError {}

pub struct Worker {}

pub fn make_all() []Worker {
	return [Worker{}]
}

pub fn (w Worker) fail() !int {
	return NotError{}
}
'
	'main.v':    'module main

import bad

fn main() {
	deps := bad.make_all()
	for _, dep in deps {
		_ := dep.fail() or { 0 }
	}
}
'
}

const uncalled_invalid_ierror_local_method_return_project = {
	'bad/bad.v': 'module bad

pub struct NotError {}

pub struct Worker {}

pub struct Good {}

pub fn make_good() Good {
	return Good{}
}

pub fn (w Worker) fail() !int {
	return NotError{}
}

pub fn (g Good) fail() int {
	return 1
}
'
	'main.v':    'module main

import bad

fn main() {
	b := bad.make_good()
	_ := b.fail()
}
'
}

const local_receiver_shadows_import_alias_project = {
	'bad/bad.v': 'module bad

pub struct NotError {}

pub struct Good {}

pub fn fail() !int {
	return NotError{}
}

pub fn make_good() Good {
	return Good{}
}

pub fn (g Good) fail() int {
	return 1
}
'
	'main.v':    'module main

import bad

fn main() {
	worker := bad.make_good()
	_ := worker.fail()
}
'
}

fn test_invalid_ierror_result_return_respects_diagnostic_files() {
	bad_errors := check_diagnostic_project('ierror_return_bad', invalid_ierror_return_project, [
		'bad/bad.v',
	])
	assert bad_errors.len == 1, bad_errors.str()
	assert bad_errors[0].msg.contains('bad.NotError')
	assert bad_errors[0].msg.contains('`!int`')

	main_errors := check_diagnostic_project('ierror_return_filtered',
		invalid_ierror_return_project, ['main.v'])
	assert main_errors.len == 0, main_errors.str()
}

fn test_selected_file_calling_invalid_ierror_result_return_reports_dependency_error() {
	main_errors := check_diagnostic_project('ierror_return_called_from_main',
		called_invalid_ierror_return_project, ['main.v'])
	assert main_errors.len == 1, main_errors.str()
	assert main_errors[0].msg.contains('cannot return')
	assert main_errors[0].msg.contains('bad.NotError')
	assert main_errors[0].msg.contains('`!int`')
}

fn test_selected_file_calling_transitive_invalid_ierror_result_return_reports_dependency_error() {
	main_errors := check_diagnostic_project('ierror_return_transitive_called_from_main',
		called_transitive_invalid_ierror_return_project, ['main.v'])
	assert main_errors.len == 1, main_errors.str()
	assert main_errors[0].msg.contains('cannot return')
	assert main_errors[0].msg.contains('bad.NotError')
	assert main_errors[0].msg.contains('`!int`')
}

fn test_selected_file_calling_invalid_ierror_local_method_return_reports_dependency_error() {
	main_errors := check_diagnostic_project('ierror_local_method_return_called_from_main',
		called_invalid_ierror_local_method_return_project, ['main.v'])
	assert main_errors.len == 1, main_errors.str()
	assert main_errors[0].msg.contains('cannot return')
	assert main_errors[0].msg.contains('bad.NotError')
	assert main_errors[0].msg.contains('`!int`')
}

fn test_selected_file_multi_return_local_receiver_reports_dependency_error() {
	main_errors := check_diagnostic_project('ierror_multi_return_method_called_from_main',
		called_invalid_ierror_multi_return_local_method_project, ['main.v'])
	assert main_errors.len == 1, main_errors.str()
	assert main_errors[0].msg.contains('cannot return')
	assert main_errors[0].msg.contains('bad.NotError')
	assert main_errors[0].msg.contains('`!int`')
}

fn test_selected_file_for_in_local_receiver_reports_dependency_error() {
	main_errors := check_diagnostic_project('ierror_for_in_method_called_from_main',
		called_invalid_ierror_for_in_local_method_project, ['main.v'])
	assert main_errors.len == 1, main_errors.str()
	assert main_errors[0].msg.contains('cannot return')
	assert main_errors[0].msg.contains('bad.NotError')
	assert main_errors[0].msg.contains('`!int`')
}

fn test_selected_file_local_receiver_different_method_receiver_stays_filtered() {
	main_errors := check_diagnostic_project('ierror_local_method_return_not_called_from_main',
		uncalled_invalid_ierror_local_method_return_project, ['main.v'])
	assert main_errors.len == 0, main_errors.str()
}

fn test_selected_file_local_receiver_uses_distinct_import_alias() {
	main_errors := check_diagnostic_project('ierror_import_alias_shadowed_by_local_receiver',
		local_receiver_shadows_import_alias_project, ['main.v'])
	assert main_errors.len == 0, main_errors.str()
}

fn test_local_shadowing_in_child_scope_and_fn_literal_is_allowed() {
	errors := check_diagnostic_project('ordinary_local_child_shadow', {
		'main.v': 'module main

fn main() {
	timers := 1
	if true {
		timers := 2
		_ = timers
	}
	cb := fn () {
		timers := 3
		_ = timers
	}
	cb()
	_ = timers
}
'
	}, ['main.v'])
	assert errors.len == 0, errors.str()
}

fn test_selected_file_calling_invalid_ierror_method_return_reports_dependency_error() {
	main_errors := check_diagnostic_project('ierror_method_return_called_from_main',
		called_invalid_ierror_method_return_project, ['main.v'])
	assert main_errors.len == 1, main_errors.str()
	assert main_errors[0].msg.contains('cannot return')
	assert main_errors[0].msg.contains('bad.NotError')
	assert main_errors[0].msg.contains('`!int`')
}

fn test_wait_on_non_thread_array_reports_error() {
	errors := check_diagnostic_project('wait_on_non_thread_array', {
		'main.v': 'module main

fn main() {
	chs2 := []int{}
	chs2.wait()
}
'
	}, ['main.v'])
	assert errors.len == 1, errors.str()
	assert errors[0].msg == '`[]int` has no method `wait()` (only thread handles and arrays of them have)'
	assert errors[0].node_value == 'chs2', errors.str()
}

fn test_unknown_method_on_map_reports_error() {
	errors := check_diagnostic_project('unknown_map_method', {
		'main.v': 'module main

fn main() {
	mut m := map[string]int{}
	m.close()
	m.foo()
	m.wait()
}
'
	}, ['main.v'])
	assert errors.len == 3, errors.str()
	for i, method in ['close', 'foo', 'wait'] {
		assert errors[i].msg == 'unknown function `m.${method}`', errors[i].msg
	}
}

fn test_builtin_map_methods_are_not_reported() {
	errors := check_diagnostic_project('builtin_map_methods', {
		'main.v': "module main

fn main() {
	mut m := map[string]int{}
	m.reserve(4)
	m['a'] = 1
	_ := m.keys()
	_ := m.values()
	c := m.clone()
	println(c.str())
	mut n := m.move()
	n.delete('a')
	n.clear()
	unsafe { n.free() }
}
"
	}, ['main.v'])
	assert errors.len == 0, errors.str()
}

fn test_user_wait_method_on_array_alias_is_allowed() {
	errors := check_diagnostic_project('array_alias_user_wait', {
		'main.v': "module main

type MyArr = []int

type Nested = MyArr

fn (a MyArr) wait() string {
	return 'ok'
}

fn main() {
	x := MyArr([1, 2])
	println(x.wait())
	y := Nested([3])
	println(y.wait())
}
"
	}, ['main.v'])
	assert errors.len == 0, errors.str()
}

fn test_unknown_enum_values_are_reported() {
	errors := check_diagnostic_project('unknown_enum_values', {
		'main.v': 'module main

enum Color {
	red
	green
}

fn main() {
	a := Color.nope
	match a {
		.red { println(1) }
		.bogus { println(2) }
		else {}
	}
}
'
	}, ['main.v'])
	assert errors.len == 2, errors.str()
	assert errors[0].msg == 'unknown enum field `nope` for `Color`', errors.str()
	assert errors[1].msg == 'unknown enum field `bogus` for `Color`', errors.str()
}

fn test_enum_statics_and_methods_are_not_reported() {
	errors := check_diagnostic_project('enum_statics', {
		'main.v': "module main

enum Color {
	red
	green
}

@[flag]
enum Permission {
	read
	write
}

fn (c Color) label() string {
	return c.str()
}

fn Color.first() Color {
	return .red
}

fn apply(f fn (Color) string) string {
	return f(Color.green)
}

fn main() {
	c := Color.from(1) or { Color.red }
	p := Permission.zero()
	s := Color.from_string('green') or { Color.red }
	f := Color.first
	println('\${c.label()} \${p} \${s} \${Color.green} \${f()} \${apply(Color.label)}')
}
"
	}, ['main.v'])
	assert errors.len == 0, errors.str()
}

fn test_escaped_keyword_enum_values_are_not_reported() {
	errors := check_diagnostic_project('escaped_enum_values', {
		'main.v': 'module main

enum Mode {
	@none
	@type
	normal
}

fn main() {
	m := Mode.@none
	match m {
		.@none { println(0) }
		.@type { println(1) }
		.normal { println(2) }
	}
}
'
	}, ['main.v'])
	assert errors.len == 0, errors.str()
}
