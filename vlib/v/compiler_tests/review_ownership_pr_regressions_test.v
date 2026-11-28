import os
import v.cmdexec
import v.tempname

const review_pr_fixture_dir = os.join_path(os.vtmp_dir(), 'v3_review_pr_fixtures_${tempname.unique_token()}')

fn testsuite_begin() {
	os.mkdir_all(review_pr_fixture_dir) or { panic(err) }
}

fn testsuite_end() {
	os.rmdir_all(review_pr_fixture_dir) or {}
}

fn review_pr_write_file(root string, rel string, source string) {
	path := os.join_path(root, rel)
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, source) or { panic(err) }
}

fn test_review_pr_type_resolution() {
	name := 'review_pr_type_resolution'
	root := os.join_path(review_pr_fixture_dir, 'v3_${name}_project')
	os.mkdir_all(root) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: '${name}' }\n") or { panic(err) }
	files := {
		'callee/callee.v':                        'module callee

pub struct S {
pub:
	n int
}

pub fn take(s S) int {
	return s.n + 1
}
'
		'imported_decl_param_module.v':           'module main

import callee

struct S {
	text string
}

fn imported_decl_param_module() {
	println(int_str(callee.take(callee.S{
		n: 4
	})))
}
'
		'selective_callee/callee.v':              'module selective_callee

import other { S }

pub fn take(s S) int {
	return s.n + 3
}
'
		'other/other.v':                          'module other

pub struct S {
pub:
	n int
}
'
		'imported_decl_param_selective_import.v': 'module main

import selective_callee
import other

fn imported_decl_param_selective_import() {
	println(int_str(selective_callee.take(other.S{
		n: 4
	})))
}
'
		'factory/factory.v':                      'module factory

pub struct Box[T] {
pub:
	value T
}

pub fn (box Box[T]) get() T {
	return box.value
}

pub struct Factory {}

pub fn (builder Factory) new() Box[int] {
	_ = builder
	return Box[int]{
		value: 23
	}
}
'
		'generic_value_new_chain.v':              'module main

import factory

fn generic_value_new_chain() {
	builder := factory.Factory{}
	println(int_str(builder.new().get()))
}
'
		'fn_ptr_concrete_upper_suffix_type.v':    'module main

struct Foo_T {
	n int
}

fn apply(cb fn (Foo_T) int) int {
	return cb(Foo_T{
		n: 8
	})
}

fn take(foo Foo_T) int {
	return foo.n + 2
}

fn fn_ptr_concrete_upper_suffix_type() {
	println(int_str(apply(take)))
}
'
		'main.v':                                 "module main

fn main() {
	match arguments()[1] {
		'imported_decl_param_module' { imported_decl_param_module() }
		'imported_decl_param_selective_import' { imported_decl_param_selective_import() }
		'generic_value_new_chain' { generic_value_new_chain() }
		'fn_ptr_concrete_upper_suffix_type' { fn_ptr_concrete_upper_suffix_type() }
		else { panic('unknown fixture') }
	}
}
"
	}
	for rel, source in files {
		review_pr_write_file(root, rel, source)
	}
	bin := os.join_path(review_pr_fixture_dir, 'v3_${name}')
	compile := cmdexec.run_in_merged_with_env(@VEXE, [root, '-b', 'c', '-o', bin], '', {
		'VFLAGS': ''
	})
	assert compile.exit_code == 0, compile.output
	for fixture, expected in {
		'imported_decl_param_module':           '5'
		'imported_decl_param_selective_import': '7'
		'generic_value_new_chain':              '23'
		'fn_ptr_concrete_upper_suffix_type':    '10'
	} {
		run := os.exec([bin, fixture])
		assert run.exit_code == 0, '${fixture}: ${run.output}'
		actual := run.output.trim_space()
		assert actual == expected, '${fixture}: expected ${expected}, got ${actual}'
	}
}
