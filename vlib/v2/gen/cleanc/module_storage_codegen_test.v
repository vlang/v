module cleanc

import os
import v2.parser
import v2.pref as vpref
import v2.token
import v2.transformer
import v2.types

fn module_storage_csrc_for_test_sources(sources map[string]string) string {
	tmp_dir := os.join_path(os.temp_dir(), 'v2_module_storage_codegen_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir) or { panic('failed to create temp dir') }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	mut paths := []string{}
	for rel_path, code in sources {
		tmp_file := os.join_path(tmp_dir, rel_path)
		os.mkdir_all(os.dir(tmp_file)) or { panic('failed to create temp source dir') }
		os.write_file(tmp_file, code) or { panic('failed to write temp source') }
		paths << tmp_file
	}
	prefs := &vpref.Preferences{
		backend:     .cleanc
		no_parallel: true
	}
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(prefs)
	files := par.parse_files(paths, mut file_set)
	env := types.Environment.new()
	mut checker := types.Checker.new(prefs, file_set, env)
	checker.check_files(files)
	mut trans := transformer.Transformer.new_with_pref(env, prefs)
	trans.set_file_set(file_set)
	transformed_files := trans.transform_files(files)
	mut gen := Gen.new_with_env_and_pref(transformed_files, env, prefs)
	return gen.gen()
}

fn test_generate_c_uses_module_prefix_for_qualified_module_storage() {
	csrc := module_storage_csrc_for_test_sources({
		'report/report.v': 'module report

pub __global mut errors = 0
'
		'main.v':          'module main

import report

fn main() {
	report.errors += 1
}
'
	})
	assert csrc.contains('int report__errors'), csrc
	assert csrc.contains('report__errors'), csrc
	assert !csrc.contains('\nint errors'), csrc
}

fn test_generate_c_resolves_module_storage_import_alias() {
	csrc := module_storage_csrc_for_test_sources({
		'report/report.v': 'module report

pub __global mut errors = 0
'
		'main.v':          'module main

import report as r

fn main() {
	r.errors += 1
}
'
	})
	assert csrc.contains('report__errors'), csrc
	assert !csrc.contains('r__errors'), csrc
}

fn test_generate_c_keeps_same_short_module_storage_names_distinct() {
	csrc := module_storage_csrc_for_test_sources({
		'a/a.v':  'module a

pub __global state = 1
'
		'b/b.v':  'module b

pub __global state = 2
'
		'main.v': 'module main

import a
import b

fn main() {
	_ = a.state + b.state
}
'
	})
	assert csrc.contains('int a__state = 1;'), csrc
	assert csrc.contains('int b__state = 2;'), csrc
	assert !csrc.contains('\nint state ='), csrc
}

fn test_generate_c_prefixes_module_storage_names_containing_dunders() {
	csrc := module_storage_csrc_for_test_sources({
		'a/a.v':  'module a

pub __global mut state__x = 1

pub fn bump() int {
	state__x += 1
	return state__x
}
	'
		'b/b.v':  'module b

pub __global mut state__x = 2

pub fn bump() int {
	state__x += 1
	return state__x
}
	'
		'main.v': 'module main

import a
import b

fn main() {
	_ = a.bump() + b.bump()
	_ = a.state__x + b.state__x
}
'
	})
	assert csrc.contains('int a__state__x = 1;'), csrc
	assert csrc.contains('int b__state__x = 2;'), csrc
	assert csrc.contains('a__state__x += 1;'), csrc
	assert csrc.contains('return a__state__x;'), csrc
	assert csrc.contains('b__state__x += 1;'), csrc
	assert csrc.contains('return b__state__x;'), csrc
	assert !csrc.contains('\nint state__x ='), csrc
	assert !csrc.contains('\n\tstate__x += 1;'), csrc
	assert !csrc.contains('\n\treturn state__x;'), csrc
}

fn test_generate_c_keeps_c_global_declaration_unmangled() {
	csrc := module_storage_csrc_for_test_sources({
		'main.v': 'module main

@[c_extern]
__global C.errno int

@[c_extern]
__global C.stdin voidptr

@[c_extern]
__global C.stdout voidptr

@[c_extern]
__global C.stderr voidptr

fn main() {
	_ = C.errno
	_ = C.stdin
	_ = C.stdout
	_ = C.stderr
}
'
	})
	assert !csrc.contains('main__C.errno'), csrc
	assert !csrc.contains('main__errno'), csrc
	assert !csrc.contains('main__stdin'), csrc
	assert !csrc.contains('main__stdout'), csrc
	assert !csrc.contains('main__stderr'), csrc
	assert csrc.contains('errno'), csrc
	assert csrc.contains('stdin'), csrc
	assert csrc.contains('stdout'), csrc
	assert csrc.contains('stderr'), csrc
}

fn test_generate_c_keeps_c_extern_global_attributes_unmangled() {
	csrc := module_storage_csrc_for_test_sources({
		'main.v': 'module main

@[c_extern; export: "raw_counter"; weak; hidden; markused]
__global raw_counter int

fn main() {
	_ = raw_counter
}
'
	})
	assert csrc.contains('extern int raw_counter;'), csrc
	assert csrc.contains('raw_counter'), csrc
	assert !csrc.contains('main__raw_counter'), csrc
}

fn test_generate_c_keeps_translated_c_v_extern_global_unmangled() {
	csrc := module_storage_csrc_for_test_sources({
		'cwrap/runtime.c.v': '@[translated]
module cwrap

@[c_extern; hidden; markused]
__global raw_status int

pub fn read_status() int {
	return raw_status
}
'
		'main.v':            'module main

import cwrap

fn main() {
	_ = cwrap.read_status()
	_ = cwrap.raw_status
}
'
	})
	assert csrc.contains('extern int raw_status;'), csrc
	assert csrc.contains('return raw_status;'), csrc
	assert csrc.count('raw_status') >= 3, csrc
	assert !csrc.contains('cwrap__raw_status'), csrc
}

fn test_generate_c_keeps_qualified_c_extern_global_alias_unmangled() {
	csrc := module_storage_csrc_for_test_sources({
		'cwrap/runtime.c.v': '@[translated]
module cwrap

@[c_extern; hidden; markused]
__global raw_status int
'
		'main.v':            'module main

import cwrap as cw

fn main() {
	_ = cw.raw_status
}
'
	})
	assert csrc.contains('extern int raw_status;'), csrc
	assert csrc.count('raw_status') >= 2, csrc
	assert !csrc.contains('cwrap__raw_status'), csrc
	assert !csrc.contains('cw__raw_status'), csrc
}

fn test_generate_c_keeps_plain_module_c_extern_global_unmangled() {
	csrc := module_storage_csrc_for_test_sources({
		'ext/ext.v': 'module ext

@[c_extern]
pub __global raw_status int

pub fn read_status() int {
	return raw_status
}
'
		'main.v':    'module main

import ext

fn main() {
	_ = ext.raw_status
	_ = ext.read_status()
}
'
	})
	assert csrc.contains('extern int raw_status;'), csrc
	assert csrc.contains('return raw_status;'), csrc
	assert csrc.contains('(void)(raw_status);'), csrc
	assert !csrc.contains('ext__raw_status'), csrc
}
