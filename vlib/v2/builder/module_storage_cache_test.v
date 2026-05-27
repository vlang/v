module builder

import os
import v2.ast
import v2.gen.v as gen_v
import v2.parser
import v2.pref
import v2.token

fn module_storage_cache_tmp_dir(label string) string {
	return os.join_path(os.temp_dir(), 'v2_module_storage_cache_${label}_${os.getpid()}')
}

fn module_storage_cached_header_source_for_test() string {
	tmp_dir := module_storage_cache_tmp_dir('source')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(os.join_path(tmp_dir, 'state')) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	source_file := os.join_path(tmp_dir, 'state', 'state.v')
	os.write_file(source_file, 'module state

pub __global mut total = 0
__global mut hidden = 0
') or {
		panic(err)
	}
	mut prefs := pref.new_preferences()
	mut b := new_builder(&prefs)
	source_files := b.parse_source_files_for_headers([source_file])
	header_ast := b.build_module_header_ast(source_files, 'state') or {
		panic('missing state header')
	}
	mut gen := gen_v.new_gen(b.pref)
	gen.gen(header_ast)
	return sanitize_header_source(gen.output_string(), map[string]string{})
}

fn module_mut_cached_header_source_for_test() string {
	tmp_dir := module_storage_cache_tmp_dir('module_mut_source')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(os.join_path(tmp_dir, 'state')) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	source_file := os.join_path(tmp_dir, 'state', 'state.v')
	os.write_file(source_file, 'module state

pub struct Counter {
pub module_mut:
	value int
pub mut:
	open int
}
') or {
		panic(err)
	}
	mut prefs := pref.new_preferences()
	mut b := new_builder(&prefs)
	source_files := b.parse_source_files_for_headers([source_file])
	header_ast := b.build_module_header_ast(source_files, 'state') or {
		panic('missing state header')
	}
	mut gen := gen_v.new_gen(b.pref)
	gen.gen(header_ast)
	return sanitize_header_source(gen.output_string(), map[string]string{})
}

fn module_mut_interface_cached_header_source_for_test() string {
	return module_mut_interface_cached_header_source_for_test_with_source_decls(false)
}

fn module_mut_interface_merged_cached_header_source_for_test() string {
	return module_mut_interface_cached_header_source_for_test_with_source_decls(true)
}

fn module_mut_interface_cached_header_source_for_test_with_source_decls(include_source_decls bool) string {
	tmp_dir := module_storage_cache_tmp_dir('module_mut_interface_source')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(os.join_path(tmp_dir, 'state')) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	source_file := os.join_path(tmp_dir, 'state', 'state.v')
	os.write_file(source_file, 'module state

pub type Handler = fn (int) bool

pub interface Mutator {
	handler Handler
	direct fn (int) bool
	handle(int) bool
mut:
	bump()
	reset()
}

pub struct Counter {
pub module_mut:
	iface Mutator
}
') or {
		panic(err)
	}
	mut prefs := pref.new_preferences()
	mut b := new_builder(&prefs)
	source_files := b.parse_source_files_for_headers([source_file])
	header_ast := b.build_module_header_ast(source_files, 'state') or {
		panic('missing state header')
	}
	mut gen := gen_v.new_gen(b.pref)
	gen.gen(header_ast)
	mut header_source := sanitize_header_source(gen.output_string(), map[string]string{})
	if include_source_decls {
		source_fn_decls := b.source_fn_decls_for_files([source_file])
		header_source = merge_missing_source_fn_decls(header_source, source_fn_decls)
	}
	return header_source
}

fn module_storage_cached_header_global(header_source string, name string) ast.FieldDecl {
	tmp_dir := module_storage_cache_tmp_dir('parse')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	header_path := os.join_path(tmp_dir, 'state.vh')
	os.write_file(header_path, header_source) or { panic(err) }
	mut prefs := pref.new_preferences()
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(&prefs)
	files := par.parse_files([header_path], mut file_set)
	for stmt in files[0].stmts {
		if stmt is ast.GlobalDecl {
			for field in stmt.fields {
				if field.name == name {
					return field
				}
			}
		}
	}
	panic('missing cached header field ${name}')
}

fn module_storage_cached_header_struct(header_source string, name string) ast.StructDecl {
	tmp_dir := module_storage_cache_tmp_dir('parse_struct')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	header_path := os.join_path(tmp_dir, 'state.vh')
	os.write_file(header_path, header_source) or { panic(err) }
	mut prefs := pref.new_preferences()
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(&prefs)
	files := par.parse_files([header_path], mut file_set)
	for stmt in files[0].stmts {
		if stmt is ast.StructDecl {
			if stmt.name == name {
				return stmt
			}
		}
	}
	panic('missing cached header struct ${name}')
}

fn module_storage_cached_header_interface(header_source string, name string) ast.InterfaceDecl {
	tmp_dir := module_storage_cache_tmp_dir('parse_interface')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	header_path := os.join_path(tmp_dir, 'state.vh')
	os.write_file(header_path, header_source) or { panic(err) }
	mut prefs := pref.new_preferences()
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(&prefs)
	files := par.parse_files([header_path], mut file_set)
	for stmt in files[0].stmts {
		if stmt is ast.InterfaceDecl {
			if stmt.name == name {
				return stmt
			}
		}
	}
	panic('missing cached header interface ${name}')
}

fn module_storage_run_cached_header_check(label string, header_source string, main_source string) (int, string) {
	tmp_dir := module_storage_cache_tmp_dir(label)
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(os.join_path(tmp_dir, 'state')) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	os.write_file(os.join_path(tmp_dir, 'state', 'state.v'), header_source) or { panic(err) }
	main_path := os.join_path(tmp_dir, 'main.v')
	os.write_file(main_path, main_source) or { panic(err) }
	out_path := os.join_path(tmp_dir, 'out.txt')
	cmd := '${os.quoted_path(@VEXE)} -v2 -backend v -o ${os.quoted_path(out_path)} ${os.quoted_path(main_path)} 2>&1'
	res := os.execute(cmd)
	return res.exit_code, res.output
}

fn test_module_storage_cached_header_preserves_flags() {
	header_source := module_storage_cached_header_source_for_test()
	assert header_source.contains('pub __global (')
	assert header_source.contains('mut total int')
	assert header_source.contains('__global (')
	assert header_source.contains('mut hidden int')
	public_field := module_storage_cached_header_global(header_source, 'total')
	assert public_field.is_public
	assert public_field.is_mut
	private_field := module_storage_cached_header_global(header_source, 'hidden')
	assert !private_field.is_public
	assert private_field.is_mut
}

fn test_module_storage_cached_header_preserves_module_mut_struct_fields() {
	header_source := module_mut_cached_header_source_for_test()
	assert header_source.contains('pub module_mut:')
	assert !header_source.contains('@[module_mut]')
	counter := module_storage_cached_header_struct(header_source, 'Counter')
	assert counter.fields[0].name == 'value'
	assert counter.fields[0].is_public
	assert counter.fields[0].is_mut
	assert counter.fields[0].is_module_mut
	assert counter.fields[1].name == 'open'
	assert counter.fields[1].is_public
	assert counter.fields[1].is_mut
	assert !counter.fields[1].is_module_mut
}

fn test_module_storage_cached_header_preserves_mut_interface_methods_for_module_mut_field() {
	header_source := module_mut_interface_cached_header_source_for_test()
	assert header_source.contains('pub interface Mutator {')
	assert header_source.contains('handler Handler')
	assert header_source.contains('direct fn(int) bool')
	assert header_source.contains('handle(int) bool')
	assert header_source.contains('mut:')
	assert header_source.contains('bump()')
	assert !header_source.contains('direct(int) bool')
	assert !header_source.contains('bump fn(')
	mutator := module_storage_cached_header_interface(header_source, 'Mutator')
	assert mutator.fields[0].name == 'handler'
	assert !mutator.fields[0].is_interface_method
	assert mutator.fields[1].name == 'direct'
	assert !mutator.fields[1].is_interface_method
	assert mutator.fields[2].name == 'handle'
	assert mutator.fields[2].is_interface_method
	assert mutator.fields[3].name == 'bump'
	assert mutator.fields[3].is_mut
	assert mutator.fields[3].is_interface_method
	assert mutator.fields[4].name == 'reset'
	assert mutator.fields[4].is_mut
	assert mutator.fields[4].is_interface_method

	module_mut_code, module_mut_output := module_storage_run_cached_header_check('module_mut_interface_method',
		header_source, 'module main

import state

fn main() {
	mut c := state.Counter{}
	c.iface.bump()
}
')
	assert module_mut_code != 0, 'cached module_mut interface method mutation should fail'
	assert module_mut_output.contains('cannot mutate module-mutable field `state.Counter.iface` outside module `state`'), module_mut_output
}

fn test_module_storage_cached_header_does_not_synthesize_interface_fn_field_decls() {
	header_source := module_mut_interface_merged_cached_header_source_for_test()
	assert header_source.contains('pub interface Mutator {')
	assert header_source.contains('direct fn(int) bool')
	assert header_source.contains('handle(int) bool')
	assert header_source.contains('pub fn (it Mutator) handle(int) bool')
	assert header_source.contains('pub fn (mut it Mutator) bump()')
	assert header_source.contains('pub fn (mut it Mutator) reset()')
	assert !header_source.contains('pub fn (it Mutator) direct fn')
	assert !header_source.contains('fn (it Mutator) direct fn')
	assert !header_source.contains('pub fn (it Mutator) bump()')

	module_mut_code, module_mut_output := module_storage_run_cached_header_check('module_mut_interface_merged_method',
		header_source, 'module main

import state

fn main() {
	mut c := state.Counter{}
	c.iface.bump()
}
')
	assert module_mut_code != 0, 'merged cached module_mut interface method mutation should fail'
	assert module_mut_output.contains('cannot mutate module-mutable field `state.Counter.iface` outside module `state`'), module_mut_output
}

fn test_module_storage_cached_header_visibility_matches_source_module() {
	header_source := module_storage_cached_header_source_for_test()
	public_code, public_output := module_storage_run_cached_header_check('public', header_source, 'module main

import state

fn main() {
	state.total += 1
}
')
	assert public_code == 0, public_output

	private_code, private_output := module_storage_run_cached_header_check('private',
		header_source, 'module main

import state

fn main() {
	state.hidden += 1
}
')
	assert private_code != 0, 'cached private module storage should fail'
	assert private_output.contains('module global `state.hidden` is private'), private_output
}

fn test_module_storage_cached_header_module_mut_visibility_matches_source_module() {
	header_source := module_mut_cached_header_source_for_test()
	public_code, public_output := module_storage_run_cached_header_check('module_mut_public',
		header_source, 'module main

import state

fn main() {
	mut c := state.Counter{}
	println(c.value)
	c.open++
}
')
	assert public_code == 0, public_output

	module_mut_code, module_mut_output := module_storage_run_cached_header_check('module_mut_private',
		header_source, 'module main

import state

fn main() {
	mut c := state.Counter{}
	c.value++
}
')
	assert module_mut_code != 0, 'cached module_mut field mutation should fail'
	assert module_mut_output.contains('cannot mutate module-mutable field `state.Counter.value` outside module `state`'), module_mut_output
}

fn test_module_storage_c_extern_import_cache_uses_raw_symbol() {
	tmp_dir := module_storage_cache_tmp_dir('c_extern_import')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(os.join_path(tmp_dir, 'ext')) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	mut prefs := pref.new_preferences()
	mut cache_builder := new_builder(&prefs)
	cache_dir := cache_builder.core_cache_dir()
	os.rmdir_all(cache_dir) or {}
	c_path := os.join_path(tmp_dir, 'raw_status.c')
	os.write_file(c_path, 'int raw_status = 7;\n') or { panic(err) }
	os.write_file(os.join_path(tmp_dir, 'ext', 'ext.v'), 'module ext

@[c_extern]
pub __global raw_status int

pub fn read_status() int {
	return raw_status
}
') or {
		panic(err)
	}
	main_path := os.join_path(tmp_dir, 'main.v')
	os.write_file(main_path, 'module main

import ext

#flag ${c_path}

fn main() {
	_ = ext.raw_status
	_ = ext.read_status()
}
') or {
		panic(err)
	}
	out_path := os.join_path(tmp_dir, 'out')
	cmd := '${os.quoted_path(@VEXE)} -v -v2 -keepc -o ${os.quoted_path(out_path)} ${os.quoted_path(main_path)} 2>&1'
	res := os.execute(cmd)
	assert res.exit_code == 0, res.output
	ext_header := os.read_file(cache_builder.core_header_path('ext')) or { panic(err) }
	assert ext_header.contains('@[c_extern]'), ext_header
	imports_c := os.read_file(os.join_path(cache_dir, 'imports.c')) or { panic(err) }
	assert imports_c.contains('return raw_status;'), imports_c
	assert !imports_c.contains('ext__raw_status'), imports_c
}
