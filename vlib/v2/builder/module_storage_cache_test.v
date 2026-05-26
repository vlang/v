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
