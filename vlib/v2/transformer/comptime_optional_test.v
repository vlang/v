// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module transformer

import os
import v2.ast
import v2.parser
import v2.pref as vpref
import v2.token
import v2.types

fn parse_optional_test_code(code string, target_os string, defines []string) []ast.File {
	tmp_file := '/tmp/v2_parser_optional_cond_test_${os.getpid()}.v'
	os.write_file(tmp_file, code) or { panic('failed to write temp file') }
	defer {
		os.rm(tmp_file) or {}
	}
	prefs := &vpref.Preferences{
		backend:               .cleanc
		no_parallel:           true
		target_os:             target_os
		user_defines:          defines
		explicit_user_defines: defines.clone()
	}
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(prefs)
	return par.parse_files([tmp_file], mut file_set)
}

fn transform_optional_test_code(code string, target_os string, defines []string) []ast.File {
	tmp_file := '/tmp/v2_transformer_optional_cond_test_${os.getpid()}.v'
	os.write_file(tmp_file, code) or { panic('failed to write temp file') }
	defer {
		os.rm(tmp_file) or {}
	}
	prefs := &vpref.Preferences{
		backend:               .cleanc
		no_parallel:           true
		target_os:             target_os
		user_defines:          defines
		explicit_user_defines: defines.clone()
	}
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(prefs)
	files := par.parse_files([tmp_file], mut file_set)
	mut env := types.Environment.new()
	mut checker := types.Checker.new(prefs, file_set, env)
	checker.check_files(files)
	mut trans := Transformer.new_with_pref(env, prefs)
	return trans.transform_files(files)
}

fn optional_test_struct_field_names(file ast.File, struct_name string) []string {
	for stmt in file.stmts {
		if stmt is ast.StructDecl && stmt.name == struct_name {
			mut names := []string{cap: stmt.fields.len}
			for field in stmt.fields {
				names << field.name
			}
			return names
		}
	}
	return []string{}
}

fn optional_test_fn_stmt_count(files []ast.File, name string) int {
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl && stmt.name == name {
				return stmt.stmts.len
			}
		}
	}
	return -1
}

fn optional_test_first_return_number(files []ast.File, name string) string {
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl && stmt.name == name {
				for nested in stmt.stmts {
					if nested is ast.ReturnStmt && nested.exprs.len == 1
						&& nested.exprs[0] is ast.BasicLiteral {
						return (nested.exprs[0] as ast.BasicLiteral).value
					}
				}
			}
		}
	}
	return ''
}

fn test_parser_struct_field_attr_optional_os_requires_user_define() {
	source := '
module main

struct Container {
	name string
	plain_linux int @[if linux]
	optional_linux int @[if linux ?]
}
'
	no_define_files := parse_optional_test_code(source, 'linux', [])
	assert no_define_files.len == 1
	assert optional_test_struct_field_names(no_define_files[0], 'Container') == [
		'name',
		'plain_linux',
	]

	defined_files := parse_optional_test_code(source, 'linux', ['linux'])
	assert defined_files.len == 1
	assert optional_test_struct_field_names(defined_files[0], 'Container') == ['name', 'plain_linux',
		'optional_linux']
}

fn test_transformer_comptime_optional_os_requires_user_define() {
	source := '
module main

fn optional_branch() int {
	$if linux ? {
		return 1
	} $else {
		return 2
	}
}

fn plain_os_branch() int {
	$if linux {
		return 3
	} $else {
		return 4
	}
}
'
	no_define_files := transform_optional_test_code(source, 'linux', [])
	assert optional_test_first_return_number(no_define_files, 'optional_branch') == '2'
	assert optional_test_first_return_number(no_define_files, 'plain_os_branch') == '3'

	defined_files := transform_optional_test_code(source, 'linux', ['linux'])
	assert optional_test_first_return_number(defined_files, 'optional_branch') == '1'
	assert optional_test_first_return_number(defined_files, 'plain_os_branch') == '3'
}

fn test_transformer_function_attr_optional_os_requires_user_define() {
	source := '
module main

@[if linux ?]
fn optional_fn() int {
	return 1
}

@[if linux]
fn plain_os_fn() int {
	return 2
}
'
	no_define_files := transform_optional_test_code(source, 'linux', [])
	assert optional_test_fn_stmt_count(no_define_files, 'optional_fn') == 0
	assert optional_test_fn_stmt_count(no_define_files, 'plain_os_fn') > 0

	defined_files := transform_optional_test_code(source, 'linux', ['linux'])
	assert optional_test_fn_stmt_count(defined_files, 'optional_fn') > 0
	assert optional_test_fn_stmt_count(defined_files, 'plain_os_fn') > 0
}
