// Copyright (c) 2020-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import os
import v2.ast
import v2.ast_dump
import v2.parser
import v2.pref
import v2.token
import v2.transformer
import v2.types

fn main() {
	args := os.args[1..]

	if args.len == 0 {
		eprintln('Usage: vast <file.v>')
		eprintln('Dumps AST to <file>_ast.json and <file>_ast_transformed.json')
		exit(1)
	}

	files := get_files(args)
	if files.len == 0 {
		eprintln('Usage: vast <file.v>')
		eprintln('Dumps AST to <file>_ast.json and <file>_ast_transformed.json')
		exit(1)
	}

	// Get base name for output files
	input_file := files[0]
	base_name := if input_file.ends_with('.v') {
		input_file[..input_file.len - 2]
	} else {
		input_file
	}

	prefs := pref.new_preferences()
	mut file_set := token.FileSet.new()
	mut p := parser.Parser.new(&prefs)

	// Parse builtin and dependencies first
	mut ast_files := []ast.File{}
	ast_files << p.parse_files(get_v_files_from_dir(prefs.get_vlib_module_path('builtin')), mut
		file_set)
	ast_files << p.parse_files(get_v_files_from_dir(prefs.get_vlib_module_path('strconv')), mut
		file_set)
	ast_files << p.parse_files(get_v_files_from_dir(prefs.get_vlib_module_path('strings')), mut
		file_set)
	ast_files << p.parse_files(get_v_files_from_dir(prefs.get_vlib_module_path('hash')), mut
		file_set)
	ast_files << p.parse_files(get_v_files_from_dir(prefs.get_vlib_module_path('math.bits')), mut
		file_set)

	// Parse the user file(s)
	ast_files << p.parse_files(files, mut file_set)

	// Write pre-transformation AST (only user files, not builtin)
	user_files := ast_files.filter(it.name in files)
	ast_json := ast_dump.dump_files(user_files)
	ast_file := '${base_name}_ast.json'
	os.write_file(ast_file, ast_json) or {
		eprintln('Failed to write ${ast_file}: ${err}')
		exit(1)
	}
	println('Wrote ${ast_file}')

	// Type check
	env := types.Environment.new()
	mut checker := types.Checker.new(&prefs, file_set, env)
	checker.check_files(ast_files)

	// Transform
	mut trans := transformer.Transformer.new_with_pref(ast_files, env, &prefs)
	transformed_files := trans.transform_files(ast_files)

	// Write post-transformation AST (only user files)
	transformed_user_files := transformed_files.filter(it.name in files)
	transformed_json := ast_dump.dump_files(transformed_user_files)
	transformed_file := '${base_name}_ast_transformed.json'
	os.write_file(transformed_file, transformed_json) or {
		eprintln('Failed to write ${transformed_file}: ${err}')
		exit(1)
	}
	println('Wrote ${transformed_file}')
}

// get_files extracts source files from args, excluding options and their values
fn get_files(args []string) []string {
	options_with_values := ['-o', '-output']
	mut files := []string{}
	mut skip_next := false
	for arg in args {
		if skip_next {
			skip_next = false
			continue
		}
		if arg.starts_with('-') {
			if arg in options_with_values {
				skip_next = true
			}
			continue
		}
		files << arg
	}
	return files
}

fn get_v_files_from_dir(dir string) []string {
	mut v_files := []string{}
	all_files := os.ls(dir) or { return v_files }
	for file in all_files {
		if file.ends_with('.v') && !file.ends_with('_test.v') {
			// Skip platform-specific files that don't match current platform
			if should_include_file(file) {
				v_files << os.join_path(dir, file)
			}
		}
	}
	return v_files
}

fn should_include_file(filename string) bool {
	// Skip test files (including _test.c.v)
	if filename.contains('_test.') {
		return false
	}
	// Skip JavaScript backend files
	if filename.ends_with('.js.v') {
		return false
	}
	// Handle platform-specific files
	current_os := os.user_os()
	// Check for OS-specific suffixes like _linux.v, _windows.v, _macos.v
	os_suffixes := ['_linux.v', '_windows.v', '_macos.v', '_darwin.v', '_freebsd.v', '_openbsd.v',
		'_netbsd.v', '_solaris.v', '_haiku.v', '_android.v']
	for suffix in os_suffixes {
		if filename.ends_with(suffix) {
			// Include only if it matches current OS
			if suffix == '_darwin.v' || suffix == '_macos.v' {
				return current_os == 'macos'
			} else if suffix == '_linux.v' {
				return current_os == 'linux'
			} else if suffix == '_windows.v' {
				return current_os == 'windows'
			}
			return false
		}
	}
	// Check for "not this OS" files like _nix.v (not windows)
	if filename.ends_with('_nix.v') {
		return current_os != 'windows'
	}
	return true
}
