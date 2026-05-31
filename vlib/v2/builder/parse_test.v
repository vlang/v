// vtest build: macos
module builder

import os
import v2.ast
import v2.pref

fn test_parse_files_keeps_single_file_inputs_isolated() {
	tmp_dir := os.join_path(os.temp_dir(), 'v2_builder_parse_single_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}

	entry_file := os.join_path(tmp_dir, 'hello.v')
	sibling_file := os.join_path(tmp_dir, 'extra.v')
	os.write_file(entry_file, 'module main\nfn main() {}\n') or { panic(err) }
	os.write_file(sibling_file, 'module main\nfn extra() {}\n') or { panic(err) }

	mut prefs := pref.new_preferences()
	prefs.skip_builtin = true
	prefs.skip_imports = true
	mut b := new_builder(&prefs)
	files := b.parse_files([entry_file])

	assert files.len == 1
}

fn test_parse_files_expands_directory_inputs() {
	tmp_dir := os.join_path(os.temp_dir(), 'v2_builder_parse_dir_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}

	entry_file := os.join_path(tmp_dir, 'hello.v')
	sibling_file := os.join_path(tmp_dir, 'extra.v')
	os.write_file(entry_file, 'module main\nfn main() {}\n') or { panic(err) }
	os.write_file(sibling_file, 'module main\nfn extra() {}\n') or { panic(err) }

	mut prefs := pref.new_preferences()
	prefs.skip_builtin = true
	prefs.skip_imports = true
	mut b := new_builder(&prefs)
	files := b.parse_files([tmp_dir])

	assert files.len == 2
}

fn test_parse_files_expands_same_module_subdirectories() {
	tmp_dir := os.join_path(os.temp_dir(), 'v2_builder_parse_dir_recursive_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(os.join_path(tmp_dir, 'user')) or { panic(err) }
	os.mkdir_all(os.join_path(tmp_dir, 'api')) or { panic(err) }
	os.mkdir_all(os.join_path(tmp_dir, 'nested_app')) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}

	entry_file := os.join_path(tmp_dir, 'main.v')
	user_file := os.join_path(tmp_dir, 'user', 'user.v')
	api_file := os.join_path(tmp_dir, 'api', 'api.v')
	nested_file := os.join_path(tmp_dir, 'nested_app', 'main.v')
	os.write_file(entry_file, 'module main\nfn main() {}\n') or { panic(err) }
	os.write_file(user_file, 'module main\nstruct User {}\n') or { panic(err) }
	os.write_file(api_file, 'module api\nstruct Payload {}\n') or { panic(err) }
	os.write_file(os.join_path(tmp_dir, 'nested_app', 'v.mod'), 'Module { name: "nested" }\n') or {
		panic(err)
	}
	os.write_file(nested_file, 'module main\nfn nested_main() {}\n') or { panic(err) }

	mut prefs := pref.new_preferences()
	prefs.skip_builtin = true
	prefs.skip_imports = true
	mut b := new_builder(&prefs)
	files := b.parse_files([tmp_dir])
	names := files.map(os.file_name(it.name))

	assert files.len == 2
	assert 'main.v' in names
	assert 'user.v' in names
}

fn test_parse_files_expands_non_main_module_files() {
	tmp_dir := os.join_path(os.temp_dir(), 'v2_builder_parse_module_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}

	entry_file := os.join_path(tmp_dir, 'math_test.v')
	sibling_file := os.join_path(tmp_dir, 'math.v')
	os.write_file(entry_file, 'module math\nconst x = twice(21)\n') or { panic(err) }
	os.write_file(sibling_file, 'module math\nfn twice(v int) int { return v * 2 }\n') or {
		panic(err)
	}

	mut prefs := pref.new_preferences()
	prefs.skip_builtin = true
	prefs.skip_imports = true
	mut b := new_builder(&prefs)
	files := b.parse_files([entry_file])

	assert files.len == 2
}

fn test_parse_files_resolves_project_root_sibling_import_from_nested_module_file() {
	tmp_dir := os.join_path(os.temp_dir(), 'v2_builder_parse_nested_import_${os.getpid()}')
	flags_dir := os.join_path(tmp_dir, 'core', 'flags')
	ignore_dir := os.join_path(tmp_dir, 'ignore')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(flags_dir) or { panic(err) }
	os.mkdir_all(ignore_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}

	entry_file := os.join_path(flags_dir, 'complete_test.v')
	ignore_file := os.join_path(ignore_dir, 'lib.v')
	os.write_file(os.join_path(tmp_dir, 'v.mod'), "Module { name: 'nested_import' }\n") or {
		panic(err)
	}
	os.write_file(entry_file, 'module flags\nimport ignore\nfn test_use_import() {}\n') or {
		panic(err)
	}
	os.write_file(ignore_file, 'module ignore\npub struct Marker {}\n') or { panic(err) }

	mut prefs := pref.new_preferences()
	prefs.skip_builtin = true
	mut b := new_builder(&prefs)
	files := b.parse_files([entry_file])
	names := files.map(os.file_name(it.name))

	assert 'complete_test.v' in names
	assert 'lib.v' in names
}

fn test_parse_files_prefers_project_root_module_over_vlib_module() {
	tmp_dir := os.join_path(os.temp_dir(), 'v2_builder_parse_local_shadow_${os.getpid()}')
	app_dir := os.join_path(tmp_dir, 'app')
	regex_dir := os.join_path(tmp_dir, 'regex')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(app_dir) or { panic(err) }
	os.mkdir_all(regex_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}

	entry_file := os.join_path(app_dir, 'main.v')
	local_regex_file := os.join_path(regex_dir, 'local_regex.v')
	os.write_file(os.join_path(tmp_dir, 'v.mod'), "Module { name: 'local_shadow' }\n") or {
		panic(err)
	}
	os.write_file(entry_file, 'module main\nimport regex\nfn main() {}\n') or { panic(err) }
	os.write_file(local_regex_file, 'module regex\npub struct RegexMatcher {}\n') or { panic(err) }

	mut prefs := pref.new_preferences()
	prefs.skip_builtin = true
	mut b := new_builder(&prefs)
	files := b.parse_files([entry_file])

	assert files.any(it.name == local_regex_file)
}

fn test_active_file_imports_follow_comptime_else_branch() {
	file := ast.File{
		mod:   'main'
		name:  'main.v'
		stmts: [
			ast.Stmt(ast.ExprStmt{
				expr: ast.ComptimeExpr{
					expr: ast.IfExpr{
						cond:      ast.Ident{
							name: 'linux'
						}
						stmts:     [
							ast.Stmt(ast.ImportStmt{
								name:  'net.openssl'
								alias: 'openssl'
							}),
						]
						else_expr: ast.IfExpr{
							cond:  ast.empty_expr
							stmts: [
								ast.Stmt(ast.ImportStmt{
									name:  'net.mbedtls'
									alias: 'mbedtls'
								}),
							]
						}
					}
				}
			}),
		]
	}
	imports := active_file_imports(file, [], 'mac')

	assert imports.len == 1
	assert imports[0].name == 'net.mbedtls'
}
