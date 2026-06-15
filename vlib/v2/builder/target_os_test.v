module builder

import os
import v2.ast
import v2.abi
import v2.gen.x64
import v2.insel
import v2.markused
import v2.mir
import v2.pref
import v2.ssa
import v2.ssa.optimize as ssa_optimize
import v2.transformer
import v2.types

fn write_test_file(path string) {
	os.write_file(path, 'module main\nfn marker() {}\n') or { panic(err) }
}

fn parse_test_files(mut b Builder, paths []string) []ast.File {
	b.parse_files(paths)
	b.flat = b.flat_builder.flat
	return b.flat.to_files()
}

fn parse_test_builder_files(mut b Builder, paths []string) {
	b.files = b.parse_files(paths)
	b.flat = b.flat_builder.flat
}

fn transform_test_builder_files(mut b Builder, mut trans transformer.Transformer) {
	new_flat, files_out := trans.transform_files_to_flat(&b.flat, b.files)
	b.flat = new_flat
	b.files = files_out
}

fn mark_used_windows_x64_test(mut b Builder) map[string]bool {
	opts := markused.MarkUsedOptions{
		minimal_runtime_roots: true
	}
	return markused.mark_used_flat_with_options(&b.flat, b.env, opts)
}

fn build_test_ssa(mut b Builder, mut ssa_builder ssa.Builder) {
	ssa_builder.build_all_from_flat(&b.flat)
}

fn test_flat_codegen_backends_keep_flat_for_codegen() {
	mut prefs := pref.Preferences{
		backend: .c
	}
	mut b := Builder{
		pref: &prefs
	}

	assert b.should_keep_flat_for_codegen()
	b.flat = ast.FlatAst{
		files: [ast.FlatFile{}]
	}
	assert b.should_build_ssa_from_flat()

	prefs.backend = .cleanc
	assert b.should_keep_flat_for_codegen()
	prefs.backend = .v
	assert !b.should_keep_flat_for_codegen()
	prefs.backend = .x64
	assert b.should_keep_flat_for_codegen()
}

fn test_gen_ssa_c_consumes_flat_codegen_input() {
	tmp_dir := os.join_path(os.temp_dir(), 'v2_builder_ssa_c_flat_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}

	output_path := os.join_path(tmp_dir, 'ssa_flat.c')
	files := [
		ast.File{
			name:  'main.v'
			mod:   'main'
			stmts: [
				ast.Stmt(ast.ModuleStmt{
					name: 'main'
				}),
			]
		},
	]
	mut prefs := pref.Preferences{
		backend:     .c
		output_file: output_path
	}
	mut b := Builder{
		pref:  &prefs
		files: files
		env:   types.Environment.new()
		flat:  ast.flatten_files(files)
	}

	b.gen_ssa_c()

	assert b.flat.files.len == 0
	assert os.exists(output_path)
	c_source := os.read_file(output_path) or { panic(err) }
	assert c_source.len > 0
}

fn test_get_v_files_from_dir_uses_windows_target_os() {
	tmp_dir := os.join_path(os.temp_dir(), 'v2_builder_filter_windows_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}

	write_test_file(os.join_path(tmp_dir, 'common.v'))
	write_test_file(os.join_path(tmp_dir, 'platform_windows.v'))
	write_test_file(os.join_path(tmp_dir, 'platform_nix.v'))
	write_test_file(os.join_path(tmp_dir, 'platform_termux.c.v'))
	write_test_file(os.join_path(tmp_dir, 'platform_linux.v'))
	write_test_file(os.join_path(tmp_dir, 'platform_darwin.v'))

	names := get_v_files_from_dir(tmp_dir, []string{}, 'windows').map(os.file_name(it))
	assert 'common.v' in names
	assert 'platform_windows.v' in names
	assert 'platform_nix.v' !in names
	assert 'platform_termux.c.v' !in names
	assert 'platform_linux.v' !in names
	assert 'platform_darwin.v' !in names
}

fn test_get_v_files_from_dir_uses_linux_and_macos_target_os() {
	tmp_dir := os.join_path(os.temp_dir(), 'v2_builder_filter_unix_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}

	write_test_file(os.join_path(tmp_dir, 'common.v'))
	write_test_file(os.join_path(tmp_dir, 'platform_windows.v'))
	write_test_file(os.join_path(tmp_dir, 'platform_nix.v'))
	write_test_file(os.join_path(tmp_dir, 'platform_termux.c.v'))
	write_test_file(os.join_path(tmp_dir, 'platform_linux.v'))
	write_test_file(os.join_path(tmp_dir, 'platform_macos.v'))
	write_test_file(os.join_path(tmp_dir, 'platform_darwin.v'))

	linux_names := get_v_files_from_dir(tmp_dir, []string{}, 'linux').map(os.file_name(it))
	assert 'common.v' in linux_names
	assert 'platform_nix.v' in linux_names
	assert 'platform_linux.v' in linux_names
	assert 'platform_windows.v' !in linux_names
	assert 'platform_termux.c.v' !in linux_names
	assert 'platform_macos.v' !in linux_names
	assert 'platform_darwin.v' !in linux_names

	macos_names := get_v_files_from_dir(tmp_dir, []string{}, 'macos').map(os.file_name(it))
	assert 'common.v' in macos_names
	assert 'platform_nix.v' in macos_names
	assert 'platform_macos.v' in macos_names
	assert 'platform_darwin.v' in macos_names
	assert 'platform_windows.v' !in macos_names
	assert 'platform_termux.c.v' !in macos_names
	assert 'platform_linux.v' !in macos_names
}

fn test_get_v_files_from_dir_uses_termux_target_os() {
	tmp_dir := os.join_path(os.temp_dir(), 'v2_builder_filter_termux_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}

	write_test_file(os.join_path(tmp_dir, 'common.v'))
	write_test_file(os.join_path(tmp_dir, 'platform_nix.v'))
	write_test_file(os.join_path(tmp_dir, 'platform_termux.c.v'))
	write_test_file(os.join_path(tmp_dir, 'platform_android_outside_termux.c.v'))
	write_test_file(os.join_path(tmp_dir, 'platform_android.c.v'))
	write_test_file(os.join_path(tmp_dir, 'platform_linux.v'))
	write_test_file(os.join_path(tmp_dir, 'platform_windows.v'))
	write_test_file(os.join_path(tmp_dir, 'platform_macos.v'))
	write_test_file(os.join_path(tmp_dir, 'platform_darwin.v'))

	termux_names := get_v_files_from_dir(tmp_dir, []string{}, 'termux').map(os.file_name(it))
	assert 'common.v' in termux_names
	assert 'platform_nix.v' in termux_names
	assert 'platform_termux.c.v' in termux_names
	assert 'platform_android.c.v' in termux_names
	assert 'platform_android_outside_termux.c.v' !in termux_names
	assert 'platform_linux.v' !in termux_names
	assert 'platform_windows.v' !in termux_names
	assert 'platform_macos.v' !in termux_names
	assert 'platform_darwin.v' !in termux_names
}

fn test_parse_files_uses_host_source_filter_for_cross_target() {
	tmp_dir := os.join_path(os.temp_dir(), 'v2_builder_filter_cross_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}

	write_test_file(os.join_path(tmp_dir, 'common.v'))
	write_test_file(os.join_path(tmp_dir, 'platform_nix.v'))
	write_test_file(os.join_path(tmp_dir, 'platform_linux.v'))
	write_test_file(os.join_path(tmp_dir, 'platform_macos.v'))
	write_test_file(os.join_path(tmp_dir, 'platform_darwin.v'))
	write_test_file(os.join_path(tmp_dir, 'platform_windows.v'))
	write_test_file(os.join_path(tmp_dir, 'platform_termux.c.v'))

	mut prefs := pref.new_preferences()
	prefs.skip_builtin = true
	prefs.skip_imports = true
	prefs.target_os = 'cross'
	prefs.output_cross_c = true
	prefs.user_defines = ['cross']
	mut b := new_builder(&prefs)
	files := parse_test_files(mut b, [tmp_dir])
	cross_names := files.map(os.file_name(it.name))
	host_os := normalize_target_os_name(os.user_os())
	assert 'common.v' in cross_names
	assert ('platform_nix.v' in cross_names) == (host_os != 'windows')
	assert ('platform_linux.v' in cross_names) == (host_os == 'linux')
	assert ('platform_macos.v' in cross_names) == (host_os == 'macos')
	assert ('platform_darwin.v' in cross_names) == (host_os == 'macos')
	assert ('platform_windows.v' in cross_names) == (host_os == 'windows')
	assert ('platform_termux.c.v' in cross_names) == (host_os == 'termux')
}

fn test_parse_files_excludes_os_variants_for_freestanding_none_target() {
	tmp_dir := os.join_path(os.temp_dir(), 'v2_builder_filter_none_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}

	write_test_file(os.join_path(tmp_dir, 'common.v'))
	write_test_file(os.join_path(tmp_dir, 'platform_nix.v'))
	write_test_file(os.join_path(tmp_dir, 'platform_linux.v'))
	write_test_file(os.join_path(tmp_dir, 'platform_macos.v'))
	write_test_file(os.join_path(tmp_dir, 'platform_darwin.v'))
	write_test_file(os.join_path(tmp_dir, 'platform_windows.v'))
	write_test_file(os.join_path(tmp_dir, 'platform_termux.c.v'))

	mut prefs := pref.new_preferences()
	prefs.skip_builtin = true
	prefs.skip_imports = true
	prefs.freestanding = true
	prefs.target_os = 'none'
	prefs.user_defines = ['freestanding']
	mut b := new_builder(&prefs)
	files := parse_test_files(mut b, [tmp_dir])
	names := files.map(os.file_name(it.name))
	assert 'common.v' in names
	assert 'platform_nix.v' !in names
	assert 'platform_termux.c.v' !in names
	assert 'platform_linux.v' !in names
	assert 'platform_macos.v' !in names
	assert 'platform_darwin.v' !in names
	assert 'platform_windows.v' !in names
}

fn test_parse_files_uses_target_os_preference_for_windows_files() {
	tmp_dir := os.join_path(os.temp_dir(), 'v2_builder_parse_windows_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}

	os.write_file(os.join_path(tmp_dir, 'main.v'), 'module main\nfn main() {}\n') or { panic(err) }
	write_test_file(os.join_path(tmp_dir, 'platform_windows.v'))
	write_test_file(os.join_path(tmp_dir, 'platform_nix.v'))
	write_test_file(os.join_path(tmp_dir, 'platform_termux.c.v'))
	write_test_file(os.join_path(tmp_dir, 'platform_linux.v'))
	write_test_file(os.join_path(tmp_dir, 'platform_darwin.v'))

	mut prefs := pref.new_preferences()
	prefs.skip_builtin = true
	prefs.skip_imports = true
	prefs.target_os = 'windows'
	mut b := new_builder(&prefs)
	files := parse_test_files(mut b, [tmp_dir])
	names := files.map(os.file_name(it.name))

	assert 'main.v' in names
	assert 'platform_windows.v' in names
	assert 'platform_nix.v' !in names
	assert 'platform_termux.c.v' !in names
	assert 'platform_linux.v' !in names
	assert 'platform_darwin.v' !in names
}

fn test_default_file_key_strips_all_supported_platform_suffixes() {
	default_key := fname_without_platform_postfix('/tmp/foo_default.c.v')
	for suffix in ['nix', 'windows', 'linux', 'darwin', 'macos', 'bsd', 'android', 'termux',
		'android_outside_termux', 'ios', 'freebsd', 'openbsd', 'netbsd', 'dragonfly', 'solaris',
		'qnx', 'serenity', 'plan9', 'vinix'] {
		assert fname_without_platform_postfix('/tmp/foo_${suffix}.c.v') == default_key
	}
}

fn test_default_files_are_skipped_for_extended_platform_variants() {
	tmp_dir := os.join_path(os.temp_dir(), 'v2_builder_extended_platform_defaults_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}

	for target in ['ios', 'qnx', 'serenity', 'plan9', 'vinix'] {
		write_test_file(os.join_path(tmp_dir, 'foo_default.c.v'))
		write_test_file(os.join_path(tmp_dir, 'foo_${target}.c.v'))
		names := get_v_files_from_dir(tmp_dir, []string{}, target).map(os.file_name(it))
		assert 'foo_${target}.c.v' in names
		assert 'foo_default.c.v' !in names
		os.rm(os.join_path(tmp_dir, 'foo_default.c.v')) or {}
		os.rm(os.join_path(tmp_dir, 'foo_${target}.c.v')) or {}
	}
}

fn test_active_file_imports_filters_conditional_imports_by_target_os() {
	file := ast.File{
		mod:   'main'
		name:  'main.v'
		stmts: [
			ast.Stmt(ast.ExprStmt{
				expr: ast.ComptimeExpr{
					expr: ast.IfExpr{
						cond:  ast.Ident{
							name: 'linux'
						}
						stmts: [
							ast.Stmt(ast.ImportStmt{
								name: 'linmod'
							}),
						]
					}
				}
			}),
			ast.Stmt(ast.ExprStmt{
				expr: ast.ComptimeExpr{
					expr: ast.IfExpr{
						cond:  ast.Ident{
							name: 'windows'
						}
						stmts: [
							ast.Stmt(ast.ImportStmt{
								name: 'winmod'
							}),
						]
					}
				}
			}),
			ast.Stmt(ast.ExprStmt{
				expr: ast.ComptimeExpr{
					expr: ast.IfExpr{
						cond:  ast.Ident{
							name: 'macos'
						}
						stmts: [
							ast.Stmt(ast.ImportStmt{
								name: 'macmod'
							}),
						]
					}
				}
			}),
			ast.Stmt(ast.ExprStmt{
				expr: ast.ComptimeExpr{
					expr: ast.IfExpr{
						cond:  ast.Ident{
							name: 'none'
						}
						stmts: [
							ast.Stmt(ast.ImportStmt{
								name: 'nonemod'
							}),
						]
					}
				}
			}),
		]
	}
	windows_names := active_file_imports(file, [], 'windows').map(it.name)
	linux_names := active_file_imports(file, [], 'linux').map(it.name)
	macos_names := active_file_imports(file, [], 'macos').map(it.name)
	none_names := active_file_imports(file, [], 'none').map(it.name)
	windows_with_linux_define_names := active_file_imports_with_explicit(file, [
		'linux',
	], [
		'linux',
	], 'windows').map(it.name)

	assert 'winmod' in windows_names
	assert 'linmod' !in windows_names
	assert 'macmod' !in windows_names
	assert 'nonemod' !in windows_names

	assert 'linmod' in linux_names
	assert 'winmod' !in linux_names
	assert 'macmod' !in linux_names
	assert 'nonemod' !in linux_names

	assert 'macmod' in macos_names
	assert 'winmod' !in macos_names
	assert 'linmod' !in macos_names
	assert 'nonemod' !in macos_names

	assert 'nonemod' in none_names
	assert 'linmod' !in none_names
	assert 'winmod' !in none_names
	assert 'macmod' !in none_names

	assert 'winmod' in windows_with_linux_define_names
	assert 'linmod' !in windows_with_linux_define_names
	assert 'macmod' !in windows_with_linux_define_names
	assert 'nonemod' !in windows_with_linux_define_names
}

fn test_header_cache_stamp_uses_target_os_preference() {
	mut linux_prefs := pref.new_preferences()
	linux_prefs.skip_builtin = true
	linux_prefs.target_os = 'linux'
	linux_builder := new_builder(&linux_prefs)
	linux_stamp := linux_builder.header_stamp_for_modules([])

	mut windows_prefs := pref.new_preferences()
	windows_prefs.skip_builtin = true
	windows_prefs.target_os = 'windows'
	windows_builder := new_builder(&windows_prefs)
	windows_stamp := windows_builder.header_stamp_for_modules([])

	assert linux_stamp.contains('target_os=linux')
	assert windows_stamp.contains('target_os=windows')
	assert linux_stamp != windows_stamp
}

fn test_cached_called_fn_names_are_persisted_for_reused_objects() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v2_cached_called_names_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	prefs := pref.new_preferences()
	mut b := new_builder(&prefs)
	b.cached_called_fn_names['local__already'] = true
	b.cached_called_fn_names['json2__decode_T_Foo'] = true
	b.write_cached_called_fn_names(tmp_dir, 'virtuals', {
		'local__already': true
	})

	mut b2 := new_builder(&prefs)
	assert b2.load_cached_called_fn_names(tmp_dir, 'virtuals')
	assert 'json2__decode_T_Foo' in b2.cached_called_fn_names
	assert 'local__already' !in b2.cached_called_fn_names
	assert !b2.load_cached_called_fn_names(tmp_dir, 'missing')
}

fn test_vh_cache_reuse_requires_cached_called_fn_metadata() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v2_cached_called_names_required_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	for cache_name in [builtin_cache_name, imports_cache_name, virtuals_cache_name] {
		os.write_file(cache_path_join(tmp_dir, '${cache_name}.o'), 'stale object') or { panic(err) }
		os.write_file(cache_path_join(tmp_dir, '${cache_name}.stamp'), 'stale stamp') or {
			panic(err)
		}
	}
	prefs := pref.new_preferences()

	mut module_builder := new_builder(&prefs)
	module_builder.used_vh_for_parse = true
	mut module_reused := true
	_ := module_builder.ensure_cached_module_object(tmp_dir, builtin_cache_name, [], [], 'cc', '',
		'', '', false) or {
		module_reused = false
		''
	}
	assert !module_reused

	mut parsed_builder := new_builder(&prefs)
	parsed_builder.used_import_vh_for_parse = true
	mut parsed_reused := true
	_ := parsed_builder.ensure_cached_parsed_module_object(tmp_dir, imports_cache_name, [], [],
		'cc', '', '', '', false) or {
		parsed_reused = false
		''
	}
	assert !parsed_reused

	mut virtual_builder := new_builder(&prefs)
	virtual_builder.used_virtual_vh_for_parse = true
	mut virtual_reused := true
	_ := virtual_builder.ensure_cached_virtual_module_object(tmp_dir, [], [], 'cc', '', '', '',
		false) or {
		virtual_reused = false
		''
	}
	assert !virtual_reused
}

fn test_linux_x64_builder_default_o0_forces_ssa_optimize() {
	if os.user_os() != 'linux' || os.uname().machine !in ['x86_64', 'amd64'] {
		return
	}
	tmp_dir := os.join_path(os.temp_dir(), 'v2_builder_linux_x64_default_o0_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}

	main_path := os.join_path(tmp_dir, 'main.v')
	out_path := os.join_path(tmp_dir, 'main')
	os.write_file(main_path, "module main\nfn main() { println('ok') }\n") or { panic(err) }

	mut prefs := pref.new_preferences_from_args(['-backend', 'x64', '-arch', 'x64', '-o', out_path,
		'-no-parallel', '-nocache'])
	assert prefs.no_optimize
	prefs.target_os = 'linux'

	cwd := os.getwd()
	os.chdir(tmp_dir) or { panic(err) }
	defer {
		os.chdir(cwd) or {}
	}
	mut b := new_builder(&prefs)
	b.build([main_path])

	run := os.execute(os.quoted_path(out_path))
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}

struct WindowsX64BuildResult {
	undefined_symbols []string
	built_functions   []string
	image             []u8
}

fn windows_x64_map_string_wyhash_source() string {
	return "module main

fn sink_map(_ map[string]int) {}

fn main() {
	graph := {
		'A': 1
		'B': 2
	}
	sink_map(graph)
}
"
}

fn assert_windows_x64_map_string_wyhash_resolved(res WindowsX64BuildResult) {
	assert 'wyhash' in res.built_functions, res.built_functions.str()
	assert 'wyhash' !in res.undefined_symbols, res.undefined_symbols.str()
}

fn test_windows_x64_empty_main_links_without_crt_or_darwin_symbols() {
	res := build_windows_x64_sample('module main\nfn main() {}\n', false, true)

	assert 'calloc' !in res.undefined_symbols, res.undefined_symbols.str()
	assert '_errno' !in res.undefined_symbols, res.undefined_symbols.str()
	assert '__stdoutp' !in res.undefined_symbols, res.undefined_symbols.str()
	assert '__stderrp' !in res.undefined_symbols, res.undefined_symbols.str()
	assert '__stdinp' !in res.undefined_symbols, res.undefined_symbols.str()
	assert '__error' !in res.undefined_symbols, res.undefined_symbols.str()

	linked := build_windows_x64_sample('module main\nfn main() {}\n', true, true)
	assert linked.image.len > 0
	assert linked.image[0] == `M`
	assert linked.image[1] == `Z`
}

fn test_windows_x64_c_globals_do_not_use_darwin_symbols() {
	source := 'module main
fn C.sink_ptr(voidptr)
fn C.sink_int(int)

fn main() {
	C.sink_ptr(C.stdout)
	C.sink_ptr(C.stderr)
	C.sink_ptr(C.stdin)
	C.sink_int(C.errno)
}
'
	res := build_windows_x64_sample(source, false, false)

	assert '__stdoutp' !in res.undefined_symbols
	assert '__stderrp' !in res.undefined_symbols
	assert '__stdinp' !in res.undefined_symbols
	assert '__error' !in res.undefined_symbols
	assert 'stdout' in res.undefined_symbols
	assert 'stderr' in res.undefined_symbols
	assert 'stdin' in res.undefined_symbols
	assert '_errno' in res.undefined_symbols
	assert 'errno' !in res.undefined_symbols
}

fn test_windows_x64_c_errno_links_with_minimal_runtime() {
	linked := build_windows_x64_sample('module main

fn main() {
	C.errno = 0
	C.errno += 1
}
',
		true, true)

	assert linked.image.len > 0
	assert linked.image[0] == `M`
	assert linked.image[1] == `Z`
}

fn test_windows_x64_minimal_runtime_builds_markused_core_functions() {
	res := build_windows_x64_sample("module main\nfn main() { println('x') }\n", false, false)

	assert 'builtin__println' in res.built_functions, res.built_functions.str()
	assert 'stderr' !in res.undefined_symbols, res.undefined_symbols.str()
	assert_no_windows_minimal_runtime_retention(res.built_functions)
}

fn test_windows_x64_minimal_runtime_map_string_generates_referenced_wyhash_stub_sequential() {
	res := build_windows_x64_sample(windows_x64_map_string_wyhash_source(), false, true)
	assert_windows_x64_map_string_wyhash_resolved(res)
}

fn test_windows_x64_minimal_runtime_map_string_generates_referenced_wyhash_stub_flat() {
	res :=
		build_windows_x64_sample_configured(windows_x64_map_string_wyhash_source(), false, true, true)
	assert_windows_x64_map_string_wyhash_resolved(res)
}

fn test_windows_x64_minimal_runtime_map_string_generates_referenced_wyhash_stub_native_builder_path() {
	res := build_windows_x64_sample_configured(windows_x64_map_string_wyhash_source(), false, true,
		false)
	assert_windows_x64_map_string_wyhash_resolved(res)
}

fn test_windows_x64_minimal_runtime_os_getwd_markused_does_not_retain_stdio_file_helpers() {
	used := build_windows_x64_markused_sample('module main
import os

fn main() {
	wd := os.getwd()
	_ = wd.len
}
')

	assert used['main|f|main'], used.str()
	assert used['os|f|getwd'], used.str()
	assert_no_windows_minimal_targeted_stdio_markused_leaks(used)
	assert !used['os|f|get_raw_line'], used.str()
	assert !used['os|f|get_raw_stdin'], used.str()
	assert !used['os|f|is_atty'], used.str()
	assert !used['os|f|input_password'], used.str()
}

fn test_windows_x64_minimal_runtime_os_getwd_build_does_not_retain_stdin_helpers() {
	res := build_windows_x64_sample('module main
import os

fn main() {
	wd := os.getwd()
	if wd.len == 0 {
		return
	}
}
',
		false, false)

	assert 'os__getwd' in res.built_functions, res.built_functions.str()
	assert 'os__get_raw_line' !in res.built_functions, res.built_functions.str()
	assert 'os__get_raw_stdin' !in res.built_functions, res.built_functions.str()
	assert 'os__is_atty' !in res.built_functions, res.built_functions.str()
	assert 'os__input_password' !in res.built_functions, res.built_functions.str()
	assert 'STD_INPUT_HANDLE' !in res.undefined_symbols, res.undefined_symbols.str()
	assert '_get_osfhandle' !in res.undefined_symbols, res.undefined_symbols.str()
}

fn test_windows_x64_minimal_runtime_get_raw_line_uses_kernel32_stdin_helpers() {
	res := build_windows_x64_sample('module main
import os

fn main() {
	line := os.get_raw_line()
	_ = line.len
}
',
		false, false)

	assert 'os__get_raw_line' in res.built_functions, res.built_functions.str()
	assert 'os__is_atty' in res.built_functions, res.built_functions.str()
	assert 'GetStdHandle' in res.undefined_symbols, res.undefined_symbols.str()
	assert 'GetConsoleMode' in res.undefined_symbols, res.undefined_symbols.str()
	assert 'ReadConsole' in res.undefined_symbols, res.undefined_symbols.str()
	assert 'ReadFile' in res.undefined_symbols, res.undefined_symbols.str()
	assert 'STD_INPUT_HANDLE' !in res.undefined_symbols, res.undefined_symbols.str()
	assert 'INVALID_HANDLE_VALUE' !in res.undefined_symbols, res.undefined_symbols.str()
	assert '_get_osfhandle' !in res.undefined_symbols, res.undefined_symbols.str()
	assert 'stdin' !in res.undefined_symbols, res.undefined_symbols.str()
	assert 'stdout' !in res.undefined_symbols, res.undefined_symbols.str()
	assert 'stderr' !in res.undefined_symbols, res.undefined_symbols.str()
}

fn test_windows_x64_invalid_handle_value_macro_does_not_become_undefined_symbol() {
	res := build_windows_x64_sample('module main

fn invalid_handle() voidptr {
	return C.INVALID_HANDLE_VALUE
}

fn main() {
	_ := invalid_handle()
}
',
		false, false)

	assert 'INVALID_HANDLE_VALUE' !in res.undefined_symbols, res.undefined_symbols.str()
}

fn test_windows_x64_minimal_runtime_prunes_crt_stdio_else_branch_before_markused() {
	used := build_windows_x64_markused_sample('module main
import os

fn C.sink_ptr(voidptr)
fn C.setvbuf(voidptr, voidptr, int, usize) int

fn minimal_marker() {}

fn guarded_stdio_branch() {
	$if v2_native_windows_pe_minimal ? {
		minimal_marker()
	} $else {
		C.sink_ptr(C.stdout)
		C.sink_ptr(C.stderr)
		C.setvbuf(C.stdout, 0, 0, usize(0))
		unbuffer_stdout()
		os.stdout()
		os.stderr()
	}
}

fn main() {
	guarded_stdio_branch()
}
')

	assert used['main|f|main'], used.str()
	assert used['main|f|guarded_stdio_branch'], used.str()
	assert used['main|f|minimal_marker'], used.str()
	assert !used['builtin|f|unbuffer_stdout'], used.str()
	assert !used['os|f|stdout'], used.str()
	assert !used['os|f|stderr'], used.str()
	assert !windows_x64_markused_key_contains(used, 'setvbuf'), used.str()
	assert !windows_x64_markused_key_contains(used, '|f|stdout'), used.str()
	assert !windows_x64_markused_key_contains(used, '|f|stderr'), used.str()
}

fn test_windows_x64_minimal_runtime_dynamic_array_markused_does_not_retain_crt_stdio() {
	used := build_windows_x64_markused_sample("module main

fn main() {
	mut a := [1, 2, 3]
	a[1] = 7
	a << 11
	mut i := 0
	mut sum := 0
	for i < a.len {
		sum += a[i]
		i += 1
	}
	if sum == 22 {
		println('ok')
	}
}
")

	assert used['main|f|main'], used.str()
	assert used['builtin|f|println'], used.str()
	assert_no_windows_minimal_targeted_stdio_markused_leaks(used)
}

fn test_windows_x64_minimal_runtime_fixed_array_slice_copy_markused_does_not_retain_crt_stdio() {
	used := build_windows_x64_markused_sample("module main

fn main() {
	mut f := [5]int{}
	f[0] = 2
	f[1] = 4
	f[2] = 6
	f[3] = 8
	f[4] = 10
	mut a := f[1..4]
	a[1] = 9
	a << 12
	if a.len == 4 && f[2] == 6 {
		println('ok')
	}
}
")

	assert used['main|f|main'], used.str()
	assert used['builtin|f|println'], used.str()
	assert_no_windows_minimal_targeted_stdio_markused_leaks(used)
}

fn test_windows_x64_array_slice_call_uses_sret_and_integer_bounds() {
	mir_mod := build_checked_windows_x64_mir_sample("module main

fn make_array() []int {
	return [1, 2, 3]
}

fn main() {
	mut a := make_array()
	if a.len == 3 && a.cap >= 3 && a[0] == 1 && a[2] == 3 {
		print('N')
	} else {
		print('n')
	}
	a << 4
	if a.len == 4 && a[1] == 2 && a[3] == 4 {
		print('G')
	} else {
		print('g')
	}
	b := a[1..3]
	if b.len == 2 && b[0] == 2 && b[1] == 3 {
		print('S')
	} else {
		print('s')
	}
	unsafe {
		a.free()
	}
	println('F')
}
", true)
	mut found_callee := false
	for f in mir_mod.funcs {
		if f.name != 'builtin__array__slice' {
			continue
		}
		found_callee = true
		assert f.abi_ret_indirect
		assert f.abi_param_class == [.indirect, .in_reg, .in_reg]
	}
	assert found_callee

	mut found_call := false
	for val in mir_mod.values {
		if val.kind != .instruction {
			continue
		}
		instr := mir_mod.instrs[val.index]
		if instr.operands.len == 0 {
			continue
		}
		callee := instr.operands[0]
		if callee <= 0 || callee >= mir_mod.values.len
			|| mir_mod.values[callee].name != 'builtin__array__slice' {
			continue
		}
		found_call = true
		assert instr.op == .call_sret
		assert instr.abi_ret_indirect
		assert instr.abi_arg_class == [.indirect, .in_reg, .in_reg]
		assert instr.operands.len >= 4
		start_arg := instr.operands[2]
		end_arg := instr.operands[3]
		assert mir_mod.type_store.types[mir_mod.values[start_arg].typ].kind == .int_t
		assert mir_mod.type_store.types[mir_mod.values[end_arg].typ].kind == .int_t
	}
	assert found_call
}

fn test_windows_x64_println_links_pe_image() {
	linked := build_windows_x64_sample("module main\nfn main() { println('x') }\n", true, true)

	assert linked.image.len > 0
	assert linked.image[0] == `M`
	assert linked.image[1] == `Z`
	assert 'WriteConsoleW' in linked.undefined_symbols, linked.undefined_symbols.str()
	assert 'MultiByteToWideChar' in linked.undefined_symbols, linked.undefined_symbols.str()
	assert 'GetProcessHeap' in linked.undefined_symbols, linked.undefined_symbols.str()
	assert 'HeapAlloc' in linked.undefined_symbols, linked.undefined_symbols.str()
	assert 'HeapFree' in linked.undefined_symbols, linked.undefined_symbols.str()
	assert 'WriteFile' in linked.undefined_symbols, linked.undefined_symbols.str()
	assert_no_windows_minimal_crt_symbols(linked.undefined_symbols)
	assert_no_windows_minimal_runtime_retention(linked.built_functions)
}

fn test_windows_x64_minimal_runtime_preserves_imported_module_init() {
	linked := build_windows_x64_sample_with_files({
		'main.v':    'module main
import dep

fn main() {}
'
		'dep/dep.v': 'module dep

fn init() {
	touch()
}

fn touch() {}
'
	}, true, true)

	assert 'dep__init' in linked.built_functions, linked.built_functions.str()
	assert 'dep__touch' in linked.built_functions, linked.built_functions.str()
	assert linked.image.len > 0
	assert linked.image[0] == `M`
	assert linked.image[1] == `Z`
}

fn test_windows_x64_minimal_runtime_preserves_imported_module_init_without_optimize() {
	res := build_windows_x64_sample_with_files({
		'main.v':    'module main
import dep

fn main() {}
'
		'dep/dep.v': 'module dep

fn init() {
	touch()
}

fn touch() {}
'
	}, false, false)

	assert 'dep__init' in res.built_functions, res.built_functions.str()
	assert 'dep__touch' in res.built_functions, res.built_functions.str()
	assert_no_windows_minimal_crt_symbols(res.undefined_symbols)
	assert_no_windows_minimal_runtime_retention(res.built_functions)
}

fn test_windows_x64_minimal_runtime_preserves_conditional_imported_module_init() {
	linked := build_windows_x64_sample_with_files({
		'main.v':    'module main
$if windows {
	import dep
}

fn main() {}
'
		'dep/dep.v': 'module dep

fn init() {
	touch()
}

fn touch() {}
'
	}, true, true)

	assert 'dep__init' in linked.built_functions, linked.built_functions.str()
	assert 'dep__touch' in linked.built_functions, linked.built_functions.str()
	assert linked.image.len > 0
	assert linked.image[0] == `M`
	assert linked.image[1] == `Z`
}

fn test_windows_x64_minimal_runtime_preserves_dotted_import_module_init() {
	linked := build_windows_x64_sample_with_files({
		'main.v':                'module main
import fixture.inner

fn main() {}
'
		'fixture/inner/inner.v': 'module inner

const dotted_runtime_value = make_value()

fn init() {
	touch()
}

fn touch() {}

fn make_value() int {
	return 7
}
'
	}, true, true)

	assert 'inner__init' in linked.built_functions, linked.built_functions.str()
	assert 'inner__touch' in linked.built_functions, linked.built_functions.str()
	assert 'inner____v_init_consts_inner' in linked.built_functions, linked.built_functions.str()
	assert 'inner__make_value' in linked.built_functions, linked.built_functions.str()
	assert linked.image.len > 0
	assert linked.image[0] == `M`
	assert linked.image[1] == `Z`
}

fn test_windows_x64_minimal_runtime_preserves_dotted_import_const_init_matrix() {
	for optimize in [false, true] {
		res := build_windows_x64_sample_with_files({
			'main.v':                'module main
import fixture.inner

fn main() {}
'
			'fixture/inner/inner.v': 'module inner

const dotted_runtime_value = make_value()

fn init() {
	touch()
}

fn touch() {}

fn make_value() int {
	return 7
}
'
		}, false, optimize)

		assert 'inner__init' in res.built_functions, res.built_functions.str()
		assert 'inner__touch' in res.built_functions, res.built_functions.str()
		assert 'inner____v_init_consts_inner' in res.built_functions, res.built_functions.str()
		assert 'inner__make_value' in res.built_functions, res.built_functions.str()
		assert_no_windows_minimal_crt_symbols(res.undefined_symbols)
		assert_no_windows_large_runtime_module_retention(res.built_functions)
	}
}

fn assert_no_windows_minimal_crt_symbols(undefined_symbols []string) {
	assert_no_windows_minimal_crt_stdio_symbols(undefined_symbols)
	for name in ['calloc', 'malloc', 'free', 'write', '_get_osfhandle'] {
		assert name !in undefined_symbols, undefined_symbols.str()
	}
}

fn assert_no_windows_minimal_crt_stdio_symbols(undefined_symbols []string) {
	for name in ['stdin', 'stdout', 'stderr', 'printf', 'fprintf', 'dprintf', 'sprintf', 'snprintf',
		'wprintf', 'puts', 'fputs', 'setvbuf', 'fflush', 'fopen', '_wfopen', 'fdopen', 'freopen',
		'_wfreopen', 'fclose', 'pclose', '_pclose', 'fread', 'fwrite', 'fgets', 'getc', 'feof',
		'ferror', 'fseek', 'ftell', 'rewind', 'fileno', '_fileno', 'fgetpos'] {
		assert name !in undefined_symbols, undefined_symbols.str()
	}
}

fn assert_no_windows_minimal_targeted_stdio_markused_leaks(used map[string]bool) {
	assert !used['os|f|stdout'], used.str()
	assert !used['os|f|stderr'], used.str()
	assert !used['builtin|f|is_terminal'], used.str()
	assert !used['builtin|f|write_buf_to_console'], used.str()
	assert !used['builtin|f|unbuffer_stdout'], used.str()
	assert !windows_x64_markused_key_contains(used, 'write_buf_to_fd_windows_non_minimal'), used.str()
	assert !windows_x64_markused_key_contains(used, 'setvbuf'), used.str()
}

fn assert_no_windows_minimal_runtime_retention(built_functions []string) {
	for name in built_functions {
		assert !name.starts_with('os__'), built_functions.str()
		assert !name.starts_with('time__'), built_functions.str()
		assert !name.starts_with('io__'), built_functions.str()
		assert !name.starts_with('dl__'), built_functions.str()
		assert !name.starts_with('sha256__'), built_functions.str()
		assert !name.starts_with('binary__'), built_functions.str()
		assert name != 'builtin__is_terminal', built_functions.str()
		assert name != 'builtin__write_buf_to_console', built_functions.str()
		assert !name.contains('write_buf_to_fd_windows_non_minimal'), built_functions.str()
		if name.starts_with('__v_init_consts_') {
			assert name == '__v_init_consts_main', built_functions.str()
		}
		assert !name.contains('____v_init_consts_'), built_functions.str()
	}
}

fn assert_no_windows_large_runtime_module_retention(built_functions []string) {
	for name in built_functions {
		assert !name.starts_with('os__'), built_functions.str()
		assert !name.starts_with('time__'), built_functions.str()
		assert !name.starts_with('io__'), built_functions.str()
		assert !name.starts_with('dl__'), built_functions.str()
		assert !name.starts_with('sha256__'), built_functions.str()
		assert !name.starts_with('binary__'), built_functions.str()
	}
}

fn build_windows_x64_sample(source string, link bool, optimize bool) WindowsX64BuildResult {
	return build_windows_x64_sample_configured(source, link, optimize, true)
}

fn build_windows_x64_sample_configured(source string, link bool, optimize bool, no_parallel bool) WindowsX64BuildResult {
	return build_windows_x64_sample_with_files_configured({
		'main.v': source
	}, link, optimize, no_parallel)
}

fn build_windows_x64_markused_sample(source string) map[string]bool {
	tmp_dir := os.join_path(os.temp_dir(), 'v2_builder_windows_x64_markused_sample_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}

	main_path := os.join_path(tmp_dir, 'main.v')
	os.write_file(main_path, source) or { panic(err) }

	mut prefs := pref.new_preferences()
	prefs.backend = .x64
	prefs.arch = .x64
	prefs.target_os = 'windows'
	prefs.skip_type_check = true
	prefs.no_parallel = true
	prefs.no_parallel_transform = true
	prefs.no_cache = true

	mut b := new_builder(&prefs)
	b.user_files = [main_path]
	parse_test_builder_files(mut b, [main_path])
	b.env = types.Environment.new()

	mut trans := transformer.Transformer.new_with_pref(b.env, b.pref)
	trans.set_file_set(b.file_set)
	transform_test_builder_files(mut b, mut trans)
	used := mark_used_windows_x64_test(mut b)
	assert used.len > 0
	return used
}

fn build_checked_windows_x64_mir_sample(source string, optimize bool) mir.Module {
	tmp_dir := os.join_path(os.temp_dir(),
		'v2_builder_windows_x64_checked_mir_${os.getpid()}_${optimize}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}

	main_path := os.join_path(tmp_dir, 'main.v')
	os.write_file(main_path, source) or { panic(err) }

	mut prefs := pref.new_preferences()
	prefs.backend = .x64
	prefs.arch = .x64
	prefs.target_os = 'windows'
	prefs.no_parallel = true
	prefs.no_parallel_transform = true
	prefs.no_cache = true

	mut b := new_builder(&prefs)
	b.user_files = [main_path]
	parse_test_builder_files(mut b, [main_path])
	b.env = b.type_check_files()

	mut trans := transformer.Transformer.new_with_pref(b.env, b.pref)
	trans.set_file_set(b.file_set)
	transform_test_builder_files(mut b, mut trans)
	b.used_fn_keys = mark_used_windows_x64_test(mut b)
	assert b.used_fn_keys.len > 0

	mut ssa_mod := ssa.Module.new('main')
	mut ssa_builder := ssa.Builder.new_with_env(ssa_mod, b.env)
	ssa_builder.guard_invalid_type_payloads = true
	ssa_builder.target_os = 'windows'
	ssa_builder.minimal_runtime_roots = true
	ssa_builder.native_backend_bulk_zero_alloca = true
	ssa_builder.used_fn_keys = b.used_fn_keys.clone()
	build_test_ssa(mut b, mut ssa_builder)
	if optimize {
		ssa_optimize.optimize(mut ssa_mod)
	}

	mut mir_mod := mir.lower_from_ssa(ssa_mod)
	abi.lower_with_x64_abi(mut mir_mod, .x64, .windows)
	insel.select(mut mir_mod, .x64)
	return mir_mod
}

fn windows_x64_markused_key_contains(used map[string]bool, needle string) bool {
	for key, is_used in used {
		if is_used && key.contains(needle) {
			return true
		}
	}
	return false
}

fn build_windows_x64_sample_with_files(sources map[string]string, link bool, optimize bool) WindowsX64BuildResult {
	return build_windows_x64_sample_with_files_configured(sources, link, optimize, true)
}

fn build_windows_x64_sample_with_files_configured(sources map[string]string, link bool, optimize bool, no_parallel bool) WindowsX64BuildResult {
	tmp_dir := os.join_path(os.temp_dir(),
		'v2_builder_windows_x64_sample_${os.getpid()}_${sources.len}_${link}_${optimize}_${no_parallel}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}

	main_path := os.join_path(tmp_dir, 'main.v')
	obj_path := os.join_path(tmp_dir, 'main.obj')
	exe_path := os.join_path(tmp_dir, 'main.exe')
	for rel_path, source in sources {
		path := os.join_path(tmp_dir, rel_path)
		os.mkdir_all(os.dir(path)) or { panic(err) }
		os.write_file(path, source) or { panic(err) }
	}

	mut prefs := pref.new_preferences()
	prefs.backend = .x64
	prefs.arch = .x64
	prefs.target_os = 'windows'
	prefs.skip_type_check = true
	prefs.no_parallel = no_parallel
	prefs.no_parallel_transform = true
	prefs.no_cache = true
	prefs.no_optimize = !optimize

	mut b := new_builder(&prefs)
	b.user_files = [main_path]
	parse_test_builder_files(mut b, [main_path])
	b.env = types.Environment.new()

	mut trans := transformer.Transformer.new_with_pref(b.env, b.pref)
	trans.set_file_set(b.file_set)
	transform_test_builder_files(mut b, mut trans)
	b.used_fn_keys = mark_used_windows_x64_test(mut b)
	assert b.used_fn_keys.len > 0

	mut ssa_mod := ssa.Module.new('main')
	mut ssa_builder := ssa.Builder.new_with_env(ssa_mod, b.env)
	ssa_builder.guard_invalid_type_payloads = true
	ssa_builder.target_os = 'windows'
	ssa_builder.minimal_runtime_roots = true
	ssa_builder.native_backend_bulk_zero_alloca = true
	ssa_builder.used_fn_keys = b.used_fn_keys.clone()
	mut mir_mod := mir.Module{}
	mut built_functions := []string{}
	if no_parallel {
		build_test_ssa(mut b, mut ssa_builder)
		if optimize {
			ssa_optimize.optimize(mut ssa_mod)
		}
		built_functions = ssa_mod.funcs.filter(it.blocks.len > 0).map(it.name)
		mir_mod = mir.lower_from_ssa(ssa_mod)
		abi.lower_with_x64_abi(mut mir_mod, .x64, .windows)
		insel.select(mut mir_mod, .x64)
	} else {
		mir_mod = b.build_native_mir_from_files(b.files, .x64, 'windows', true, b.used_fn_keys,
			'Windows x64 sample')
		built_functions = mir_mod.funcs.filter(it.blocks.len > 0).map(it.name)
	}

	mut gen := x64.Gen.new_with_format_and_abi(&mir_mod, .coff, .windows)
	gen.gen()
	gen.write_file(obj_path)
	mut image := []u8{}
	if link {
		gen.link_executable(exe_path) or { panic(err) }
		image = os.read_bytes(exe_path) or { panic(err) }
	}
	return WindowsX64BuildResult{
		undefined_symbols: coff_undefined_symbols(os.read_bytes(obj_path) or { panic(err) })
		built_functions:   built_functions
		image:             image
	}
}

fn coff_undefined_symbols(data []u8) []string {
	ptr_to_symbols := int(coff_u32(data, 8))
	number_of_symbols := int(coff_u32(data, 12))
	string_table_off := ptr_to_symbols + number_of_symbols * 18
	mut symbols := []string{}
	mut i := 0
	for i < number_of_symbols {
		off := ptr_to_symbols + i * 18
		section_number := i16(coff_u16(data, off + 12))
		aux_count := int(data[off + 17])
		if section_number == 0 {
			symbols << coff_symbol_name(data, off, string_table_off)
		}
		i += 1 + aux_count
	}
	return symbols
}

fn coff_symbol_name(data []u8, off int, string_table_off int) string {
	if coff_u32(data, off) == 0 {
		name_off := int(coff_u32(data, off + 4))
		return coff_string(data, string_table_off + name_off)
	}
	mut end := off
	for end < off + 8 && data[end] != 0 {
		end++
	}
	return data[off..end].bytestr()
}

fn coff_string(data []u8, off int) string {
	mut end := off
	for end < data.len && data[end] != 0 {
		end++
	}
	return data[off..end].bytestr()
}

fn coff_u16(data []u8, off int) u16 {
	return u16(data[off]) | (u16(data[off + 1]) << 8)
}

fn coff_u32(data []u8, off int) u32 {
	return u32(data[off]) | (u32(data[off + 1]) << 8) | (u32(data[off + 2]) << 16) | (u32(data[
		off + 3]) << 24)
}
