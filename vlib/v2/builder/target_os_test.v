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
	files := b.parse_files([tmp_dir])
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
	files := b.parse_files([tmp_dir])
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
	files := b.parse_files([tmp_dir])
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
		]
	}
	windows_names := active_file_imports(file, [], 'windows').map(it.name)
	linux_names := active_file_imports(file, [], 'linux').map(it.name)
	macos_names := active_file_imports(file, [], 'macos').map(it.name)

	assert 'winmod' in windows_names
	assert 'linmod' !in windows_names
	assert 'macmod' !in windows_names

	assert 'linmod' in linux_names
	assert 'winmod' !in linux_names
	assert 'macmod' !in linux_names

	assert 'macmod' in macos_names
	assert 'winmod' !in macos_names
	assert 'linmod' !in macos_names
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

struct WindowsX64ProbeResult {
	undefined_symbols []string
	built_functions   []string
	image             []u8
}

fn test_windows_x64_empty_main_links_without_crt_or_darwin_symbols() {
	res := build_windows_x64_probe('module main\nfn main() {}\n', false, true)

	assert 'calloc' !in res.undefined_symbols, res.undefined_symbols.str()
	assert '__stdoutp' !in res.undefined_symbols, res.undefined_symbols.str()
	assert '__stderrp' !in res.undefined_symbols, res.undefined_symbols.str()
	assert '__stdinp' !in res.undefined_symbols, res.undefined_symbols.str()
	assert '__error' !in res.undefined_symbols, res.undefined_symbols.str()

	linked := build_windows_x64_probe('module main\nfn main() {}\n', true, true)
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
	res := build_windows_x64_probe(source, false, false)

	assert '__stdoutp' !in res.undefined_symbols
	assert '__stderrp' !in res.undefined_symbols
	assert '__stdinp' !in res.undefined_symbols
	assert '__error' !in res.undefined_symbols
	assert 'stdout' in res.undefined_symbols
	assert 'stderr' in res.undefined_symbols
	assert 'stdin' in res.undefined_symbols
	assert 'errno' in res.undefined_symbols
}

fn test_windows_x64_minimal_runtime_builds_markused_core_functions() {
	res := build_windows_x64_probe("module main\nfn main() { println('x') }\n", false, false)

	assert 'builtin__println' in res.built_functions, res.built_functions.str()
	assert 'stderr' !in res.undefined_symbols, res.undefined_symbols.str()
	assert_no_windows_minimal_runtime_retention(res.built_functions)
}

fn test_windows_x64_println_links_pe_image() {
	linked := build_windows_x64_probe("module main\nfn main() { println('x') }\n", true, true)

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
	linked := build_windows_x64_probe_with_files({
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

fn test_windows_x64_minimal_runtime_preserves_conditional_imported_module_init() {
	linked := build_windows_x64_probe_with_files({
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
	linked := build_windows_x64_probe_with_files({
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

fn assert_no_windows_minimal_crt_symbols(undefined_symbols []string) {
	for name in ['calloc', 'malloc', 'free', 'fwrite', 'fflush', 'write', 'stdout', 'stderr',
		'_get_osfhandle'] {
		assert name !in undefined_symbols, undefined_symbols.str()
	}
}

fn assert_no_windows_minimal_runtime_retention(built_functions []string) {
	for name in built_functions {
		assert !name.starts_with('os__'), built_functions.str()
		assert !name.starts_with('time__'), built_functions.str()
		assert !name.starts_with('io__'), built_functions.str()
		assert !name.starts_with('dl__'), built_functions.str()
		assert !name.starts_with('sha256__'), built_functions.str()
		assert !name.starts_with('binary__'), built_functions.str()
		if name.starts_with('__v_init_consts_') {
			assert name == '__v_init_consts_main', built_functions.str()
		}
		assert !name.contains('____v_init_consts_'), built_functions.str()
	}
}

fn build_windows_x64_probe(source string, link bool, optimize bool) WindowsX64ProbeResult {
	return build_windows_x64_probe_with_files({
		'main.v': source
	}, link, optimize)
}

fn build_windows_x64_probe_with_files(sources map[string]string, link bool, optimize bool) WindowsX64ProbeResult {
	tmp_dir := os.join_path(os.temp_dir(),
		'v2_builder_windows_x64_probe_${os.getpid()}_${sources.len}_${link}_${optimize}')
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
	prefs.no_parallel = true
	prefs.no_parallel_transform = true
	prefs.no_cache = true

	mut b := new_builder(&prefs)
	b.user_files = [main_path]
	b.files = b.parse_files([main_path])
	b.env = types.Environment.new()

	mut trans := transformer.Transformer.new_with_pref(b.files, b.env, b.pref)
	trans.set_file_set(b.file_set)
	b.files = trans.transform_files(b.files)
	b.used_fn_keys = markused.mark_used_with_options(b.files, b.env, markused.MarkUsedOptions{
		minimal_runtime_roots: true
	})
	assert b.used_fn_keys.len > 0

	mut ssa_mod := ssa.Module.new('main')
	mut ssa_builder := ssa.Builder.new_with_env(ssa_mod, b.env)
	ssa_builder.guard_invalid_type_payloads = true
	ssa_builder.target_os = 'windows'
	ssa_builder.minimal_runtime_roots = true
	ssa_builder.used_fn_keys = b.used_fn_keys.clone()
	ssa_builder.build_all(b.files)
	if optimize {
		ssa_optimize.optimize(mut ssa_mod)
	}
	built_functions := ssa_mod.funcs.filter(it.blocks.len > 0).map(it.name)

	mut mir_mod := mir.lower_from_ssa(ssa_mod)
	abi.lower_with_x64_abi(mut mir_mod, .x64, .windows)
	insel.select(mut mir_mod, .x64)

	mut gen := x64.Gen.new_with_format_and_abi(&mir_mod, .coff, .windows)
	gen.gen()
	gen.write_file(obj_path)
	mut image := []u8{}
	if link {
		gen.link_executable(exe_path) or { panic(err) }
		image = os.read_bytes(exe_path) or { panic(err) }
	}
	return WindowsX64ProbeResult{
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
