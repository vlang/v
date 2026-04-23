module builder

import os
import v.pref

fn test_ccompiler_is_available_with_existing_absolute_path() {
	assert ccompiler_is_available(@VEXE)
}

fn test_ccompiler_is_available_with_missing_compiler() {
	assert !ccompiler_is_available('missing_compiler_17126_for_builder_test')
}

fn test_c_error_looks_like_cpp_header_with_clang_style_output() {
	clang_output := "error: unknown type name 'namespace'\nerror: expected ';' after top level declarator"
	assert c_error_looks_like_cpp_header(clang_output)
}

fn test_c_error_looks_like_cpp_header_with_source_excerpt() {
	gcc_output := '/usr/include/H5File.h:18: error: \';\' expected (got "H5")\n| namespace H5 {\n'
	assert c_error_looks_like_cpp_header(gcc_output)
}

fn test_c_error_looks_like_cpp_header_with_imgui_style_operator_overload() {
	imgui_output := "/tmp/fake_imgui.h:3:16: error: 'operator' declared as array of functions of type 'float (unsigned long)'\n    3 |     float operator[](unsigned long idx) const { return (&x)[idx]; }\n"
	assert c_error_looks_like_cpp_header(imgui_output)
}

fn test_c_error_looks_like_cpp_header_with_class_keyword() {
	class_output := "/usr/include/foo.hpp:4:1: error: unknown type name 'class'\n    4 | class Foo {\n"
	assert c_error_looks_like_cpp_header(class_output)
}

fn test_c_error_looks_like_cpp_header_with_regular_c_error() {
	c_output := "error: unknown type name 'my_missing_type'"
	assert !c_error_looks_like_cpp_header(c_output)
}

fn test_c_output_suggests_missing_sokol_shader_symbol_with_clang_style_output() {
	c_output := [
		"/tmp/v_501/simple_shader.tmp.c:21250:43: error: use of undeclared identifier 'ATTR_vs_aposition'",
		'        pipeline_desc.layout.attrs[v_fixed_index(ATTR_vs_aposition, 16)].format = 1;',
	].join('\n')
	assert c_output_suggests_missing_sokol_shader_symbol(c_output) == 'ATTR_vs_aposition'
}

fn test_c_output_suggests_missing_sokol_shader_symbol_with_gcc_style_output() {
	c_output := [
		"/tmp/v_501/simple_shader.tmp.c:21250:43: error: 'SLOT_fs_params' undeclared (first use in this function)",
		'        gfx_apply_uniforms(SLOT_fs_params);',
	].join('\n')
	assert c_output_suggests_missing_sokol_shader_symbol(c_output) == 'SLOT_fs_params'
}

fn test_c_output_suggests_missing_sokol_shader_symbol_ignores_regular_c_errors() {
	c_output := "error: use of undeclared identifier 'my_missing_type'"
	assert c_output_suggests_missing_sokol_shader_symbol(c_output) == ''
}

fn test_macos_compile_args_do_not_force_version_min_by_default() {
	compile_args := macos_compile_args(['-os', 'macos', '-cc', 'clang', hello_world_example()])
	assert macos_version_min_flags(compile_args) == []string{}
}

fn test_macos_compile_args_keep_explicit_cflag_version_min() {
	compile_args := macos_compile_args([
		'-os',
		'macos',
		'-cc',
		'clang',
		'-cflags',
		'-mmacosx-version-min=11.0',
		hello_world_example(),
	])
	assert macos_version_min_flags(compile_args) == ['-mmacosx-version-min=11.0']
}

fn test_macos_compile_args_append_macosx_version_min_after_cflags() {
	compile_args := macos_compile_args([
		'-os',
		'macos',
		'-cc',
		'clang',
		'-cflags',
		'-mmacosx-version-min=10.7',
		'-macosx-version-min',
		'11.0',
		hello_world_example(),
	])
	assert macos_version_min_flags(compile_args) == [
		'-mmacosx-version-min=10.7',
		'-mmacosx-version-min=11.0',
	]
}

fn test_cc_from_string_detects_cl_as_msvc() {
	assert pref.cc_from_string('cl') == .msvc
	assert pref.cc_from_string('C:/Program Files/Microsoft Visual Studio/cl.exe') == .msvc
}

fn test_cc_from_string_detects_tiny_gcc_as_tinyc() {
	assert pref.cc_from_string('tiny_gcc') == .tinyc
	assert pref.cc_from_string('/usr/local/bin/tiny_gcc') == .tinyc
}

fn test_ccompiler_type_from_version_output_detects_openbsd_clang() {
	detected := ccompiler_type_from_version_output('OpenBSD clang version 16.0.6') or { panic(err) }
	assert detected == .clang
}

fn test_ccompiler_type_from_version_output_detects_tcc() {
	detected := ccompiler_type_from_version_output('tcc version 0.9.27 (x86_64 Linux)') or {
		panic(err)
	}
	assert detected == .tinyc
}

fn test_ccompiler_type_from_name_detects_tiny_gcc() {
	detected := ccompiler_type_from_name('/opt/tiny_gcc') or { panic(err) }
	assert detected == .tinyc
}

fn test_ccompiler_type_from_version_output_detects_tiny_gcc() {
	detected := ccompiler_type_from_version_output('tiny_gcc version 0.9.27') or { panic(err) }
	assert detected == .tinyc
}

fn test_is_tcc_compilation_failure_detects_tiny_gcc_compiler_name() {
	assert is_tcc_compilation_failure('/opt/bin/tiny_gcc', .unknown, '')
}

fn test_resolve_ccompiler_type_detects_cc_alias_path_as_clang() {
	$if windows {
		return
	}
	test_root := os.join_path(os.vtmp_dir(), 'v_builder_cc_alias_${os.getpid()}')
	alias_cc := prepare_test_ccompiler_alias(test_root, 'cc', 'OpenBSD clang version 16.0.6')
	defer {
		os.rmdir_all(test_root) or {}
	}
	assert resolve_ccompiler_type(alias_cc, pref.cc_from_string(alias_cc)) == .clang
}

fn test_resolve_ccompiler_type_detects_real_path_without_running_alias() {
	$if windows {
		return
	}
	test_root := os.join_path(os.vtmp_dir(), 'v_builder_cc_symlink_${os.getpid()}')
	os.rmdir_all(test_root) or {}
	os.mkdir_all(test_root) or { panic(err) }
	defer {
		os.rmdir_all(test_root) or {}
	}
	clang_path := os.join_path(test_root, 'clang')
	os.write_file(clang_path, '#!/bin/sh
exit 1
') or { panic(err) }
	os.chmod(clang_path, 0o700) or { panic(err) }
	link_path := os.join_path(test_root, 'cc')
	os.symlink(clang_path, link_path) or { panic(err) }
	assert resolve_ccompiler_type(link_path, pref.cc_from_string(link_path)) == .clang
}

fn test_ccompiler_type_from_resolved_path_detects_macos_cc_wrapper_as_clang() {
	$if macos {
		detected := ccompiler_type_from_resolved_path('/usr/bin/cc') or { panic(err) }
		assert detected == .clang
	}
}

fn test_setup_ccompiler_options_detects_cl_path_as_msvc() {
	mut full_args := ['']
	full_args << hello_world_example()
	mut prefs, _ := pref.parse_args_and_show_errors([], full_args, false)
	prefs.ccompiler = 'C:/Program Files/Microsoft Visual Studio/cl.exe'
	prefs.ccompiler_type = pref.cc_from_string(prefs.ccompiler)
	mut builder := new_builder(prefs)
	builder.out_name_c = os.join_path(os.vtmp_dir(), 'builder_cc_test.tmp.c')
	builder.setup_ccompiler_options(prefs.ccompiler)
	assert builder.ccoptions.cc == .msvc
}

fn test_msvc_thirdparty_obj_path_uses_cached_location_for_target_arch() {
	obj_file := os.join_path(@VEXEROOT, 'thirdparty', 'mbedtls', 'library', 'bignum.o')
	mut builder64 := new_builder_for_args(['-cc', 'msvc', '-m64', hello_world_example()])
	mut builder32 := new_builder_for_args(['-cc', 'msvc', '-m32', hello_world_example()])
	obj64 := builder64.msvc_thirdparty_obj_path('mbedtls', obj_file, '')
	obj32 := builder32.msvc_thirdparty_obj_path('mbedtls', obj_file, '')
	source_obj := os.real_path(obj_file.all_before_last('.o') + '.obj')
	assert obj64 != obj32
	assert obj64 != source_obj
	assert obj32 != source_obj
	assert obj64.ends_with('.obj')
	assert obj32.ends_with('.obj')
}

fn test_msvc_thirdparty_obj_path_keeps_debug_objects_separate() {
	obj_file := os.join_path(@VEXEROOT, 'thirdparty', 'mbedtls', 'library', 'bignum.o')
	mut release_builder := new_builder_for_args(['-cc', 'msvc', '-m64', hello_world_example()])
	mut debug_builder := new_builder_for_args(['-cc', 'msvc', '-m64', '-g', hello_world_example()])
	release_obj := release_builder.msvc_thirdparty_obj_path('mbedtls', obj_file, '')
	debug_obj := debug_builder.msvc_thirdparty_obj_path('mbedtls', obj_file, '')
	assert debug_obj.ends_with('.debug.obj')
	assert release_obj != debug_obj
}

fn test_sqlite_thirdparty_validation_error_for_missing_amalgamation() {
	obj_path := os.join_path(@VEXEROOT, 'thirdparty', 'sqlite', 'sqlite3.o')
	msg := sqlite_thirdparty_validation_error('db.sqlite', obj_path, '', .unknown)
	assert msg.contains('sqlite3.c')
	assert msg.contains('sqlite3.h')
	assert msg.contains('install_thirdparty_sqlite.vsh')
}

fn test_sqlite_thirdparty_validation_error_for_sqlite3_cpp() {
	obj_path := os.join_path(@VEXEROOT, 'thirdparty', 'sqlite', 'sqlite3.o')
	source_path := obj_path.all_before_last('.o') + '.cpp'
	msg := sqlite_thirdparty_validation_error('db.sqlite', obj_path, source_path, .cpp)
	assert msg.contains('Do not rename `sqlite3.c` to `sqlite3.cpp`')
	assert msg.contains('SQLite amalgamation package')
}

fn test_sqlite_thirdparty_validation_error_ignores_sqlite3_c() {
	obj_path := os.join_path(@VEXEROOT, 'thirdparty', 'sqlite', 'sqlite3.o')
	source_path := obj_path.all_before_last('.o') + '.c'
	assert sqlite_thirdparty_validation_error('db.sqlite', obj_path, source_path, .c) == ''
}

fn test_sqlite_thirdparty_validation_error_ignores_other_modules() {
	obj_path := os.join_path(@VEXEROOT, 'thirdparty', 'sqlite', 'sqlite3.o')
	source_path := obj_path.all_before_last('.o') + '.cpp'
	assert sqlite_thirdparty_validation_error('json.cjson', obj_path, source_path, .cpp) == ''
}

fn test_linux_cross_target_for_amd64() {
	target := linux_cross_target_for_arch(.amd64) or { panic(err) }
	assert target.triple == 'x86_64-linux-gnu'
	assert target.lib_dir == 'x86_64-linux-gnu'
	assert target.dynamic_linker == '/lib/x86_64-linux-gnu/ld-linux-x86-64.so.2'
	assert target.linker_emulation == 'elf_x86_64'
}

fn test_linux_cross_target_for_arm64_errors() {
	if target := linux_cross_target_for_arch(.arm64) {
		assert false, 'unexpected target: ${target}'
	} else {
		assert err.msg().contains('only `-arch amd64`')
		assert err.msg().contains('linuxroot')
	}
}

fn test_git_symlink_target_path_detects_placeholder_file() {
	test_root := os.join_path(os.vtmp_dir(), 'v_builder_git_symlink_target_${os.getpid()}')
	os.rmdir_all(test_root) or {}
	defer {
		os.rmdir_all(test_root) or {}
	}
	lib_dir := os.join_path(test_root, 'lib', 'x86_64-linux-gnu')
	os.mkdir_all(lib_dir)!
	target_file := os.join_path(lib_dir, 'libm-2.31.so')
	placeholder := os.join_path(lib_dir, 'libm.so.6')
	os.write_file(target_file, 'ELF PLACEHOLDER')!
	os.write_file(placeholder, 'libm-2.31.so\n')!
	assert git_symlink_target_path(placeholder) or { panic(err) } == target_file
}

fn test_git_symlink_target_path_ignores_linker_script() {
	test_root := os.join_path(os.vtmp_dir(), 'v_builder_git_symlink_script_${os.getpid()}')
	os.rmdir_all(test_root) or {}
	defer {
		os.rmdir_all(test_root) or {}
	}
	lib_dir := os.join_path(test_root, 'lib', 'x86_64-linux-gnu')
	os.mkdir_all(lib_dir)!
	linker_script := os.join_path(lib_dir, 'libm.so')
	os.write_file(linker_script, '/* GNU ld script */\nGROUP ( /lib/x86_64-linux-gnu/libm.so.6 )\n')!
	assert git_symlink_target_path(linker_script) == none
}

fn test_git_symlink_materialization_source_follows_placeholder_chain() {
	test_root := os.join_path(os.vtmp_dir(), 'v_builder_git_symlink_chain_${os.getpid()}')
	os.rmdir_all(test_root) or {}
	defer {
		os.rmdir_all(test_root) or {}
	}
	lib_dir := os.join_path(test_root, 'lib', 'x86_64-linux-gnu')
	os.mkdir_all(lib_dir)!
	final_target := os.join_path(lib_dir, 'libm-2.31.so')
	first_link := os.join_path(lib_dir, 'libm.so.6')
	second_link := os.join_path(lib_dir, 'libm.so')
	os.write_file(final_target, 'ELF FINAL')!
	os.write_file(first_link, 'libm-2.31.so\n')!
	os.write_file(second_link, 'libm.so.6\n')!
	assert git_symlink_materialization_source(second_link) or { panic(err) } == final_target
}

fn test_repair_cross_sysroot_git_symlink_placeholders_in_paths_materializes_target_copy() {
	test_root := os.join_path(os.vtmp_dir(), 'v_builder_git_symlink_repair_${os.getpid()}')
	os.rmdir_all(test_root) or {}
	defer {
		os.rmdir_all(test_root) or {}
	}
	lib_dir := os.join_path(test_root, 'lib', 'x86_64-linux-gnu')
	os.mkdir_all(lib_dir)!
	final_target := os.join_path(lib_dir, 'libm-2.31.so')
	placeholder := os.join_path(lib_dir, 'libm.so.6')
	final_bytes := 'ELF DATA BINARY'
	os.write_file(final_target, final_bytes)!
	os.write_file(placeholder, 'libm-2.31.so\n')!
	repaired := repair_cross_sysroot_git_symlink_placeholders_in_paths([placeholder], true) or {
		panic(err)
	}
	assert repaired == 1
	assert os.read_file(placeholder)! == final_bytes
}

fn test_msvc_should_use_rsp_for_ascii_args() {
	builder := new_builder_for_args(['-cc', 'msvc', hello_world_example()])
	assert builder.msvc_should_use_rsp(['/OUT:"C:\\Users\\russo\\Desktop\\main.exe"'])
}

fn test_msvc_should_not_use_rsp_for_non_ascii_args() {
	builder := new_builder_for_args(['-cc', 'msvc', hello_world_example()])
	assert !builder.msvc_should_use_rsp([
		'/OUT:"C:\\Users\\russo\\OneDrive\\Рабочий стол\\main.exe"',
	])
}

fn test_msvc_should_not_use_rsp_when_no_rsp_is_requested() {
	builder := new_builder_for_args(['-cc', 'msvc', '-no-rsp', hello_world_example()])
	assert !builder.msvc_should_use_rsp(['/OUT:"C:\\Users\\russo\\Desktop\\main.exe"'])
}

fn test_live_termux_linker_args_include_rdynamic_without_debug() {
	linker_args := builder_linker_args([
		'-os',
		'termux',
		'-cc',
		'clang',
		'-live',
		hello_world_example(),
	])
	assert linker_args.contains('-rdynamic')
}

fn test_thirdparty_cross_compile_config_for_linux_matches_target() {
	builder := new_builder_for_args(['-os', 'linux', hello_world_example()])
	cfg := builder.thirdparty_cross_compile_config()
	if current_os == 'linux' {
		assert cfg.sysroot == ''
		assert cfg.target_args == []string{}
		assert cfg.trailing_include_args == []string{}
		return
	}
	assert cfg.target_args == ['-target x86_64-linux-gnu']
	assert cfg.sysroot.ends_with('/linuxroot')
	assert cfg.trailing_include_args == [
		'-I',
		os.quoted_path('${cfg.sysroot}/include'),
	]
}

fn test_thirdparty_cross_compile_config_for_freebsd_matches_target() {
	builder := new_builder_for_args(['-os', 'freebsd', hello_world_example()])
	cfg := builder.thirdparty_cross_compile_config()
	if current_os == 'freebsd' {
		assert cfg.sysroot == ''
		assert cfg.target_args == []string{}
		assert cfg.trailing_include_args == []string{}
		return
	}
	assert cfg.target_args == ['-target x86_64-unknown-freebsd14.0']
	assert cfg.sysroot.ends_with('/freebsdroot')
	assert cfg.trailing_include_args == [
		'-I',
		os.quoted_path('${cfg.sysroot}/include'),
		'-I',
		os.quoted_path('${cfg.sysroot}/usr/include'),
	]
}

fn test_live_windows_main_linker_args_export_host_symbols() {
	linker_args := builder_linker_args([
		'-os',
		'windows',
		'-cc',
		'gcc',
		'-live',
		hot_reload_graph_example(),
	])
	assert linker_args.contains('-Wl,--export-all-symbols')
	assert linker_args.contains('-Wl,--out-implib,')
	assert linker_args.contains(live_windows_import_lib_path(hot_reload_graph_example()))
}

fn test_live_windows_shared_linker_args_include_host_import_lib() {
	linker_args := builder_linker_args([
		'-os',
		'windows',
		'-cc',
		'gcc',
		'-sharedlive',
		'-shared',
		hot_reload_graph_example(),
	])
	assert linker_args.contains(live_windows_import_lib_path(hot_reload_graph_example()))
}

fn test_shared_windows_builds_do_not_add_subsystem_flags() {
	mut builder := new_test_builder(['-os', 'windows', '-shared', hello_world_example()])
	assert builder.get_subsystem_flag() == ''
	compile_args := builder.get_compile_args().join(' ')
	assert !compile_args.contains('-municode')
	assert !compile_args.contains('-mwindows')
	assert !compile_args.contains('-mconsole')
}

fn test_should_use_rsp_for_linux_by_default() {
	builder := new_test_builder([hello_world_example()])
	assert builder.should_use_rsp(['-o', builder.out_name_c])
}

fn test_should_not_use_rsp_for_termux() {
	builder := new_test_builder(['-os', 'termux', hello_world_example()])
	assert !builder.should_use_rsp(['-o', builder.out_name_c])
}

fn test_should_not_use_rsp_for_args_with_embedded_single_quotes() {
	builder := new_test_builder([hello_world_example()])
	assert !builder.should_use_rsp(["'\\''"])
}

fn test_setup_ccompiler_options_detects_cc_alias_path_as_clang() {
	$if windows {
		return
	}
	test_root := os.join_path(os.vtmp_dir(), 'v_builder_cc_setup_${os.getpid()}')
	alias_cc := prepare_test_ccompiler_alias(test_root, 'cc', 'OpenBSD clang version 16.0.6')
	defer {
		os.rmdir_all(test_root) or {}
	}
	mut full_args := ['']
	full_args << hello_world_example()
	mut prefs, _ := pref.parse_args_and_show_errors([], full_args, false)
	prefs.ccompiler = alias_cc
	prefs.ccompiler_type = pref.cc_from_string(prefs.ccompiler)
	mut builder := new_builder(prefs)
	builder.out_name_c = os.join_path(os.vtmp_dir(), 'builder_cc_test.tmp.c')
	builder.setup_ccompiler_options(prefs.ccompiler)
	assert builder.pref.ccompiler_type == .clang
	assert builder.ccoptions.cc == .clang
}

fn macos_compile_args(args []string) string {
	return builder_compile_args(args)
}

fn builder_compile_args(args []string) string {
	builder := new_test_builder(args)
	return builder.get_compile_args().join(' ')
}

fn builder_linker_args(args []string) string {
	builder := new_test_builder(args)
	return builder.get_linker_args().join(' ')
}

fn new_test_builder(args []string) Builder {
	mut full_args := ['']
	full_args << args
	prefs, _ := pref.parse_args_and_show_errors([], full_args, false)
	mut builder := new_builder(prefs)
	builder.out_name_c = os.join_path(os.vtmp_dir(), 'builder_cc_test.tmp.c')
	builder.setup_ccompiler_options(prefs.ccompiler)
	builder.setup_output_name()
	return builder
}

fn new_builder_for_args(args []string) Builder {
	mut full_args := ['']
	full_args << args
	prefs, _ := pref.parse_args_and_show_errors([], full_args, false)
	return new_builder(prefs)
}

fn macos_version_min_flags(compile_args string) []string {
	return compile_args.split(' ').filter(it.starts_with('-mmacosx-version-min='))
}

fn hello_world_example() string {
	return os.join_path(@VEXEROOT, 'examples', 'hello_world.v')
}

fn hot_reload_graph_example() string {
	return os.join_path(@VEXEROOT, 'examples', 'hot_reload', 'graph.v')
}

fn test_c_output_suggests_missing_typedef_for_c_struct_with_issue_19050_output() {
	c_output := [
		"/tmp/v_501/c_struct.6580681062929530137.tmp.c:12966:17: error: incomplete result type 'struct string_c' in function definition",
		'struct string_c main__convert(string s) {',
		'                ^',
		"/tmp/v_501/c_struct.6580681062929530137.tmp.c:1962:8: note: forward declaration of 'struct string_c'",
		'struct string_c main__convert(string s);',
		'       ^',
		"/tmp/v_501/c_struct.6580681062929530137.tmp.c:12967:25: error: variable has incomplete type 'struct string_c'",
		'        struct string_c _t1 = ((struct string_c){.content = s.str,.len = ((u32)(s.len)),});',
		'                               ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~',
	].join('\n')
	assert c_output_suggests_missing_typedef_for_c_struct(c_output, {
		'string_c': true
	}) == 'string_c'
}

fn test_c_output_suggests_missing_typedef_for_c_struct_requires_matching_redeclaration() {
	c_output := [
		"/tmp/v_501/c_struct.tmp.c:1:1: error: incomplete result type 'struct string_c' in function definition",
		"/tmp/v_501/c_struct.tmp.c:2:1: note: forward declaration of 'struct string_c'",
	].join('\n')
	assert c_output_suggests_missing_typedef_for_c_struct(c_output, {
		'other_c_struct': true
	}) == ''
}

fn test_extract_quoted_identifier_supports_double_quotes() {
	assert extract_quoted_identifier('error: \';\' expected (got "glfw__GLFWwindow")') == 'glfw__GLFWwindow'
}

fn test_c_output_suggests_missing_header_for_typedef_c_struct_with_issue_25384_tcc_output() {
	c_output := 'C:/Users/si_z_/AppData/Local/Temp/v_0/TestV.tmp.c:904: error: \';\' expected (got "glfw__GLFWwindow")'
	assert c_output_suggests_missing_header_for_typedef_c_struct(c_output, {
		'GLFWwindow': true
	}, {
		'glfw__GLFWwindow': 'GLFWwindow'
	}) == 'GLFWwindow'
}

fn test_c_output_suggests_missing_header_for_typedef_c_struct_with_issue_25384_gcc_output() {
	c_output := [
		"/tmp/v_502/issue25384_windows.exe.tmp.c:1198:9: error: unknown type name 'GLFWwindow'",
		' 1198 | typedef GLFWwindow glfw__GLFWwindow;',
	].join('\n')
	assert c_output_suggests_missing_header_for_typedef_c_struct(c_output, {
		'GLFWwindow': true
	}, {
		'glfw__GLFWwindow': 'GLFWwindow'
	}) == 'GLFWwindow'
}

fn test_c_output_suggests_missing_header_for_typedef_c_struct_requires_known_type() {
	c_output := 'C:/Users/si_z_/AppData/Local/Temp/v_0/TestV.tmp.c:904: error: \';\' expected (got "glfw__GLFWwindow")'
	assert c_output_suggests_missing_header_for_typedef_c_struct(c_output, {}, {}) == ''
}

fn test_c_output_suggests_missing_header_for_typedef_c_struct_with_issue_23648_tcc_output() {
	c_output := 'D:/Temp/Temp/v_0/main.tmp.c:1028: error: \';\' expected (got "duarteroso__glfw__GLFWmonitor")'
	assert c_output_suggests_missing_header_for_typedef_c_struct(c_output, {
		'GLFWmonitor': true
	}, {}) == 'GLFWmonitor'
}

fn test_c_error_missing_library_name_detects_tcc_output() {
	tcc_output := "tcc: error: library 'pq' not found"
	lib_name := c_error_missing_library_name(tcc_output)
	assert lib_name == 'pq'
}

fn test_c_error_missing_libatomic_marker_with_tcc_output() {
	c_output := "/tmp/v/vdoc.tmp.c:24184: warning: assignment makes pointer from integer without a cast\ntcc: error: library 'atomic' not found\n"
	assert c_error_missing_libatomic_marker(c_output) == "library 'atomic' not found"
	assert c_error_looks_like_missing_libatomic(c_output)
}

fn test_c_error_missing_libatomic_marker_with_ld_output() {
	c_output := '/usr/bin/ld: cannot find -latomic\ncollect2: error: ld returned 1 exit status\n'
	assert c_error_missing_libatomic_marker(c_output) == 'cannot find -latomic'
	assert c_error_looks_like_missing_libatomic(c_output)
}

fn test_c_error_missing_libatomic_marker_with_regular_c_error() {
	c_output := "error: unknown type name 'my_missing_type'"
	assert c_error_missing_libatomic_marker(c_output) == ''
	assert !c_error_looks_like_missing_libatomic(c_output)
}

fn test_c_error_missing_library_name_with_macos_ld_output() {
	c_output := "ld: library 'mbedtls' not found\nclang: error: linker command failed with exit code 1\n"
	assert c_error_missing_library_name(c_output) == 'mbedtls'
}

fn test_c_error_missing_library_name_with_gnu_ld_output() {
	c_output := '/usr/bin/ld: cannot find -lssl\ncollect2: error: ld returned 1 exit status\n'
	assert c_error_missing_library_name(c_output) == 'ssl'
}

fn test_c_error_missing_library_name_with_regular_c_error() {
	c_output := "error: unknown type name 'my_missing_type'"
	assert c_error_missing_library_name(c_output) == ''
}

fn prepare_test_ccompiler_alias(test_root string, compiler_name string, version_output string) string {
	os.rmdir_all(test_root) or {}
	os.mkdir_all(test_root) or { panic(err) }
	compiler_path := os.join_path(test_root, compiler_name)
	os.write_file(compiler_path, '#!/bin/sh
if [ "\$1" = "--version" ] || [ "\$1" = "-v" ]; then
	echo "${version_output}"
	exit 0
fi
exit 1
') or {
		panic(err)
	}
	os.chmod(compiler_path, 0o700) or { panic(err) }
	return compiler_path
}
