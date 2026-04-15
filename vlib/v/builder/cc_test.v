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
