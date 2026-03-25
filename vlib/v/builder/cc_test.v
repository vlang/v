module builder

import os
import v.pref

fn test_c_error_looks_like_cpp_header_with_clang_style_output() {
	clang_output := "error: unknown type name 'namespace'\nerror: expected ';' after top level declarator"
	assert c_error_looks_like_cpp_header(clang_output)
}

fn test_c_error_looks_like_cpp_header_with_source_excerpt() {
	gcc_output := '/usr/include/H5File.h:18: error: \';\' expected (got "H5")\n| namespace H5 {\n'
	assert c_error_looks_like_cpp_header(gcc_output)
}

fn test_c_error_looks_like_cpp_header_with_regular_c_error() {
	c_output := "error: unknown type name 'my_missing_type'"
	assert !c_error_looks_like_cpp_header(c_output)
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

fn macos_compile_args(args []string) string {
	mut full_args := ['']
	full_args << args
	prefs, _ := pref.parse_args_and_show_errors([], full_args, false)
	mut builder := new_builder(prefs)
	builder.out_name_c = os.join_path(os.vtmp_dir(), 'builder_cc_test.tmp.c')
	builder.setup_ccompiler_options(prefs.ccompiler)
	builder.setup_output_name()
	return builder.get_compile_args().join(' ')
}

fn macos_version_min_flags(compile_args string) []string {
	return compile_args.split(' ').filter(it.starts_with('-mmacosx-version-min='))
}

fn hello_world_example() string {
	return os.join_path(@VEXEROOT, 'examples', 'hello_world.v')
}
