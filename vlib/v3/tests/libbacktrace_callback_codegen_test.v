import os

fn test_libbacktrace_callback_codegen_matches_header() {
	$if !linux && !macos {
		return
	}
	vroot := @VEXEROOT
	v3_dir := os.join_path(vroot, 'vlib', 'v3')
	vlib_dir := os.join_path(vroot, 'vlib')
	temp_dir := os.join_path(os.vtmp_dir(), 'v3_libbacktrace_callback_${os.getpid()}')
	os.rmdir_all(temp_dir) or {}
	os.mkdir_all(temp_dir) or { panic(err) }
	defer {
		os.rmdir_all(temp_dir) or {}
	}

	v3_bin := os.join_path(temp_dir, 'v3')
	build := os.execute('${os.quoted_path(@VEXE)} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${os.quoted_path(v3_bin)} ${os.quoted_path(os.join_path(v3_dir, 'v3.v'))}')
	assert build.exit_code == 0, build.output

	source := os.join_path(temp_dir, 'main.v')
	os.write_file(source, "fn main() {\n\tpanic('boom')\n}\n") or { panic(err) }
	c_path := os.join_path(temp_dir, 'main.c')
	generate := os.execute('${os.quoted_path(v3_bin)} -d use_libbacktrace -b c -o ${os.quoted_path(c_path)} ${os.quoted_path(source)}')
	assert generate.exit_code == 0, generate.output

	c_source := os.read_file(c_path) or { panic(err) }
	assert c_source.contains('size_t arg1, const char* arg2, i32 arg3, const char* arg4'), c_source
	assert c_source.contains('void* arg0, const char* arg1, i32 arg2'), c_source
	assert c_source.contains('backtrace_full(bt_state, frames_to_skip, bt_print_callback_callback_adapter_'), c_source
	assert c_source.contains('backtrace_create_state(filename, 1, bt_error_handler_callback_adapter_'), c_source

	cc := os.find_abs_path_of_executable('cc') or { return }
	cc_version := os.execute('${os.quoted_path(cc)} --version').output.to_lower_ascii()
	pointer_warning := if cc_version.contains('clang') {
		'incompatible-function-pointer-types'
	} else {
		'incompatible-pointer-types'
	}
	include_dir := os.join_path(vroot, 'thirdparty', 'libbacktrace')
	check := os.execute('${os.quoted_path(cc)} -Werror=${pointer_warning} -fsyntax-only -I${os.quoted_path(include_dir)} ${os.quoted_path(c_path)}')
	assert check.exit_code == 0, check.output
}
