module util

import os

fn test_tool_recompilation_args_force_system_cc_for_vdoc_on_freebsd() {
	assert tool_recompilation_args('vdoc', 'freebsd') == ['-cc', 'cc']
}

fn test_tool_recompilation_args_do_not_change_other_tools_or_platforms() {
	assert tool_recompilation_args('vfmt', 'freebsd').len == 0
	assert tool_recompilation_args('vdoc', 'linux').len == 0
}

fn test_fallback_tool_executable_path_uses_vtmp_for_missing_single_file_tool() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'util_test_fallback_tool_executable_path')
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	tool_source := os.join_path(tmp_dir, 'vdoctor.v')
	os.write_file(tool_source, 'fn main() {}') or { panic(err) }
	tool_exe := os.join_path(tmp_dir, 'vdoctor')

	fallback := fallback_tool_executable_path('/opt/vlang', 'vdoctor', tool_source, tool_exe, true)

	assert fallback != tool_exe
	assert fallback.starts_with(os.join_path(os.vtmp_dir(), 'tools'))
	assert fallback.ends_with(path_of_executable('vdoctor'))
}

fn test_fallback_tool_executable_path_keeps_directory_tools_in_place() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'util_test_fallback_tool_directory')
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	tool_source := os.join_path(tmp_dir, 'vdoc')
	os.mkdir_all(tool_source) or { panic(err) }
	tool_exe := os.join_path(tool_source, 'vdoc')

	fallback := fallback_tool_executable_path('/opt/vlang', 'vdoc', tool_source, tool_exe, true)

	assert fallback == tool_exe
}
