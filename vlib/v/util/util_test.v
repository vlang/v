module util

import os
import time

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

	fallback := fallback_tool_executable_path(@VEXE, '/opt/vlang', 'vdoctor', tool_source,
		tool_exe, true)

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

	fallback :=
		fallback_tool_executable_path(@VEXE, '/opt/vlang', 'vdoc', tool_source, tool_exe, true)

	assert fallback == tool_exe
}

fn test_fallback_tool_executable_path_uses_vtmp_for_outdated_single_file_tool_in_readonly_dir() {
	$if windows {
		return
	}
	if os.geteuid() == 0 {
		return
	}
	tmp_dir := os.join_path(os.vtmp_dir(),
		'util_test_fallback_tool_executable_path_readonly_outdated')
	os.mkdir_all(tmp_dir) or { panic(err) }
	tool_source := os.join_path(tmp_dir, 'vrepl.v')
	tool_exe := os.join_path(tmp_dir, 'vrepl')
	os.write_file(tool_exe, '') or { panic(err) }
	time.sleep(1100 * time.millisecond)
	os.write_file(tool_source, 'fn main() {}') or { panic(err) }
	os.chmod(tmp_dir, 0o500) or { panic(err) }
	defer {
		os.chmod(tmp_dir, 0o700) or {}
		os.rmdir_all(tmp_dir) or {}
	}
	if os.is_writable(tmp_dir) {
		return
	}

	fallback := fallback_tool_executable_path(@VEXE, '/opt/vlang', 'vrepl', tool_source, tool_exe,
		false)

	assert fallback != tool_exe
	assert fallback.starts_with(os.join_path(os.vtmp_dir(), 'tools'))
	assert fallback.ends_with(path_of_executable('vrepl'))
}

fn test_fallback_tool_executable_path_keeps_current_single_file_tool_in_readonly_dir() {
	$if windows {
		return
	}
	if os.geteuid() == 0 {
		return
	}
	tmp_dir := os.join_path(os.vtmp_dir(),
		'util_test_fallback_tool_executable_path_readonly_current')
	os.mkdir_all(tmp_dir) or { panic(err) }
	tool_source := os.join_path(tmp_dir, 'vrepl.v')
	tool_exe := os.join_path(tmp_dir, 'vrepl')
	os.write_file(tool_source, 'fn main() {}') or { panic(err) }
	time.sleep(1100 * time.millisecond)
	os.write_file(tool_exe, '') or { panic(err) }
	os.chmod(tmp_dir, 0o500) or { panic(err) }
	defer {
		os.chmod(tmp_dir, 0o700) or {}
		os.rmdir_all(tmp_dir) or {}
	}
	if os.is_writable(tmp_dir) {
		return
	}

	fallback := fallback_tool_executable_path(@VEXE, '/opt/vlang', 'vrepl', tool_source, tool_exe,
		false)

	assert fallback == tool_exe
}
