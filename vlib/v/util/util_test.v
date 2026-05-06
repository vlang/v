module util

import os
import time
import v.pref

fn test_tool_recompilation_args_force_system_cc_for_vdoc_on_freebsd() {
	assert tool_recompilation_args('vdoc', 'freebsd') == ['-cc', 'cc']
}

fn test_tool_recompilation_args_use_openssl_for_vpm() {
	assert tool_recompilation_args('vpm', 'macos') == ['-d', 'use_openssl']
	assert tool_recompilation_args('vpm', 'linux') == ['-d', 'use_openssl']
	assert tool_recompilation_args('vpm', 'windows') == ['-d', 'use_openssl']
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

fn test_vlines_escape_path_does_not_restore_old_tcc_prefix_workaround() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'util_test_vlines_escape_path_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	source_path := os.join_path(tmp_dir, 'probe.v')
	os.write_file(source_path, 'fn main() {}') or { panic(err) }

	expected := cescaped_path(os.real_path(source_path))
	assert vlines_escape_path(source_path, 'gcc') == expected

	escaped_tcc_path := vlines_escape_path(source_path, 'tcc')
	assert escaped_tcc_path == expected
	assert !escaped_tcc_path.starts_with('../../../../../..')
	$if windows {
		assert escaped_tcc_path.starts_with(os.windows_volume(source_path))
	}
}

fn test_qualify_import_stops_at_nearest_vmod_issue_26828() {
	root := os.join_path(os.vtmp_dir(), 'v_qualify_import_issue_26828_${os.getpid()}')
	os.rmdir_all(root) or {}
	defer {
		os.rmdir_all(root) or {}
	}
	project_root := os.join_path(root, 'outer')
	module_root := os.join_path(project_root, 'cli004')
	os.mkdir_all(os.join_path(module_root, 'sub'))!
	os.write_file(os.join_path(project_root, 'v.mod'), "Module {\n\tname: 'outer'\n}\n")!
	os.write_file(os.join_path(module_root, 'v.mod'), "Module {\n\tname: 'cli004'\n}\n")!
	main_file := os.join_path(module_root, 'cli004.v')
	os.write_file(main_file, 'module main\n\nimport sub\n\nfn main() {}\n')!

	mut p := pref.new_preferences()
	p.path = '.'
	old_dir := os.getwd()
	defer {
		os.chdir(old_dir) or { panic(err) }
	}
	os.chdir(module_root)!

	assert qualify_import(p, 'sub', main_file) == 'sub'
	assert qualify_import(p, 'sub', 'cli004.v') == 'sub'
}
