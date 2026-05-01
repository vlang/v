module builder

import os
import v2.pref

fn test_get_v_files_from_dir_uses_target_os() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v2_builder_target_os_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	os.write_file(os.join_path(tmp_dir, 'common.v'), 'module test') or { panic(err) }
	os.write_file(os.join_path(tmp_dir, 'platform_linux.v'), 'module test') or { panic(err) }
	os.write_file(os.join_path(tmp_dir, 'platform_macos.v'), 'module test') or { panic(err) }

	macos_files := get_v_files_from_dir(tmp_dir, []string{}, 'mac')
	macos_names := macos_files.map(os.file_name(it))
	assert 'common.v' in macos_names
	assert 'platform_macos.v' in macos_names
	assert 'platform_linux.v' !in macos_names

	linux_files := get_v_files_from_dir(tmp_dir, []string{}, 'linux')
	linux_names := linux_files.map(os.file_name(it))
	assert 'common.v' in linux_names
	assert 'platform_linux.v' in linux_names
	assert 'platform_macos.v' !in linux_names
}

fn test_flag_helpers_use_target_os() {
	assert flag_os_matches('macos', 'mac')
	assert !flag_os_matches('linux', 'mac')
	assert comptime_cond_matches('macos', 'mac')
	assert !comptime_cond_matches('linux', 'mac')
	assert comptime_cond_matches('macos && !linux', 'mac')
	macos_flag := parse_flag_directive_line('#flag macos -framework Cocoa', '/tmp/source.v', 'mac') or {
		''
	}
	linux_flag := parse_flag_directive_line('#flag linux -lm', '/tmp/source.v', 'mac') or { '' }
	assert macos_flag == '-framework Cocoa'
	assert linux_flag == ''
}

fn test_file_has_incompatible_os_suffix_windows() {
	assert pref.file_has_incompatible_os_suffix('time_solaris.c.v', 'windows')
	assert pref.file_has_incompatible_os_suffix('time_freebsd.c.v', 'windows')
	assert pref.file_has_incompatible_os_suffix('time_openbsd.c.v', 'windows')
	assert pref.file_has_incompatible_os_suffix('time_netbsd.c.v', 'windows')
	assert pref.file_has_incompatible_os_suffix('time_dragonfly.c.v', 'windows')
	assert pref.file_has_incompatible_os_suffix('time_nix.c.v', 'windows')
	assert !pref.file_has_incompatible_os_suffix('time_windows.c.v', 'windows')
}

fn test_file_has_incompatible_os_suffix_non_windows_targets() {
	assert !pref.file_has_incompatible_os_suffix('time_solaris.c.v', 'solaris')
	assert !pref.file_has_incompatible_os_suffix('time_freebsd.c.v', 'freebsd')
	assert !pref.file_has_incompatible_os_suffix('time_darwin.c.v', 'macos')
	assert !pref.file_has_incompatible_os_suffix('time_macos.c.v', 'darwin')
	assert !pref.file_has_incompatible_os_suffix('time_android_outside_termux.c.v', 'android')
	assert pref.file_has_incompatible_os_suffix('time_windows.c.v', 'linux')
	assert pref.file_has_incompatible_os_suffix('time_android_outside_termux.c.v', 'linux')
}

fn test_default_cc_uses_tcc_when_available() {
	vroot := os.dir(os.dir(@FILE))
	tcc_path := os.join_path(vroot, 'thirdparty', 'tcc', 'tcc.exe')
	cc := default_cc(vroot)
	if os.exists(tcc_path) {
		assert cc.contains('tcc')
	} else {
		assert cc == 'cc'
	}
}
