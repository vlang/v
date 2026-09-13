module main

import os
import v.pref

fn test_macos_v3_routes_cross_modes_to_v1_compatibility() {
	cross_os := if pref.get_host_os() == .windows { pref.OS.linux } else { pref.OS.windows }
	mut prefs := &pref.Preferences{
		path: 'main.v'
		backend: .c
		os: cross_os
	}
	assert macos_v3_needs_v1_compatibility('main.v', prefs)
	prefs.new_compiler = true
	assert !macos_v3_needs_v1_compatibility('main.v', prefs)

	portable := &pref.Preferences{
		path: 'cmd/v'
		backend: .c
		output_cross_c: true
	}
	assert macos_v3_needs_v1_compatibility('cmd/v', portable)
}

fn test_windows_msvc_routes_to_v1_compatibility() {
	mut prefs := &pref.Preferences{
		path: 'main.v'
		backend: .c
		ccompiler: 'msvc'
		ccompiler_type: .msvc
	}
	assert macos_v3_windows_msvc_needs_v1_compatibility(prefs, .windows)
	assert !macos_v3_windows_msvc_needs_v1_compatibility(prefs, .linux)
	prefs.new_compiler = true
	assert macos_v3_windows_msvc_needs_v1_compatibility(prefs, .windows)
	$if windows {
		assert macos_v3_needs_v1_compatibility('main.v', prefs)
	}
}

fn test_macos_v3_routes_internal_tool_bootstrap_to_v1_compatibility() {
	vroot := os.dir(@VEXE)
	tool_path := os.join_path(vroot, 'cmd', 'tools', 'vpm')
	mut prefs := &pref.Preferences{
		path: tool_path
		backend: .c
	}
	assert macos_v3_needs_v1_compatibility(tool_path, prefs)
	prefs.new_compiler = true
	assert !macos_v3_needs_v1_compatibility(tool_path, prefs)
}

fn test_macos_v3_routes_unsupported_c_options_to_v1_compatibility() {
	mut prefs := &pref.Preferences{
		path: 'main.v'
		backend: .c
		no_std: true
	}
	assert macos_v3_needs_v1_compatibility('main.v', prefs)
	prefs.new_compiler = true
	assert !macos_v3_needs_v1_compatibility('main.v', prefs)
}

fn test_macos_v3_does_not_forward_private_flags_to_v1() {
	args := ['-silent', macos_v3_internal_quiet_flag, macos_v3_compat_c99_flag, 'main.v']
	assert macos_v1_fallback_args(args) == ['-silent', 'main.v']
}

fn test_native_cmd_v_self_build_does_not_preselect_v1_compatibility() {
	vroot := os.dir(@VEXE)
	prefs := &pref.Preferences{
		path: os.join_path(vroot, 'cmd', 'v')
		backend: .c
	}
	assert !macos_v3_needs_v1_compatibility('cmd/v', prefs)
}

fn test_temporary_self_build_bootstraps_only_before_v1_fallback_exists() {
	vroot := os.dir(@VEXE)
	mut prefs := &pref.Preferences{
		path: os.join_path(vroot, 'cmd', 'v')
		backend: .c
	}
	missing_fallback := os.join_path(os.vtmp_dir(), 'missing_v1_fallback_${os.getpid()}')
	os.rm(missing_fallback) or {}
	assert macos_v3_needs_bootstrap_before_v1_fallback(prefs, true, os.join_path(vroot, 'v1'), missing_fallback)
	assert !macos_v3_needs_bootstrap_before_v1_fallback(prefs, false, os.join_path(vroot, 'v1'), missing_fallback)
	prefs.old_compiler = true
	assert macos_v3_needs_bootstrap_before_v1_fallback(prefs, false, os.join_path(vroot, 'v1'), missing_fallback)
	assert !macos_v3_needs_bootstrap_before_v1_fallback(prefs, true, os.join_path(vroot, 'vnew'), missing_fallback)
	assert !macos_v3_needs_bootstrap_before_v1_fallback(prefs, true, os.join_path(vroot, 'v1'), @VEXE)
}

fn test_lazy_v1_fallback_finds_the_v_source_tree() {
	vroot := os.real_path(os.dir(@VEXE))
	workdir := macos_v3_make_workdir(os.join_path(vroot, macos_v3_v1_fallback_binary)) or {
		panic('could not find the V source tree from ${vroot}')
	}
	assert workdir == vroot
}

fn test_lazy_v1_fallback_runs_make_v1_when_the_binary_is_missing() {
	$if !windows {
		vroot := os.real_path(os.dir(@VEXE))
		root := os.join_path(vroot, '.tmp_v1_fallback_test_${os.getpid()}')
		bin_dir := os.join_path(root, 'bin')
		fallback := os.join_path(root, macos_v3_v1_fallback_binary)
		make_log := os.join_path(root, 'make_args')
		primary := os.join_path(root, 'v')
		fake_make := os.join_path(bin_dir, 'make')
		old_path := os.getenv('PATH')
		old_vexe := os.getenv_opt('VEXE')
		os.rmdir_all(root) or {}
		os.mkdir_all(bin_dir)!
		defer {
			os.setenv('PATH', old_path, true)
			if vexe := old_vexe {
				os.setenv('VEXE', vexe, true)
			} else {
				os.unsetenv('VEXE')
			}
			os.rmdir_all(root) or {}
		}
		os.write_file(primary, '')!
		os.chmod(primary, 0o700)!
		make_script := '#!/bin/sh\n' + 'printf \'%s\\n\' "\$*" > ${os.quoted_path(make_log)}\n' + "printf '%s\\n' '#!/bin/sh' 'exit 0' > ${os.quoted_path(fallback)}\n" + 'chmod +x ${os.quoted_path(fallback)}\n'
		os.write_file(fake_make, make_script)!
		os.chmod(fake_make, 0o700)!
		os.setenv('PATH', '${bin_dir}:${old_path}', true)
		os.setenv('VEXE', primary, true)
		assert build_missing_macos_v1_fallback(fallback, 'test fallback')
		assert os.is_executable(fallback)
		assert os.read_file(make_log)!.trim_space() == 'VEXE=${os.real_path(primary)} v1'
	}
}
