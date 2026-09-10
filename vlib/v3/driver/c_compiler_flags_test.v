module driver

import os
import v3.cmdexec
import v3.pref

fn test_input_is_cmd_v_accepts_relative_entry_file() {
	assert input_is_cmd_v('cmd/v')
	assert input_is_cmd_v('cmd/v/v.v')
}

fn test_v3_tcc_backtrace_enabled() {
	assert !v3_tcc_backtrace_enabled('macos', 'arm64', false)
	assert v3_tcc_backtrace_enabled('macos', 'amd64', false)
	assert v3_tcc_backtrace_enabled('linux', 'arm64', false)
	assert !v3_tcc_backtrace_enabled('linux', 'arm64', true)
}

fn test_v3_prefers_bundled_tcc_for_debug_selfhost() {
	host := pref.host_target()
	assert v3_should_prefer_bundled_tcc_for_selfhost(true, 'c', false, false, false, false, host, true)
	assert !v3_should_prefer_bundled_tcc_for_selfhost(false, 'c', false, false, false, false, host, true)
	assert !v3_should_prefer_bundled_tcc_for_selfhost(true, 'fastc', false, false, false, false, host, true)
	assert !v3_should_prefer_bundled_tcc_for_selfhost(true, 'c', true, false, false, false, host, true)
	assert !v3_should_prefer_bundled_tcc_for_selfhost(true, 'c', false, true, false, false, host, true)
	assert !v3_should_prefer_bundled_tcc_for_selfhost(true, 'c', false, false, true, false, host, true)
	assert !v3_should_prefer_bundled_tcc_for_selfhost(true, 'c', false, false, false, true, host, true)
	assert !v3_should_prefer_bundled_tcc_for_selfhost(true, 'c', false, false, false, false, host, false)
}

fn test_v3_regenerates_cc_fallback_after_preferred_tcc() {
	assert !v3_should_regenerate_for_cc_fallback(false, false, 0)
	assert !v3_should_regenerate_for_cc_fallback(false, true, 1)
	assert !v3_should_regenerate_for_cc_fallback(true, true, 0)
	assert v3_should_regenerate_for_cc_fallback(true, true, 1)
	assert v3_should_regenerate_for_cc_fallback(true, false, 0)
}

fn test_v3_tcc_flag_plan_skips_backtrace_on_macos_arm64() {
	vroot := os.join_path(os.temp_dir(), 'v3_tcc_flag_plan')
	plan := v3_c_compiler_flag_plan(V3CCompilerFlagOptions{
		is_tcc: true
		target_os: 'macos'
		target_arch: 'arm64'
		vroot: vroot
	})
	assert '-bt25' !in plan.before_inputs
	tcc_install_dir := os.join_path(vroot, 'thirdparty', 'tcc', 'lib')
	assert '-B${tcc_install_dir}' in plan.before_inputs
	assert '-I${os.join_path_single(tcc_install_dir, 'include')}' in plan.before_inputs
	assert '-L${tcc_install_dir}' in plan.before_inputs
}

fn test_v3_tcc_resource_flags_use_windows_bundle_root() {
	vroot := os.join_path(os.temp_dir(), 'v3_windows_tcc_flag_plan_${os.getpid()}')
	os.rmdir_all(vroot) or {}
	tcc_root := os.join_path(vroot, 'thirdparty', 'tcc')
	tcc_lib := os.join_path_single(tcc_root, 'lib')
	tcc_include := os.join_path_single(tcc_root, 'include')
	os.mkdir_all(tcc_lib)!
	os.mkdir_all(os.join_path_single(tcc_include, 'winapi'))!
	defer {
		os.rmdir_all(vroot) or {}
	}
	resources := v3_tcc_resource_flags(vroot)
	assert resources.base_arg == '-B${tcc_root}'
	assert resources.include_arg == '-I${tcc_include}'
	assert resources.library_arg == '-L${tcc_lib}'
}

fn test_v3_windows_tcc_prod_flag_plan_uses_tcc_resources() {
	vroot := os.join_path(os.vtmp_dir(), 'v3_windows_tcc_prod_flag_plan_${os.getpid()}')
	os.rmdir_all(vroot) or {}
	tcc_root := os.join_path(vroot, 'thirdparty', 'tcc')
	tcc_lib := os.join_path_single(tcc_root, 'lib')
	tcc_include := os.join_path_single(tcc_root, 'include')
	os.mkdir_all(tcc_lib)!
	os.mkdir_all(os.join_path_single(tcc_include, 'winapi'))!
	defer {
		os.rmdir_all(vroot) or {}
	}
	plan := v3_c_compiler_flag_plan(V3CCompilerFlagOptions{
		is_tcc: true
		is_prod: true
		target_os: 'windows'
		target_arch: 'amd64'
		c_compiler: 'tinyc'
		vroot: vroot
	})
	assert '-O3' in plan.before_inputs
	assert '-flto' !in plan.before_inputs
	assert '-B${tcc_root}' in plan.before_inputs
	assert '-I${tcc_include}' in plan.before_inputs
	assert '-L${tcc_lib}' in plan.before_inputs
}

fn test_v3_tcc_flag_plan_restores_native_local_prefix() {
	host_os := os.user_os()
	plan := v3_c_compiler_flag_plan(V3CCompilerFlagOptions{
		is_tcc: true
		target_os: host_os
		target_arch: 'amd64'
		vroot: os.join_path(os.temp_dir(), 'v3_tcc_native_flag_plan')
	})
	if host_os == 'windows' {
		assert '-I/usr/local/include' !in plan.before_inputs
		assert '-L/usr/local/lib' !in plan.before_inputs
	} else {
		assert '-I/usr/local/include' in plan.before_inputs
		assert '-L/usr/local/lib' in plan.before_inputs
	}
}

fn test_v3_windows_default_tcc_prod_build() {
	$if !windows {
		return
	}
	bundled_tcc := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe')
	assert os.is_executable(bundled_tcc)
	root := os.join_path(os.vtmp_dir(), 'v3_windows_default_tcc_prod_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	output := os.join_path(root, 'main.exe')
	os.write_file(source, 'fn main() {\n\texit(42)\n}\n')!
	old_vflags := os.getenv_opt('VFLAGS')
	os.unsetenv('VFLAGS')
	defer {
		if value := old_vflags {
			os.setenv('VFLAGS', value, true)
		}
	}
	build := cmdexec.run(@VEXE, ['-new-compiler', '-nocache', '-prod', '-showcc', '-o', output,
		source])
	assert build.exit_code == 0, build.output
	normalized_output := build.output.replace('\\', '/')
	assert normalized_output.contains('thirdparty/tcc/tcc.exe'), build.output
	assert normalized_output.contains('-B') && normalized_output.contains('thirdparty/tcc'), build.output
	assert !normalized_output.contains('-flto'), build.output
	run_result := cmdexec.run(output, [])
	assert run_result.exit_code == 42, run_result.output
}

fn test_add_v3_tcc_compat_defines() {
	mut macos_arm64 := []string{}
	add_v3_tcc_compat_defines(mut macos_arm64, 'macos', 'arm64', false, true)
	assert macos_arm64 == ['no_backtrace']

	mut shared_defines := ['custom']
	add_v3_tcc_compat_defines(mut shared_defines, 'linux', 'amd64', true, true)
	assert shared_defines == ['custom', 'no_backtrace']

	mut supported := []string{}
	add_v3_tcc_compat_defines(mut supported, 'linux', 'arm64', false, true)
	assert supported.len == 0

	mut other_compiler := []string{}
	add_v3_tcc_compat_defines(mut other_compiler, 'macos', 'arm64', false, false)
	assert other_compiler.len == 0
}

fn test_v3_default_linker_flags() {
	assert v3_default_linker_flags('windows', false) == []
	assert v3_default_linker_flags('linux', false) == ['-lm', '-lpthread']
	assert v3_default_linker_flags('freebsd', false) == ['-lm', '-lpthread', '-lexecinfo', '-lelf']
	assert v3_default_linker_flags('netbsd', false) == ['-lm', '-lpthread', '-lexecinfo', '-lelf']
	assert v3_default_linker_flags('linux', true) == []
}

fn test_v3_default_linker_flags_do_not_duplicate_existing_flags() {
	mut flags := ['-lpthread', '-lm']
	add_v3_default_linker_flags(mut flags, 'linux', false)
	assert flags == ['-lpthread', '-lm']
}

fn test_v3_fastc_default_linker_flags() {
	assert v3_fastc_default_linker_flags('windows', true) == []
	assert v3_fastc_default_linker_flags('linux', false) == ['-lm']
	assert v3_fastc_default_linker_flags('linux', true) == ['-lpthread', '-lm']
}

fn test_v3_windows_executable_linker_flags() {
	expected := ['-municode', '-Wl,-stack=33554432']
	assert v3_windows_executable_linker_flags('windows', 'tinyc', false, false) == expected
	assert v3_windows_executable_linker_flags('windows', 'gcc', false, false) == expected
	assert v3_windows_executable_linker_flags('windows', 'msvc', false, false) == []
	assert v3_windows_executable_linker_flags('windows', 'tinyc', true, false) == []
	assert v3_windows_executable_linker_flags('windows', 'tinyc', false, true) == []
	assert v3_windows_executable_linker_flags('linux', 'tinyc', false, false) == []
	plan := v3_c_compiler_flag_plan(V3CCompilerFlagOptions{
		target_os: 'windows'
		c_compiler: 'tinyc'
	})
	assert '-municode' in plan.before_inputs
	assert '-Wl,-stack=33554432' in plan.before_inputs
}

fn test_add_c_language_runtime_link_flags() {
	target := pref.Target{
		os: 'linux'
	}
	mut objective_c := []string{}
	add_c_language_runtime_link_flags(mut objective_c, [], 'objective-c', target)
	assert objective_c == ['-lobjc']

	mut objective_cpp := []string{}
	add_c_language_runtime_link_flags(mut objective_cpp, [], 'objective-c++', target)
	assert objective_cpp == ['-lstdc++', '-lobjc']

	mut existing := ['-lstdc++', '-lobjc']
	add_c_language_runtime_link_flags(mut existing, existing.clone(), 'objective-c++', target)
	assert existing == ['-lstdc++', '-lobjc']
}
