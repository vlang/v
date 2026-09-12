module driver

import os
import v3.cmdexec
import v3.pref

fn v3_driver_test_executable() string {
	if os.base(@VEXE) in ['v1_fallback', 'v1_fallback.exe'] {
		return os.join_path(os.dir(@VEXE), 'v' + $if windows { '.exe' } $else { '' })
	}
	return @VEXE
}

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

fn test_v3_default_compiler_uses_implicit_tcc() {
	bundled_tcc := os.join_path(os.temp_dir(), 'thirdparty', 'tcc', 'tcc.exe')
	system_tcc := os.join_path(os.temp_dir(), 'bin', 'tcc.exe')
	assert v3_select_implicit_c_compiler('cc', false, bundled_tcc) == bundled_tcc
	assert v3_select_implicit_c_compiler('cc', false, system_tcc) == system_tcc
	assert v3_select_implicit_c_compiler('cc', false, '') == 'cc'
	assert v3_select_implicit_c_compiler('clang', true, bundled_tcc) == 'clang'
}

fn test_v3_platform_c_compiler() {
	assert v3_platform_c_compiler('windows') == 'gcc'
	assert v3_platform_c_compiler('linux') == 'cc'
	assert v3_platform_c_compiler('macos') == 'cc'
}

fn test_v3_implicit_tcc_uses_platform_compiler_for_non_c_objects() {
	implicit_tcc := os.join_path(os.temp_dir(), 'bin', 'tcc')
	assert c_source_object_compiler('', implicit_tcc, true, 'linux') == implicit_tcc
	assert c_source_object_compiler('objective-c', implicit_tcc, true, 'linux') == 'cc'
	assert c_source_object_compiler('objective-c', implicit_tcc, true, 'windows') == 'gcc'
	assert c_source_object_compiler('c++', implicit_tcc, true, 'linux') == 'c++'
	assert c_source_object_compiler('objective-c++', implicit_tcc, true, 'linux') == 'c++'
	assert c_source_object_compiler('objective-c', 'cc', false, 'linux') == 'cc'
	assert c_source_object_compiler('c++', 'cc', false, 'linux') == 'c++'
	assert c_source_object_compiler('objective-c', 'clang', false, 'linux') == 'clang'
	assert c_source_object_compiler('c++', 'clang', false, 'linux') == 'clang'
	assert c_source_object_compiler('objective-c', 'tcc', false, 'linux') == 'tcc'
	assert c_source_object_compiler('c++', 'tcc', false, 'linux') == 'tcc'
}

fn test_v3_bundled_tcc_probe_eligibility() {
	linux_target := pref.Target{
		os: 'linux'
		arch: 'amd64'
	}
	bundled_tcc := os.join_path(os.vtmp_dir(), 'v3_probe_eligibility', 'thirdparty', 'tcc', 'tcc.exe')
	base := V3BundledTccProbeOptions{
		backend: 'c'
		c_compiler: 'cc'
		host_os: 'linux'
		host_target: linux_target
		target: linux_target
		bundled_tcc: bundled_tcc
	}
	assert v3_should_probe_bundled_tcc(base)
	assert !v3_should_probe_bundled_tcc(V3BundledTccProbeOptions{
		...base
		backend: 'wasm'
	})
	assert !v3_should_probe_bundled_tcc(V3BundledTccProbeOptions{
		...base
		c_only: true
	})
	assert !v3_should_probe_bundled_tcc(V3BundledTccProbeOptions{
		...base
		is_prod: true
	})
	assert !v3_should_probe_bundled_tcc(V3BundledTccProbeOptions{
		...base
		is_c_debug: true
	})
	assert !v3_should_probe_bundled_tcc(V3BundledTccProbeOptions{
		...base
		dump_c_flags: true
	})
	assert !v3_should_probe_bundled_tcc(V3BundledTccProbeOptions{
		...base
		parallel_cc: true
	})
	windows_target := pref.Target{
		os: 'windows'
		arch: 'amd64'
	}
	assert v3_should_probe_bundled_tcc(V3BundledTccProbeOptions{
		...base
		parallel_cc: true
		host_os: 'windows'
		host_target: windows_target
		target: windows_target
	})
	assert v3_should_probe_bundled_tcc(V3BundledTccProbeOptions{
		...base
		c_compiler: 'tcc'
		c_compiler_explicit: true
		dump_c_flags: true
	})
	assert v3_should_probe_bundled_tcc(V3BundledTccProbeOptions{
		...base
		c_compiler: 'tcc'
		c_compiler_explicit: true
		parallel_cc: true
	})
	assert !v3_should_probe_bundled_tcc(V3BundledTccProbeOptions{
		...base
		c_compiler: 'clang'
		c_compiler_explicit: true
	})
	assert !v3_should_probe_bundled_tcc(V3BundledTccProbeOptions{
		...base
		target: pref.Target{
			os: 'linux'
			arch: 'arm64'
		}
	})
	assert v3_should_probe_bundled_tcc(V3BundledTccProbeOptions{
		...base
		is_prod: true
		c_compiler: 'tcc'
		c_compiler_explicit: true
	})
	assert v3_should_probe_bundled_tcc(V3BundledTccProbeOptions{
		...base
		c_compiler: bundled_tcc
		c_compiler_explicit: true
	})
	assert !v3_should_probe_bundled_tcc(V3BundledTccProbeOptions{
		...base
		c_compiler: os.join_path(os.vtmp_dir(), 'bin', 'tcc')
		c_compiler_explicit: true
	})
	assert v3_should_probe_bundled_tcc(V3BundledTccProbeOptions{
		...base
		is_prod: true
		is_c_debug: true
		host_os: 'windows'
		host_target: windows_target
		target: windows_target
	})
}

fn test_v3_bundled_tcc_probe_does_not_run_when_ineligible() {
	$if windows {
		return
	}
	test_root := os.join_path(os.vtmp_dir(), 'v3_bundled_tcc_probe_${os.getpid()}')
	probe_marker := os.join_path(test_root, 'tcc_was_probed')
	bundled_tcc := os.join_path(test_root, 'thirdparty', 'tcc', 'tcc.exe')
	os.mkdir_all(os.dir(bundled_tcc)) or { panic(err) }
	os.write_file(bundled_tcc, '#!/bin/sh\nprintf probed > ${os.quoted_path(probe_marker)}\nexit 0\n') or { panic(err) }
	os.chmod(bundled_tcc, 0o700) or { panic(err) }
	defer {
		os.rmdir_all(test_root) or {}
	}
	linux_target := pref.Target{
		os: 'linux'
		arch: 'amd64'
	}
	base := V3BundledTccProbeOptions{
		backend: 'c'
		c_compiler: 'cc'
		host_os: 'linux'
		host_target: linux_target
		target: linux_target
		bundled_tcc: bundled_tcc
	}
	assert !v3_bundled_tcc_available(V3BundledTccProbeOptions{
		...base
		is_prod: true
	})
	assert !os.is_file(probe_marker)
	assert v3_bundled_tcc_available(base)
	assert os.is_file(probe_marker)
}

fn test_v3_default_tcc_compiler_uses_working_system_tcc() {
	$if windows {
		return
	}
	test_root := os.join_path(os.vtmp_dir(), 'v3_default_system_tcc_${os.getpid()}')
	system_tcc := write_v3_test_tcc(os.join_path(test_root, 'bin', 'tcc'), 0)
	old_path := os.getenv_opt('PATH')
	os.setenv('PATH', os.dir(system_tcc), true)
	defer {
		if value := old_path {
			os.setenv('PATH', value, true)
		} else {
			os.unsetenv('PATH')
		}
		os.rmdir_all(test_root) or {}
	}
	bundled_tcc := write_v3_test_tcc(os.join_path(test_root, 'thirdparty', 'tcc', 'tcc.exe'), 1)
	assert !v3_usable_tcc_compiler(bundled_tcc)
	implicit_tcc := v3_default_tcc_compiler(bundled_tcc, v3_usable_tcc_compiler(bundled_tcc), true, false, 'linux')
	assert implicit_tcc == system_tcc
	assert v3_effective_c_compiler_for_codegen('c', 'cc', implicit_tcc != '', pref.host_target()) == 'tinyc'
	write_v3_test_tcc(bundled_tcc, 0)
	assert v3_usable_tcc_compiler(bundled_tcc)
	assert v3_default_tcc_compiler(bundled_tcc, v3_usable_tcc_compiler(bundled_tcc), true, false, 'linux') == bundled_tcc
	assert v3_default_tcc_compiler(bundled_tcc, true, true, true, 'linux') == ''
	assert v3_default_tcc_compiler(bundled_tcc, false, false, false, 'linux') == ''
	assert v3_default_tcc_compiler(bundled_tcc, false, true, false, 'macos') == ''
}

fn test_v3_default_tcc_compiler_skips_broken_system_tcc() {
	$if windows {
		return
	}
	test_root := os.join_path(os.vtmp_dir(), 'v3_broken_system_tcc_${os.getpid()}')
	system_tcc := write_v3_test_tcc(os.join_path(test_root, 'bin', 'tcc'), 1)
	old_path := os.getenv_opt('PATH')
	os.setenv('PATH', os.dir(system_tcc), true)
	defer {
		if value := old_path {
			os.setenv('PATH', value, true)
		} else {
			os.unsetenv('PATH')
		}
		os.rmdir_all(test_root) or {}
	}
	bundled_tcc := os.join_path(test_root, 'missing', 'tcc.exe')
	assert v3_default_tcc_compiler(bundled_tcc, false, true, false, 'linux') == ''
}

fn test_v3_system_tcc_does_not_use_bundled_resources() {
	vroot := os.join_path(os.temp_dir(), 'v3_system_tcc_resources')
	bundled_tcc := os.join_path(vroot, 'thirdparty', 'tcc', 'tcc.exe')
	system_tcc := os.join_path(os.path_separator, 'usr', 'bin', 'tcc')
	resources := v3_tcc_resource_flags_for_compiler(vroot, system_tcc, bundled_tcc, false)
	assert resources == V3TccResourceFlags{}
	bundled_resources := v3_tcc_resource_flags_for_compiler(vroot, bundled_tcc, bundled_tcc, true)
	assert bundled_resources.base_arg.contains('thirdparty')
}

fn test_v3_system_tcc_runtime_requires_windows_openlibm() {
	test_root := os.join_path(os.vtmp_dir(), 'v3_system_tcc_runtime_${os.getpid()}')
	os.rmdir_all(test_root) or {}
	defer {
		os.rmdir_all(test_root) or {}
	}
	assert v3_system_tcc_runtime_available(test_root, 'linux')
	assert !v3_system_tcc_runtime_available(test_root, 'windows')
	openlibm := os.join_path(test_root, 'thirdparty', 'tcc', 'lib', 'openlibm.o')
	os.mkdir_all(os.dir(openlibm)) or { panic(err) }
	os.write_file(openlibm, '') or { panic(err) }
	assert v3_system_tcc_runtime_available(test_root, 'windows')
}

fn test_v3_regenerates_cc_fallback_after_implicit_tcc() {
	assert !v3_should_regenerate_after_implicit_tcc(true, false, false, 0)
	assert !v3_should_regenerate_after_implicit_tcc(true, false, true, 1)
	assert !v3_should_regenerate_after_implicit_tcc(true, true, true, 0)
	assert v3_should_regenerate_after_implicit_tcc(true, true, true, 1)
	assert v3_should_regenerate_after_implicit_tcc(true, true, false, 0)
	assert !v3_should_regenerate_after_implicit_tcc(false, true, true, 1)
	assert !v3_should_regenerate_after_implicit_tcc(false, true, false, 0)
}

fn test_v3_retry_compilation_preserves_internal_quiet() {
	args := [macos_v3_internal_quiet_flag, macos_v3_compat_c99_flag, '-autofree', 'run', 'main.v']
	retry := v3_retry_compilation_args(args, -1, 'cc')
	assert macos_v3_internal_quiet_flag in retry
	assert macos_v3_compat_c99_flag !in retry
	assert retry[0] == '-no-retry-compilation'
}

fn write_v3_test_tcc(tcc_path string, exit_code int) string {
	os.mkdir_all(os.dir(tcc_path)) or { panic(err) }
	os.write_file(tcc_path, '#!/bin/sh\nexit ${exit_code}\n') or { panic(err) }
	os.chmod(tcc_path, 0o700) or { panic(err) }
	return tcc_path
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
	build := cmdexec.run(v3_driver_test_executable(), ['-new-compiler', '-nocache', '-prod', '-showcc',
		'-o', output, source])
	assert build.exit_code == 0, build.output
	normalized_output := build.output.replace('\\', '/')
	assert normalized_output.contains('thirdparty/tcc/tcc.exe'), build.output
	assert normalized_output.contains('-B') && normalized_output.contains('thirdparty/tcc'), build.output
	assert !normalized_output.contains('-flto'), build.output
	run_result := cmdexec.run(output, [])
	assert run_result.exit_code == 42, run_result.output
}

fn test_v3_windows_auto_gui_build_uses_windows_subsystem() {
	$if !windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v3_windows_auto_gui_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	output := os.join_path(root, 'main.exe')
	os.write_file(source, '#flag windows -lgdi32\n\nfn main() {}\n')!
	build := cmdexec.run(v3_driver_test_executable(), ['-new-compiler', '-nocache', '-showcc', '-o',
		output, source])
	assert build.exit_code == 0, build.output
	assert build.output.contains('-mwindows'), build.output
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
	expected := ['-municode', '-Wl,--stack=33554432']
	assert v3_windows_executable_linker_flags('windows', 'tinyc', false, false, .auto, false) == expected
	assert v3_windows_executable_linker_flags('windows', 'gcc', false, false, .auto, false) == expected
	assert v3_windows_executable_linker_flags('windows', 'tinyc', false, false, .auto, true) == [
		'-municode',
		'-mwindows',
		'-Wl,--stack=33554432',
	]
	assert v3_windows_executable_linker_flags('windows', 'tinyc', false, false, .console, true) == [
		'-municode',
		'-mconsole',
		'-Wl,--stack=33554432',
	]
	assert v3_windows_executable_linker_flags('windows', 'tinyc', false, false, .windows, false) == [
		'-municode',
		'-mwindows',
		'-Wl,--stack=33554432',
	]
	assert v3_windows_executable_linker_flags('windows', 'msvc', false, false, .auto, true) == []
	assert v3_windows_executable_linker_flags('windows', 'tinyc', true, false, .auto, true) == []
	assert v3_windows_executable_linker_flags('windows', 'tinyc', false, true, .auto, true) == []
	assert v3_windows_executable_linker_flags('linux', 'tinyc', false, false, .auto, true) == []
	plan := v3_c_compiler_flag_plan(V3CCompilerFlagOptions{
		target_os: 'windows'
		c_compiler: 'tinyc'
		windows_gui_app: true
	})
	assert '-municode' in plan.before_inputs
	assert '-mwindows' in plan.before_inputs
	assert '-Wl,--stack=33554432' in plan.before_inputs
}

fn test_v3_cgen_metadata_preserves_windows_gui_entry_point() {
	encoded := encode_v3_cgen_metadata(['-lgdi32'], 'interfaces', 'prefix', true, []V3CachedTypeDiagnostic{})
	decoded := decode_v3_cgen_metadata(encoded) or { panic('could not decode Cgen metadata') }
	assert decoded.windows_gui_entry_point
	assert decoded.flags == ['-lgdi32']
}

fn test_v3_fastc_rejects_windows_gui_subsystem() {
	root := os.join_path(os.vtmp_dir(), 'v3_fastc_windows_subsystem_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'fn main() {}\n')!
	build := cmdexec.run(v3_driver_test_executable(), ['-new-compiler', '-nocache', '-b', 'fastc',
		'-os', 'windows', '-subsystem', 'windows', source])
	assert build.exit_code != 0, build.output
	assert build.output.contains('the V3 fastc backend does not support `-subsystem windows`'), build.output
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

fn test_tcc_atomic_object_key_separates_compilers_targets_and_args() {
	root := os.join_path(os.vtmp_dir(), 'v3_atomic_object_key_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	target := pref.Target{
		os: 'macos'
		arch: 'arm64'
		object_format: 'macho'
	}
	first_tcc := os.join_path(root, 'first_tcc')
	second_tcc := os.join_path(root, 'second_tcc')
	os.write_file(first_tcc, 'first')!
	os.write_file(second_tcc, 'second compiler build')!
	args := ['-std=gnu11', '-fwrapv']
	key := tcc_atomic_object_key('source-signature', first_tcc, args, target)

	assert key == tcc_atomic_object_key('source-signature', first_tcc, args, target)
	// A different compiler build must not reuse the object: tcc emits ELF
	// objects even on macOS, and rejects a Mach-O object with
	// "unrecognized file type" instead of falling back to reassembling atomic.S.
	assert key != tcc_atomic_object_key('source-signature', second_tcc, args, target)
	assert key != tcc_atomic_object_key('other-signature', first_tcc, args, target)
	assert key != tcc_atomic_object_key('source-signature', first_tcc, ['-std=gnu11'], target)
	assert key != tcc_atomic_object_key('source-signature', first_tcc, args, pref.Target{
		...target
		arch: 'amd64'
	})
}

fn test_tcc_compiler_identity_tracks_rebuilds() {
	root := os.join_path(os.vtmp_dir(), 'v3_atomic_compiler_identity_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	compiler := os.join_path(root, 'tcc.exe')
	os.write_file(compiler, 'first')!
	first := tcc_compiler_identity(compiler)
	assert first == tcc_compiler_identity(compiler)
	// `make` re-pulls and rebuilds thirdparty/tcc in place, so the identity has
	// to change when the executable at the same path does.
	os.write_file(compiler, 'a rebuilt compiler with a different size')!
	assert first != tcc_compiler_identity(compiler)
}

fn test_tcc_compiler_identity_tracks_same_size_rebuilds() {
	root := os.join_path(os.vtmp_dir(), 'v3_atomic_identity_same_size_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	compiler := os.join_path(root, 'tcc.exe')
	os.write_file(compiler, 'first build')!
	first := tcc_compiler_identity(compiler)
	// Rewritten in place to the same byte count, within one filesystem timestamp
	// second. Size plus a second-resolution mtime cannot separate these two
	// compilers, and reusing an object across them is what produces an
	// "unrecognized file type" link failure that no later build clears.
	os.write_file(compiler, 'other build')!
	assert os.file_size(compiler) == 11
	assert first != tcc_compiler_identity(compiler)
}
