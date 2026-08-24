module driver

fn test_v3_tcc_backtrace_enabled() {
	assert !v3_tcc_backtrace_enabled('macos', 'arm64', false)
	assert v3_tcc_backtrace_enabled('macos', 'amd64', false)
	assert v3_tcc_backtrace_enabled('linux', 'arm64', false)
	assert !v3_tcc_backtrace_enabled('linux', 'arm64', true)
}

fn test_v3_explicit_tcc_flag_plan_skips_backtrace_on_macos_arm64() {
	plan := v3_c_compiler_flag_plan(V3CCompilerFlagOptions{
		explicit_tcc: true
		target_os:    'macos'
		target_arch:  'arm64'
	})
	assert '-bt25' !in plan.before_inputs
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
	assert v3_default_linker_flags('windows', false) == ['-lm']
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
