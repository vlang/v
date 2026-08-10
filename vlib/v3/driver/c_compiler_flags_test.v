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
