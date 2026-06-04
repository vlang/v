module pref

fn test_comptime_flag_value_uses_target_os_preference() {
	mut prefs := new_preferences()
	prefs.target_os = 'windows'
	assert comptime_flag_value(&prefs, 'windows')
	assert !comptime_flag_value(&prefs, 'linux')
	assert !comptime_flag_value(&prefs, 'macos')

	prefs.target_os = 'darwin'
	assert comptime_flag_value(&prefs, 'macos')
	assert comptime_flag_value(&prefs, 'darwin')
	assert comptime_flag_value(&prefs, 'bsd')
	assert !comptime_flag_value(&prefs, 'windows')

	prefs.target_os = 'termux'
	assert comptime_flag_value(&prefs, 'termux')
	assert !comptime_flag_value(&prefs, 'android')
	assert !comptime_flag_value(&prefs, 'linux')
}

fn test_comptime_flag_value_allows_nil_preferences() {
	prefs := unsafe { &Preferences(nil) }
	assert comptime_flag_value(prefs, 'linux') == (prefs.target_os_or_host() == 'linux')
	assert !comptime_flag_value(prefs, 'definitely_missing_flag')
}

fn test_comptime_pkgconfig_value_reports_missing_packages_as_false() {
	assert !comptime_pkgconfig_value('__v2_definitely_missing_pkgconfig_test_package__')
}

fn test_comptime_optional_flag_value_uses_user_defines_only() {
	mut prefs := new_preferences()
	prefs.target_os = 'linux'
	assert comptime_flag_value(&prefs, 'linux')
	assert !comptime_optional_flag_value(&prefs, 'linux')

	prefs.user_defines = ['linux']
	prefs.explicit_user_defines = ['linux']
	assert comptime_optional_flag_value(&prefs, 'linux')

	prefs.target_os = 'windows'
	assert !comptime_flag_value(&prefs, 'linux')
	assert comptime_optional_flag_value(&prefs, 'linux')
}

fn test_comptime_optional_target_modes_ignore_synthesized_defines() {
	mut cross_prefs := new_preferences()
	cross_prefs.target_os = 'cross'
	cross_prefs.output_cross_c = true
	cross_prefs.user_defines = ['cross']
	assert comptime_flag_value(&cross_prefs, 'cross')
	assert !comptime_optional_flag_value(&cross_prefs, 'cross')
	cross_prefs.explicit_user_defines = ['cross']
	assert comptime_optional_flag_value(&cross_prefs, 'cross')

	mut free_prefs := new_preferences()
	free_prefs.freestanding = true
	free_prefs.user_defines = ['freestanding']
	assert comptime_flag_value(&free_prefs, 'freestanding')
	assert !comptime_optional_flag_value(&free_prefs, 'freestanding')
	free_prefs.explicit_user_defines = ['freestanding']
	assert comptime_optional_flag_value(&free_prefs, 'freestanding')

	mut none_prefs := new_preferences()
	none_prefs.freestanding = true
	none_prefs.target_os = 'none'
	none_prefs.user_defines = ['freestanding']
	assert comptime_flag_value(&none_prefs, 'none')
	assert !comptime_optional_flag_value(&none_prefs, 'none')
	none_prefs.explicit_user_defines = ['none']
	assert comptime_optional_flag_value(&none_prefs, 'none')
}

fn test_comptime_optional_bare_preserves_explicit_user_define() {
	mut prefs := new_preferences()
	prefs.user_defines = ['bare']
	assert !comptime_optional_flag_value(&prefs, 'bare')

	prefs.explicit_user_defines = ['bare']
	assert comptime_optional_flag_value(&prefs, 'bare')
}

fn test_comptime_optional_flag_value_keeps_internal_capability_flags() {
	mut prefs := new_preferences()
	prefs.backend = .x64
	prefs.arch = .x64
	prefs.target_os = 'windows'
	assert comptime_flag_value(&prefs, 'v2_native_windows_pe_minimal')
	assert comptime_optional_flag_value(&prefs, 'v2_native_windows_pe_minimal')
	assert comptime_flag_value(&prefs, 'builtin_write_buf_to_fd_should_use_c_write')
	assert comptime_optional_flag_value(&prefs, 'builtin_write_buf_to_fd_should_use_c_write')
}

fn test_comptime_flag_value_native_backend_backtrace_guards() {
	mut prefs := new_preferences()
	prefs.backend = .x64
	assert comptime_flag_value(&prefs, 'no_backtrace')
	assert comptime_flag_value(&prefs, 'tinyc')

	prefs.backend = .arm64
	assert comptime_flag_value(&prefs, 'no_backtrace')
	assert comptime_flag_value(&prefs, 'tinyc')

	prefs.backend = .cleanc
	assert !comptime_flag_value(&prefs, 'no_backtrace')
	assert !comptime_flag_value(&prefs, 'tinyc')

	prefs.user_defines = ['no_backtrace']
	assert comptime_flag_value(&prefs, 'no_backtrace')
	assert !comptime_flag_value(&prefs, 'tinyc')
}

fn test_effective_arch_uses_target_os_preference() {
	mut prefs := new_preferences()
	prefs.arch = .auto
	prefs.target_os = 'macos'
	assert prefs.get_effective_arch() == .arm64

	prefs.target_os = 'windows'
	assert prefs.get_effective_arch() == .x64
}

fn test_v2_native_windows_pe_minimal_flag() {
	mut prefs := new_preferences()
	prefs.backend = .x64
	prefs.arch = .x64
	prefs.target_os = 'windows'
	assert comptime_flag_value(&prefs, 'v2_native_windows_pe_minimal')

	prefs.target_os = 'linux'
	assert !comptime_flag_value(&prefs, 'v2_native_windows_pe_minimal')
	prefs.target_os = 'windows'
	prefs.backend = .arm64
	assert !comptime_flag_value(&prefs, 'v2_native_windows_pe_minimal')
}

fn test_freestanding_hook_comptime_flags() {
	mut prefs := new_preferences()
	prefs.freestanding = true
	prefs.freestanding_hooks = ['output', 'alloc']
	assert comptime_flag_value(&prefs, 'freestanding')
	assert comptime_flag_value(&prefs, 'freestanding_hooks')
	assert comptime_flag_value(&prefs, 'freestanding_output')
	assert comptime_flag_value(&prefs, 'freestanding_alloc')
	assert !comptime_flag_value(&prefs, 'freestanding_panic')
}

fn test_freestanding_none_has_no_concrete_os_comptime_flags() {
	mut prefs := new_preferences()
	prefs.freestanding = true
	prefs.target_os = 'none'
	assert comptime_flag_value(&prefs, 'freestanding')
	assert !comptime_flag_value(&prefs, 'cross')
	assert !comptime_flag_value(&prefs, 'linux')
	assert !comptime_flag_value(&prefs, 'macos')
	assert !comptime_flag_value(&prefs, 'darwin')
	assert !comptime_flag_value(&prefs, 'windows')
	assert !comptime_flag_value(&prefs, 'bsd')
}
