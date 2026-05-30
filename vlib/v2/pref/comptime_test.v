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
}

fn test_comptime_flag_value_allows_nil_preferences() {
	prefs := unsafe { &Preferences(nil) }
	assert comptime_flag_value(prefs, 'linux') == (prefs.target_os_or_host() == 'linux')
	assert !comptime_flag_value(prefs, 'definitely_missing_flag')
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
