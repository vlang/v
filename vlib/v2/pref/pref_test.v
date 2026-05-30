module pref

import os

fn test_source_files_from_args_skips_os_option_value() {
	files := source_files_from_args(['-b', 'x64', '-os', 'windows', '-o', 'app', 'main.v'])
	assert files == ['main.v']
}

fn test_source_files_from_args_skips_fhooks_option_value() {
	files := source_files_from_args(['-freestanding', '-fhooks', 'output,panic', '-o', 'app',
		'main.v'])
	assert files == ['main.v']
}

fn test_new_preferences_from_args_defaults_to_host_target() {
	prefs := new_preferences_from_args(['main.v'])
	assert prefs.target_os == normalize_target_os_name(os.user_os())
	assert prefs.normalized_target_os() == normalize_target_os_name(os.user_os())
	assert !prefs.is_cross_target()
	assert !prefs.is_freestanding()
	assert 'cross' !in prefs.user_defines
	assert 'freestanding' !in prefs.user_defines
}

fn test_new_preferences_from_args_parses_target_os() {
	linux_prefs := new_preferences_from_args(['-os', 'linux', 'main.v'])
	assert linux_prefs.target_os == 'linux'
	assert linux_prefs.normalized_target_os() == 'linux'
	assert !linux_prefs.is_cross_target()

	windows_prefs := new_preferences_from_args(['-os', 'windows', 'main.v'])
	assert windows_prefs.target_os == 'windows'
	assert windows_prefs.normalized_target_os() == 'windows'
}

fn test_new_preferences_from_args_normalizes_macos_target_aliases() {
	macos_prefs := new_preferences_from_args(['-os', 'macos', 'main.v'])
	assert macos_prefs.target_os == 'macos'
	assert macos_prefs.normalized_target_os() == 'macos'

	darwin_prefs := new_preferences_from_args(['-os', 'darwin', 'main.v'])
	assert darwin_prefs.target_os == 'macos'
	assert darwin_prefs.normalized_target_os() == 'macos'

	mac_prefs := new_preferences_from_args(['-os', 'mac', 'main.v'])
	assert mac_prefs.target_os == 'macos'
	assert mac_prefs.normalized_target_os() == 'macos'
}

fn test_new_preferences_from_args_cross_is_not_concrete_os() {
	prefs := new_preferences_from_args(['-os', 'cross', 'main.v'])
	assert prefs.target_os == 'cross'
	assert prefs.normalized_target_os() == 'cross'
	assert prefs.is_cross_target()
	assert 'cross' in prefs.user_defines
	assert comptime_flag_value(&prefs, 'cross')
	assert !comptime_flag_value(&prefs, 'linux')
	assert !comptime_flag_value(&prefs, 'macos')
	assert !comptime_flag_value(&prefs, 'darwin')
	assert !comptime_flag_value(&prefs, 'windows')
}

fn test_new_preferences_from_args_freestanding_is_distinct_from_cross() {
	prefs := new_preferences_from_args(['-freestanding', '-os', 'linux', 'main.v'])
	assert prefs.is_freestanding()
	assert !prefs.is_cross_target()
	assert prefs.normalized_target_os() == 'linux'
	assert comptime_flag_value(&prefs, 'freestanding')
	assert comptime_flag_value(&prefs, 'linux')
	assert !comptime_flag_value(&prefs, 'cross')
	assert 'freestanding' in prefs.user_defines
}

fn test_new_preferences_from_args_accepts_freestanding_none_target() {
	prefs := new_preferences_from_args(['-freestanding', '-os', 'none', 'main.v'])
	assert prefs.is_freestanding()
	assert !prefs.is_cross_target()
	assert prefs.target_os == 'none'
	assert prefs.normalized_target_os() == 'none'
	assert prefs.source_filter_target_os() == 'none'
	assert !prefs.can_compile_cleanc_locally()
	assert comptime_flag_value(&prefs, 'freestanding')
	assert !comptime_flag_value(&prefs, 'cross')
	assert !comptime_flag_value(&prefs, 'linux')
	assert !comptime_flag_value(&prefs, 'macos')
	assert !comptime_flag_value(&prefs, 'windows')
}

fn test_can_compile_cleanc_locally_follows_target_contract() {
	host := normalize_target_os_name(os.user_os())
	host_prefs := new_preferences_from_args(['-os', host, 'main.v'])
	assert host_prefs.can_compile_cleanc_locally()

	cross_prefs := new_preferences_from_args(['-os', 'cross', 'main.v'])
	assert cross_prefs.can_compile_cleanc_locally()
	assert cross_prefs.source_filter_target_os() == host

	freestanding_prefs := new_preferences_from_args(['-freestanding', '-os', host, 'main.v'])
	assert !freestanding_prefs.can_compile_cleanc_locally()
	assert freestanding_prefs.source_filter_target_os() == host

	non_host := if host == 'windows' { 'linux' } else { 'windows' }
	non_host_prefs := new_preferences_from_args(['-os', non_host, 'main.v'])
	assert !non_host_prefs.can_compile_cleanc_locally()
	assert non_host_prefs.source_filter_target_os() == non_host
}

fn test_can_run_target_binary_locally_follows_effective_target() {
	host := normalize_target_os_name(os.user_os())
	host_prefs := new_preferences_from_args(['-os', host, 'main.v'])
	assert host_prefs.can_run_target_binary_locally()

	cross_prefs := new_preferences_from_args(['-os', 'cross', 'main.v'])
	assert cross_prefs.can_run_target_binary_locally()

	native_cross_prefs := new_preferences_from_args(['-b', 'x64', '-os', 'cross', 'main.v'])
	assert native_cross_prefs.backend == .x64
	assert !native_cross_prefs.can_run_target_binary_locally()

	freestanding_prefs := new_preferences_from_args(['-freestanding', '-os', host, 'main.v'])
	assert !freestanding_prefs.can_run_target_binary_locally()

	mut native_freestanding_prefs := new_preferences_from_args(['-b', 'x64', '-freestanding', '-os',
		host, 'main.v'])
	native_freestanding_prefs.backend = .x64
	assert !native_freestanding_prefs.can_run_target_binary_locally()

	non_host := if host == 'windows' { 'linux' } else { 'windows' }
	non_host_prefs := new_preferences_from_args(['-os', non_host, 'main.v'])
	assert !non_host_prefs.can_run_target_binary_locally()

	mut native_non_host_prefs := new_preferences_from_args(['-b', 'x64', '-os', non_host, 'main.v'])
	native_non_host_prefs.backend = .x64
	assert !native_non_host_prefs.can_run_target_binary_locally()
}

fn test_new_preferences_from_args_parses_freestanding_hooks() {
	prefs := new_preferences_from_args(['-freestanding', '-fhooks', 'output,panic', '-os', 'linux',
		'main.v'])
	assert prefs.is_freestanding()
	assert prefs.freestanding_hook_list() == ['output', 'panic']
	assert prefs.has_freestanding_hooks()
	assert prefs.has_freestanding_hook('output')
	assert prefs.has_freestanding_hook('panic')
	assert !prefs.has_freestanding_hook('alloc')
	assert 'freestanding_hooks' in prefs.user_defines
	assert 'freestanding_output' in prefs.user_defines
	assert 'freestanding_hooks_output' in prefs.user_defines
	assert 'freestanding_panic' in prefs.user_defines
	assert 'freestanding_hooks_panic' in prefs.user_defines
	assert 'freestanding_alloc' !in prefs.user_defines
	assert 'freestanding_hooks_alloc' !in prefs.user_defines
	assert comptime_flag_value(&prefs, 'freestanding_hooks')
	assert comptime_flag_value(&prefs, 'freestanding_output')
	assert comptime_flag_value(&prefs, 'freestanding_panic')
	assert !comptime_flag_value(&prefs, 'freestanding_alloc')
}

fn test_freestanding_hook_defines_do_not_grant_hook_capabilities() {
	prefs := new_preferences_from_args(['-freestanding', '-d', 'freestanding_output', '-d',
		'freestanding_panic', '-d', 'freestanding_alloc', 'main.v'])
	assert prefs.is_freestanding()
	assert !prefs.has_freestanding_hooks()
	assert !prefs.has_freestanding_hook('output')
	assert !prefs.has_freestanding_hook('panic')
	assert !prefs.has_freestanding_hook('alloc')
	assert 'freestanding_output' in prefs.user_defines
	assert 'freestanding_panic' in prefs.user_defines
	assert 'freestanding_alloc' in prefs.user_defines
	assert comptime_flag_value(&prefs, 'freestanding_output')
	assert comptime_flag_value(&prefs, 'freestanding_panic')
	assert comptime_flag_value(&prefs, 'freestanding_alloc')
}

fn test_new_preferences_from_args_expands_minimal_freestanding_hooks() {
	prefs := new_preferences_from_args(['-freestanding', '-fhooks', 'minimal', 'main.v'])
	assert prefs.freestanding_hook_list() == ['output', 'panic', 'alloc']
	assert prefs.has_freestanding_hook('output')
	assert prefs.has_freestanding_hook('panic')
	assert prefs.has_freestanding_hook('alloc')
	assert comptime_flag_value(&prefs, 'freestanding_output')
	assert comptime_flag_value(&prefs, 'freestanding_panic')
	assert comptime_flag_value(&prefs, 'freestanding_alloc')
}

fn test_new_preferences_using_options_accepts_target_os_and_freestanding() {
	prefs := new_preferences_using_options(['--cleanc', '--os-windows', '--freestanding'])
	assert prefs.backend == .cleanc
	assert prefs.target_os == 'windows'
	assert prefs.normalized_target_os() == 'windows'
	assert !prefs.is_cross_target()
	assert prefs.is_freestanding()
	assert comptime_flag_value(&prefs, 'windows')
	assert comptime_flag_value(&prefs, 'freestanding')
	assert 'freestanding' in prefs.user_defines
}

fn test_new_preferences_using_options_accepts_freestanding_hooks() {
	prefs := new_preferences_using_options(['--cleanc', '--os-linux', '--freestanding',
		'--fhooks-output,alloc'])
	assert prefs.is_freestanding()
	assert prefs.freestanding_hook_list() == ['output', 'alloc']
	assert prefs.has_freestanding_hook('output')
	assert prefs.has_freestanding_hook('alloc')
	assert !prefs.has_freestanding_hook('panic')
	assert comptime_flag_value(&prefs, 'freestanding_output')
	assert comptime_flag_value(&prefs, 'freestanding_alloc')
	assert !comptime_flag_value(&prefs, 'freestanding_panic')
}

fn test_new_preferences_using_options_accepts_freestanding_none_target() {
	prefs := new_preferences_using_options(['--cleanc', '--freestanding', '--os-none'])
	assert prefs.is_freestanding()
	assert !prefs.is_cross_target()
	assert prefs.target_os == 'none'
	assert prefs.source_filter_target_os() == 'none'
	assert !prefs.can_compile_cleanc_locally()
}

fn test_new_preferences_using_options_accepts_cross_target() {
	prefs := new_preferences_using_options(['--cleanc', '--os-cross'])
	assert prefs.target_os == 'cross'
	assert prefs.is_cross_target()
	assert 'cross' in prefs.user_defines
	assert comptime_flag_value(&prefs, 'cross')
	assert !comptime_flag_value(&prefs, 'linux')
	assert !comptime_flag_value(&prefs, 'macos')
	assert !comptime_flag_value(&prefs, 'windows')
}

fn test_new_preferences_from_args_accepted_targets_match_comptime_flags() {
	for target in ['linux', 'macos', 'darwin', 'mac', 'windows', 'freebsd', 'openbsd', 'netbsd',
		'dragonfly', 'android', 'ios', 'solaris', 'qnx', 'serenity', 'plan9', 'vinix'] {
		prefs := new_preferences_from_args(['-os', target, 'main.v'])
		flag_name := match target {
			'darwin', 'mac' { 'macos' }
			else { prefs.normalized_target_os() }
		}

		assert comptime_flag_value(&prefs, flag_name), '${target} should match ${flag_name}'
	}
}

fn test_cross_and_freestanding_still_allow_user_defines() {
	mut prefs := new_preferences()
	prefs.user_defines = ['cross', 'freestanding']
	assert comptime_flag_value(&prefs, 'cross')
	assert comptime_flag_value(&prefs, 'freestanding')
}

fn test_file_suffix_filter_cross_target_contract() {
	assert !file_has_incompatible_os_suffix('common.v', 'cross')
	assert file_has_incompatible_os_suffix('platform_linux.v', 'cross')
	assert file_has_incompatible_os_suffix('platform_macos.v', 'cross')
	assert file_has_incompatible_os_suffix('platform_darwin.v', 'cross')
	assert file_has_incompatible_os_suffix('platform_windows.v', 'cross')
	assert file_has_incompatible_os_suffix('platform_termux.c.v', 'cross')
	assert !file_has_incompatible_os_suffix('platform_nix.v', 'cross')
}

fn test_file_suffix_filter_termux_is_excluded_for_current_targets() {
	for target in ['linux', 'macos', 'windows', 'android', 'ios', 'freebsd', 'openbsd', 'netbsd',
		'dragonfly', 'solaris', 'qnx', 'serenity', 'plan9', 'vinix'] {
		assert file_has_incompatible_os_suffix('platform_termux.c.v', target)
	}
	assert !file_has_incompatible_os_suffix('platform_android_outside_termux.c.v', 'android')
}

fn test_file_suffix_filter_none_target_excludes_all_os_variants() {
	assert !file_has_incompatible_os_suffix('common.v', 'none')
	assert file_has_incompatible_os_suffix('platform_linux.v', 'none')
	assert file_has_incompatible_os_suffix('platform_macos.v', 'none')
	assert file_has_incompatible_os_suffix('platform_darwin.v', 'none')
	assert file_has_incompatible_os_suffix('platform_windows.v', 'none')
	assert file_has_incompatible_os_suffix('platform_nix.v', 'none')
	assert file_has_incompatible_os_suffix('platform_termux.c.v', 'none')
	assert file_has_incompatible_os_suffix('platform_bsd.v', 'none')
}
