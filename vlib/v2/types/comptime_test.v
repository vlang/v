// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module types

import v2.pref
import v2.token

fn test_checker_comptime_flags_match_pref_for_native_backends() {
	x64_prefs := &pref.Preferences{
		backend: .x64
	}
	arm64_prefs := &pref.Preferences{
		backend: .arm64
	}
	mut file_set := token.FileSet.new()
	env := Environment.new()
	x64_checker := Checker.new(x64_prefs, file_set, env)
	arm64_checker := Checker.new(arm64_prefs, file_set, env)

	assert x64_checker.eval_comptime_flag('no_backtrace')
	assert x64_checker.eval_comptime_flag('tinyc')
	assert arm64_checker.eval_comptime_flag('no_backtrace')
	assert arm64_checker.eval_comptime_flag('tinyc')
}

fn test_checker_comptime_flags_match_pref_for_non_native_backend() {
	cleanc_prefs := &pref.Preferences{
		backend: .cleanc
	}
	mut file_set := token.FileSet.new()
	env := Environment.new()
	cleanc_checker := Checker.new(cleanc_prefs, file_set, env)

	assert !cleanc_checker.eval_comptime_flag('no_backtrace')
	assert !cleanc_checker.eval_comptime_flag('tinyc')
}

fn test_checker_comptime_flags_honor_no_backtrace_user_define_for_non_native_backend() {
	cleanc_prefs := &pref.Preferences{
		backend:      .cleanc
		user_defines: ['no_backtrace']
	}
	mut file_set := token.FileSet.new()
	env := Environment.new()
	cleanc_checker := Checker.new(cleanc_prefs, file_set, env)

	assert cleanc_checker.eval_comptime_flag('no_backtrace')
	assert !cleanc_checker.eval_comptime_flag('tinyc')
}

fn test_checker_comptime_flags_follow_pref_target_os_and_native_windows_pe() {
	mut prefs := pref.new_preferences()
	prefs.backend = .x64
	prefs.arch = .x64
	prefs.target_os = 'windows'
	mut file_set := token.FileSet.new()
	env := Environment.new()
	checker := Checker.new(&prefs, file_set, env)

	assert checker.eval_comptime_flag('windows')
	assert !checker.eval_comptime_flag('linux')
	assert !checker.eval_comptime_flag('macos')
	assert checker.eval_comptime_flag('v2_native_windows_pe_minimal')

	prefs.target_os = 'linux'
	assert !checker.eval_comptime_flag('windows')
	assert checker.eval_comptime_flag('linux')
	assert !checker.eval_comptime_flag('v2_native_windows_pe_minimal')

	prefs.target_os = 'windows'
	prefs.backend = .arm64
	assert checker.eval_comptime_flag('windows')
	assert !checker.eval_comptime_flag('v2_native_windows_pe_minimal')
}
