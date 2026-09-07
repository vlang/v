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

fn test_linux_routes_implicit_cmd_v_self_build_to_v1_compatibility() {
	$if linux {
		vroot := os.dir(@VEXE)
		mut prefs := &pref.Preferences{
			path: os.join_path(vroot, 'cmd', 'v')
			backend: .c
		}
		assert macos_v3_needs_v1_compatibility('cmd/v', prefs)
		prefs.new_compiler = true
		assert !macos_v3_needs_v1_compatibility('cmd/v', prefs)
	}
}

fn test_temporary_self_build_bootstraps_only_before_v1_fallback_exists() {
	$if macos || linux {
		vroot := os.dir(@VEXE)
		mut prefs := &pref.Preferences{
			path: os.join_path(vroot, 'cmd', 'v')
			backend: .c
		}
		missing_fallback := os.join_path(os.vtmp_dir(), 'missing_v1_fallback_${os.getpid()}')
		os.rm(missing_fallback) or {}
		// Simulate the implicit Linux compatibility decision independently of the host.
		assert macos_v3_needs_bootstrap_before_v1_fallback(prefs, true, os.join_path(vroot, 'v1'), missing_fallback)
		assert !macos_v3_needs_bootstrap_before_v1_fallback(prefs, false, os.join_path(vroot, 'v1'), missing_fallback)
		prefs.old_compiler = true
		assert macos_v3_needs_bootstrap_before_v1_fallback(prefs, false, os.join_path(vroot, 'v1'), missing_fallback)
		assert !macos_v3_needs_bootstrap_before_v1_fallback(prefs, true, os.join_path(vroot, 'vnew'), missing_fallback)
		assert !macos_v3_needs_bootstrap_before_v1_fallback(prefs, true, os.join_path(vroot, 'v1'), @VEXE)
	}
}
