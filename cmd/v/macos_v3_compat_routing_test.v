module main

import os
import v.pref

fn test_macos_v3_routes_cross_modes_to_v1_compatibility() {
	cross_os := if pref.get_host_os() == .windows { pref.OS.linux } else { pref.OS.windows }
	mut prefs := &pref.Preferences{
		path:    'main.v'
		backend: .c
		os:      cross_os
	}
	assert macos_v3_needs_v1_compatibility('main.v', prefs)
	prefs.new_compiler = true
	assert !macos_v3_needs_v1_compatibility('main.v', prefs)

	portable := &pref.Preferences{
		path:           'cmd/v'
		backend:        .c
		output_cross_c: true
	}
	assert macos_v3_needs_v1_compatibility('cmd/v', portable)
}

fn test_macos_v3_routes_internal_tool_bootstrap_to_v1_compatibility() {
	vroot := os.dir(@VEXE)
	tool_path := os.join_path(vroot, 'cmd', 'tools', 'vpm')
	mut prefs := &pref.Preferences{
		path:    tool_path
		backend: .c
	}
	assert macos_v3_needs_v1_compatibility(tool_path, prefs)
	prefs.new_compiler = true
	assert !macos_v3_needs_v1_compatibility(tool_path, prefs)
}
