module checker

import os
import v.ast
import v.parser
import v.pref

const pkgconfig_test_source = 'module main
#pkgconfig --cflags --libs sdl2

fn main() {}
'

fn test_pkgconfig_is_skipped_for_cross_compilation() {
	sample_dir := os.join_path(@VEXEROOT, 'vlib', 'v', 'pkgconfig', 'test_samples')
	old_path := os.getenv('PKG_CONFIG_PATH')
	old_defaults := os.getenv('PKG_CONFIG_PATH_DEFAULTS')
	defer {
		os.setenv('PKG_CONFIG_PATH', old_path, true)
		os.setenv('PKG_CONFIG_PATH_DEFAULTS', old_defaults, true)
	}
	os.setenv('PKG_CONFIG_PATH', sample_dir, true)
	os.setenv('PKG_CONFIG_PATH_DEFAULTS', '', true)

	native_flags := checked_pkgconfig_flags(pref.get_host_os())
	assert native_flags.any(it == '-lSDL2')
	assert native_flags.any(it.starts_with('-I') && it.contains('SDL2'))

	cross_target := if pref.get_host_os() == .windows { pref.OS.linux } else { pref.OS.windows }
	cross_flags := checked_pkgconfig_flags(cross_target)
	assert cross_flags.len == 0
}

fn checked_pkgconfig_flags(target_os pref.OS) []string {
	mut table := ast.new_table()
	mut pref_ := pref.new_preferences()
	pref_.os = target_os
	mut file := parser.parse_text(pkgconfig_test_source, os.join_path('/virtual', 'main.v'), mut
		table, .skip_comments, pref_)
	mut chk := new_checker(table, pref_)
	chk.check(mut file)
	return table.cflags.map(it.format() or { '' }).filter(it != '')
}
