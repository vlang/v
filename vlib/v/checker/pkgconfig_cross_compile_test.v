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
	old_path := os.getenv_opt('PKG_CONFIG_PATH')
	old_defaults := os.getenv_opt('PKG_CONFIG_PATH_DEFAULTS')
	defer {
		if path := old_path {
			os.setenv('PKG_CONFIG_PATH', path, true)
		} else {
			os.unsetenv('PKG_CONFIG_PATH')
		}
		if defaults := old_defaults {
			os.setenv('PKG_CONFIG_PATH_DEFAULTS', defaults, true)
		} else {
			os.unsetenv('PKG_CONFIG_PATH_DEFAULTS')
		}
	}
	os.setenv('PKG_CONFIG_PATH', sample_dir, true)
	os.setenv('PKG_CONFIG_PATH_DEFAULTS', '', true)

	native_flags := checked_pkgconfig_flags(pref.get_host_os())
	assert native_flags.ordinary.any(it == '-lSDL2')
	assert native_flags.ordinary.any(it.starts_with('-I') && it.contains('SDL2'))
	assert native_flags.ordered == []

	cross_target := if pref.get_host_os() == .windows { pref.OS.linux } else { pref.OS.windows }
	cross_flags := checked_pkgconfig_flags(cross_target)
	assert cross_flags.ordinary == []
	assert cross_flags.ordered == []
}

struct CheckedPkgConfigFlags {
	ordinary []string
	ordered  []string
}

fn checked_pkgconfig_flags(target_os pref.OS) CheckedPkgConfigFlags {
	mut table := ast.new_table()
	mut pref_ := pref.new_preferences()
	pref_.os = target_os
	mut file := parser.parse_text(pkgconfig_test_source, os.join_path('/virtual', 'main.v'), mut
		table, .skip_comments, pref_)
	mut chk := new_checker(table, pref_)
	chk.check(mut file)
	mut ordered := []string{}
	for segment in table.link_flag_segments {
		if !segment.is_pkgconfig {
			continue
		}
		ordered << segment.flags.map(it.format() or { '' }).filter(it != '')
	}
	return CheckedPkgConfigFlags{
		ordinary: table.cflags.map(it.format() or { '' }).filter(it != '')
		ordered:  ordered
	}
}
