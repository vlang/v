module checker

import os
import v.ast
import v.parser
import v.pref

const issue74_static_source = 'module main
#pkgconfig static-root-74

fn main() {}
'

const issue74_explicit_static_source = 'module main
#pkgconfig --static --cflags --libs static-root-74

fn main() {}
'

const issue74_static_shorthand_source = 'module main
#pkgconfig --static static-root-74

fn main() {}
'

const issue74_short_static_source = 'module main
#pkgconfig -s static-root-74

fn main() {}
'

const issue74_short_static_debug_source = 'module main
#pkgconfig -s -D static-root-74

fn main() {}
'

const issue74_short_static_modversion_source = 'module main
#pkgconfig -s -V static-root-74

fn main() {}
'

const issue74_short_static_libs_source = 'module main
#pkgconfig -s -l static-root-74

fn main() {}
'

const issue74_static_assignment_source = 'module main
#pkgconfig --static=true static-root-74

fn main() {}
'

const issue74_short_static_cflags_debug_source = 'module main
#pkgconfig -s -cD static-root-74

fn main() {}
'

const issue74_short_static_libs_debug_source = 'module main
#pkgconfig -s -lD static-root-74

fn main() {}
'

const issue74_explicit_dynamic_cflags_source = 'module main
#pkgconfig --cflags static-root-74

fn main() {}
'

const issue74_static_false_source = 'module main
#pkgconfig --static=false static-root-74

fn main() {}
'

const issue74_short_dynamic_cflags_source = 'module main
#pkgconfig -c static-root-74

fn main() {}
'

const issue74_static_false_cflags_source = 'module main
#pkgconfig --static --cflags=false static-root-74

fn main() {}
'

const issue74_static_false_libs_source = 'module main
#pkgconfig --static --libs=false static-root-74

fn main() {}
'

const issue74_structured_private_link_source = 'module main
#pkgconfig --static --libs structured-link-boundary-74

fn main() {}
'

const issue74_dynamic_compile_defines = ['ISSUE74_ROOT', 'ISSUE74_PUBLIC', 'ISSUE74_PRIVATE']

const issue74_static_compile_defines = ['ISSUE74_ROOT', 'ISSUE74_PUBLIC', 'ISSUE74_PRIVATE',
	'ISSUE74_ROOT_PRIVATE', 'ISSUE74_PUBLIC_PRIVATE', 'ISSUE74_PRIVATE_PRIVATE']

const issue74_dynamic_linker_segment = ['-lissue74_root', '-lissue74_public', '-lissue74_private']

const issue74_static_linker_segment = ['-lissue74_root', '-Wl,--start-group', '-lissue74_cycle_a',
	'-lissue74_cycle_b', '-lissue74_cycle_a', '-Wl,--end-group', '-lissue74_root_private',
	'-lissue74_public', '-lissue74_public_private', '-lissue74_private', '-lissue74_private_private']

struct Issue74CheckedFlags {
	compile_defines  []string
	all_linker       []string
	ordinary_linker  []string
	pkgconfig_linker []string
	checker_errors   []string
}

fn issue74_is_linker_token(flag string) bool {
	return flag.starts_with('-lissue74_') || flag.starts_with('-Wl,--') || flag.starts_with('-L')
		|| flag.ends_with('.a')
}

fn issue74_checked_flags(ccompiler string, cflags_value string) Issue74CheckedFlags {
	return issue74_checked_flags_for_source(ccompiler, cflags_value, '', issue74_static_source)
}

fn issue74_checked_flags_with_ldflags(ccompiler string, ldflags_value string) Issue74CheckedFlags {
	return issue74_checked_flags_for_source(ccompiler, '', ldflags_value, issue74_static_source)
}

fn issue74_checked_flags_for_source(ccompiler string, cflags_value string, ldflags_value string, source string) Issue74CheckedFlags {
	fixture_dir := os.join_path(@VEXEROOT, 'vlib', 'v', 'pkgconfig', 'testdata', 'static_pkgconfig')
	old_path := os.getenv_opt('PKG_CONFIG_PATH')
	old_defaults := os.getenv_opt('PKG_CONFIG_PATH_DEFAULTS')
	os.unsetenv('PKG_CONFIG_PATH')
	os.setenv('PKG_CONFIG_PATH_DEFAULTS', fixture_dir, true)
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

	mut args := ['-cc', ccompiler]
	if cflags_value != '' {
		args << ['-cflags', cflags_value]
	}
	if ldflags_value != '' {
		args << ['-ldflags', ldflags_value]
	}
	args << @FILE
	prefs, _ := pref.parse_args([], args)
	mut table := ast.new_table()
	mut file := parser.parse_text(source, os.join_path('/virtual', 'main.v'), mut table,
		.skip_comments, prefs)
	mut chk := new_checker(table, prefs)
	chk.check(mut file)
	ordinary :=
		table.cflags.map(it.format() or { '' }).filter(it != '').filter(issue74_is_linker_token(it))
	mut pkgconfig_linker := []string{}
	for segment in table.link_flag_segments {
		if !segment.is_pkgconfig {
			continue
		}
		pkgconfig_linker << segment.flags.map(it.format() or { '' }).filter(it != '').filter(issue74_is_linker_token(it))
	}
	mut all_linker := ordinary.clone()
	all_linker << pkgconfig_linker
	return Issue74CheckedFlags{
		compile_defines:  table.cflags.filter(it.name == '-D' && it.value.starts_with('ISSUE74_')).map(it.value)
		all_linker:       all_linker
		ordinary_linker:  ordinary
		pkgconfig_linker: pkgconfig_linker
		checker_errors:   chk.errors.map(it.message)
	}
}

fn test_exact_static_token_in_cflags_activates_pkgconfig_for_supported_compilers() {
	for ccompiler in ['gcc', 'clang', 'mingw'] {
		flags := issue74_checked_flags(ccompiler, '-static')
		assert flags.all_linker == issue74_static_linker_segment, ccompiler
		assert flags.ordinary_linker == [], ccompiler
		assert flags.pkgconfig_linker == issue74_static_linker_segment, ccompiler
	}
}

fn test_static_token_inside_combined_cflags_value_activates_pkgconfig() {
	for ccompiler in ['gcc', 'clang', 'mingw'] {
		flags := issue74_checked_flags(ccompiler,
			'-static -Wno-error -Wno-incompatible-pointer-types')
		assert flags.all_linker == issue74_static_linker_segment, ccompiler
	}
}

fn test_exact_static_token_in_ldflags_activates_pkgconfig_for_supported_compilers() {
	for ccompiler in ['gcc', 'clang', 'mingw'] {
		flags := issue74_checked_flags_with_ldflags(ccompiler, '-Wl,--issue74-before -static')
		assert flags.all_linker == issue74_static_linker_segment, ccompiler
		assert flags.ordinary_linker == [], ccompiler
		assert flags.pkgconfig_linker == issue74_static_linker_segment, ccompiler
	}
}

fn test_static_lookalikes_do_not_activate_pkgconfig() {
	lookalikes := [
		'-static-libgcc',
		'-Wl,-Bstatic',
		'-DISSUE74_VALUE=-static',
		'-L/tmp/issue74-static',
		'-I/tmp/-static/include',
		'-DISSUE74_TEXT="prefix -static suffix"',
	]
	for ccompiler in ['gcc', 'clang', 'mingw'] {
		for option in ['cflags', 'ldflags'] {
			for value in lookalikes {
				flags := if option == 'cflags' {
					issue74_checked_flags(ccompiler, value)
				} else {
					issue74_checked_flags_with_ldflags(ccompiler, value)
				}
				context := '${ccompiler} ${option}: ${value}'
				assert flags.all_linker == issue74_dynamic_linker_segment, context
				assert flags.ordinary_linker == issue74_dynamic_linker_segment, context
				assert flags.pkgconfig_linker == [], context
			}
		}
	}
}

fn test_static_pkgconfig_segment_stays_ordered_after_target() {
	flags := issue74_checked_flags('gcc', '-static')
	assert flags.all_linker == issue74_static_linker_segment
	assert flags.ordinary_linker == []
	assert flags.pkgconfig_linker == issue74_static_linker_segment
}

fn test_explicit_static_directive_uses_ordered_transport_without_global_static_cflag() {
	for ccompiler in ['gcc', 'clang'] {
		flags := issue74_checked_flags_for_source(ccompiler, '', '', issue74_explicit_static_source)
		assert flags.all_linker == issue74_static_linker_segment, ccompiler
		assert flags.ordinary_linker == [], ccompiler
		assert flags.pkgconfig_linker == issue74_static_linker_segment, ccompiler
	}
}

fn test_package_without_options_keeps_historical_dynamic_defaults() {
	flags := issue74_checked_flags_for_source('gcc', '', '', issue74_static_source)
	assert flags.compile_defines == issue74_dynamic_compile_defines
	assert flags.all_linker == issue74_dynamic_linker_segment
	assert flags.ordinary_linker == issue74_dynamic_linker_segment
	assert flags.pkgconfig_linker == []
}

fn test_static_directive_without_actions_adds_only_default_cflags_and_libs() {
	for ccompiler in ['gcc', 'clang'] {
		shorthand := issue74_checked_flags_for_source(ccompiler, '', '',
			issue74_static_shorthand_source)
		explicit := issue74_checked_flags_for_source(ccompiler, '', '',
			issue74_explicit_static_source)
		assert shorthand.compile_defines == issue74_static_compile_defines, ccompiler
		assert shorthand.compile_defines == explicit.compile_defines, ccompiler
		assert shorthand.all_linker == issue74_static_linker_segment, ccompiler
		assert shorthand.ordinary_linker == [], ccompiler
		assert shorthand.pkgconfig_linker == explicit.pkgconfig_linker, ccompiler
	}
}

fn test_short_static_option_matches_long_static_option() {
	short := issue74_checked_flags_for_source('gcc', '', '', issue74_short_static_source)
	long := issue74_checked_flags_for_source('gcc', '', '', issue74_static_shorthand_source)
	assert short.compile_defines == long.compile_defines
	assert short.all_linker == long.all_linker
	assert short.ordinary_linker == long.ordinary_linker
	assert short.pkgconfig_linker == long.pkgconfig_linker
}

fn test_static_assignment_without_actions_matches_static_option() {
	assignment := issue74_checked_flags_for_source('gcc', '', '', issue74_static_assignment_source)
	long := issue74_checked_flags_for_source('gcc', '', '', issue74_static_shorthand_source)
	assert assignment.compile_defines == long.compile_defines
	assert assignment.all_linker == long.all_linker
	assert assignment.ordinary_linker == long.ordinary_linker
	assert assignment.pkgconfig_linker == long.pkgconfig_linker
}

fn test_short_static_debug_modifier_adds_default_actions() {
	flags := issue74_checked_flags_for_source('gcc', '', '', issue74_short_static_debug_source)
	assert flags.compile_defines == issue74_static_compile_defines
	assert flags.all_linker == issue74_static_linker_segment
	assert flags.ordinary_linker == []
	assert flags.pkgconfig_linker == issue74_static_linker_segment
}

fn test_short_static_clusters_keep_their_explicit_action() {
	cflags := issue74_checked_flags_for_source('gcc', '', '',
		issue74_short_static_cflags_debug_source)
	assert cflags.compile_defines == issue74_static_compile_defines
	assert cflags.all_linker == []
	assert cflags.ordinary_linker == []
	assert cflags.pkgconfig_linker == []

	libs := issue74_checked_flags_for_source('gcc', '', '', issue74_short_static_libs_debug_source)
	assert libs.compile_defines == []
	assert libs.all_linker == issue74_static_linker_segment
	assert libs.ordinary_linker == []
	assert libs.pkgconfig_linker == issue74_static_linker_segment
}

fn test_short_static_modversion_action_does_not_add_default_actions() {
	flags := issue74_checked_flags_for_source('gcc', '', '', issue74_short_static_modversion_source)
	assert flags.compile_defines == []
	assert flags.all_linker == []
	assert flags.ordinary_linker == []
	assert flags.pkgconfig_linker == []
}

fn test_short_static_libs_action_does_not_inject_cflags() {
	flags := issue74_checked_flags_for_source('gcc', '', '', issue74_short_static_libs_source)
	assert flags.compile_defines == []
	assert flags.all_linker == issue74_static_linker_segment
	assert flags.ordinary_linker == []
	assert flags.pkgconfig_linker == issue74_static_linker_segment
}

fn test_static_pkgconfig_preserves_private_link_fragment_boundaries_and_repetition() {
	flags := issue74_checked_flags_for_source('gcc', '', '', issue74_structured_private_link_source)
	lib_dir := '/issue74/structured-boundary'
	expected := ['-L"${os.real_path(lib_dir)}"', '${lib_dir}/libissue74_b.a',
		'${lib_dir}/libissue74_a.a', '${lib_dir}/libissue74_b.a']
	assert flags.checker_errors == []
	assert flags.ordinary_linker == []
	assert flags.pkgconfig_linker == expected
	assert flags.all_linker == expected
}

fn test_explicit_dynamic_cflags_action_does_not_inject_libs() {
	flags := issue74_checked_flags_for_source('gcc', '', '', issue74_explicit_dynamic_cflags_source)
	assert flags.compile_defines == issue74_dynamic_compile_defines
	assert flags.all_linker == []
	assert flags.ordinary_linker == []
	assert flags.pkgconfig_linker == []
}

fn test_static_false_keeps_historical_empty_flag_error() {
	flags := issue74_checked_flags_for_source('gcc', '', '', issue74_static_false_source)
	assert flags.checker_errors == ['flag is empty']
}

fn test_short_dynamic_cflags_keeps_historical_package_error() {
	flags := issue74_checked_flags_for_source('gcc', '', '', issue74_short_dynamic_cflags_source)
	assert flags.checker_errors == ['Cannot find "-c" pkgconfig file']
}

fn test_explicit_false_static_actions_do_not_inject_defaults() {
	for source in [issue74_static_false_cflags_source, issue74_static_false_libs_source] {
		flags := issue74_checked_flags_for_source('gcc', '', '', source)
		assert flags.checker_errors == []
		assert flags.compile_defines == []
		assert flags.all_linker == []
		assert flags.ordinary_linker == []
		assert flags.pkgconfig_linker == []
	}
}

fn test_msvc_pkgconfig_stays_dynamic_by_default() {
	flags := issue74_checked_flags('msvc', '')
	assert flags.all_linker == issue74_dynamic_linker_segment
	assert flags.ordinary_linker == issue74_dynamic_linker_segment
	assert flags.pkgconfig_linker == []
}

fn test_static_token_does_not_enable_static_pkgconfig_for_msvc() {
	flags := issue74_checked_flags('msvc', '-static')
	assert flags.all_linker == issue74_dynamic_linker_segment
	assert flags.ordinary_linker == issue74_dynamic_linker_segment
	assert flags.pkgconfig_linker == []
}

fn test_static_token_does_not_enable_static_pkgconfig_for_tcc() {
	flags := issue74_checked_flags('tcc', '-static')
	assert flags.all_linker == issue74_dynamic_linker_segment
	assert flags.ordinary_linker == issue74_dynamic_linker_segment
	assert flags.pkgconfig_linker == []
}

fn test_static_token_in_ldflags_does_not_enable_static_pkgconfig_for_msvc_or_tcc() {
	for ccompiler in ['msvc', 'tcc'] {
		flags := issue74_checked_flags_with_ldflags(ccompiler, '-static')
		assert flags.all_linker == issue74_dynamic_linker_segment, ccompiler
		assert flags.ordinary_linker == issue74_dynamic_linker_segment, ccompiler
		assert flags.pkgconfig_linker == [], ccompiler
	}
}
