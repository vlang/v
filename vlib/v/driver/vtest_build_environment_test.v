module driver

import os
import v.pref

const facts_env_name = 'VBUILD_FACTS'
const defines_env_name = 'VBUILD_DEFINES'

fn restore_build_environment_variable(name string, old_value string, was_set bool) {
	if was_set {
		os.setenv(name, old_value, true)
	} else {
		os.unsetenv(name)
	}
}

// with_clean_build_environment runs callback without the inherited facts, defines
// and CI job name, which test_build_environment would otherwise merge into its result.
fn with_clean_build_environment(callback fn ()) {
	names := [facts_env_name, defines_env_name, 'GITHUB_JOB']
	old_values := names.map(os.getenv(it))
	were_set := names.map(it in os.environ())
	defer {
		for i, name in names {
			restore_build_environment_variable(name, old_values[i], were_set[i])
		}
	}
	for name in names {
		os.unsetenv(name)
	}
	callback()
}

fn test_default_options_describe_the_host() {
	with_clean_build_environment(fn () {
		environment := vtest_build_environment(@VEXEROOT, [])
		host := pref.host_target()
		assert host.os in environment.facts, environment.facts.str()
		assert host.arch in environment.facts, environment.facts.str()
		assert 'prod' !in environment.facts, environment.facts.str()
		compilers := environment.facts.filter(it in ['tinyc', 'gcc', 'clang', 'msvc', 'mingw',
			'cplusplus'])
		assert compilers.len == 1, environment.facts.str()
		assert environment.defines.filter(it !in ['glibc', 'musl']) == []
	})
}

fn test_prod_is_a_fact() {
	with_clean_build_environment(fn () {
		assert 'prod' in vtest_build_environment(@VEXEROOT, ['-prod']).facts
	})
}

fn test_the_explicit_c_compiler_is_the_compiler_fact() {
	with_clean_build_environment(fn () {
		gcc := vtest_build_environment(@VEXEROOT, ['-cc', 'gcc']).facts
		assert 'gcc' in gcc, gcc.str()
		assert 'tinyc' !in gcc, gcc.str()
		clang := vtest_build_environment(@VEXEROOT, ['-cc', 'clang', '-prod']).facts
		assert 'clang' in clang, clang.str()
		assert 'gcc' !in clang, clang.str()
		assert 'prod' in clang, clang.str()
	})
}

fn test_the_bundled_tcc_is_the_tinyc_fact() {
	bundled_tcc := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe')
	if !os.is_file(bundled_tcc) {
		eprintln('> skipping: no bundled tcc at ${bundled_tcc}')
		return
	}
	with_clean_build_environment(fn [bundled_tcc] () {
		facts := vtest_build_environment(@VEXEROOT, ['-cc', bundled_tcc]).facts
		assert 'tinyc' in facts, facts.str()
		assert 'gcc' !in facts, facts.str()
	})
}

fn test_user_defines_are_recorded_by_name() {
	with_clean_build_environment(fn () {
		defines := vtest_build_environment(@VEXEROOT, ['-d', 'foo', '-dbar', '-define', 'baz=1',
			'-d', 'foo']).defines
		for name in ['foo', 'bar', 'baz'] {
			assert name in defines, defines.str()
		}
		assert 'baz=1' !in defines, defines.str()
		assert defines.filter(it == 'foo').len == 1, defines.str()
	})
}

fn test_options_starting_with_d_are_not_defines() {
	with_clean_build_environment(fn () {
		defines := vtest_build_environment(@VEXEROOT, ['-dump-c-flags', 'flags.txt', '-dump-files',
			'files.txt', '-d', 'kept']).defines
		assert 'kept' in defines, defines.str()
		assert defines.filter(it.starts_with('ump')) == [], defines.str()
		assert 'flags.txt' !in defines, defines.str()
	})
}

fn test_options_with_values_do_not_hide_later_options() {
	with_clean_build_environment(fn () {
		environment := vtest_build_environment(@VEXEROOT, ['-o', 'out', '-gc', 'boehm', '-cflags',
			'-DPROD', '-prod', '-d', 'kept'])
		assert 'prod' in environment.facts, environment.facts.str()
		assert 'kept' in environment.defines, environment.defines.str()
		assert 'PROD' !in environment.defines, environment.defines.str()
	})
}

fn test_custom_environment_facts_and_defines_are_kept() {
	with_clean_build_environment(fn () {
		os.setenv(facts_env_name, 'custom_lane,${pref.host_target().os}', true)
		os.setenv(defines_env_name, 'lane_define', true)
		environment := vtest_build_environment(@VEXEROOT, ['-d', 'lane_define'])
		assert 'custom_lane' in environment.facts, environment.facts.str()
		assert environment.facts.filter(it == pref.host_target().os).len == 1, environment.facts.str()
		assert environment.defines.filter(it == 'lane_define').len == 1, environment.defines.str()
	})
}
