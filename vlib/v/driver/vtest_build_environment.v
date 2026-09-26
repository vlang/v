module driver

import os
import v.pref

// TestBuildEnvironment is what a `// vtest build:` expression is evaluated
// against: the facts of a compilation (its OS, architecture, C compiler kind,
// `prod`, the CI job) and the defines it sets.
pub struct TestBuildEnvironment {
pub:
	facts   []string
	defines []string
}

// vtest_build_environment resolves the facts and defines of the compilation
// that the compiler options `args` describe (`-os`, `-arch`, `-cc`, `-prod`,
// `-d`, ...), the way `run` resolves them for `v <args> file_test.v`. A test
// runner started with those options exports the result through
// pref.set_build_flags_and_defines, so that its `// vtest build:` decisions
// match the compiler's own.
pub fn vtest_build_environment(vroot string, args []string) TestBuildEnvironment {
	mut backend := 'c'
	mut target_os := os.user_os()
	mut target_arch := pref.host_arch()
	mut cross_output := false
	mut c_compiler := 'cc'
	mut c_compiler_explicit := false
	mut is_prod := false
	mut is_c_debug := false
	mut parallel_cc := false
	mut dump_c_flags := false
	mut libc_mode := ''
	mut user_defines := []string{}
	mut compile_values := map[string]string{}
	mut i := 0
	for i < args.len {
		arg := args[i]
		has_value := i + 1 < args.len
		if arg in ['-b', '-backend'] && has_value {
			backend = if args[i + 1] in ['js_browser', 'js_node'] { 'js' } else { args[i + 1] }
			i += 2
		} else if arg == '-os' && has_value {
			target_os = args[i + 1]
			i += 2
		} else if arg == '-cross' {
			cross_output = true
			i++
		} else if arg == '-arch' && has_value {
			target_arch = args[i + 1]
			i += 2
		} else if arg == '-cc' && has_value {
			c_compiler = args[i + 1]
			c_compiler_explicit = true
			i += 2
		} else if arg == '-prod' {
			is_prod = true
			i++
		} else if arg in ['-cg', '-cdebug'] {
			is_c_debug = true
			i++
		} else if arg == '-parallel-cc' {
			parallel_cc = true
			i++
		} else if arg == '-musl' {
			libc_mode = 'musl'
			i++
		} else if arg == '-glibc' {
			libc_mode = 'glibc'
			i++
		} else if arg in ['-d', '-define'] && has_value {
			record_user_define(mut user_defines, mut compile_values, args[i + 1])
			i += 2
		} else if arg == '-dump-c-flags' {
			// Its value is optional, so it must not be read as a `-d` shorthand.
			dump_c_flags = true
			i += if has_value { 2 } else { 1 }
		} else if arg.starts_with('-d') && arg.len > 2 && !v3_driver_option_consumes_value(arg) {
			record_user_define(mut user_defines, mut compile_values, arg[2..])
			i++
		} else if v3_driver_option_consumes_value(arg) && has_value {
			i += 2
		} else {
			i++
		}
	}
	if pref.normalized_os(target_os.trim_space().to_lower()) == 'cross' {
		cross_output = true
		target_os = os.user_os()
	}
	target := pref.target_from(target_os, target_arch) or { pref.host_target() }
	host_target := pref.host_target()
	selection := v3_select_c_compiler(vroot, V3BundledTccProbeOptions{
		backend:             backend
		is_prod:             is_prod
		is_c_debug:          is_c_debug
		c_compiler:          c_compiler
		c_compiler_explicit: c_compiler_explicit
		dump_c_flags:        dump_c_flags
		parallel_cc:         parallel_cc
		host_os:             os.user_os()
		host_target:         host_target
		target:              target
		bundled_tcc:         os.join_path(vroot, 'thirdparty', 'tcc', 'tcc.exe')
	})
	v3_apply_libc_define(mut user_defines, mut compile_values, libc_mode, selection.c_compiler,
		v3_should_infer_host_libc(false, false, '', cross_output, target, host_target))
	mut defines := []string{}
	for define in os.getenv('VBUILD_DEFINES').split_any(',') {
		name := define.trim_space()
		if name.len > 0 && name !in defines {
			defines << name
		}
	}
	for define in user_defines {
		name := define.all_before('=').trim_space()
		if name.len > 0 && name !in defines {
			defines << name
		}
	}
	return TestBuildEnvironment{
		facts:   v3_test_build_facts(target, selection.effective_c_compiler, is_prod)
		defines: defines
	}
}
