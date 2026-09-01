module main

import os
import crypto.sha256
import v.pref
import v.builder

fn test_macos_v3_embedded_driver_matches_cross_source_selection() {
	$if cross ? {
		assert !macos_v3_driver_is_available()
	} $else $if macos {
		assert macos_v3_driver_is_available()
	}
}

fn test_macos_v3_driver_source_selection_matches_cross_define() {
	driver_files := ['macos_v3_driver_d_cross.v', 'macos_v3_driver_notd_cross.v']
	native_prefs := &pref.Preferences{}
	native_files :=
		native_prefs.should_compile_filtered_files('cmd/v', driver_files).map(os.base(it))
	assert native_files == ['macos_v3_driver_notd_cross.v']

	cross_prefs := &pref.Preferences{
		compile_defines:     ['cross']
		compile_defines_all: ['cross']
	}
	cross_files := cross_prefs.should_compile_filtered_files('cmd/v', driver_files).map(os.base(it))
	assert cross_files == ['macos_v3_driver_d_cross.v']
}

fn test_macos_v3_keeps_established_compiler_sources_on_v1() {
	vroot := os.real_path(os.dir(@VEXE))
	for path in ['vlib/v/checker/pkgconfig_static_mode_test.v',
		os.join_path(vroot, 'vlib/v/builder/c_error_report_test.v'), 'vlib/v/compiler_errors_test.v'] {
		assert is_macos_v3_v1_compiler_source(path)
		assert !is_macos_v3_relevant_command(path, &pref.Preferences{
			path: path
		})
	}
	for path in ['vlib/v/tests/array_test.v', '/workspace/v/vlib/v/slow_tests/example_test.v',
		'vlib/v/gen/c/testdata/backend_independent_struct_layout_v3.v',
		'vlib/v3/tests/driver_cli_test.v', 'examples/hello_world.v'] {
		assert !is_macos_v3_v1_compiler_source(path)
		assert is_macos_v3_relevant_command(path, &pref.Preferences{
			path: path
		})
	}
	user_path := os.join_path(os.vtmp_dir(), 'project/vlib/v/app/main.v')
	assert !is_macos_v3_v1_compiler_source(user_path)
	assert is_macos_v3_relevant_command(user_path, &pref.Preferences{
		path: user_path
	})
}

fn test_macos_v3_relevant_command_selects_user_compilation_and_tests() {
	$if macos {
		mut prefs := &pref.Preferences{
			path:      'main.v'
			backend:   .c
			os:        .macos
			is_script: true
			gc_mode:   .no_gc
		}
		assert is_macos_v3_relevant_command('main.v', prefs)
		prefs.old_compiler = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.old_compiler = false
		prefs.path = ''
		assert !is_macos_v3_relevant_command('test', prefs)
		prefs.path = 'main.v'

		// V3 owns supported user compilation modes.
		prefs.coverage_dir = '/tmp/vcovdir'
		assert is_macos_v3_relevant_command('main.v', prefs)
		prefs.is_o = true
		assert is_macos_v3_relevant_command('main.v', prefs)
		prefs.is_vlines = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.is_vlines = false
		prefs.gc_mode = .boehm_full_opt
		prefs.gc_set_by_flag = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.gc_mode = .no_gc
		prefs.gc_set_by_flag = false
		prefs.sanitize = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.sanitize = false
		prefs.is_livemain = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.is_livemain = false
		prefs.is_liveshared = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.is_liveshared = false
		prefs.is_prof = true
		assert is_macos_v3_relevant_command('main.v', prefs)
		prefs.is_prof = false
		prefs.profile_fns = ['main__work']
		assert is_macos_v3_relevant_command('main.v', prefs)
		prefs.profile_fns.clear()
		prefs.use_os_system_to_run = true
		assert !is_macos_v3_relevant_command('run', prefs)
		prefs.use_os_system_to_run = false
		prefs.output_cross_c = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.output_cross_c = false
		prefs.experimental = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.experimental = false
		prefs.is_apk = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.is_apk = false
		prefs.json_errors = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.json_errors = false
		prefs.no_preludes = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.no_preludes = false
		prefs.skip_warnings = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.skip_warnings = false
		prefs.skip_notes = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.skip_notes = false
		prefs.fatal_errors = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.fatal_errors = false
		prefs.print_watched_files = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.print_watched_files = false
		prefs.dump_modules = 'modules.txt'
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.dump_modules = ''
		prefs.dump_files = 'files.txt'
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.dump_files = ''
		prefs.dump_defines = 'defines.txt'
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.dump_defines = ''
		prefs.warn_impure_v = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.warn_impure_v = false
		prefs.test_runner = 'tap'
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.test_runner = ''
		prefs.exclude = ['@vlib/math/*.c.v']
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.exclude.clear()
		prefs.ldflags = '-L/custom/lib -lcustom'
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.ldflags = ''
		prefs.nofloat = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.nofloat = false
		prefs.fast_math = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.fast_math = false
		prefs.no_closures = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.no_closures = false
		prefs.print_autofree_vars = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.print_autofree_vars = false
		prefs.trace_calls = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.trace_calls = false
		prefs.trace_fns = ['main.main']
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.trace_fns.clear()
		prefs.disable_explicit_mutability = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.disable_explicit_mutability = false
		prefs.compress = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.compress = false
		prefs.is_bare = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.is_bare = false
		prefs.assert_failure_mode = .continues
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.assert_failure_mode = .default
		prefs.build_options << '-m32'
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.build_options.clear()
		prefs.is_run = true
		prefs.autofree = true
		assert !is_macos_v3_relevant_command('run', prefs)
		prefs.autofree = false
		assert is_macos_v3_relevant_command('run', prefs)
		prefs.backend = .wasm
		assert !is_macos_v3_relevant_command('run', prefs)
		prefs.is_run = false
		assert is_macos_v3_relevant_command('main.v', prefs)
		prefs.backend = .c
		prefs.is_shared = true
		assert is_macos_v3_relevant_command('main.v', prefs)
		prefs.is_cstrict = true
		assert is_macos_v3_relevant_command('main.v', prefs)
		prefs.is_prod = true
		assert is_macos_v3_relevant_command('main.v', prefs)
		prefs.out_name_is_dir = true
		assert is_macos_v3_relevant_command('main.v', prefs)
		prefs.backend = .js_node
		prefs.os = .linux
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.backend = .c
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.os = .macos

		prefs.path = 'vlib/v3'
		prefs.is_test = true
		assert !is_macos_v3_relevant_command('test', prefs)
		prefs.path = 'vlib/v3/tests/review_transform_regressions_test.v'
		assert !is_macos_v3_relevant_command('test', prefs)
		prefs.path = 'program.txt'
		assert is_macos_v3_relevant_command('run', prefs)
		assert is_macos_v3_relevant_command('build', prefs)
		prefs.is_script = false
		prefs.path = 'vlib/math'
		assert !is_macos_v3_relevant_command('build-module', prefs)
		prefs.is_script = true
		prefs.path = 'script.vsh'
		prefs.is_vsh = true
		assert !is_macos_v3_relevant_command('script.vsh', prefs)
		prefs.is_vsh = false
		assert !is_macos_v3_relevant_command('crun', prefs)
		for path in ['foo.c.v', 'foo.js.v', 'foo.wasm.v', '.v'] {
			prefs.path = path
			assert is_macos_v3_relevant_command(path, prefs)
		}
		prefs.path = 'fixture.vv'
		assert !is_macos_v3_relevant_command(prefs.path, prefs)

		prefs.path = 'cmd/v'
		assert !is_macos_v3_relevant_command('cmd/v', prefs)
		prefs.path = 'cmd/v/macos_v3_test.v'
		assert !is_macos_v3_relevant_command(prefs.path, prefs)
		prefs.path = 'cmd/tools/vfmt.v'
		assert is_macos_v3_relevant_command('cmd/tools/vfmt.v', prefs) == (os.getenv('VCHILD') != 'true')
		assert is_macos_v3_internal_tool_bootstrap('cmd/tools/vfmt.v', true)
		assert !is_macos_v3_internal_tool_bootstrap('cmd/tools/vfmt.v', false)
		prefs.path = 'vlib/v3/v3.v'
		assert !is_macos_v3_relevant_command(prefs.path, prefs)
		prefs.path = 'version'
		assert !is_macos_v3_relevant_command('version', prefs)
	}
}

fn test_macos_v3_compiler_bootstrap_is_detected_from_any_cwd() {
	// Repo-relative and absolute spellings are recognized directly.
	assert is_macos_v3_compiler_bootstrap('vlib/v3/v3.v')
	assert is_macos_v3_compiler_bootstrap('/home/user/v/vlib/v3/v3.v')
	// Non-bootstrap targets stay on the V3 path.
	assert !is_macos_v3_compiler_bootstrap('main.v')
	assert !is_macos_v3_compiler_bootstrap('cmd/v')
	assert !is_macos_v3_compiler_bootstrap('some/other/place/v3.v')

	// A bare `v3.v` invoked from inside vlib/v3 must resolve to the bootstrap so
	// it builds with the compatibility compiler instead of the embedded V3 driver.
	// Use an isolated <tmp>/vlib/v3/v3.v so this exercises the real-path
	// resolution unconditionally, independent of where this test file lives.
	root := os.join_path(os.real_path(os.vtmp_dir()), 'macos_v3_bootstrap_${os.getpid()}')
	v3_dir := os.join_path(root, 'vlib', 'v3')
	other_dir := os.join_path(root, 'elsewhere')
	os.rmdir_all(root) or {}
	os.mkdir_all(v3_dir) or { panic(err) }
	os.mkdir_all(other_dir) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(v3_dir, 'v3.v'), 'module main\n') or { panic(err) }
	os.write_file(os.join_path(other_dir, 'v3.v'), 'module main\n') or { panic(err) }

	saved := os.getwd()
	defer {
		os.chdir(saved) or {}
	}
	os.chdir(v3_dir) or { panic(err) }
	bare := is_macos_v3_compiler_bootstrap('v3.v')
	dotted := is_macos_v3_compiler_bootstrap('./v3.v')
	// A bare `v3.v` that is not under vlib/v3 must stay on the V3 path.
	os.chdir(other_dir) or { panic(err) }
	non_bootstrap := is_macos_v3_compiler_bootstrap('v3.v')
	os.chdir(saved) or {}
	assert bare
	assert dotted
	assert !non_bootstrap
}

fn test_macos_v3_dispatch_allows_the_implicit_gc_default() {
	$if macos {
		implicit_gc, _ := pref.parse_args_and_show_errors([], ['', 'main.v'], false)
		assert implicit_gc.gc_mode == .boehm_full_opt
		assert !implicit_gc.gc_set_by_flag
		assert is_macos_v3_relevant_command('main.v', implicit_gc)

		explicit_boehm, _ := pref.parse_args_and_show_errors([], ['', '-gc', 'boehm', 'main.v'],
			false)
		assert explicit_boehm.gc_mode == .boehm_full_opt
		assert explicit_boehm.gc_set_by_flag
		assert !is_macos_v3_relevant_command('main.v', explicit_boehm)

		explicit_none, _ := pref.parse_args_and_show_errors([], ['', '-gc', 'none', 'main.v'],
			false)
		assert explicit_none.gc_mode == .no_gc
		assert explicit_none.gc_set_by_flag
		assert is_macos_v3_relevant_command('main.v', explicit_none)

		prealloc, _ := pref.parse_args_and_show_errors([], ['', '-prealloc', 'main.v'], false)
		assert prealloc.gc_mode == .no_gc
		assert !prealloc.gc_set_by_flag
		assert is_macos_v3_relevant_command('main.v', prealloc)
	}
}

fn test_autofree_notice_suppression_requires_standard_compiler() {
	prefs, _ := pref.parse_args_and_show_errors([], ['', '-autofree', '-n', 'main.v'], false)
	assert prefs.autofree
	assert prefs.skip_notes
	assert autofree_requires_standard_compiler(prefs)
}

fn test_remaining_unsupported_autofree_modes_require_standard_compiler() {
	$if macos {
		coroutines, _ := pref.parse_args_and_show_errors([], [
			'',
			'-autofree',
			'-use-coroutines',
			'main.v',
		], false)
		assert coroutines.use_coroutines
		assert autofree_requires_standard_compiler(coroutines)

		cutoff, _ := pref.parse_args_and_show_errors([], [
			'',
			'-autofree',
			'-checker-match-exhaustive-cutoff-limit',
			'20',
			'main.v',
		], false)
		assert cutoff.checker_match_exhaustive_cutoff_limit == 20
		assert autofree_requires_standard_compiler(cutoff)
	}
}

fn test_selective_profile_autofree_uses_v3_compiler() {
	$if macos {
		prefs, _ := pref.parse_args_and_show_errors([], [
			'',
			'-autofree',
			'-profile-fns',
			'main__work',
			'main.v',
		], false)
		assert prefs.profile_fns == ['main__work']
		assert !autofree_requires_standard_compiler(prefs)
	}
}

fn test_no_relaxed_gcc14_autofree_requires_standard_compiler() {
	prefs, _ := pref.parse_args_and_show_errors([], [
		'',
		'-autofree',
		'-no-relaxed-gcc14',
		'main.v',
	], false)
	assert !prefs.relaxed_gcc14
	assert autofree_requires_standard_compiler(prefs)
}

fn test_fatal_errors_requires_standard_compiler() {
	prefs, _ := pref.parse_args_and_show_errors([], ['', '-Wfatal-errors', 'main.v'], false)
	assert prefs.fatal_errors
	assert v3_has_v1_only_preferences(prefs)
}

fn test_obfuscation_aliases_require_standard_compiler() {
	for option in ['-obf', '-obfuscate'] {
		prefs, _ := pref.parse_args_and_show_errors([], ['', option, 'main.v'], false)
		assert prefs.obfuscate_removed
		assert v3_has_v1_only_preferences(prefs)
	}
}

fn test_vls_mode_requires_standard_compiler() {
	prefs, _ := pref.parse_args_and_show_errors([], ['', '-check', '-vls-mode', 'main.v'], false)
	assert prefs.is_vls
	assert v3_has_v1_only_preferences(prefs)
}

fn test_new_transformer_requires_standard_compiler() {
	prefs, _ := pref.parse_args_and_show_errors([], ['', '-new-transformer', 'main.v'], false)
	assert prefs.new_transform
	assert v3_has_v1_only_preferences(prefs)
}

fn test_unsupported_compiler_modes_require_standard_compiler() {
	cmain, _ := pref.parse_args_and_show_errors([], [
		'',
		'-autofree',
		'-cmain',
		'SDL_main',
		'main.v',
	], false)
	assert cmain.cmain == 'SDL_main'
	assert autofree_requires_standard_compiler(cmain)

	prelude_path := os.join_path(os.vtmp_dir(), 'macos_v3_custom_prelude_${os.getpid()}.h')
	os.write_file(prelude_path, '/* custom prelude */')!
	defer {
		os.rm(prelude_path) or {}
	}
	custom_prelude, _ := pref.parse_args_and_show_errors([], [
		'',
		'-autofree',
		'-custom-prelude',
		prelude_path,
		'main.v',
	], false)
	assert custom_prelude.custom_prelude == '/* custom prelude */'
	assert autofree_requires_standard_compiler(custom_prelude)

	check_return, _ := pref.parse_args_and_show_errors([], [
		'',
		'-autofree',
		'-check-return',
		'main.v',
	], false)
	assert check_return.is_check_return
	assert autofree_requires_standard_compiler(check_return)

	div_by_zero, _ := pref.parse_args_and_show_errors([], [
		'',
		'-autofree',
		'-div-by-zero-is-zero',
		'main.v',
	], false)
	assert div_by_zero.div_by_zero_is_zero
	assert autofree_requires_standard_compiler(div_by_zero)
}

fn test_autofree_no_std_requires_standard_compiler() {
	prefs, _ := pref.parse_args_and_show_errors([], ['', '-autofree', '-no-std', 'main.v'], false)
	assert prefs.autofree
	assert prefs.no_std
	assert autofree_requires_standard_compiler(prefs)
}

fn test_autofree_no_closures_requires_standard_compiler() {
	prefs, _ := pref.parse_args_and_show_errors([], ['', '-autofree', '-no-closures', 'main.v'],
		false)
	assert prefs.autofree
	assert prefs.no_closures
	assert autofree_requires_standard_compiler(prefs)
}

fn test_autofree_inspection_requires_standard_compiler() {
	all_vars, _ := pref.parse_args_and_show_errors([], [
		'',
		'-autofree',
		'-print_autofree_vars',
		'main.v',
	], false)
	assert all_vars.autofree
	assert all_vars.print_autofree_vars
	assert autofree_requires_standard_compiler(all_vars)

	function_vars, _ := pref.parse_args_and_show_errors([], [
		'',
		'-autofree',
		'-print_autofree_vars_in_fn',
		'main.main',
		'main.v',
	], false)
	assert function_vars.autofree
	assert function_vars.print_autofree_vars
	assert function_vars.print_autofree_vars_in_fn == 'main.main'
	assert autofree_requires_standard_compiler(function_vars)
}

fn test_autofree_inspection_output_requires_standard_compiler() {
	for option in ['-show-asserts', '-show-callgraph', '-show-depgraph'] {
		prefs, _ := pref.parse_args_and_show_errors([], ['', '-autofree', option, 'main.v'], false)
		assert prefs.autofree
		match option {
			'-show-asserts' { assert prefs.show_asserts }
			'-show-callgraph' { assert prefs.show_callgraph }
			'-show-depgraph' { assert prefs.show_depgraph }
			else { assert false }
		}
		assert autofree_requires_standard_compiler(prefs)
	}
}

fn test_autofree_hide_auto_str_requires_standard_compiler() {
	prefs, _ := pref.parse_args_and_show_errors([], ['', '-autofree', '-hide-auto-str', 'main.v'],
		false)
	assert prefs.autofree
	assert prefs.hide_auto_str
	assert autofree_requires_standard_compiler(prefs)
}

fn test_autofree_response_files_and_message_limits_require_standard_compiler() {
	no_rsp, _ := pref.parse_args_and_show_errors([], ['', '-autofree', '-no-rsp', 'main.v'], false)
	assert no_rsp.autofree
	assert no_rsp.no_rsp
	assert autofree_requires_standard_compiler(no_rsp)

	for limit in ['1', '-1'] {
		prefs, _ := pref.parse_args_and_show_errors([], ['', '-autofree', '-message-limit', limit,
			'main.v'], false)
		assert prefs.autofree
		assert prefs.message_limit == limit.int()
		assert autofree_requires_standard_compiler(prefs)
	}

	$if macos {
		assert autofree_args_require_standard_compiler(['-autofree', '-message-limit', '200',
			'main.v'], 'main.v')
	}
}

fn test_autofree_allocation_warnings_require_standard_compiler() {
	prefs, _ := pref.parse_args_and_show_errors([], [
		'',
		'-autofree',
		'-warn-about-allocs',
		'main.v',
	], false)
	assert prefs.autofree
	assert prefs.warn_about_allocs
	assert autofree_requires_standard_compiler(prefs)
}

fn test_autofree_bug_report_url_requires_standard_compiler() {
	prefs, _ := pref.parse_args_and_show_errors([], [
		'',
		'-autofree',
		'-bug-report-url',
		'https://bugs.example.test',
		'main.v',
	], false)
	assert prefs.autofree
	assert prefs.c_error_bug_report_url == 'https://bugs.example.test'
	assert autofree_requires_standard_compiler(prefs)
}

fn test_line_info_requires_standard_compiler() {
	prefs, _ := pref.parse_args_and_show_errors([], [
		'',
		'-autofree',
		'-line-info',
		'main.v:24:7',
		'main.v',
	], false)
	assert prefs.autofree
	assert prefs.line_info == 'main.v:24:7'
	assert prefs.linfo.path == 'main.v'
	assert prefs.linfo.line_nr == 23
	assert prefs.linfo.col == 6
	assert autofree_requires_standard_compiler(prefs)
	$if macos {
		assert !is_macos_v3_relevant_command('main.v', prefs)
	}
}

fn test_autofree_cross_target_requires_standard_compiler() {
	$if macos {
		for target in ['ios', 'linux', 'windows'] {
			prefs, _ := pref.parse_args_and_show_errors([], [
				'',
				'-autofree',
				'-os',
				target,
				'main.v',
			], false)
			assert prefs.autofree
			assert prefs.backend == .c
			assert prefs.os != .macos
			assert autofree_requires_standard_compiler(prefs)
		}

		native, _ := pref.parse_args_and_show_errors([], ['', '-autofree', 'main.v'], false)
		assert native.os == .macos
		assert !autofree_requires_standard_compiler(native)

		wasm, _ := pref.parse_args_and_show_errors([], [
			'',
			'-autofree',
			'-b',
			'wasm',
			'main.v',
		], false)
		assert wasm.backend == .wasm
		assert wasm.os == .wasi
		assert !autofree_requires_standard_compiler(wasm)
	}
}

fn test_autofree_wasm_options_require_standard_compiler() {
	validate, _ := pref.parse_args_and_show_errors([], [
		'',
		'-autofree',
		'-b',
		'wasm',
		'-wasm-validate',
		'main.v',
	], false)
	assert validate.autofree
	assert validate.backend == .wasm
	assert validate.wasm_validate
	assert autofree_requires_standard_compiler(validate)

	stack_top, _ := pref.parse_args_and_show_errors([], [
		'',
		'-autofree',
		'-b',
		'wasm',
		'-wasm-stack-top',
		'32768',
		'main.v',
	], false)
	assert stack_top.autofree
	assert stack_top.backend == .wasm
	assert stack_top.wasm_stack_top == 32768
	assert autofree_requires_standard_compiler(stack_top)

	$if macos {
		assert autofree_args_require_standard_compiler(['-autofree', '-b', 'wasm', '-wasm-validate',
			'main.v'], 'main.v')
		assert autofree_args_require_standard_compiler(['-autofree', '-b', 'wasm', '-wasm-stack-top',
			'17408', 'main.v'], 'main.v')
		assert !autofree_args_require_standard_compiler(['-autofree', '-b', 'wasm', 'main.v',
			'-wasm-validate'], 'main.v')
	}
}

fn test_autofree_debug_alias_requires_standard_compiler() {
	prefs, _ := pref.parse_args_and_show_errors([], ['', '-autofree', '-debug', 'main.v'], false)
	assert prefs.autofree
	assert prefs.is_debug
	assert prefs.is_vlines
	assert autofree_requires_standard_compiler(prefs)
	$if macos {
		assert autofree_args_require_standard_compiler(['-autofree', '-debug', 'main.v'], 'main.v')
	}
}

fn test_autofree_debug_tcc_requires_standard_compiler() {
	$if macos {
		prefs, _ := pref.parse_args_and_show_errors([], [
			'',
			'-autofree',
			'-debug-tcc',
			'main.v',
		], false)
		assert prefs.autofree
		assert prefs.ccompiler_type == .tinyc
		assert !prefs.retry_compilation
		assert prefs.show_cc
		assert prefs.show_c_output
		assert prefs.build_options.any(it.starts_with('-debug-tcc'))
		assert autofree_requires_standard_compiler(prefs)
		assert autofree_args_require_standard_compiler(['-autofree', '-debug-tcc', 'main.v'],
			'main.v')

		explicit, _ := pref.parse_args_and_show_errors([], [
			'',
			'-autofree',
			'-cc',
			'tcc',
			'-showcc',
			'-show-c-output',
			'-no-retry-compilation',
			'main.v',
		], false)
		assert explicit.ccompiler_type == .tinyc
		assert !explicit.retry_compilation
		assert explicit.show_cc
		assert explicit.show_c_output
		assert !autofree_requires_standard_compiler(explicit)
	}
}

fn test_autofree_tracing_requires_standard_compiler() {
	trace_calls, _ := pref.parse_args_and_show_errors([], [
		'',
		'-autofree',
		'-trace-calls',
		'main.v',
	], false)
	assert trace_calls.autofree
	assert trace_calls.trace_calls
	assert autofree_requires_standard_compiler(trace_calls)

	trace_fns, _ := pref.parse_args_and_show_errors([], [
		'',
		'-autofree',
		'-trace-fns',
		'main.main',
		'main.v',
	], false)
	assert trace_fns.autofree
	assert trace_fns.trace_fns == ['main.main']
	assert autofree_requires_standard_compiler(trace_fns)
}

fn test_autofree_relaxed_mutability_requires_standard_compiler() {
	for option in ['-disable-explicit-mutability', '--disable-explicit-mutability'] {
		prefs, _ := pref.parse_args_and_show_errors([], ['', '-autofree', option, 'main.v'], false)
		assert prefs.autofree
		assert prefs.disable_explicit_mutability
		assert autofree_requires_standard_compiler(prefs)
	}
}

fn test_autofree_dump_reports_require_standard_compiler() {
	for option in ['-dump-modules', '-dump-files', '-dump-defines'] {
		prefs, _ := pref.parse_args_and_show_errors([], [
			'',
			'-autofree',
			option,
			'report.txt',
			'main.v',
		], false)
		assert prefs.autofree
		match option {
			'-dump-modules' { assert prefs.dump_modules == 'report.txt' }
			'-dump-files' { assert prefs.dump_files == 'report.txt' }
			'-dump-defines' { assert prefs.dump_defines == 'report.txt' }
			else { assert false }
		}
		assert autofree_requires_standard_compiler(prefs)
	}
}

fn test_macos_v3_implicit_gc_default_uses_v3() {
	$if macos {
		root := os.join_path(os.real_path(os.vtmp_dir()), 'macos_v3_implicit_gc_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root)!
		defer {
			os.rmdir_all(root) or {}
		}
		source := os.join_path(root, 'main.v')
		output := os.join_path(root, 'main')
		os.write_file(source, "fn main() { println('implicit gc v3') }\n")!
		mut environment := os.environ()
		environment['CFLAGS'] = ''
		environment['LDFLAGS'] = ''
		environment['VFLAGS'] = ''
		environment['VOSARGS'] = ''
		mut process := os.new_process(@VEXE)
		process.set_args(['-v', '-nocache', '-o', output, source])
		process.set_environment(environment)
		process.set_redirect_stdio()
		process.run()
		process.wait()
		compiler_output := process.stdout_slurp() + process.stderr_slurp()
		exit_code := process.code
		process.close()
		assert exit_code == 0, compiler_output
		assert compiler_output.contains('Running macOS V3 compiler in process:'), compiler_output
		assert os.is_executable(output)
		run := os.execute(os.quoted_path(output))
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == 'implicit gc v3'
	}
}

fn test_macos_v3_use_os_system_to_run_stays_on_v1() {
	$if macos {
		root := os.join_path(os.real_path(os.vtmp_dir()), 'macos_v3_system_run_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root)!
		defer {
			os.rmdir_all(root) or {}
		}
		source := os.join_path(root, 'main.v')
		os.write_file(source, "fn main() { println('system run v1') }\n")!
		mut environment := os.environ()
		environment['CFLAGS'] = ''
		environment['LDFLAGS'] = ''
		environment['VFLAGS'] = ''
		environment['VOSARGS'] = ''
		mut process := os.new_process(@VEXE)
		process.set_args(['-v', '-gc', 'none', '-use-os-system-to-run', 'run', source])
		process.set_environment(environment)
		process.set_redirect_stdio()
		process.run()
		process.wait()
		compiler_output := process.stdout_slurp() + process.stderr_slurp()
		exit_code := process.code
		process.close()
		assert exit_code == 0, compiler_output
		assert !compiler_output.contains('Running macOS V3 compiler in process:'), compiler_output
		assert !compiler_output.contains('unknown option'), compiler_output
		assert compiler_output.contains('system run v1'), compiler_output
	}
}

fn test_macos_v3_detects_v1_only_leading_options() {
	$if macos {
		assert macos_v3_has_v1_only_leading_option(['-autofree', '-debug', 'main.v'], 'main.v')
		assert macos_v3_has_v1_only_leading_option(['-message-limit', '0', 'main.v'], 'main.v')
		assert macos_v3_has_v1_only_leading_option(['-message-limit', '5', 'run', 'main.v'], 'run')
		assert macos_v3_has_v1_only_leading_option(['-gc', 'none', '-o', 'run', '-message-limit',
			'0', 'run', 'bad.v'], 'run')
		assert macos_v3_has_v1_only_leading_option(['-autofree', '-use-coroutines', 'main.v'],
			'main.v')
		assert macos_v3_has_v1_only_leading_option(['-autofree',
			'-checker-match-exhaustive-cutoff-limit', '12', 'main.v'], 'main.v')
		assert macos_v3_has_v1_only_leading_option(['-raw-vsh-tmp-prefix', 'tmp', 'script'],
			'script')
		assert macos_v3_has_v1_only_leading_option(['-c++', 'clang++', 'main.v'], 'main.v')
		assert macos_v3_has_v1_only_leading_option(['-check-unused-fn-args', 'main.v'], 'main.v')
		assert autofree_args_require_standard_compiler(['-autofree', '-check-unused-fn-args',
			'main.v'], 'main.v')
		assert macos_v3_has_v1_only_leading_option(['-subsystem', 'console', 'main.v'], 'main.v')
		assert autofree_args_require_standard_compiler(['-autofree', '-subsystem', 'console',
			'main.v'], 'main.v')
		assert macos_v3_has_v1_only_leading_option(['-autofree', '-translated-go', 'main.v'],
			'main.v')
		assert autofree_args_require_standard_compiler(['-autofree', '-translated-go', 'main.v'],
			'main.v')
		for option in ['-musl', '-glibc'] {
			assert macos_v3_has_v1_only_leading_option(['-autofree', option, 'main.v'], 'main.v')
			assert autofree_args_require_standard_compiler(['-autofree', option, 'main.v'],
				'main.v')
		}
		assert !macos_v3_has_v1_only_leading_option(['run', 'main.v', '-message-limit', '5'], 'run')
		assert !macos_v3_has_v1_only_leading_option(['--', '-message-limit', '5', 'main.v'],
			'main.v')
	}
}

fn test_macos_v3_forwards_environment_driven_skip_running() {
	$if macos {
		prefs := &pref.Preferences{
			skip_running: true
		}
		forwarded := macos_v3_forwarded_args(prefs, ['script.vsh'])
		assert '-skip-running' in forwarded
		assert forwarded.count(it == '-skip-running') == 1
		already_explicit := macos_v3_forwarded_args(prefs, ['-skip-running', 'script.vsh'])
		assert already_explicit.count(it == '-skip-running') == 1
	}
}

fn test_macos_v3_normalizes_legacy_x86_arch_alias() {
	$if macos {
		mut prefs := &pref.Preferences{
			arch: .amd64
		}
		prefs.build_options << '-arch x86'
		forwarded := macos_v3_forwarded_args(prefs, ['-arch', 'x86', 'main.v'])
		arch_index := forwarded.index('-arch')
		assert arch_index >= 0
		assert forwarded[arch_index + 1] == 'amd64'
		duplicate := macos_v3_forwarded_args(prefs, ['-arch', 'x86', '-arch', 'x86', 'main.v'])
		assert duplicate.count(it == 'amd64') == 2
		assert 'x86' !in duplicate

		prefs.build_options.clear()
		program_args := macos_v3_forwarded_args(prefs, ['run', 'main.v', '-arch', 'x86'])
		assert program_args.last() == 'x86'
	}
}

fn test_macos_v3_normalizes_enable_globals_alias() {
	$if macos {
		prefs := &pref.Preferences{
			enable_globals: true
		}
		forwarded := macos_v3_forwarded_args(prefs, ['--enable-globals', 'main.v'])
		assert '-enable-globals' in forwarded
		assert '--enable-globals' !in forwarded
		duplicate := macos_v3_forwarded_args(prefs, [
			'--enable-globals',
			'--enable-globals',
			'main.v',
		])
		assert duplicate.count(it == '-enable-globals') == 2
		assert '--enable-globals' !in duplicate

		program_args := macos_v3_forwarded_args(&pref.Preferences{}, [
			'run',
			'main.v',
			'--enable-globals',
		])
		assert program_args.last() == '--enable-globals'
	}
}

fn test_macos_v3_forwards_showcc_with_quiet_benchmarks() {
	$if macos {
		prefs := &pref.Preferences{
			show_cc: true
		}
		forwarded := macos_v3_forwarded_args(prefs, ['-showcc', 'main.v'])
		assert macos_v3_internal_quiet_flag in forwarded
		assert '-silent' !in forwarded
		assert '-showcc' in forwarded
		explicit_silent := macos_v3_forwarded_args(prefs, ['-silent', '-showcc', 'main.v'])
		assert '-silent' in explicit_silent
		assert macos_v3_internal_quiet_flag !in explicit_silent
	}
}

fn test_macos_v3_show_c_output_prints_successful_compiler_output() {
	$if macos {
		root := os.join_path(os.real_path(os.vtmp_dir()), 'macos_v3_show_c_output_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root) or { panic(err) }
		defer {
			os.rmdir_all(root) or {}
		}
		compiler := os.join_path(root, 'cc')
		source := os.join_path(root, 'main.v')
		output := os.join_path(root, 'main')
		os.write_file(compiler,
			'#!/bin/sh\necho "V3_SHOW_C_OUTPUT_MARKER" >&2\nexec /usr/bin/cc "\$@"\n')!
		os.chmod(compiler, 0o700)!
		os.write_file(source, 'fn main() {}\n')!
		mut environment := os.environ()
		environment['CFLAGS'] = ''
		environment['LDFLAGS'] = ''
		environment['VFLAGS'] = ''
		environment['VOSARGS'] = ''
		mut process := os.new_process(@VEXE)
		process.set_args(['-v', '-gc', 'none', '-nocache', '-show-c-output', '-cc', compiler, '-o',
			output, source])
		process.set_environment(environment)
		process.set_redirect_stdio()
		process.run()
		process.wait()
		compiler_output := process.stdout_slurp() + process.stderr_slurp()
		exit_code := process.code
		process.close()
		assert exit_code == 0, compiler_output
		assert compiler_output.contains('Running macOS V3 compiler in process:'), compiler_output
		assert compiler_output.contains('Output of the C Compiler'), compiler_output
		assert compiler_output.contains('V3_SHOW_C_OUTPUT_MARKER'), compiler_output
		assert os.is_executable(output)
	}
}

fn test_macos_v3_parallel_cc_ignores_inactive_header_definitions() {
	$if macos {
		root := os.join_path(os.real_path(os.vtmp_dir()),
			'macos_v3_parallel_cc_inactive_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root)!
		defer {
			os.rmdir_all(root) or {}
		}
		source := os.join_path(root, 'main.v')
		output := os.join_path(root, 'main')
		os.write_file(os.join_path(root, 'windows_impl.h'),
			'int windows_impl(void) { return 1; }\n')!
		os.write_file(source, '
$if windows {
	#include "@DIR/windows_impl.h"
}

fn main() {
	println("active target")
}
')!
		mut environment := os.environ()
		environment['CFLAGS'] = ''
		environment['LDFLAGS'] = ''
		environment['VFLAGS'] = ''
		environment['VOSARGS'] = ''
		mut process := os.new_process(@VEXE)
		process.set_args(['-v', '-gc', 'none', '-parallel-cc', '-nocache', '-o', output, source])
		process.set_environment(environment)
		process.set_redirect_stdio()
		process.run()
		process.wait()
		compiler_output := process.stdout_slurp() + process.stderr_slurp()
		exit_code := process.code
		process.close()
		assert exit_code == 0, compiler_output
		assert compiler_output.contains('Running macOS V3 compiler in process:'), compiler_output
		assert !compiler_output.contains('failed to link after parallel C compilation'), compiler_output
		assert os.is_executable(output)
		run := os.execute(os.quoted_path(output))
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == 'active target'
	}
}

fn test_macos_v3_forwards_compatibility_c99_mode() {
	$if macos {
		mut prefs := &pref.Preferences{}
		forwarded := macos_v3_forwarded_args(prefs, ['main.v'])
		assert macos_v3_compat_c99_flag in forwarded
		assert '-nocache' !in forwarded
		assert '--no-cache' !in forwarded
		assert '-no-memory-limit' !in forwarded
		assert '--no-memory-limit' !in forwarded
		assert '-no-parallel' !in forwarded
		assert forwarded.count(it == macos_v3_compat_c99_flag) == 1
		already_present := macos_v3_forwarded_args(prefs, [macos_v3_compat_c99_flag, 'main.v'])
		assert already_present.count(it == macos_v3_compat_c99_flag) == 1
		assert already_present.count(it in ['-nocache', '--no-cache']) == 0
		assert already_present.count(it in ['-no-memory-limit', '--no-memory-limit']) == 0
		explicit_no_cache := macos_v3_forwarded_args(prefs, ['--no-cache', 'main.v'])
		assert explicit_no_cache.count(it in ['-nocache', '--no-cache']) == 1
		explicit_memory_limit := macos_v3_forwarded_args(prefs, ['--no-memory-limit', 'main.v'])
		assert explicit_memory_limit.count(it in ['-no-memory-limit', '--no-memory-limit']) == 1
	}
}

fn test_autofree_non_direct_commands_stay_on_the_standard_command_path() {
	mut prefs := &pref.Preferences{
		path:   'app.v'
		is_run: true
	}
	assert !is_ownership_relevant_command('run', prefs)
	assert !is_ownership_relevant_command('test', prefs)
	prefs.autofree = true
	$if macos {
		assert !is_macos_v3_relevant_command('run', prefs)
	}
	prefs.autofree = false
	prefs.is_run = false
	assert is_ownership_relevant_command('app.v', prefs)
}

fn test_ownership_delegation_is_platform_scoped_and_honors_old_compiler() {
	assert !ownership_delegation_is_requested(false, false, false, false, 'macos')
	assert ownership_delegation_is_requested(true, false, false, false, 'linux')
	assert ownership_delegation_is_requested(true, false, false, false, 'windows')
	assert ownership_delegation_is_requested(true, true, false, false, 'linux')
	assert ownership_delegation_is_requested(false, true, false, false, 'macos')
	assert !ownership_delegation_is_requested(false, true, false, false, 'linux')
	assert !ownership_delegation_is_requested(false, true, false, false, 'windows')
	assert !ownership_delegation_is_requested(false, true, true, false, 'macos')
	assert !ownership_delegation_is_requested(true, false, true, false, 'macos')
	assert !ownership_delegation_is_requested(false, true, false, true, 'macos')
	assert ownership_delegation_is_requested(true, true, false, true, 'macos')
}

fn test_ownership_forwarding_adds_the_compile_time_define_once() {
	prefs := &pref.Preferences{}
	forwarded := v3_ownership_forwarded_args(prefs, ['-ownership', 'main.v'])
	assert '-ownership' !in forwarded
	assert v3_args_have_ownership_define(forwarded)
	explicit := v3_ownership_forwarded_args(prefs, ['-ownership', '-d', 'ownership', 'main.v'])
	assert explicit.count(it == 'ownership') == 1
	invalid_compact := v3_ownership_forwarded_args(prefs, ['-ownership', '-d=ownership', 'main.v'])
	assert '-d=ownership' in invalid_compact
	assert invalid_compact.count(it == '-d') == 1
	assert invalid_compact.count(it == 'ownership') == 1
}

fn test_macos_v3_ownership_forwarding_is_quiet_and_normalizes_x86() {
	$if macos {
		prefs, _ := pref.parse_args_and_show_errors([], [
			'',
			'-autofree',
			'-arch',
			'x86',
			'main.v',
		], false)
		forwarded := v3_ownership_forwarded_args(prefs, ['-arch', 'x86', '-autofree', '-arch',
			'x86', 'main.v'])
		assert macos_v3_internal_quiet_flag in forwarded
		assert '-ownership' !in forwarded
		assert v3_args_have_ownership_define(forwarded)
		assert forwarded.count(it == 'amd64') == 2
		assert 'x86' !in forwarded

		for option in ['-stats', '-v', '-show-timings'] {
			explicit_prefs, _ := pref.parse_args_and_show_errors([], ['', '-autofree', option,
				'main.v'], false)
			explicit := v3_ownership_forwarded_args(explicit_prefs, ['-autofree', option, 'main.v'])
			assert macos_v3_internal_quiet_flag !in explicit
		}
	}
}

fn test_ownership_delegation_defines_target_ownership() {
	$if !linux && !macos {
		return
	}
	root := os.join_path(os.real_path(os.vtmp_dir()), 'v3_ownership_target_define_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	module_dir := os.join_path(root, 'marker')
	os.mkdir_all(module_dir) or { panic(err) }
	os.write_file(os.join_path(module_dir, 'marker.v'), 'module marker
')!
	os.write_file(os.join_path(module_dir, 'marker_d_ownership.v'), 'module marker

pub fn value() string {
	return "ownership"
}
')!
	os.write_file(source, r'
import marker

$if !ownership ? {
	$compile_error("ownership define was not forwarded to target")
}

fn main() {
	println(marker.value())
}
')!
	mut environment := os.environ()
	environment['VFLAGS'] = ''
	environment['VOSARGS'] = ''
	mut process := os.new_process(@VEXE)
	process.set_args(['-ownership', '-no-parallel', '-d=ownership', '-check', source])
	process.set_environment(environment)
	process.set_redirect_stdio()
	process.run()
	process.wait()
	compiler_output := process.stdout_slurp() + process.stderr_slurp()
	exit_code := process.code
	process.close()
	assert exit_code == 0, compiler_output
}

fn test_autofree_unsupported_modes_stay_on_the_standard_compiler() {
	mut prefs := &pref.Preferences{}
	assert !autofree_requires_standard_compiler(prefs)
	prefs.path = 'fixture.vv'
	assert autofree_requires_standard_compiler(prefs)
	prefs.path = ''
	prefs.is_quiet = true
	assert autofree_requires_standard_compiler(prefs)
	prefs.is_quiet = false
	prefs.sanitize = true
	assert autofree_requires_standard_compiler(prefs)
	prefs.sanitize = false
	prefs.output_cross_c = true
	assert autofree_requires_standard_compiler(prefs)
	prefs.output_cross_c = false
	prefs.experimental = true
	assert autofree_requires_standard_compiler(prefs)
	prefs.experimental = false
	prefs.use_os_system_to_run = true
	assert autofree_requires_standard_compiler(prefs)
	prefs.use_os_system_to_run = false
	prefs.macosx_version_min = '11.0'
	assert autofree_requires_standard_compiler(prefs)
	prefs.macosx_version_min = '0'
	prefs.gc_set_by_flag = true
	prefs.gc_mode = .boehm_full_opt
	assert autofree_requires_standard_compiler(prefs)
}

fn test_autofree_libc_selections_require_standard_compiler() {
	musl, _ := pref.parse_args_and_show_errors([], ['', '-autofree', '-musl', 'main.v'], false)
	assert musl.is_musl
	assert autofree_requires_standard_compiler(musl)
	glibc, _ := pref.parse_args_and_show_errors([], ['', '-autofree', '-glibc', 'main.v'], false)
	assert glibc.is_glibc
	assert autofree_requires_standard_compiler(glibc)
}

fn test_macos_v3_keeps_v1_only_autofree_and_experimental_builds_on_v1() {
	$if macos {
		root := os.join_path(os.real_path(os.vtmp_dir()), 'macos_v3_v1_only_modes_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root)!
		defer {
			os.rmdir_all(root) or {}
		}
		mut environment := os.environ()
		environment['CFLAGS'] = ''
		environment['LDFLAGS'] = ''
		environment['VFLAGS'] = ''
		environment['VOSARGS'] = ''
		cross_source := os.join_path(root, 'cross.v')
		cross_output := os.join_path(root, 'cross.c')
		os.write_file(cross_source, 'fn main() {}\n')!
		mut cross_process := os.new_process(@VEXE)
		cross_process.set_args(['-v', '-autofree', '-cross', '-o', cross_output, cross_source])
		cross_process.set_environment(environment)
		cross_process.set_redirect_stdio()
		cross_process.run()
		cross_process.wait()
		cross_build_output := cross_process.stdout_slurp() + cross_process.stderr_slurp()
		cross_exit_code := cross_process.code
		cross_process.close()
		assert cross_exit_code == 0, cross_build_output
		assert !cross_build_output.contains('Launching v3_ownership:'), cross_build_output
		assert os.is_file(cross_output)
		assert !os.is_executable(cross_output)

		sanitize_source := os.join_path(root, 'sanitize.v')
		os.write_file(sanitize_source, 'fn main() {}\n')!
		mut sanitize_process := os.new_process(@VEXE)
		sanitize_process.set_args(['-v', '-autofree', '-sanitize', '-check', sanitize_source])
		sanitize_process.set_environment(environment)
		sanitize_process.set_redirect_stdio()
		sanitize_process.run()
		sanitize_process.wait()
		sanitize_build_output := sanitize_process.stdout_slurp() + sanitize_process.stderr_slurp()
		sanitize_exit_code := sanitize_process.code
		sanitize_process.close()
		assert sanitize_exit_code == 0, sanitize_build_output
		assert !sanitize_build_output.contains('Launching v3_ownership:'), sanitize_build_output

		deployment_source := os.join_path(root, 'deployment.v')
		os.write_file(deployment_source, 'fn main() {}\n')!
		mut deployment_process := os.new_process(@VEXE)
		deployment_process.set_args(['-v', '-autofree', '-macosx-version-min', '11.0', '-check',
			deployment_source])
		deployment_process.set_environment(environment)
		deployment_process.set_redirect_stdio()
		deployment_process.run()
		deployment_process.wait()
		deployment_build_output := deployment_process.stdout_slurp() +
			deployment_process.stderr_slurp()
		deployment_exit_code := deployment_process.code
		deployment_process.close()
		assert deployment_exit_code == 0, deployment_build_output
		assert !deployment_build_output.contains('Launching v3_ownership:'), deployment_build_output

		quiet_source := os.join_path(root, 'quiet.v')
		os.write_file(quiet_source, 'fn main() {}\n')!
		mut quiet_process := os.new_process(@VEXE)
		quiet_process.set_args(['-v', '-autofree', '-q', '-check', quiet_source])
		quiet_process.set_environment(environment)
		quiet_process.set_redirect_stdio()
		quiet_process.run()
		quiet_process.wait()
		quiet_build_output := quiet_process.stdout_slurp() + quiet_process.stderr_slurp()
		quiet_exit_code := quiet_process.code
		quiet_process.close()
		assert quiet_exit_code == 0, quiet_build_output
		assert !quiet_build_output.contains('Launching v3_ownership:'), quiet_build_output

		experimental_source := os.join_path(root, 'experimental.v')
		experimental_output := os.join_path(root, 'experimental')
		os.write_file(experimental_source, '
enum Color {
	Red
}

fn main() {
	println(Color.Red)
}
')!
		mut experimental_process := os.new_process(@VEXE)
		experimental_process.set_args(['-v', '-gc', 'none', '-experimental', '-o',
			experimental_output, experimental_source])
		experimental_process.set_environment(environment)
		experimental_process.set_redirect_stdio()
		experimental_process.run()
		experimental_process.wait()
		experimental_build_output := experimental_process.stdout_slurp() +
			experimental_process.stderr_slurp()
		experimental_exit_code := experimental_process.code
		experimental_process.close()
		assert experimental_exit_code == 0, experimental_build_output
		assert !experimental_build_output.contains('Running macOS V3 compiler in process:'), experimental_build_output
		assert os.is_executable(experimental_output)
		run := os.execute(os.quoted_path(experimental_output))
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == 'Red'
	}
}

fn test_macos_v3_manualfree_overrides_vflags_autofree() {
	$if macos {
		root := os.join_path(os.real_path(os.vtmp_dir()), 'macos_v3_manualfree_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root) or { panic(err) }
		defer {
			os.rmdir_all(root) or {}
		}
		source := os.join_path(root, 'main.v')
		output := os.join_path(root, 'main')
		os.write_file(source,
			"\$if autofree {\n\t\$compile_error('autofree remained enabled')\n}\n\nfn main() {}\n")!
		mut environment := os.environ()
		environment['VFLAGS'] = '-autofree'
		environment['VOSARGS'] = ''
		mut process := os.new_process(@VEXE)
		process.set_args(['-manualfree', '-gc', 'none', '-o', output, source])
		process.set_environment(environment)
		process.set_redirect_stdio()
		process.run()
		process.wait()
		compiler_output := process.stdout_slurp() + process.stderr_slurp()
		exit_code := process.code
		process.close()
		assert exit_code == 0, compiler_output
		assert os.is_executable(output)
	}
}

fn test_autofree_delegation_detects_and_forwards_vflags() {
	$if macos {
		root := os.join_path(os.real_path(os.vtmp_dir()), 'v3_autofree_vflags_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root) or { panic(err) }
		defer {
			os.rmdir_all(root) or {}
		}
		source := os.join_path(root, 'main.v')
		output := os.join_path(root, 'main.c')
		os.write_file(source,
			"\$if ownership_vflags_feature ? {\n} \$else {\n\t\$compile_error('VFLAGS define was not forwarded')\n}\n\nfn main() {}\n")!
		mut environment := os.environ()
		environment['VFLAGS'] = '-autofree -d ownership_vflags_feature'
		environment['VOSARGS'] = ''
		mut process := os.new_process(@VEXE)
		process.set_args(['-v', '-gc', 'none', '-o', output, source])
		process.set_environment(environment)
		process.set_redirect_stdio()
		process.run()
		process.wait()
		compiler_output := process.stdout_slurp() + process.stderr_slurp()
		exit_code := process.code
		process.close()
		assert exit_code == 0, compiler_output
		assert compiler_output.contains('Launching v3_ownership:'), compiler_output
		assert !compiler_output.contains('ownership support is not compiled into this v3 executable'), compiler_output

		assert os.is_file(output)
	}
}

fn test_macos_v3_child_environment_forwards_compiler_hashes() {
	$if macos || linux {
		caller_environment := {
			'PATH':                     '/usr/bin'
			'CFLAGS':                   '-I/caller/include -DCALLER_FLAG=1'
			'LDFLAGS':                  '-L/caller/lib -lcaller'
			'VEXE':                     'caller-vexe'
			'VCHILD':                   'caller-vchild'
			'V_MACOS_V3_NO_FALLBACK':   'caller-no-fallback'
			'V_MACOS_V3_FALLBACK_FILE': '/tmp/stale-fallback'
			'V_MACOS_V3_C_ERROR_DIR':   '/tmp/stale-c-error'
			'V_MACOS_V3_RETRY':         '1'
		}
		environment := macos_v3_child_environment(@VEXE, '/tmp/macos_v3_fallback',
			caller_environment)
		assert environment[macos_v3_vhash_env] == @VHASH
		assert environment[macos_v3_vcurrent_hash_env] == @VCURRENTHASH
		assert environment[macos_v3_c_error_dir_env] == '/tmp/macos_v3_fallback.c_error'
		assert macos_v3_retry_env !in environment
		assert environment[macos_v3_embedded_env] == '1'
		assert environment['VEXE'] == os.real_path(@VEXE)
		assert environment['VCHILD'] == 'true'
		assert environment['CFLAGS'] == '-I/caller/include -DCALLER_FLAG=1'
		assert environment['LDFLAGS'] == '-L/caller/lib -lcaller'
		assert environment[macos_v3_caller_vexe_present_env] == '1'
		assert environment[macos_v3_caller_vexe_env] == 'caller-vexe'
		assert environment[macos_v3_caller_vchild_present_env] == '1'
		assert environment[macos_v3_caller_vchild_env] == 'caller-vchild'
		assert environment[macos_v3_caller_no_fallback_present_env] == '1'
		assert environment[macos_v3_caller_no_fallback_env] == 'caller-no-fallback'

		unset_environment := macos_v3_child_environment(@VEXE, '/tmp/macos_v3_fallback', {
			'PATH': '/usr/bin'
		})
		assert unset_environment[macos_v3_caller_vexe_present_env] == '0'
		assert unset_environment[macos_v3_caller_vexe_env] == ''
		assert unset_environment[macos_v3_caller_vchild_present_env] == '0'
		assert unset_environment[macos_v3_caller_vchild_env] == ''
		assert unset_environment[macos_v3_caller_no_fallback_present_env] == '0'
		assert unset_environment[macos_v3_caller_no_fallback_env] == ''
	}
}

fn test_macos_v3_redispatch_preserves_original_caller_environment() {
	dispatch_environment := {
		'PATH':                                  '/usr/bin'
		'VEXE':                                  '/internal/v'
		'VCHILD':                                'true'
		'V_MACOS_V3_FALLBACK_FILE':              '/tmp/old-fallback'
		'V_MACOS_V3_C_ERROR_DIR':                '/tmp/old-c-error'
		'V_MACOS_V3_VHASH':                      'internal-hash'
		'V_MACOS_V3_VCURRENT_HASH':              'internal-current-hash'
		'V_MACOS_V3_EMBEDDED':                   '1'
		'V_MACOS_V3_NO_FALLBACK':                '1'
		'V3_CRUN_BUILD_IDENTITY':                'restart-identity'
		'V3_INTERNAL_RESTART':                   '1'
		macos_v3_caller_vexe_env:                'caller-vexe'
		macos_v3_caller_vexe_present_env:        '1'
		macos_v3_caller_vchild_env:              'caller-vchild'
		macos_v3_caller_vchild_present_env:      '1'
		macos_v3_caller_no_fallback_env:         'caller-no-fallback'
		macos_v3_caller_no_fallback_present_env: '1'
	}
	caller_environment := macos_v3_original_caller_environment(dispatch_environment)
	assert caller_environment['PATH'] == '/usr/bin'
	assert caller_environment['VEXE'] == 'caller-vexe'
	assert caller_environment['VCHILD'] == 'caller-vchild'
	assert caller_environment[macos_v3_no_fallback_env] == 'caller-no-fallback'
	assert macos_v3_fallback_file_env !in caller_environment
	assert macos_v3_c_error_dir_env !in caller_environment
	assert macos_v3_vhash_env !in caller_environment
	assert macos_v3_vcurrent_hash_env !in caller_environment
	assert macos_v3_embedded_env !in caller_environment
	assert 'V3_CRUN_BUILD_IDENTITY' !in caller_environment
	assert 'V3_INTERNAL_RESTART' !in caller_environment
	assert macos_v3_caller_vexe_env !in caller_environment
	assert macos_v3_caller_vchild_env !in caller_environment

	child_environment := macos_v3_child_environment(@VEXE, '/tmp/new-fallback',
		dispatch_environment)
	assert child_environment['VEXE'] == os.real_path(@VEXE)
	assert child_environment['VCHILD'] == 'true'
	assert child_environment[macos_v3_caller_vexe_env] == 'caller-vexe'
	assert child_environment[macos_v3_caller_vexe_present_env] == '1'
	assert child_environment[macos_v3_caller_vchild_env] == 'caller-vchild'
	assert child_environment[macos_v3_caller_vchild_present_env] == '1'
	assert child_environment[macos_v3_caller_no_fallback_env] == 'caller-no-fallback'
	assert child_environment[macos_v3_caller_no_fallback_present_env] == '1'
	// Internal restart state remains available to the next V3 pass.
	assert child_environment['V3_CRUN_BUILD_IDENTITY'] == 'restart-identity'
	assert child_environment['V3_INTERNAL_RESTART'] == '1'

	mut unset_dispatch_environment := dispatch_environment.clone()
	unset_dispatch_environment[macos_v3_caller_vexe_env] = ''
	unset_dispatch_environment[macos_v3_caller_vexe_present_env] = '0'
	unset_dispatch_environment[macos_v3_caller_vchild_env] = ''
	unset_dispatch_environment[macos_v3_caller_vchild_present_env] = '0'
	unset_dispatch_environment[macos_v3_caller_no_fallback_env] = ''
	unset_dispatch_environment[macos_v3_caller_no_fallback_present_env] = '0'
	unset_caller_environment := macos_v3_original_caller_environment(unset_dispatch_environment)
	assert 'VEXE' !in unset_caller_environment
	assert 'VCHILD' !in unset_caller_environment
	assert macos_v3_no_fallback_env !in unset_caller_environment
}

fn test_macos_v3_redispatch_run_observes_original_caller_environment() {
	$if macos || linux {
		root := os.join_path(os.real_path(os.vtmp_dir()), 'v3_redispatch_env_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root) or { panic(err) }
		defer {
			os.rmdir_all(root) or {}
		}
		source := os.join_path(root, 'main.v')
		os.write_file(source,
			"module main\n\nimport os\n\nconst compile_vexe = \$env('VEXE')\nconst compile_vchild = \$env('VCHILD')\nconst compile_no_fallback = \$env('V_MACOS_V3_NO_FALLBACK')\n\nfn main() {\n\truntime_vexe := os.getenv('VEXE')\n\truntime_vchild := os.getenv('VCHILD')\n\truntime_no_fallback := os.getenv('V_MACOS_V3_NO_FALLBACK')\n\tprintln('\${compile_vexe}|\${compile_vchild}|\${runtime_vexe}|\${runtime_vchild}|\${compile_no_fallback}|\${runtime_no_fallback}')\n}\n")!
		mut environment := os.environ()
		environment['CFLAGS'] = ''
		environment['LDFLAGS'] = ''
		environment['VFLAGS'] = ''
		environment['VOSARGS'] = ''
		environment['VEXE'] = os.real_path(@VEXE)
		environment['VCHILD'] = 'true'
		environment[macos_v3_caller_vexe_env] = 'caller-vexe'
		environment[macos_v3_caller_vexe_present_env] = '1'
		environment[macos_v3_caller_vchild_env] = 'caller-vchild'
		environment[macos_v3_caller_vchild_present_env] = '1'
		environment[macos_v3_no_fallback_env] = '1'
		environment[macos_v3_caller_no_fallback_env] = 'caller-no-fallback'
		environment[macos_v3_caller_no_fallback_present_env] = '1'
		environment[macos_v3_embedded_env] = '1'
		environment['V3_INTERNAL_RESTART'] = '1'
		mut process := os.new_process(@VEXE)
		process.set_args(['-new-compiler', '-gc', 'none', 'run', source])
		process.set_environment(environment)
		process.set_redirect_stdio()
		process.run()
		process.wait()
		output := process.stdout_slurp() + process.stderr_slurp()
		exit_code := process.code
		process.close()
		assert exit_code == 0, output
		assert output.contains('caller-vexe|caller-vchild|caller-vexe|caller-vchild|caller-no-fallback|caller-no-fallback'), output
	}
}

fn test_macos_v3_embedded_driver_reuses_module_cache() {
	$if macos || linux {
		root := os.join_path(os.real_path(os.vtmp_dir()), 'macos_v3_embedded_cache_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(os.join_path(root, 'wrapper')) or { panic(err) }
		defer {
			os.rmdir_all(root) or {}
		}
		main_file := os.join_path(root, 'main.v')
		os.write_file(os.join_path(root, 'wrapper', 'wrapper.v'), 'module wrapper

pub fn value() int {
	return 42
}
')!
		os.write_file(main_file, 'module main

import wrapper

fn main() {
	println(wrapper.value())
}
')!
		mut environment := os.environ()
		environment['CFLAGS'] = ''
		environment['LDFLAGS'] = ''
		environment['VFLAGS'] = ''
		environment['VOSARGS'] = ''
		environment['V3CACHE'] = os.join_path(root, 'cache')
		environment['V3_CACHE_TRACE'] = '1'
		environment.delete('V_MACOS_V3_NO_FALLBACK')
		mut outputs := []string{}
		for name in ['first', 'second'] {
			output := os.join_path(root, name)
			mut process := os.new_process(@VEXE)
			process.set_args(['-gc', 'none', '-o', output, main_file])
			process.set_environment(environment)
			process.set_redirect_stdio()
			process.run()
			process.wait()
			compiler_output := process.stdout_slurp() + process.stderr_slurp()
			exit_code := process.code
			process.close()
			assert exit_code == 0, compiler_output
			assert os.is_executable(output)
			outputs << compiler_output
		}
		assert outputs[0].contains('V3 module cache miss:'), outputs[0]
		assert !outputs[1].contains('V3 module cache miss:'), outputs[1]
		cache_headers := os.walk_ext(environment['V3CACHE'], '.vh')
		assert cache_headers.any(os.base(it).starts_with('wrapper_')), cache_headers.str()
		run := os.execute(os.quoted_path(os.join_path(root, 'second')))
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == '42'

		// Keep the imported wrapper warm while changing only the program to one that
		// V3 cannot compile yet. Its cached `.vh` must contribute the original wrapper
		// source digest to the fallback manifest, or the stable retry will incorrectly
		// report that the source inputs changed (PR #28131 review).
		os.write_file(main_file, 'module main

import wrapper

struct Box[T] {
	value T
}

fn (b Box[T]) convert[U](value U) U {
	return value
}

fn main() {
	assert wrapper.value() == 42
	result := Box[int]{42}.convert[i64](43)
	assert result == 43
	}
')!
		environment['V_C_ERROR_BUG_REPORT_DISABLED'] = '1'
		fallback_output := os.join_path(root, 'fallback')
		mut fallback_process := os.new_process(@VEXE)
		fallback_process.set_args(['-gc', 'none', '-o', fallback_output, main_file])
		fallback_process.set_environment(environment)
		fallback_process.set_redirect_stdio()
		fallback_process.run()
		fallback_process.wait()
		fallback_text := fallback_process.stdout_slurp() + fallback_process.stderr_slurp()
		fallback_exit_code := fallback_process.code
		fallback_process.close()
		assert fallback_exit_code == 0, fallback_text
		assert fallback_text.contains('V3 could not build this program'), fallback_text
		assert !fallback_text.contains('source inputs changed'), fallback_text
		assert os.is_executable(fallback_output)
	}
}

fn test_macos_v3_cached_generic_keeps_main_type_homonym() {
	$if macos {
		root := os.join_path(os.real_path(os.vtmp_dir()),
			'macos_v3_generic_main_type_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(os.join_path(root, 'mid')) or { panic(err) }
		defer {
			os.rmdir_all(root) or {}
		}
		main_file := os.join_path(root, 'main.v')
		os.write_file(os.join_path(root, 'mid', 'mid.v'), 'module mid

pub struct Context {
pub:
	name string
}

pub struct Middleware[T] {
pub:
	handler fn (mut T) string
}

pub fn make[T]() Middleware[T] {
	return Middleware[T]{
		handler: fn [T](mut ctx T) string {
			return ctx.Context.name
		}
	}
}
')!
		os.write_file(main_file, 'module main

import mid

struct Context {
	mid.Context
}

fn main() {
	mut context := Context{
		Context: mid.Context{
			name: "main context"
		}
	}
	middleware := mid.make[Context]()
	println(middleware.handler(mut context))
}
')!
		mut environment := os.environ()
		environment['CFLAGS'] = ''
		environment['LDFLAGS'] = ''
		environment['VFLAGS'] = ''
		environment['VOSARGS'] = ''
		environment['V3CACHE'] = os.join_path(root, 'cache')
		environment['V_MACOS_V3_NO_FALLBACK'] = '1'
		for name in ['first', 'second'] {
			output := os.join_path(root, name)
			mut process := os.new_process(@VEXE)
			process.set_args(['-gc', 'none', '-o', output, main_file])
			process.set_environment(environment)
			process.set_redirect_stdio()
			process.run()
			process.wait()
			compiler_output := process.stdout_slurp() + process.stderr_slurp()
			exit_code := process.code
			process.close()
			assert exit_code == 0, compiler_output
			run := os.execute(os.quoted_path(output))
			assert run.exit_code == 0, run.output
			assert run.output.trim_space() == 'main context'
		}
	}
}

fn test_macos_v3_reads_c_error_fallback_report() {
	$if macos {
		root := os.join_path(os.vtmp_dir(), 'macos_v3_c_error_report_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root) or { panic(err) }
		defer {
			os.rmdir_all(root) or {}
		}
		os.write_file(os.join_path(root, macos_v3_c_error_source_name_file), 'src.c')!
		os.write_file(os.join_path(root, macos_v3_c_error_compiler_file), 'clang')!
		os.write_file(os.join_path(root, macos_v3_c_error_output_file),
			'src.c:2:1: error: generated failure')!
		parsed_source := os.join_path(root, 'parsed.v')
		parsed_digest := 'a'.repeat(sha256.size * 2)
		os.write_file(os.join_path(root, macos_v3_c_error_v_sources_file), parsed_source)!
		os.write_file(os.join_path(root, macos_v3_c_error_v_source_digests_file), parsed_digest)!
		os.write_file(os.join_path(root, 'src.c'), 'int main(void) { return missing; }\n')!
		report := read_macos_v3_c_error_report(root) or {
			assert false
			return
		}
		assert report.ccompiler == 'clang'
		assert report.c_output.contains('generated failure')
		assert report.c_file == os.join_path(root, 'src.c')
		assert report.v_sources[parsed_source] == parsed_digest
		assert report.report_dir == root
	}
}

fn test_macos_v3_discards_fallback_report_when_native_input_changes() {
	$if macos || linux {
		real_cc := os.find_abs_path_of_executable('cc') or { return }
		root := os.join_path(os.real_path(os.vtmp_dir()),
			'macos_v3_native_input_retry_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root) or { panic(err) }
		defer {
			os.rmdir_all(root) or {}
		}
		header := os.join_path(root, 'project.h')
		source := os.join_path(root, 'main.v')
		compiler := os.join_path(root, 'cc-wrapper')
		output := os.join_path(root, 'main')
		os.write_file(header, 'static int project_value(void) { return 41; }\n')!
		os.write_file(source, 'module main

#include "@DIR/project.h"

fn C.project_value() int

fn main() {
	assert C.project_value() == 42
}
')!
		// Dependency discovery preprocesses native inputs before the snapshot. Delegate
		// those probes unchanged, then rewrite the header only when V3 starts its real C
		// compilation. The stable retry succeeds with the new bytes and must not report
		// the old V3 failure as a V3-only compiler bug.
		os.write_file(compiler, '#!/bin/sh
for arg in "\$@"; do
	if [ "\$arg" = "-E" ] || [ "\$arg" = "-dM" ]; then
		exec "\$REAL_CC" "\$@"
	fi
done
if [ "\$V_MACOS_V3_EMBEDDED" = "1" ]; then
	printf "%s\\n" "static int project_value(void) { return 42; }" > "\$NATIVE_HEADER"
	exit 1
fi
exec "\$REAL_CC" "\$@"
')!
		os.chmod(compiler, 0o700)!
		mut environment := os.environ()
		environment['CFLAGS'] = ''
		environment['LDFLAGS'] = ''
		environment['VFLAGS'] = ''
		environment['VOSARGS'] = ''
		environment['REAL_CC'] = real_cc
		environment['NATIVE_HEADER'] = header
		environment['V_C_ERROR_BUG_REPORT_DISABLED'] = '1'
		environment.delete('V_MACOS_V3_NO_FALLBACK')
		mut process := os.new_process(@VEXE)
		process.set_args(['-gc', 'none', '-nocache', '-cc', compiler, '-o', output, source])
		process.set_environment(environment)
		process.set_redirect_stdio()
		process.run()
		process.wait()
		compiler_output := process.stdout_slurp() + process.stderr_slurp()
		exit_code := process.code
		process.close()
		assert exit_code == 0, compiler_output
		// The unverified V3 fallback report is dropped silently, without any user-facing note.
		assert !compiler_output.contains('source inputs changed'), compiler_output
		assert !compiler_output.contains('V3 could not build this program'), compiler_output
		assert os.is_executable(output)
		run := os.execute(os.quoted_path(output))
		assert run.exit_code == 0, run.output
	}
}

fn test_macos_v3_compiler_failures_fall_back_to_old_compiler() {
	$if macos {
		root := os.join_path(os.real_path(os.vtmp_dir()), 'macos_v3_c_error_retry_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root) or { panic(err) }
		defer {
			os.rmdir_all(root) or {}
		}
		target := os.join_path(root, 'target.v')
		output := os.join_path(root, 'target')
		asm_body := $if arm64 {
			'asm arm64 {
		mov output, 1
		; +r (output)
	}'
		} $else {
			'asm amd64 {
		mov eax, 1
		mov output, eax
		; =r (output)
		; ; eax
	}'
		}
		os.write_file(target, 'fn main() {
	mut output := 0
	${asm_body}
	assert output == 1
}
')!
		mut environment := os.environ()
		// This test exercises a real V3->V1 fallback, so clear the job-level no-fallback
		// guard that CI sets for eligible V3 builds — otherwise V3 exits at the failure
		// instead of retrying and the fallback never happens (PR #28131 review).
		environment.delete('V_MACOS_V3_NO_FALLBACK')
		environment['GITHUB_ACTIONS'] = 'true'
		environment['V_C_ERROR_BUG_REPORT_DISABLED'] = '1'
		environment['VFLAGS'] = ''
		environment['VOSARGS'] = ''
		mut process := os.new_process(@VEXE)
		process.set_args(['-v', '-gc', 'none', '-o', output, target])
		process.set_environment(environment)
		process.set_redirect_stdio()
		process.run()
		process.wait()
		compiler_output := process.stdout_slurp() + process.stderr_slurp()
		exit_code := process.code
		process.close()
		assert exit_code == 0, compiler_output
		assert compiler_output.contains('Running macOS V3 compiler in process:'), compiler_output
		assert !compiler_output.contains('Launching macOS V3 compiler:'), compiler_output
		assert compiler_output.contains('compatibility compiler for inline assembly'), compiler_output
		// An inline-assembly fallback is a known limitation, not a bug (so no report is
		// filed), but the standard fallback notice must still be printed once the stable
		// build succeeds, matching doc/docs.md (PR #28131 review).
		assert compiler_output.contains('V3 could not build this program'), compiler_output
		assert os.is_executable(output)
		run := os.execute(os.quoted_path(output))
		assert run.exit_code == 0

		os.rm(output)!
		failing_target := os.join_path(root, 'failing_target.v')
		failing_output := os.join_path(root, 'failing_target')
		os.write_file(failing_target, '#flag -lmacos_v3_missing_library_${os.getpid()}

fn main() {}
')!
		mut failing_process := os.new_process(@VEXE)
		failing_process.set_args(['-gc', 'none', '-o', failing_output, failing_target])
		failing_process.set_environment(environment)
		failing_process.set_redirect_stdio()
		failing_process.run()
		failing_compiler_pid := failing_process.pid
		failing_process.wait()
		failing_output_text := failing_process.stdout_slurp() + failing_process.stderr_slurp()
		failing_exit_code := failing_process.code
		failing_process.close()
		assert failing_exit_code != 0, failing_output_text
		assert !failing_output_text.contains('V3 C compilation failed; retrying with `-old-compiler`.')
		assert failing_output_text.contains('macos_v3_missing_library_')
		failing_report_dir := os.join_path(os.vtmp_dir(),
			'macos_v3_fallback_${failing_compiler_pid}.c_error')
		assert !os.exists(failing_report_dir), 'failed compatibility build left staged report directory: ${failing_report_dir}'
	}
}

// The compiler-error fallback captures the input V source into content in the trusted
// process, uploading the full file when it fits the byte budget (so the report is reproducible)
// and a bounded snapshot otherwise. A directory build or a non-V / missing input uploads nothing
// (a metadata-only report). Runs wherever the dispatcher is embedded (macOS and Linux) without
// triggering a real V3 failure (PR #28131 review).
fn test_macos_v3_compiler_error_content_extraction() {
	$if macos || linux {
		root := os.join_path(os.real_path(os.vtmp_dir()), 'macos_v3_ce_content_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root) or { panic(err) }
		defer {
			os.rmdir_all(root) or {}
		}
		// A single V source file is uploaded in full so the report is reproducible.
		source := os.join_path(root, 'prog.v')
		mut lines := []string{}
		for i in 0 .. 200 {
			lines << 'fn f${i}() { println(${i}) }'
		}
		whole := lines.join('\n')
		os.write_file(source, whole)!
		compiler_error := macos_v3_compiler_error_message('source parsing')
		snapshot := macos_v3_compiler_error_input_snapshot(source)
		v_file, v_source, v_source_focus, v_source_truncated := snapshot.current_report_source()
		assert v_file == 'prog.v'
		assert compiler_error.contains('during source parsing')
		// The full file is captured (it fits the byte budget), so the report reproduces. An
		// internal error has no mapped failing line, so its focus is 0 (head+tail if bounded).
		assert v_source == whole
		assert v_source_focus == 0
		assert !v_source_truncated
		// A large input keeps only a payload-sized snapshot in the at-exit retry state, so a
		// successful V3 build does not retain an unbounded second copy until process exit.
		large_source := ('fn large() { println("' + 'x'.repeat(1000) + '") }\n').repeat(100)
		os.write_file(source, large_source)!
		large_snapshot := macos_v3_compiler_error_input_snapshot(source)
		_, large_report_source, _, large_source_truncated := large_snapshot.current_report_source()
		assert large_report_source.len <= 64 * 1024
		assert large_report_source.len < large_source.len
		assert large_source_truncated
		// Rewriting the file after the pre-V3 snapshot suppresses source completely: the
		// fallback must not upload bytes that V3 never parsed.
		os.write_file(source, whole + '\nfn changed_after_snapshot() {}')!
		changed_file, changed_source, _, _ := snapshot.current_report_source()
		assert changed_file == ''
		assert changed_source == ''
		// A directory build, a non-V file, or a missing input yields no source, so the
		// report stays metadata-only.
		note := os.join_path(root, 'note.txt')
		os.write_file(note, 'not v source')!
		for empty in [root, note, os.join_path(root, 'missing.v'), ''] {
			empty_snapshot := macos_v3_compiler_error_input_snapshot(empty)
			ef, es, _, _ := empty_snapshot.current_report_source()
			assert ef == '', empty
			assert es == '', empty
		}
		legacy_reason, legacy_stage := macos_v3_fallback_reason_and_stage('compiler_error')
		assert legacy_reason == macos_v3_compiler_error_fallback
		assert legacy_stage == ''
		reason, stage := macos_v3_fallback_reason_and_stage('compiler_error\nsemantic checking')
		assert reason == macos_v3_compiler_error_fallback
		assert stage == 'semantic checking'
	}
}

// End-to-end: a construct V3 cannot build yet (a generic method on a generic
// struct) must fall back to the stable compiler, print the user-facing notice, and
// still produce a working program. The bug endpoint is pointed at an unroutable
// address so the report path runs without ever touching the network.
fn test_macos_v3_compiler_error_falls_back_and_notifies() {
	$if macos {
		root := os.join_path(os.real_path(os.vtmp_dir()), 'macos_v3_compiler_error_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root) or { panic(err) }
		defer {
			os.rmdir_all(root) or {}
		}
		source := os.join_path(root, 'generic_method.v')
		output := os.join_path(root, 'generic_method')
		os.write_file(source, 'struct Opt[T] {
	val  T
	some bool
}

fn some[T](val T) Opt[T] {
	return Opt[T]{
		val:  val
		some: true
	}
}

fn (f Opt[T]) map[U](op fn (T) U) Opt[U] {
	if f.some {
		return some[U](op(f.val))
	}
	return Opt[U]{}
}

fn main() {
	result := some("hello").map(|s| s.len)
	assert result.some && result.val == 5
	println("generic ok")
}
')!
		mut environment := os.environ()
		// This test exercises a real V3->V1 fallback, so clear the job-level no-fallback
		// guard that CI sets for eligible V3 builds — otherwise V3 exits at the failure
		// instead of retrying and the fallback never happens (PR #28131 review).
		environment.delete('V_MACOS_V3_NO_FALLBACK')
		environment['V_C_ERROR_BUG_REPORT_DISABLED'] = ''
		// Unroutable endpoint: the submission attempt fails fast, so the test never
		// contacts the real bug server while still exercising the report path.
		environment['V_C_ERROR_BUG_REPORT_URL'] = 'http://127.0.0.1:1/bug-report'
		environment['VFLAGS'] = ''
		environment['VOSARGS'] = ''
		mut process := os.new_process(@VEXE)
		process.set_args(['-gc', 'none', '-o', output, source])
		process.set_environment(environment)
		process.set_redirect_stdio()
		process.run()
		process.wait()
		compiler_output := process.stdout_slurp() + process.stderr_slurp()
		exit_code := process.code
		process.close()
		assert exit_code == 0, compiler_output
		assert compiler_output.contains('V3 could not build this program'), compiler_output
		assert os.is_executable(output)
		run := os.execute(os.quoted_path(output))
		assert run.exit_code == 0, run.output
		assert run.output.contains('generic ok'), run.output
	}
}

// End-to-end (directory build): the same V3-incompatible program built as a
// directory (`v <dir>`), so `input_path` is a directory and no source reproducer
// can be staged. The fallback must still print the notice instead of going silent
// (PR #28131 review feedback).
fn test_macos_v3_compiler_error_directory_build_notifies() {
	$if macos {
		root := os.join_path(os.real_path(os.vtmp_dir()),
			'macos_v3_compiler_error_dir_${os.getpid()}')
		os.rmdir_all(root) or {}
		app_dir := os.join_path(root, 'app')
		os.mkdir_all(app_dir) or { panic(err) }
		defer {
			os.rmdir_all(root) or {}
		}
		os.write_file(os.join_path(app_dir, 'main.v'), 'struct Opt[T] {
	val  T
	some bool
}

fn some[T](val T) Opt[T] {
	return Opt[T]{
		val:  val
		some: true
	}
}

fn (f Opt[T]) map[U](op fn (T) U) Opt[U] {
	if f.some {
		return some[U](op(f.val))
	}
	return Opt[U]{}
}

fn main() {
	result := some("hello").map(|s| s.len)
	assert result.some && result.val == 5
	println("generic dir ok")
}
')!
		output := os.join_path(root, 'app_bin')
		mut environment := os.environ()
		// This test exercises a real V3->V1 fallback, so clear the job-level no-fallback
		// guard that CI sets for eligible V3 builds — otherwise V3 exits at the failure
		// instead of retrying and the fallback never happens (PR #28131 review).
		environment.delete('V_MACOS_V3_NO_FALLBACK')
		environment['V_C_ERROR_BUG_REPORT_DISABLED'] = ''
		environment['V_C_ERROR_BUG_REPORT_URL'] = 'http://127.0.0.1:1/bug-report'
		environment['VFLAGS'] = ''
		environment['VOSARGS'] = ''
		mut process := os.new_process(@VEXE)
		process.set_args(['-gc', 'none', '-o', output, app_dir])
		process.set_environment(environment)
		process.set_redirect_stdio()
		process.run()
		process.wait()
		compiler_output := process.stdout_slurp() + process.stderr_slurp()
		exit_code := process.code
		process.close()
		assert exit_code == 0, compiler_output
		// The notice must appear even though no single source file could be staged.
		assert compiler_output.contains('V3 could not build this program'), compiler_output
		// Without a complete parser-owned input manifest, the fallback deliberately
		// suppresses report submission instead of uploading unverified inputs.
		assert !compiler_output.contains('V3 compiler bug report'), compiler_output
		assert os.is_executable(output)
		run := os.execute(os.quoted_path(output))
		assert run.exit_code == 0, run.output
		assert run.output.contains('generic dir ok'), run.output
	}
}

// End-to-end (PR #28131 review): a V3 internal-error fallback for `v -o - source.v` must
// keep stdout as pure generated C. V1 has already written the C to stdout, so the report
// banner, its context, and the fallback notice all go to stderr — never stdout — or the
// documented `-o -` output would be invalid C for exactly the programs that needed the
// fallback.
fn test_macos_v3_fallback_report_stays_off_generated_c_stdout() {
	$if macos {
		root := os.join_path(os.real_path(os.vtmp_dir()), 'macos_v3_stdout_c_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root) or { panic(err) }
		defer {
			os.rmdir_all(root) or {}
		}
		source := os.join_path(root, 'gen.v')
		os.write_file(source, 'struct Opt[T] {
	val  T
	some bool
}

fn some[T](val T) Opt[T] {
	return Opt[T]{
		val:  val
		some: true
	}
}

fn (f Opt[T]) map[U](op fn (T) U) Opt[U] {
	if f.some {
		return some[U](op(f.val))
	}
	return Opt[U]{}
}

fn main() {
	result := some("hello").map(|s| s.len)
	assert result.some && result.val == 5
}
')!
		mut environment := os.environ()
		// Exercise a real fallback; clear the job-level no-fallback guard CI may set.
		environment.delete('V_MACOS_V3_NO_FALLBACK')
		environment['V_C_ERROR_BUG_REPORT_DISABLED'] = ''
		// Keep an unroutable endpoint configured in case this fixture gains a complete
		// parser-owned input manifest in the future.
		environment['V_C_ERROR_BUG_REPORT_URL'] = 'http://127.0.0.1:1/bug-report'
		environment['VFLAGS'] = ''
		environment['VOSARGS'] = ''
		stdout_path := os.join_path(root, 'stdout.c')
		stderr_path := os.join_path(root, 'stderr.txt')
		environment['V_TEST_STDOUT'] = stdout_path
		environment['V_TEST_STDERR'] = stderr_path
		mut process := os.new_process('/bin/sh')
		process.set_args([
			'-c',
			'exec "\$@" > "\$V_TEST_STDOUT" 2> "\$V_TEST_STDERR"',
			'sh',
			@VEXE,
			'-gc',
			'none',
			'-o',
			'-',
			source,
		])
		process.set_environment(environment)
		process.run()
		process.wait()
		exit_code := process.code
		process.close()
		stdout := os.read_file(stdout_path) or { '' }
		stderr := os.read_file(stderr_path) or { '' }
		assert exit_code == 0, stdout + stderr
		// The fallback happened and its diagnostics went to stderr.
		assert stderr.contains('V3 could not build this program'), stderr
		// stdout is the generated C only — valid C, with no report text of any kind.
		assert stdout.contains('typedef') || stdout.contains('#define'), 'stdout is not generated C'
		for leaked in ['could not build this program', 'compiler bug report',
			'bug report was not sent', 'so this can be fixed', 'opt out of these automatic'] {
			assert !stdout.contains(leaked), 'report text leaked onto `-o -` stdout: `${leaked}`'
		}
	}
}

fn test_macos_v3_inline_asm_trace_stays_off_generated_c_stdout() {
	$if macos || linux {
		root := os.join_path(os.real_path(os.vtmp_dir()),
			'macos_v3_inline_asm_stdout_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root) or { panic(err) }
		defer {
			os.rmdir_all(root) or {}
		}
		source := os.join_path(root, 'gen.v')
		asm_body := $if arm64 {
			'asm arm64 {
		mov output, 1
		; +r (output)
	}'
		} $else {
			'asm amd64 {
		mov eax, 1
		mov output, eax
		; =r (output)
		; ; eax
	}'
		}
		os.write_file(source, 'fn main() {
	mut output := 0
	${asm_body}
	assert output == 1
}
')!
		mut environment := os.environ()
		environment.delete('V_MACOS_V3_NO_FALLBACK')
		environment['V3_CACHE_TRACE'] = '1'
		environment['V_C_ERROR_BUG_REPORT_DISABLED'] = '1'
		environment['VFLAGS'] = ''
		environment['VOSARGS'] = ''
		stdout_path := os.join_path(root, 'stdout.c')
		stderr_path := os.join_path(root, 'stderr.txt')
		environment['V_TEST_STDOUT'] = stdout_path
		environment['V_TEST_STDERR'] = stderr_path
		mut process := os.new_process('/bin/sh')
		process.set_args([
			'-c',
			'exec "\$@" > "\$V_TEST_STDOUT" 2> "\$V_TEST_STDERR"',
			'sh',
			@VEXE,
			'-gc',
			'none',
			'-o',
			'-',
			source,
		])
		process.set_environment(environment)
		process.run()
		process.wait()
		exit_code := process.code
		process.close()
		stdout := os.read_file(stdout_path) or { '' }
		stderr := os.read_file(stderr_path) or { '' }
		assert exit_code == 0, stdout + stderr
		assert stderr.contains('compatibility compiler for inline assembly'), stderr
		assert stderr.contains('V3 could not build this program'), stderr
		assert stdout.contains('typedef') || stdout.contains('#define'), 'stdout is not generated C'
		for leaked in ['compatibility compiler for inline assembly',
			'V3 could not build this program'] {
			assert !stdout.contains(leaked), 'fallback trace leaked onto `-o -` stdout: `${leaked}`'
		}
	}
}

fn clear_macos_v3_report_env() {
	for suffix in ['PRESENT', 'KIND', 'CCOMPILER', 'COUTPUT', 'TAG', 'VFILE', 'VSOURCE',
		'INPUT_DIGESTS', 'INPUT_DIGESTS_COMPLETE'] {
		os.unsetenv('V_MACOS_V3_REPORT_${suffix}')
	}
}

// Unit-level content handoff (PR #28131 review): the retry consumes only the bounded
// content the owning process forwarded through V_MACOS_V3_REPORT_*, and the returned
// report carries no directory path — there is no c_file to read or report_dir to delete.
// A caller-supplied V_MACOS_V3_C_ERROR_DIR is not read here at all, so it cannot make the
// retry open or remove anything. Authentication is therefore unnecessary, which is what
// makes the handoff robust against an exec wrapper that can predict the pid and control
// VTMP (a path handoff cannot be authenticated across such an execvp).
fn test_take_macos_v3_report_content_carries_no_path() {
	$if macos || linux {
		clear_macos_v3_report_env()
		// A directory env var alone yields no report: only V_MACOS_V3_REPORT_* content is
		// read, and it is absent here.
		os.setenv(macos_v3_c_error_dir_env, '/some/victim/dir', true)
		if _ := take_macos_v3_report_content() {
			assert false, 'no V_MACOS_V3_REPORT_* content was set, so no report may be returned'
		}
		os.unsetenv(macos_v3_c_error_dir_env)
		// A forwarded content report round-trips as content only.
		compiler_error := macos_v3_compiler_error_message('type specialization')
		export_macos_v3_report_content(macos_v3_compiler_error_fallback, 'v3', compiler_error, '',
			map[string]string{}, false)
		report := take_macos_v3_report_content() or {
			assert false, 'the forwarded content report must be returned'
			return
		}
		assert report.kind == macos_v3_compiler_error_fallback
		assert report.ccompiler == 'v3'
		assert report.c_output == compiler_error
		// The variables are cleared, so a second take finds nothing.
		if _ := take_macos_v3_report_content() {
			assert false, 'the content variables must be cleared after a take'
		}
	}
}

// An early V3 parser failure can request the stable compiler before V3 has staged its
// complete input-digest manifest. A successful stable retry must still print the normal
// compatibility-fallback notice, while submitting no unverified V3 bug report.
fn test_macos_v3_unavailable_input_manifest_preserves_fallback_notice() {
	$if macos || linux {
		root := os.join_path(os.real_path(os.vtmp_dir()),
			'macos_v3_unavailable_manifest_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root) or { panic(err) }
		defer {
			os.rmdir_all(root) or {}
		}
		source := os.join_path(root, 'main.v')
		output := os.join_path(root, 'main')
		os.write_file(source, 'fn main() { println(42) }\n')!

		clear_macos_v3_report_env()
		builder.export_external_v3_report_to_env(builder.ExternalCErrorBugReport{
			kind:                   macos_v3_compiler_error_fallback
			ccompiler:              'v3'
			c_output:               macos_v3_compiler_error_message('source parsing')
			source_inline:          true
			input_digests_complete: false
			tag:                    'V3'
		})
		mut environment := os.environ()
		clear_macos_v3_report_env()
		environment[macos_v3_retry_env] = '1'
		environment['V_C_ERROR_BUG_REPORT_DISABLED'] = '1'
		environment['VFLAGS'] = ''
		environment['VOSARGS'] = ''
		mut process := os.new_process(@VEXE)
		process.set_args(['-gc', 'none', '-o', output, source])
		process.set_environment(environment)
		process.set_redirect_stdio()
		process.run()
		process.wait()
		compiler_output := process.stdout_slurp() + process.stderr_slurp()
		exit_code := process.code
		process.close()
		assert exit_code == 0, compiler_output
		assert compiler_output.contains('V3 could not build this program, so V used the stable compiler instead.'), compiler_output

		assert !compiler_output.contains('source inputs changed'), compiler_output
		assert os.is_executable(output)
	}
}

// End-to-end (PR #28131 review): the retry no longer reads any directory named by the
// environment. A caller-supplied V_MACOS_V3_C_ERROR_DIR — via an explicit `-old-compiler`
// or an inherited `V_MACOS_V3_RETRY=1`, even holding a perfectly well-formed report — is
// ignored, so a successful build neither uploads a file from nor deletes that directory.
fn test_macos_v3_c_error_report_rejects_unowned_directory() {
	$if macos || linux {
		root := os.join_path(os.real_path(os.vtmp_dir()), 'macos_v3_unowned_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root) or { panic(err) }
		defer {
			os.rmdir_all(root) or {}
		}
		source := os.join_path(root, 'main.v')
		os.write_file(source, 'fn main() { println(21 * 2) }\n')!
		// A directory the dispatcher never staged, holding a well-formed compiler-error
		// report (so only provenance, not a parse failure, can reject it) plus an
		// unrelated file that must survive.
		victim := os.join_path(root, 'victim')
		os.mkdir_all(victim) or { panic(err) }
		secret := os.join_path(victim, 'secret.txt')
		os.write_file(secret, 'top secret')!
		os.cp(source, os.join_path(victim, 'main.v'))!
		os.write_file(os.join_path(victim, 'compiler'), 'v3')!
		os.write_file(os.join_path(victim, 'output'),
			'error: the experimental V3 compiler hit an internal compiler error building this program')!
		os.write_file(os.join_path(victim, 'kind'), 'compiler_error')!
		os.write_file(os.join_path(victim, 'source_name'), 'main.v')!
		// Both consuming entry points must reject the caller-named directory.
		entry_points := [
			['-old-compiler', '-gc', 'none'], // the `-old-compiler` path
			['-gc', 'none'], // reached with an inherited V_MACOS_V3_RETRY=1
		]
		for i, leading in entry_points {
			output := os.join_path(root, 'main_bin_${i}')
			os.rm(output) or {}
			mut environment := os.environ()
			environment['V_MACOS_V3_C_ERROR_DIR'] = victim
			environment['V_C_ERROR_BUG_REPORT_DISABLED'] = ''
			environment['V_C_ERROR_BUG_REPORT_URL'] = 'http://127.0.0.1:1/bug-report'
			environment['VFLAGS'] = ''
			environment['VOSARGS'] = ''
			if i == 1 {
				environment['V_MACOS_V3_RETRY'] = '1'
			} else {
				environment.delete('V_MACOS_V3_RETRY')
			}
			mut args := leading.clone()
			args << ['-o', output, source]
			mut process := os.new_process(@VEXE)
			process.set_args(args)
			process.set_environment(environment)
			process.set_redirect_stdio()
			process.run()
			process.wait()
			out := process.stdout_slurp() + process.stderr_slurp()
			exit_code := process.code
			process.close()
			// The build succeeds on the established compiler, but the injected report is
			// not consumed: no fallback notice, no bug report submission attempt.
			assert exit_code == 0, out
			assert !out.contains('could not build this program'), out
			assert !out.contains('bug report'), out
			// The caller-named directory and its unrelated file are left untouched.
			assert os.is_dir(victim), out
			assert os.is_file(secret), out
			assert os.is_file(os.join_path(victim, 'main.v')), out
		}
	}
}

fn test_macos_v3_test_command_uses_v3() {
	$if macos {
		root := os.join_path(os.real_path(os.vtmp_dir()), 'macos_v3_test_command_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root) or { panic(err) }
		defer {
			os.rmdir_all(root) or {}
		}
		test_file := os.join_path(root, 'sample_test.v')
		os.write_file(test_file, 'fn test_v3_default() {\n\tassert 2 + 2 == 4\n}\n')!
		mut environment := os.environ()
		environment['CFLAGS'] = ''
		environment['LDFLAGS'] = ''
		environment['VFLAGS'] = ''
		environment['VOSARGS'] = ''
		mut process := os.new_process(@VEXE)
		process.set_args(['-v', '-gc', 'none', 'test', test_file])
		process.set_environment(environment)
		process.set_redirect_stdio()
		process.run()
		process.wait()
		compiler_output := process.stdout_slurp() + process.stderr_slurp()
		exit_code := process.code
		process.close()
		assert exit_code == 0, compiler_output
		assert compiler_output.contains('Running macOS V3 compiler in process:'), compiler_output
	}
}

fn test_macos_v3_directory_c_output_differs_from_old_compiler() {
	$if macos {
		root := os.join_path(os.real_path(os.vtmp_dir()), 'macos_v3_directory_${os.getpid()}')
		source_dir := os.join_path(root, 'app')
		v3_output := os.join_path(root, 'new.c')
		old_output := os.join_path(root, 'old.c')
		os.rmdir_all(root) or {}
		os.mkdir_all(source_dir) or { panic(err) }
		defer {
			os.rmdir_all(root) or {}
		}
		os.write_file(os.join_path(source_dir, 'main.v'), 'fn main() {\n\tprintln("v3")\n}\n')!
		mut environment := os.environ()
		environment['CFLAGS'] = ''
		environment['LDFLAGS'] = ''
		environment['VFLAGS'] = ''
		environment['VOSARGS'] = ''
		mut v3_process := os.new_process(@VEXE)
		v3_process.set_args(['-v', '-gc', 'none', '-o', v3_output, source_dir])
		v3_process.set_environment(environment)
		v3_process.set_redirect_stdio()
		v3_process.run()
		v3_process.wait()
		v3_build_output := v3_process.stdout_slurp() + v3_process.stderr_slurp()
		v3_exit_code := v3_process.code
		v3_process.close()
		assert v3_exit_code == 0, v3_build_output
		assert v3_build_output.contains('Running macOS V3 compiler in process:'), v3_build_output
		mut old_process := os.new_process(@VEXE)
		old_process.set_args(['-o', old_output, '-old-compiler', source_dir])
		old_process.set_environment(environment)
		old_process.set_redirect_stdio()
		old_process.run()
		old_process.wait()
		old_build_output := old_process.stdout_slurp() + old_process.stderr_slurp()
		old_exit_code := old_process.code
		old_process.close()
		assert old_exit_code == 0, old_build_output
		assert os.read_file(v3_output)! != os.read_file(old_output)!
	}
}

fn test_macos_v3_directory_default_output_is_source_adjacent() {
	$if macos || linux {
		root := os.join_path(os.real_path(os.vtmp_dir()),
			'macos_v3_directory_output_${os.getpid()}')
		source_dir := os.join_path(root, 'app')
		caller_dir := os.join_path(root, 'caller')
		expected_output := os.join_path(source_dir, 'app')
		wrong_output := os.join_path(caller_dir, 'app')
		os.rmdir_all(root) or {}
		os.mkdir_all(source_dir) or { panic(err) }
		os.mkdir_all(caller_dir) or { panic(err) }
		defer {
			os.rmdir_all(root) or {}
		}
		os.write_file(os.join_path(source_dir, 'main.v'), 'fn main() {}\n')!
		mut environment := os.environ()
		environment['CFLAGS'] = ''
		environment['LDFLAGS'] = ''
		environment['VFLAGS'] = ''
		environment['VOSARGS'] = ''
		mut process := os.new_process(@VEXE)
		process.set_args(['-v', '-gc', 'none', source_dir])
		process.set_environment(environment)
		process.set_work_folder(caller_dir)
		process.set_redirect_stdio()
		process.run()
		process.wait()
		compiler_output := process.stdout_slurp() + process.stderr_slurp()
		exit_code := process.code
		process.close()
		assert exit_code == 0, compiler_output
		assert compiler_output.contains('Running macOS V3 compiler in process:'), compiler_output
		assert os.is_executable(expected_output)
		assert !os.exists(wrong_output)
	}
}

fn test_macos_v3_default_executable_excludes_temporary_self_hosted_compilers() {
	$if macos {
		assert is_macos_v3_default_executable('/tmp/v')
		assert is_macos_v3_default_executable('/tmp/vnew')
		assert !is_macos_v3_default_executable('/tmp/v2')
		assert !is_macos_v3_default_executable('/tmp/vstrict1')
		assert !is_macos_v3_default_executable('/tmp/vp')
	}
}

fn test_macos_v3_forwarded_args_strip_new_compiler_flag() {
	$if macos || linux {
		prefs := &pref.Preferences{}
		forwarded := macos_v3_forwarded_args(prefs, ['-new-compiler', 'main.v'])
		assert '-new-compiler' !in forwarded
		assert 'main.v' in forwarded

		run_prefs := &pref.Preferences{
			is_run:   true
			run_args: ['-new-compiler']
		}
		run_forwarded := macos_v3_forwarded_args(run_prefs, [
			'-new-compiler',
			'run',
			'main.v',
			'-new-compiler',
		])
		assert run_forwarded.count(it == '-new-compiler') == 1
		assert run_forwarded.last() == '-new-compiler'
	}
}

fn test_macos_v3_force_requested_forces_v3_for_compile_targets() {
	$if macos {
		build := &pref.Preferences{
			new_compiler: true
			path:         'main.v'
		}
		assert macos_v3_force_requested('build', build)
		run := &pref.Preferences{
			new_compiler: true
			path:         'main.v'
		}
		assert macos_v3_force_requested('run', run)
	}
}

fn test_macos_v3_force_requested_respects_hard_limits() {
	$if macos {
		// not requested without the flag
		assert !macos_v3_force_requested('build', &pref.Preferences{
			path: 'main.v'
		})
		// `-old-compiler` always wins
		assert !macos_v3_force_requested('build', &pref.Preferences{
			new_compiler: true
			old_compiler: true
			path:         'main.v'
		})
		// the `test` command stays with vtest
		assert !macos_v3_force_requested('test', &pref.Preferences{
			new_compiler: true
			path:         'main_test.v'
		})
		// V3 is never forced onto options it cannot honor yet
		assert !macos_v3_force_requested('build', &pref.Preferences{
			new_compiler:   true
			path:           'main.v'
			use_coroutines: true
		})
	}
}

// Autofree builds must never reach the ordinary embedded V3, which lacks
// `ownership` support and would exit with "ownership support is not compiled into
// this v3 executable". On macOS a direct autofree build is delegated to the
// ownership compiler earlier; on Linux it is not, so implicit dispatch stays on
// V1 while an explicit `-new-compiler` request is rejected. Unguarded: these gates
// are shared by every platform's dispatcher (PR #28131 review).
fn test_macos_v3_keeps_autofree_builds_off_non_ownership_v3() {
	assert !is_macos_v3_relevant_command('build', &pref.Preferences{
		autofree: true
		path:     'main.v'
	})
	assert !is_macos_v3_relevant_command('run', &pref.Preferences{
		autofree: true
		is_run:   true
		path:     'main.v'
	})
	assert !macos_v3_force_requested('build', &pref.Preferences{
		new_compiler: true
		autofree:     true
		path:         'main.v'
	})
	assert !macos_v3_force_requested('run', &pref.Preferences{
		new_compiler: true
		autofree:     true
		path:         'main.v'
	})
	assert macos_v3_explicit_autofree_is_unsupported(&pref.Preferences{
		new_compiler: true
		autofree:     true
		path:         'main.v'
	})
	assert !macos_v3_explicit_autofree_is_unsupported(&pref.Preferences{
		autofree: true
		path:     'main.v'
	})
	assert !macos_v3_explicit_autofree_is_unsupported(&pref.Preferences{
		new_compiler: true
		old_compiler: true
		autofree:     true
		path:         'main.v'
	})
	// A plain build (no autofree) is still taken over by V3.
	assert is_macos_v3_relevant_command('build', &pref.Preferences{
		path: 'main.v'
	})
	assert macos_v3_force_requested('build', &pref.Preferences{
		new_compiler: true
		path:         'main.v'
	})
}

fn test_explicit_v3_autofree_build_is_rejected_before_ownership_delegation() {
	$if linux || macos {
		root := os.join_path(os.real_path(os.vtmp_dir()), 'v3_explicit_autofree_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root) or { panic(err) }
		defer {
			os.rmdir_all(root) or {}
		}
		source := os.join_path(root, 'main.v')
		os.write_file(source, 'fn main() {}\n')!
		mut environment := os.environ()
		environment['VFLAGS'] = ''
		environment['VOSARGS'] = ''
		mut process := os.new_process(@VEXE)
		process.set_args(['-new-compiler', '-autofree', source])
		process.set_environment(environment)
		process.set_redirect_stdio()
		process.run()
		process.wait()
		output := process.stdout_slurp() + process.stderr_slurp()
		exit_code := process.code
		process.close()
		assert exit_code == 1, output
		assert output.contains('`-new-compiler` cannot be combined with `-autofree`'), output
	}
}

fn test_explicit_v3_rejects_structured_v1_only_preferences() {
	assert macos_v3_explicit_compilation_requested('build', &pref.Preferences{
		new_compiler: true
		path:         'main.v'
	})
	assert macos_v3_explicit_compilation_requested('build', &pref.Preferences{
		new_compiler: true
		path:         'fixture.vv'
	})
	assert !macos_v3_explicit_compilation_requested('fmt', &pref.Preferences{
		new_compiler: true
		is_quiet:     true
		path:         'main.v'
	})
	assert macos_v3_explicit_v1_preferences_are_unsupported(&pref.Preferences{
		new_compiler: true
		sanitize:     true
		path:         'main.v'
	})
	assert macos_v3_explicit_v1_preferences_are_unsupported(&pref.Preferences{
		new_compiler:   true
		gc_set_by_flag: true
		gc_mode:        .boehm_full_opt
		path:           'main.v'
	})
	assert macos_v3_explicit_v1_preferences_are_unsupported(&pref.Preferences{
		new_compiler: true
		is_livemain:  true
		path:         'main.v'
	})
	assert macos_v3_explicit_v1_preferences_are_unsupported(&pref.Preferences{
		new_compiler:  true
		is_liveshared: true
		path:          'main.v'
	})
	assert !macos_v3_explicit_v1_preferences_are_unsupported(&pref.Preferences{
		new_compiler: true
		path:         'main.v'
	})
	assert !macos_v3_explicit_v1_preferences_are_unsupported(&pref.Preferences{
		new_compiler: true
		old_compiler: true
		sanitize:     true
		path:         'main.v'
	})
}

fn test_embedded_v3_explicit_vv_build_is_rejected() {
	$if macos || linux {
		root := os.join_path(os.real_path(os.vtmp_dir()), 'v3_explicit_vv_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root) or { panic(err) }
		defer {
			os.rmdir_all(root) or {}
		}
		source := os.join_path(root, 'fixture.vv')
		os.write_file(source, 'fn main() {}\n')!
		mut environment := os.environ()
		environment['VFLAGS'] = ''
		environment['VOSARGS'] = ''
		mut process := os.new_process(@VEXE)
		process.set_args(['-new-compiler', source])
		process.set_environment(environment)
		process.set_redirect_stdio()
		process.run()
		process.wait()
		output := process.stdout_slurp() + process.stderr_slurp()
		exit_code := process.code
		process.close()
		assert exit_code == 1, output
		assert output.contains('options that require the established compiler'), output
	}
}

fn test_explicit_v3_options_do_not_reject_external_tools() {
	root := os.join_path(os.real_path(os.vtmp_dir()), 'v3_external_tool_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'formatted.v')
	os.write_file(source, 'fn main() {}\n')!
	mut environment := os.environ()
	environment['VFLAGS'] = ''
	environment['VOSARGS'] = ''
	mut process := os.new_process(@VEXE)
	process.set_args(['-new-compiler', '-q', 'fmt', '-verify', source])
	process.set_environment(environment)
	process.set_redirect_stdio()
	process.run()
	process.wait()
	output := process.stdout_slurp() + process.stderr_slurp()
	exit_code := process.code
	process.close()
	assert exit_code == 0, output
	assert !output.contains('`-new-compiler` cannot be combined'), output
}

fn test_embedded_v3_explicit_sanitize_build_is_rejected() {
	$if macos || linux {
		root := os.join_path(os.real_path(os.vtmp_dir()), 'v3_explicit_sanitize_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root) or { panic(err) }
		defer {
			os.rmdir_all(root) or {}
		}
		source := os.join_path(root, 'main.v')
		os.write_file(source, 'fn main() {}\n')!
		mut environment := os.environ()
		environment['VFLAGS'] = ''
		environment['VOSARGS'] = ''
		mut process := os.new_process(@VEXE)
		process.set_args(['-new-compiler', '-sanitize', source])
		process.set_environment(environment)
		process.set_redirect_stdio()
		process.run()
		process.wait()
		output := process.stdout_slurp() + process.stderr_slurp()
		exit_code := process.code
		process.close()
		assert exit_code == 1, output
		assert output.contains('options that require the established compiler'), output
	}
}

// The executable-name gate is for implicit default dispatch only; an explicit
// `-new-compiler` must still run V3 in-process even when the binary is installed or
// copied under a name other than v/vnew, e.g. `vlang` (PR #28131 review).
fn test_macos_v3_new_compiler_honored_for_renamed_executable() {
	$if macos {
		vroot := os.dir(@VEXE) // has vlib adjacent, so the renamed copy resolves vroot
		renamed := os.join_path(vroot, 'vlang_renamed_${os.getpid()}')
		os.cp(@VEXE, renamed) or { panic(err) }
		os.chmod(renamed, 0o755) or {}
		defer {
			os.rm(renamed) or {}
		}
		src := os.join_path(os.real_path(os.vtmp_dir()), 'renamed_exe_${os.getpid()}.v')
		os.write_file(src, 'fn main() {\n\tprintln(6 * 7)\n}\n')!
		defer {
			os.rm(src) or {}
		}
		mut environment := os.environ()
		environment['VFLAGS'] = ''
		environment['VOSARGS'] = ''
		mut process := os.new_process(renamed)
		process.set_args(['-v', '-new-compiler', 'run', src])
		process.set_environment(environment)
		process.set_redirect_stdio()
		process.run()
		process.wait()
		out := process.stdout_slurp() + process.stderr_slurp()
		code := process.code
		process.close()
		assert code == 0, out
		assert out.contains('Running macOS V3 compiler in process:'), out
		assert out.contains('42'), out
	}
}

// This gate is shared by the Darwin and non-macOS dispatchers, so keep it
// unguarded: it protects command routing on every platform where `-new-compiler`
// is accepted.
fn test_macos_v3_new_compiler_routing_and_precedence() {
	// Compilation commands are taken over by V3.
	assert macos_v3_force_requested('run', &pref.Preferences{
		new_compiler: true
		path:         'main.v'
	})
	assert macos_v3_force_requested('build', &pref.Preferences{
		new_compiler: true
		path:         'main.v'
	})
	// Non-compilation commands are never handed to V3 as compiler inputs.
	assert !macos_v3_force_requested('fmt', &pref.Preferences{
		new_compiler: true
		path:         'main.v'
	})
	assert !macos_v3_force_requested('doc', &pref.Preferences{
		new_compiler: true
		path:         'main.v'
	})
	assert !macos_v3_force_requested('version', &pref.Preferences{
		new_compiler: true
	})
	// The test command stays with the vtest dispatcher.
	assert !macos_v3_force_requested('test', &pref.Preferences{
		new_compiler: true
		path:         'main_test.v'
	})
	// V3 recognizes only run/build/test. Other builtin commands whose token the
	// launcher turns into a path (crun -> `.v`, build-module -> directory,
	// interpret/translate -> `.v`) must not be handed to V3, or the command token
	// becomes its first input path and collides with the real target.
	assert !macos_v3_force_requested('crun', &pref.Preferences{
		new_compiler: true
		path:         'main.v'
	})
	assert !macos_v3_force_requested('build-module', &pref.Preferences{
		new_compiler: true
		path:         os.temp_dir()
	})
	assert !macos_v3_force_requested('interpret', &pref.Preferences{
		new_compiler: true
		path:         'main.v'
	})
	assert !macos_v3_force_requested('translate', &pref.Preferences{
		new_compiler: true
		path:         'main.v'
	})
	// `-old-compiler` takes precedence over `-new-compiler`.
	assert !macos_v3_force_requested('run', &pref.Preferences{
		new_compiler: true
		old_compiler: true
		path:         'main.v'
	})
	// Without the flag, V3 is not forced at all.
	assert !macos_v3_force_requested('run', &pref.Preferences{
		path: 'main.v'
	})
}

fn test_macos_v3_fastc_routes_compiler_selfhost_targets() {
	for target in ['cmd/v', 'cmd/v/v.v', 'vlib/v3/v3.v'] {
		assert macos_v3_force_requested('build', &pref.Preferences{
			new_compiler:  true
			path:          target
			build_options: ['-b fastc']
			is_fastc:      true
		})
		assert !macos_v3_force_requested('build', &pref.Preferences{
			new_compiler:  true
			path:          target
			build_options: ['-b c']
		})
	}
	assert macos_v3_fastc_requested(&pref.Preferences{
		build_options: ['-backend fastc']
		is_fastc:      true
	})
	assert !macos_v3_fastc_requested(&pref.Preferences{
		build_options: ['-b fastc', '-b c']
	})
	repeated_backends, _ := pref.parse_args_and_show_errors([], ['-b', 'fastc', '-b', 'c', '-b',
		'fastc', 'cmd/v'], false)
	assert macos_v3_fastc_requested(repeated_backends)
	assert macos_v3_force_requested('run', &pref.Preferences{
		new_compiler:  true
		autofree:      true
		is_run:        true
		path:          'main.v'
		build_options: ['-b fastc']
		is_fastc:      true
	})
}

fn test_macos_v3_fastc_rejects_incompatible_preferences() {
	fastc_boehm, _ := pref.parse_args_and_show_errors([],
		['-b', 'fastc', '-gc', 'boehm', 'main.v'], false)
	message := macos_v3_fastc_incompatibility(fastc_boehm) or {
		assert false, 'expected explicit FastC with Boehm GC to be rejected'
		return
	}
	assert message.contains('`-b fastc` only supports `-gc none`')

	overridden, _ := pref.parse_args_and_show_errors([], ['-b', 'fastc', '-gc', 'boehm', '-b',
		'c', 'main.v'], false)
	assert macos_v3_fastc_incompatibility(overridden) == none
}

fn test_macos_v3_fastc_allows_explicit_target_os() {
	fastc_linux, _ := pref.parse_args_and_show_errors([],
		['-b', 'fastc', '-os', 'linux', 'main.v'], false)
	assert fastc_linux.backend == .c
	assert fastc_linux.is_fastc
	assert fastc_linux.os == .linux
	assert !v3_has_v1_only_preferences(fastc_linux)
	assert macos_v3_fastc_incompatibility(fastc_linux) == none
	assert macos_v3_force_requested('build', fastc_linux)

	fastc_cross_c, _ := pref.parse_args_and_show_errors([], ['-b', 'fastc', '-os', 'windows', '-o',
		'main.c', 'main.v'], false)
	assert fastc_cross_c.backend == .c
	assert fastc_cross_c.is_fastc
	assert fastc_cross_c.os == .windows
	assert fastc_cross_c.out_name.ends_with('main.c')
	assert !v3_has_v1_only_preferences(fastc_cross_c)
	assert macos_v3_fastc_incompatibility(fastc_cross_c) == none
	assert macos_v3_force_requested('build', fastc_cross_c)

	standard_linux, _ := pref.parse_args_and_show_errors([], ['-b', 'c', '-os', 'linux', 'main.v'],
		false)
	assert !standard_linux.is_fastc
	assert v3_has_v1_only_preferences(standard_linux) == (pref.get_host_os() != .linux)
}

fn test_macos_v3_vtest_ownership_modes_use_v1_except_fastc() {
	mut prefs := &pref.Preferences{
		skip_running: true
		autofree:     true
	}
	assert macos_v3_test_ownership_uses_v1(prefs, ['-skip-running', '-autofree', 'main.v'])
	prefs.autofree = false
	assert macos_v3_test_ownership_uses_v1(prefs, ['-skip-running', '-ownership', 'main.v'])
	prefs.build_options = ['-b fastc']
	prefs.is_fastc = true
	assert !macos_v3_test_ownership_uses_v1(prefs, ['-skip-running', '-ownership', '-b', 'fastc',
		'main.v'])
}
