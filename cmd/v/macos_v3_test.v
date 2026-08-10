module main

import os
import v.pref

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
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.is_prof = false
		prefs.profile_fns = ['main__work']
		assert !is_macos_v3_relevant_command('main.v', prefs)
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
		assert is_macos_v3_relevant_command('script.vsh', prefs)
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

fn test_selective_profile_autofree_requires_standard_compiler() {
	$if macos {
		prefs, _ := pref.parse_args_and_show_errors([], [
			'',
			'-autofree',
			'-profile-fns',
			'main__work',
			'main.v',
		], false)
		assert prefs.profile_fns == ['main__work']
		assert autofree_requires_standard_compiler(prefs)
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
		assert '-no-memory-limit' in forwarded
		assert '-no-parallel' !in forwarded
		assert forwarded.count(it == macos_v3_compat_c99_flag) == 1
		already_present := macos_v3_forwarded_args(prefs, [macos_v3_compat_c99_flag, 'main.v'])
		assert already_present.count(it == macos_v3_compat_c99_flag) == 1
		assert already_present.count(it in ['-nocache', '--no-cache']) == 0
		assert already_present.count(it == '-no-memory-limit') == 1
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
	assert !ownership_delegation_is_requested(false, false, false, 'macos')
	assert ownership_delegation_is_requested(true, false, false, 'linux')
	assert ownership_delegation_is_requested(true, false, false, 'windows')
	assert ownership_delegation_is_requested(true, true, false, 'linux')
	assert ownership_delegation_is_requested(false, true, false, 'macos')
	assert !ownership_delegation_is_requested(false, true, false, 'linux')
	assert !ownership_delegation_is_requested(false, true, false, 'windows')
	assert !ownership_delegation_is_requested(false, true, true, 'macos')
	assert !ownership_delegation_is_requested(true, false, true, 'macos')
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
	$if macos {
		caller_environment := {
			'PATH':                     '/usr/bin'
			'CFLAGS':                   '-I/caller/include -DCALLER_FLAG=1'
			'LDFLAGS':                  '-L/caller/lib -lcaller'
			'VEXE':                     'caller-vexe'
			'VCHILD':                   'caller-vchild'
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

		unset_environment := macos_v3_child_environment(@VEXE, '/tmp/macos_v3_fallback', {
			'PATH': '/usr/bin'
		})
		assert unset_environment[macos_v3_caller_vexe_present_env] == '0'
		assert unset_environment[macos_v3_caller_vexe_env] == ''
		assert unset_environment[macos_v3_caller_vchild_present_env] == '0'
		assert unset_environment[macos_v3_caller_vchild_env] == ''
	}
}

fn test_macos_v3_embedded_driver_reuses_module_cache() {
	$if macos {
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
		os.write_file(os.join_path(root, 'src.c'), 'int main(void) { return missing; }\n')!
		report := read_macos_v3_c_error_report(root) or {
			assert false
			return
		}
		assert report.ccompiler == 'clang'
		assert report.c_output.contains('generated failure')
		assert report.c_file == os.join_path(root, 'src.c')
		assert report.report_dir == root
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
	$if macos {
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
