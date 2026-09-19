	return hash.hex()
}

fn cli_usage() string {
	return 'usage: v3 [run|test] <file.v|directory> [options]\n' + '  -o <output>                 output binary or C file\n' + '  -b <c|fastc|arm64|wasm|eval> backend\n' + '  -os <name> -arch <name>     target platform\n' + '  -cc <compiler>               C compiler executable\n' + '  -cflags <flags>              extra C compiler options\n' + '  -ldflags <flags>             extra options appended to the link command\n' + '  -thread-stack-size <bytes>   spawned-thread stack size\n' + '  -prod -c99 -shared -strict  C build modes\n' + '  -v                           verbose stage profiling\n' + '  -silent                      suppress benchmark output\n' + '  -showcc                      print C compiler commands\n' + '  -trace-calls                 trace function entries to stderr\n' + '  -trace-fns <patterns>        restrict tracing to functions or modules\n' + '  -profile [file]              write V1-compatible function profile data\n' + '  -profile-fns <names>         profile only named functions and their callees\n' + '  -profile-no-inline           omit @[inline] functions from the profile\n' + '  -no-memory-limit             disable the 10176 MiB user-build memory safety limit\n' + '  -d <name>                    compile-time define'
}

fn shared_library_postfix(target_os string) string {
	return match pref.normalized_os(target_os) {
fn v3_driver_option_requires_value(option string) bool {
	return option in ['-o', '-output', '-b', '-backend', '-os', '-arch', '-compile-backend',
		'--compile-backend', '-d', '-define', '-gc', '-cc', '-thread-stack-size', '-path', '-cov',
		'-coverage', '-file-list', '-message-limit', '-printfn', '-generate-c-project', '-test-runner',
		'-run-only', '-profile-fns', '-trace-fns', '-subsystem', '-exclude', '-dump-files']
}

fn v3_driver_option_consumes_value(option string) bool {
	return v3_driver_option_requires_value(option)
	mut is_prof := false
	mut profile_file := ''
	mut profile_no_inline := false
	mut profile_fns := []string{}
	mut is_trace_calls := false
	mut trace_fns := []string{}
	mut command_seen := false
	mut macos_sdk_root_cache := V3MacosSdkRootCache{}
	environment_c_flags := parse_v3_environment_flags('CFLAGS')
	environment_ld_flags := parse_v3_environment_flags('LDFLAGS')
			i += 2
		} else if args[i] == '-printfn' && i + 1 < args.len {
			print_fn_names << args[i + 1].split(',')
			no_cache = true
			i += 2
		} else if args[i] == '-trace-calls' {
			is_trace_calls = true
			i++
		} else if args[i] == '-trace-fns' {
			for pattern in args[i + 1].split(',') {
				if pattern.trim_space().len > 0 {
					trace_fns << pattern.trim_space()
				}
			}
			i += 2
		} else if args[i] in ['-prof', '-profile'] {
			parsed_profile_file, profile_file_consumed := v3_profile_optional_arg_value(args, i, command_seen)
			profile_file = parsed_profile_file
	// `-ldflags` comes after the ambient `LDFLAGS`, so an explicitly passed option
	// wins, exactly like V1 orders `env_ldflags` before the `-ldflags` value.
	mut link_ld_flags := environment_ld_flags.clone()
	link_ld_flags << user_ld_flags
	if is_trace_calls {
		if backend != 'c' {
			eprintln('option `-trace-calls` is only supported by the C backend')
			exit(1)
		}
		// Cached module bodies were generated without function-entry hooks.
		no_cache = true
	}
	if is_prof && backend !in ['c', 'fastc'] {
		eprintln('option `-profile` is only supported by the C backend')
		exit(1)
	}
			b.print_report()
			return
		}
	}
	minimal_literal_output := !is_prof && !is_trace_calls
		&& input_uses_minimal_literal_output_builtin(input_file, prefs, is_test_command, is_checker_fixture)
	mut use_parallel_c_compilation := parallel_cc && backend == 'c' && !c_only && !effective_tcc
		&& !is_o && target.os != 'windows' && coverage_dir.len == 0 && profile_file.len == 0
		&& !is_trace_calls
		&& v3_parallel_cc_monolithic_define !in user_defines
	// `-keepc` and explicit `-b c` promise a complete generated C translation unit.
	// The module cache splits imported implementations into separate objects, so its main source
	// alone cannot reproduce the build. Literal output uses a deliberately reduced
		}
	}
	if is_prof {
		user_files << os.join_path(prefs.vroot, 'vlib', 'v', 'preludes', 'profiled_program.v')
	}
	if is_trace_calls {
		// Parse the runtime import before user files. Implicit imports such as
		// $embed_file's runtime belong to the last user file, not this prelude.
		trace_prelude := os.join_path(prefs.vroot, 'vlib', 'v', 'preludes', 'trace_calls.v')
		parse_files_dispatch_profiled(mut p, [trace_prelude], false, mut parse_timing)
	}
	prefs.is_test = user_files.any(is_v3_test_file(it, backend, prefs.target))
	parse_files_dispatch_profiled(mut p, user_files, !current_no_parallel, mut parse_timing)
	if is_linux_wayland_only_session(target.os, os.getenv('DISPLAY'), os.getenv('WAYLAND_DISPLAY'), os.getenv('XDG_SESSION_TYPE'))
		mut ckpre_sw := time.new_stopwatch()
		set_diagnostic_files(mut pre_tc, user_files)
		// The C generator has a dedicated literal-output path. The SSA/native backend
		// still builds ordinary builtin bodies, so it needs their full dependency set.
		trivial_literal_output = !is_trace_calls && backend != 'arm64' && test_files.len == 0 && !is_checker_fixture
			&& markused.is_trivial_literal_output_program(a, pre_tc.diagnostic_files)
		if verbose {
			eprintln('  [ttime]   ck trivial gate  ${f64(ckpre_sw.elapsed().microseconds()) / 1000.0:7.2f} ms')
		}
			g.set_show_test_file_results(is_test_command && 'silent' !in prefs.user_defines)
			g.set_test_run_only(run_only)
			g.set_print_fn_names(print_fn_names)
			g.set_profile(profile_file, profile_no_inline, profile_fns)
			g.set_trace_calls(is_trace_calls, trace_fns)
			g.set_shared(prefs.is_shared)
			g.set_object_file_mode(is_o)
			g.set_suppress_main('no_main' in prefs.user_defines)
			g.set_coverage(coverage_dir, args.join(' '))
			g.set_show_test_file_results(is_test_command && 'silent' !in prefs.user_defines)
			g.set_test_run_only(run_only)
			g.set_print_fn_names(print_fn_names)
			g.set_profile(profile_file, profile_no_inline, profile_fns)
			g.set_trace_calls(is_trace_calls, trace_fns)
			g.set_shared(prefs.is_shared)
			g.set_object_file_mode(is_o)
			g.set_suppress_main('no_main' in prefs.user_defines)
			g.set_coverage(coverage_dir, args.join(' '))
