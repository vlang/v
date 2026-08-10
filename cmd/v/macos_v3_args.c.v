module main

// These helpers are shared by the native Darwin dispatcher and the default
// implementation selected while generating cross-platform VC sources, so this
// file has to stay platform neutral (no `_darwin.c.v` suffix): `v -os cross`
// keeps *both* sides of a `$if macos {}` and guards them with a C `#if`, so the
// macOS branch is codegen'ed even when the VC sources are produced on Linux.
// The top level `$if macos {}` below preserves that, while keeping the helpers
// out of the checker's unused-declaration scan on non macOS hosts - their only
// callers are themselves inside `$if macos {}` blocks (see cmd/v/v.v's
// `autofree_args_require_standard_compiler` and `v3_ownership_forwarded_args`),
// and the checker does not walk inactive comptime branches.
$if macos {
	import v.pref

	const macos_v3_compat_c99_flag = '-macos-v3-compat-c99'
	const macos_v3_internal_quiet_flag = '-macos-v3-internal-quiet'

	fn macos_v3_has_v1_only_leading_option(args []string, command string) bool {
		mut i := 0
		for i < args.len {
			arg := args[i]
			if arg == '--' {
				return false
			}
			if arg in ['-message-limit', '-debug', '-debug-tcc', '-wasm-validate', '-wasm-stack-top',
				'-use-coroutines', '-checker-match-exhaustive-cutoff-limit', '-raw-vsh-tmp-prefix',
				'-c++', '-check-unused-fn-args', '-subsystem', '-translated-go', '-musl', '-glibc'] {
				return true
			}
			if macos_v3_leading_option_consumes_value(arg) {
				i += 2
				continue
			}
			if command.len > 0 && arg == command {
				return false
			}
			i++
		}
		return false
	}

	fn macos_v3_leading_option_consumes_value(option string) bool {
		return option in ['-wasm-stack-top', '-arch', '-assert', '-e', '-subsystem', '-icon',
			'--icon', '-seticon', '--seticon', '-gc', '-print_autofree_vars_in_fn', '-trace-fns',
			'-cov', '-coverage', '-profile-fns', '-bug-report-url', '-run-only', '-exclude',
			'-file-list', '-test-runner', '-dump-c-flags', '-dump-modules', '-dump-files',
			'-dump-defines', '-generate-c-project', '-macosx-version-min', '-os', '-printfn',
			'-cflags', '-ldflags', '-d', '-define', '-message-limit', '-thread-stack-size', '-cc',
			'-c++', '-checker-match-exhaustive-cutoff-limit', '-o', '-output', '-b', '-backend',
			'-compile-backend', '--compile-backend', '-path', '-bare-builtin-dir', '-custom-prelude',
			'-raw-vsh-tmp-prefix', '-cmain', '-line-info']
	}

	fn macos_v3_forwarded_args(prefs &pref.Preferences, raw_args []string) []string {
		mut forwarded_args := raw_args.clone()
		if prefs.enable_globals {
			for i, arg in forwarded_args {
				if arg == '--enable-globals' {
					forwarded_args[i] = '-enable-globals'
				}
			}
		}
		// V1 treats `x86` as an amd64 alias, while V3 reserves it for the 32-bit target.
		if prefs.arch == .amd64 && '-arch x86' in prefs.build_options {
			for i in 0 .. forwarded_args.len {
				if i + 1 < forwarded_args.len && forwarded_args[i] == '-arch'
					&& forwarded_args[i + 1] == 'x86' {
					forwarded_args[i + 1] = 'amd64'
				}
			}
		}
		if macos_v3_compat_c99_flag !in forwarded_args {
			forwarded_args.insert(0, macos_v3_compat_c99_flag)
		}
		if prefs.skip_running && '-skip-running' !in forwarded_args {
			forwarded_args.insert(0, '-skip-running')
		}
		if !prefs.is_verbose && !prefs.is_stats && !prefs.show_timings
			&& '-silent' !in forwarded_args && macos_v3_internal_quiet_flag !in forwarded_args {
			forwarded_args.insert(0, macos_v3_internal_quiet_flag)
		}
		// The compatibility fallback must not select a different compiler merely
		// because a valid V3 build crosses the standalone driver's safety cap.
		if '-no-memory-limit' !in forwarded_args && '--no-memory-limit' !in forwarded_args {
			forwarded_args.insert(0, '-no-memory-limit')
		}
		return forwarded_args
	}
}
