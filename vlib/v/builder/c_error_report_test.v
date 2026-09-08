module builder

import os
import crypto.sha256
import v.ast
import v.gen.c as cgen
import v.parser
import v.pref

fn restore_env_var(name string, old_value ?string) {
	if value := old_value {
		os.setenv(name, value, true)
	} else {
		os.unsetenv(name)
	}
}

fn test_codegen_build_options_reports_flags_and_custom_defines() {
	p := pref.Preferences{
		autofree:                      true
		gc_mode:                       .boehm_full
		is_prod:                       true
		skip_unused:                   true
		prealloc:                      true
		is_bare:                       true
		no_builtin:                    true
		no_preludes:                   true
		no_prod_options:               true
		enable_globals:                true
		experimental:                  true
		fast_math:                     true
		no_std:                        true
		no_rsp:                        true
		cmain:                         'SDL_main'
		force_bounds_checking:         true
		div_by_zero_is_zero:           true
		is_check_overflow:             true
		relaxed_gcc14:                 false
		assert_failure_mode:           .backtraces
		subsystem:                     .windows
		is_ios_simulator:              true
		thread_stack_size:             4194304
		thread_stack_size_set_by_flag: true
		is_prof:                       true
		profile_file:                  'some/file'
		profile_no_inline:             true
		profile_fns:                   ['foo_*', 'bar']
		trace_calls:                   true
		trace_fns:                     ['baz_*']
		is_coverage:                   true
		coverage_dir:                  'cov/out'
		// value-carrying options and explicit bare flags are recorded verbatim in build_options
		build_options: ['-d foo', '-d pad=7', '-d header=', '-cflags "-Werror"', '-ldflags "-s"',
			'-custom-prelude prelude.h', '-bare-builtin-dir bare/dir', '-macosx-version-min 10.7',
			'-path "my/mods"', '-musl', '-m64', '-cc gcc']
	}
	opts := codegen_build_options(&p)
	assert opts.contains('autofree')
	assert opts.contains('gc:boehm_full')
	assert opts.contains('prod')
	assert opts.contains('skip_unused')
	assert opts.contains('prealloc')
	assert opts.contains('freestanding')
	assert opts.contains('no_builtin')
	assert opts.contains('no_preludes')
	assert opts.contains('no_prod_options')
	// `-enable-globals` gates the checker (`__global`); without it a report cannot be replayed
	assert opts.contains('enable_globals')
	// `-experimental` gates checker constructs and changes autofree C
	assert opts.contains('experimental')
	// `-fast-math`, `-no-std` and `-no-rsp` change the C compiler command; `-cmain` the entry point
	assert opts.contains('fast_math')
	assert opts.split(' ').any(it == 'no_std')
	assert opts.split(' ').any(it == 'no_rsp')
	assert opts.contains('cmain:SDL_main')
	// `-force-bounds-checking` keeps checks even in `@[direct_array_access]` functions
	assert opts.contains('force_bounds_checking')
	// `-assert backtraces` changes the post-failure C path cgen emits
	assert opts.contains('assert:backtraces')
	// `-subsystem windows` changes the generated main function and the Windows linker command
	assert opts.contains('subsystem:windows')
	// `-os ios -simulator` selects the simulator SDK/clang flags
	assert opts.split(' ').any(it == 'ios_simulator')
	// `-div-by-zero-is-zero` makes cgen emit different safe div/mod helpers
	assert opts.split(' ').any(it == 'div_by_zero_is_zero')
	// `-check-overflow` inserts runtime overflow-check paths
	assert opts.split(' ').any(it == 'check_overflow')
	// `-no-relaxed-gcc14` drops the gcc-14 diagnostic-relaxing pragmas (default on)
	assert opts.split(' ').any(it == 'no_relaxed_gcc14')
	// `-thread-stack-size` is embedded in the spawn/go thread creation call
	assert opts.contains('thread_stack_size:4194304')
	// the profile output path is embedded in the generated C, so keep it
	assert opts.contains('profile:some/file')
	assert opts.contains('profile_no_inline')
	// the profiled/traced function filters change which functions are instrumented
	assert opts.contains('profile_fns:foo_*,bar')
	assert opts.contains('trace_calls')
	assert opts.contains('trace_fns:baz_*')
	assert opts.contains('coverage:cov/out')
	// custom `-d` defines must be recorded, since `$if foo ?` / `$d()` change codegen
	assert opts.contains('-d foo')
	// valued defines keep their value, including an explicitly empty one (`$d()` reads it)
	assert opts.contains('-d pad=7')
	assert opts.contains('-d header=')
	// value-carrying C/link/prelude/builtin options are passed to the compiler and can decide
	// whether the error reproduces, so they are kept verbatim
	assert opts.contains('-cflags "-Werror"')
	assert opts.contains('-ldflags "-s"')
	assert opts.contains('-custom-prelude prelude.h')
	assert opts.contains('-bare-builtin-dir bare/dir')
	// `-macosx-version-min` is passed to clang and selects the SDK deployment target
	assert opts.contains('-macosx-version-min 10.7')
	// `-path` decides which imported module is resolved, so it is kept verbatim
	assert opts.contains('-path "my/mods"')
	// an explicit libc flag is kept (it changes `$if musl` and the libgc C flags)
	assert opts.split(' ').any(it == '-musl')
	// but a libc flag that was not passed is not invented
	assert !opts.split(' ').any(it == '-glibc')
	// an explicit machine-width flag is kept (it selects the C compiler target width)
	assert opts.split(' ').any(it == '-m64')
	assert !opts.split(' ').any(it == '-m32')
	// unrelated recorded options (e.g. `-cc`, covered by the ccompiler field) are not pulled in
	assert !opts.contains('-cc gcc')
}

fn test_external_c_error_report_build_options_include_v3_tag() {
	prefs := pref.Preferences{
		gc_mode: .no_gc
	}
	options := c_error_report_build_options(&prefs, 'V3')
	assert options.split(' ').first() == 'V3'
	assert options.contains('gc:no_gc')
}

fn test_codegen_build_options_reports_no_skip_unused_override() {
	// a C build with skip_unused off means `-no-skip-unused` was passed (it defaults to true);
	// replay must disable it too, or a smaller C program could miss the error
	opts := codegen_build_options(&pref.Preferences{ skip_unused: false })
	assert opts.split(' ').any(it == 'no_skip_unused')
	assert !opts.split(' ').any(it == 'skip_unused')

	// the default (skip_unused on) is reported as plain `skip_unused`, not the override
	on_opts := codegen_build_options(&pref.Preferences{ skip_unused: true })
	assert on_opts.split(' ').any(it == 'skip_unused')
	assert !on_opts.split(' ').any(it == 'no_skip_unused')

	// `-build-module` already turns skip_unused off by itself, so it is not the override
	module_opts := codegen_build_options(&pref.Preferences{
		skip_unused: false
		build_mode:  .build_module
	})
	assert !module_opts.split(' ').any(it == 'no_skip_unused')

	// `-cross` forces skip_unused off in fill_with_defaults, so it must not be reported as the
	// `-no-skip-unused` override; the cross mode itself is recorded instead
	cross_opts := codegen_build_options(&pref.Preferences{
		skip_unused:    false
		output_cross_c: true
	})
	assert !cross_opts.split(' ').any(it == 'no_skip_unused')
	assert cross_opts.split(' ').any(it == 'cross')
}

fn test_codegen_build_options_distinguishes_g_from_cg() {
	// `-g` => is_debug + is_vlines (V #line output)
	g := pref.Preferences{
		is_debug:  true
		is_vlines: true
	}
	g_opts := codegen_build_options(&g)
	assert g_opts.contains('-g')
	assert !g_opts.contains('-cg')

	// `-cg` => is_debug only (C-line debug mode; different generated C)
	cg := pref.Preferences{
		is_debug:  true
		is_vlines: false
	}
	cg_opts := codegen_build_options(&cg)
	assert cg_opts.contains('-cg')
	// (must not be reported as plain `-g`, whose token is a substring of `-cg`)
	assert !cg_opts.split(' ').any(it == '-g')
}

fn test_codegen_build_options_reports_live_modes() {
	// `-live`
	live_opts := codegen_build_options(&pref.Preferences{ is_livemain: true })
	assert live_opts.split(' ').any(it == 'live')

	// `-sharedlive` sets is_liveshared and is_shared, but must not collapse to `shared`
	sharedlive_opts := codegen_build_options(&pref.Preferences{
		is_liveshared: true
		is_shared:     true
	})
	assert sharedlive_opts.contains('sharedlive')
	assert !sharedlive_opts.split(' ').any(it == 'shared')

	// plain `-shared`
	shared_opts := codegen_build_options(&pref.Preferences{ is_shared: true })
	assert shared_opts.split(' ').any(it == 'shared')
}

fn restore_c_error_bug_report_url_env(old_url ?string) {
	restore_env_var('V_C_ERROR_BUG_REPORT_URL', old_url)
}

fn restore_c_error_bug_report_disabled_env(old_value ?string) {
	restore_env_var(c_error_bug_report_disabled_env, old_value)
}

fn test_c_error_location_for_generated_c_parses_gcc_output() {
	loc := c_error_location_for_generated_c('/tmp/program.tmp.c:42:7: error: unknown type name',
		'/tmp/program.tmp.c') or {
		assert false
		return
	}
	assert loc.line == 42
}

fn test_c_error_location_for_generated_c_parses_msvc_output() {
	loc := c_error_location_for_generated_c('C:\\tmp\\program.tmp.c(19): error C2143: syntax error',
		'C:\\tmp\\program.tmp.c') or {
		assert false
		return
	}
	assert loc.line == 19
}

fn test_v_source_for_report_returns_small_window_around_failing_line() {
	mut lines := []string{}
	for i in 1 .. 201 {
		lines << 'line_${i}'
	}
	// error on line 100, radius 3 => only lines 97..103 are uploaded, nothing else
	chunk := v_source_for_report(lines, 100, 3)
	assert chunk.text == 'line_97\nline_98\nline_99\nline_100\nline_101\nline_102\nline_103'
	// the failing line sits at the reported focus position
	assert chunk.text.split('\n')[chunk.focus - 1] == 'line_100'
	// far-away lines are not disclosed
	assert !chunk.text.split('\n').any(it == 'line_1')
	assert !chunk.text.split('\n').any(it == 'line_200')
}

fn test_v_source_for_report_clamps_window_to_file_bounds() {
	lines := ['a', 'b', 'c', 'd']
	// near the start of the file
	assert v_source_for_report(lines, 1, 2).text == 'a\nb\nc'
	// near the end of the file
	assert v_source_for_report(lines, 4, 2).text == 'b\nc\nd'
}

fn test_v_source_for_report_is_empty_without_mapped_line() {
	// no mapped V line (center <= 0) => upload no source at all
	assert v_source_for_report(['a', 'b', 'c'], 0, 40).text == ''
}

fn test_v_source_is_whole_file_detects_full_coverage() {
	// The strict-subset rule drops an excerpt that covers the whole mapped file, so a
	// short C-error fallback does not auto-upload the whole file (PR #28131 review).
	full := 'fn a() {}\nfn b() {}\nfn c() {}'
	assert v_source_is_whole_file(full, full)
	assert v_source_is_whole_file(full + '\n', full) // trailing newline tolerated
	assert !v_source_is_whole_file('fn b() {}', full) // a strict subset is kept
	assert !v_source_is_whole_file('', full) // an already-empty excerpt is not "whole"
}

fn test_v_source_exposes_whole_file_ignores_omitted_whitespace() {
	// An 82-line file mapped at line 41 yields an 81-line window. When the omitted
	// final line is whitespace-only, that window still exposes every substantive
	// source line and must be discarded (PR #28131 review).
	mut mapped := []string{}
	for i in 1 .. 82 {
		mapped << 'fn f${i}() {}'
	}
	mapped << '   '
	mapped_source := mapped.join('\n')
	excerpt := v_source_for_report(mapped, 41, c_error_v_source_radius).text
	assert excerpt.split_into_lines().len == 81
	assert !v_source_is_whole_file(excerpt, mapped_source)
	assert v_source_exposes_whole_file(excerpt, mapped_source, mapped)
	strict_subset := v_source_for_report(mapped, 41, 5).text
	assert !v_source_exposes_whole_file(strict_subset, mapped_source, mapped)
}

fn test_report_includes_v_source_counts_v_context() {
	// A report whose whole-file v_source was dropped can still upload v_context lines,
	// so the notice must not call it metadata-only (PR #28131 review).
	assert !report_includes_v_source(CErrorBugReport{})
	assert report_includes_v_source(CErrorBugReport{
		v_source: 'fn main() {}'
	})
	assert report_includes_v_source(CErrorBugReport{
		v_context: [CErrorReportLine{
			line: 6
			text: 'x := 1'
		}]
	})
	// c_error and c_context (generated C) are not the user's V source.
	assert !report_includes_v_source(CErrorBugReport{
		c_error:   'error: something'
		c_context: [CErrorReportLine{
			line: 3
			text: 'int x;'
		}]
	})
}

fn test_report_uploaded_complete_source_distinguishes_context_only() {
	// A context-only upload (v_source empty, v_context present) is a bounded excerpt, not the
	// complete file, even though report_includes_v_source is true — so the notice must not claim
	// the complete source was submitted (PR #28234 review).
	context_only := CErrorBugReport{
		v_context: [CErrorReportLine{
			line: 6
			text: 'x := 1'
		}]
	}
	assert report_includes_v_source(context_only)
	assert !report_uploaded_complete_source(context_only)
	// A nonempty, non-truncated v_source is the complete file.
	assert report_uploaded_complete_source(CErrorBugReport{
		v_source: 'fn main() {}'
	})
	// A truncated v_source is a bounded excerpt, not complete.
	assert !report_uploaded_complete_source(CErrorBugReport{
		v_source:           'fn main() {}'
		v_source_truncated: true
	})
	assert !report_uploaded_complete_source(CErrorBugReport{})
}

fn test_env_c_output_budget_caps_at_value_limit_and_max() {
	// The diagnostic budget is capped at both the per-variable value limit and the fixed maximum.
	assert env_c_output_budget(1000, 500) == 500 // value limit binds
	assert env_c_output_budget(500, 1000) == 500 // available binds
	// The fixed maximum binds when both are larger.
	big := 4 * c_error_bug_report_max_env_c_output_bytes
	assert env_c_output_budget(big, big) == c_error_bug_report_max_env_c_output_bytes
}

fn test_plan_env_report_content_reclaims_omitted_source_budget_for_diagnostic() {
	// A tiny content budget that cannot hold even the truncation marker omits the source; the half
	// that was reserved for it must be reclaimed so a short missing-library diagnostic survives
	// intact, staying recognizable to the receiver's filter instead of being reported as a compiler
	// bug (PR #28234 review).
	marker_min := c_error_v_source_truncation_notice.len + 2
	short_diagnostic := "ld: library 'macos_v3_absent' not found"
	value_limit := v3_report_max_env_payload_bytes
	// A budget below marker_min forces omission; the source is larger than the budget.
	tiny_budget := marker_min - 2
	plan := plan_env_report_content(ExternalCErrorBugReport{
		c_output: short_diagnostic
		v_source: 'module main\nfn main() {}\n' + 'x'.repeat(1000)
	}, tiny_budget, value_limit)
	// Source is omitted (the budget cannot hold the marker)...
	assert plan.v_source == ''
	assert !plan.v_source_truncated
	// ...and the reclaimed capacity keeps the whole short diagnostic (no truncation notice).
	assert plan.c_output == short_diagnostic
	assert !plan.c_output.contains(c_error_bug_report_truncation_notice)
}

fn test_plan_env_report_content_bounds_source_and_diagnostic_when_both_fit() {
	// With ample budget both are forwarded whole; a source larger than its half-share is bounded
	// (marked truncated) while the diagnostic stays intact.
	value_limit := v3_report_max_env_payload_bytes
	small := plan_env_report_content(ExternalCErrorBugReport{
		c_output: 'error: use of undeclared identifier missing'
		v_source: 'module main\nfn main() {}'
	}, 4096, value_limit)
	assert small.v_source == 'module main\nfn main() {}'
	assert !small.v_source_truncated
	assert small.c_output == 'error: use of undeclared identifier missing'
	// A source that overflows its half-share is bounded and flagged truncated.
	big_source := 'module main\n' + 'fn f() {}\n'.repeat(400)
	bounded := plan_env_report_content(ExternalCErrorBugReport{
		c_output: 'error: x'
		v_source: big_source
	}, 512, value_limit)
	assert bounded.v_source.len < big_source.len
	assert bounded.v_source_truncated
}

fn test_bounded_v_source_with_focus_reports_failing_line_in_excerpt() {
	// A large file bounded around a middle failing line keeps that line and reports its new 1-based
	// position within the excerpt, so a later re-bound can stay centered on it (PR #28234 review).
	mut lines := []string{}
	for i in 0 .. 4000 {
		lines << 'fn f${i}() { println(${i}) }'
	}
	failing := 2000 // 1-based
	lines[failing - 1] = 'fn the_failing_line() {}'
	source := lines.join('\n')
	excerpt, focus := bounded_v_source_with_focus(source, 4096, failing)
	assert excerpt.len < source.len
	assert excerpt.contains('fn the_failing_line() {}')
	assert focus > 0
	excerpt_lines := excerpt.split_into_lines()
	assert excerpt_lines[focus - 1] == 'fn the_failing_line() {}'
	// A whole file that fits keeps the focus unchanged; a head+tail window reports no focus.
	whole, whole_focus := bounded_v_source_with_focus('module main\nfn main() {}', 4096, 2)
	assert whole == 'module main\nfn main() {}'
	assert whole_focus == 2
	_, headtail_focus := bounded_v_source_with_focus(source, 4096, 0)
	assert headtail_focus == 0
}

fn test_plan_env_report_content_reports_excerpt_focus_for_rebound() {
	// When source is bounded, the plan reports the failing line's position within the excerpt so
	// the wasm re-export re-bounds around it instead of dropping it (PR #28234 review).
	value_limit := v3_report_max_env_payload_bytes
	mut lines := []string{}
	for i in 0 .. 400 {
		lines << 'fn f${i}() {}'
	}
	failing := 200
	lines[failing - 1] = 'fn the_failing_line() {}'
	source := lines.join('\n')
	plan := plan_env_report_content(ExternalCErrorBugReport{
		c_output:       'error: x'
		v_source:       source
		v_source_focus: failing
	}, 512, value_limit)
	assert plan.v_source_truncated
	assert plan.v_source.contains('fn the_failing_line() {}')
	assert plan.v_source_focus > 0
	plan_lines := plan.v_source.split_into_lines()
	assert plan_lines[plan.v_source_focus - 1] == 'fn the_failing_line() {}'
}

fn test_plan_env_report_content_omits_focused_source_until_code_fits() {
	// A focused middle excerpt needs two markers and at least one byte from the failing line.
	// Smaller budgets must omit source instead of forwarding marker-only text whose focus points
	// at a marker (PR #28234 review).
	minimum := 2 * c_error_v_source_truncation_notice.len + 3
	source := 'a'.repeat(100) + '\nX\n' + 'b'.repeat(100)
	omitted := plan_env_report_content(ExternalCErrorBugReport{
		v_source:       source
		v_source_focus: 2
	}, minimum - 1, v3_report_max_env_payload_bytes)
	assert omitted.v_source == ''
	assert omitted.v_source_focus == 0
	kept := plan_env_report_content(ExternalCErrorBugReport{
		v_source:       source
		v_source_focus: 2
	}, minimum, v3_report_max_env_payload_bytes)
	assert kept.v_source_truncated
	assert kept.v_source_focus > 0
	assert kept.v_source.split_into_lines()[kept.v_source_focus - 1] == 'X'
}

fn test_export_external_v3_report_round_trips_v_source_focus() {
	// The failing-line focus is carried through the environment handoff (PR #28234 review).
	clear_v3_report_env()
	export_external_v3_report_to_env(ExternalCErrorBugReport{
		kind:           '' // generated-C compilation error
		ccompiler:      'clang'
		c_output:       'error: use of undeclared identifier missing'
		v_file:         'main.v'
		v_source:       'module main\nfn main() {}'
		v_source_focus: 2
		source_inline:  true
		tag:            'V3'
	})
	got := take_external_v3_report_from_env() or {
		assert false, 'no report round-tripped'
		return
	}
	if got.kind == external_v3_transport_limited_kind {
		return
	}
	assert got.v_source == 'module main\nfn main() {}'
	assert got.v_source_focus == 2
}

fn test_bounded_v_source_marks_single_oversized_line_hard_clamp() {
	// The failing V line is the whole file and is longer than the byte budget. The safety
	// hard-clamp used to drop a markerless prefix (PR #28234 review); it must now carry the
	// truncation marker so a truncated upload is never a silently complete-looking prefix.
	long_line := 'const x = "' + 'a'.repeat(200) + '"'
	budget := 64
	out := bounded_v_source(long_line, budget, 1) // focus on line 1 (the only line)
	assert out.len <= budget
	assert out.len < long_line.len
	assert out.contains(c_error_v_source_truncation_notice)
}

fn test_bounded_v_source_focused_hard_clamp_keeps_code_between_markers() {
	// A long focused line in the middle needs both markers plus actual source. A smaller budget
	// must omit it, while the smallest useful budget keeps one source byte at the reported focus.
	long_line := 'const x = "' + 'a'.repeat(200) + '"'
	source := 'module main\n${long_line}\nfn main() {}'
	minimum := 2 * c_error_v_source_truncation_notice.len + 3
	too_small, too_small_focus := bounded_v_source_with_focus(source, minimum - 1, 2)
	assert too_small == ''
	assert too_small_focus == 0
	out, focus := bounded_v_source_with_focus(source, minimum, 2)
	assert out.len == minimum
	assert out.split_into_lines()[focus - 1] == 'c'
}

fn test_external_v3_report_env_round_trip() {
	// The fallback report is handed to the external builder as self-contained content
	// through the environment: the owning process bounds the source (reading its own
	// trusted file) with bounded_v3_fallback_source and forwards only that content with
	// export_external_v3_report_to_env; take returns an inline, path-free report the
	// builder submits on its own build success (PR #28131 review).
	clear_v3_report_env()
	dir := os.join_path(os.vtmp_dir(), 'v3_report_env_round_trip_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	c_file := os.join_path(dir, 'main.v')
	// The full failing file is captured as content so the report is reproducible.
	mut lines := []string{}
	for i in 0 .. 4 * c_error_v_source_radius {
		lines << 'fn f${i}() { println(${i}) }'
	}
	source := lines.join('\n')
	os.write_file(c_file, source)!
	// The owning process captures the source as content, with focus 0 (head+tail if bounded)...
	v_file, v_source, v_source_focus := bounded_v3_internal_fallback_source(c_file, source)
	assert v_file == 'main.v'
	assert v_source == source
	assert v_source_focus == 0
	// The path-based extractor must never reopen an internal-error input after V3 fails.
	late_file, late_source, _ := bounded_v3_fallback_source(external_v3_compiler_error_kind,
		'error: v3 failed', c_file, map[string]string{})
	assert late_file == ''
	assert late_source == ''
	parsed_path := os.real_path(c_file) + '\nwith-separator'
	parsed_digest := sha256.hexhash(lines.join('\n'))
	// ...then forwards only that content; export reads no path and deletes no directory.
	export_external_v3_report_to_env(ExternalCErrorBugReport{
		kind:                   external_v3_compiler_error_kind
		ccompiler:              'v3'
		c_output:               'error: v3 failed'
		v_file:                 v_file
		v_source:               v_source
		source_inline:          true
		input_digests:          {
			parsed_path: parsed_digest
		}
		input_digests_complete: true
		tag:                    'V3'
	})
	got := take_external_v3_report_from_env() or {
		assert false, 'no report round-tripped'
		return
	}
	assert got.kind == external_v3_compiler_error_kind
	assert got.ccompiler == 'v3'
	assert got.c_output == 'error: v3 failed'
	assert got.tag == 'V3'
	// The report is inline content only: no path to read, no directory to delete.
	assert got.source_inline
	assert got.c_file == ''
	assert got.cleanup_dir == ''
	assert got.v_file == 'main.v'
	assert got.v_source == v_source
	// The small source fit the budget, so it round-trips complete and untruncated.
	assert !got.v_source_truncated
	assert got.input_digests_complete
	assert got.input_digests == {
		parsed_path: parsed_digest
	}
	// the variables are cleared, so a second take finds nothing
	if second := take_external_v3_report_from_env() {
		assert false, 'variables were not cleared: ${second.kind}'
	}
}

fn test_v3_fallback_input_verification_uses_stable_parser_digests() {
	vroot := os.real_path(@VEXEROOT)
	root := os.join_path(os.temp_dir(), 'v3_retry_inputs_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	path := os.real_path(os.join_path(root, 'main.v'))
	native_candidate := os.join_path(root, 'native.h')
	native_source := '#define V3_RETRY_NATIVE_VALUE 41\n'
	os.write_file(native_candidate, native_source)!
	native_path := os.real_path(native_candidate)
	defer {
		os.rmdir_all(root) or {}
	}
	parsed_digest := sha256.hexhash('the exact stable parser bytes')
	shared_builtin_path := os.real_path(os.join_path(vroot, 'vlib', 'builtin', 'builtin.v'))
	shared_builtin_digest := sha256.hexhash('shared builtin parser bytes')
	shared_vlib_path := os.real_path(os.join_path(vroot, 'vlib', 'os', 'os.v'))
	shared_vlib_digest := sha256.hexhash('shared vlib parser bytes')
	mut b := &Builder{
		compiled_dir: os.dir(native_path)
		pref:         &pref.Preferences{
			vroot: vroot
		}
		table:        ast.new_table()
		parsed_files: [
			&ast.File{
				path:          path
				source_digest: parsed_digest
			},
			// Parser-generated declarations reuse the source path but contain derived
			// text, so they must not appear as a conflicting retry input.
			&ast.File{
				path:          path
				is_parse_text: true
				source_digest: sha256.hexhash('generated declarations')
			},
			&ast.File{
				path:          shared_builtin_path
				source_digest: shared_builtin_digest
			},
			// V1 and V3 select opposite versions of this internal interface.
			&ast.File{
				path:          os.join_path(vroot, 'vlib', 'builtin',
					'ownership_interface_notd_v3_backend.v')
				source_digest: sha256.hexhash('stable compiler interface')
			},
			// Internal preallocation support is not part of the shared fallback inputs.
			&ast.File{
				path:          os.join_path(vroot, 'vlib', 'builtin', 'prealloc.c.v')
				source_digest: sha256.hexhash('stable prealloc support')
			},
			&ast.File{
				path:          shared_vlib_path
				source_digest: shared_vlib_digest
			},
		]
	}
	matching := ExternalCErrorBugReport{
		input_digests:          {
			path:                                               parsed_digest
			shared_builtin_path:                                shared_builtin_digest
			shared_vlib_path:                                   shared_vlib_digest
			v3_fallback_native_manifest_key:                    sha256.hexhash(v3_fallback_native_manifest_value)
			'${v3_fallback_native_input_prefix}${native_path}': sha256.hexhash(native_source)
		}
		input_digests_complete: true
	}
	assert b.v3_fallback_input_status(matching) == .unchanged
	assert b.matches_v3_fallback_inputs(matching)
	untrusted_candidate := os.join_path(os.temp_dir(), 'v3_retry_untrusted_${os.getpid()}.h')
	os.write_file(untrusted_candidate, native_source)!
	untrusted_path := os.real_path(untrusted_candidate)
	defer {
		os.rm(untrusted_path) or {}
	}
	mut untrusted_digests := matching.input_digests.clone()
	untrusted_digests.delete('${v3_fallback_native_input_prefix}${native_path}')
	untrusted_digests['${v3_fallback_native_input_prefix}${untrusted_path}'] =
		sha256.hexhash(native_source)
	assert b.v3_fallback_input_status(ExternalCErrorBugReport{
		...matching
		input_digests: untrusted_digests
	}) == .changed
	os.write_file(native_path, '#define V3_RETRY_NATIVE_VALUE 42\n')!
	assert b.v3_fallback_input_status(matching) == .changed
	os.write_file(native_path, native_source)!
	changed := ExternalCErrorBugReport{
		...matching
		input_digests: {
			path:                                               sha256.hexhash('rewritten after V3')
			shared_builtin_path:                                shared_builtin_digest
			shared_vlib_path:                                   shared_vlib_digest
			v3_fallback_native_manifest_key:                    sha256.hexhash(v3_fallback_native_manifest_value)
			'${v3_fallback_native_input_prefix}${native_path}': sha256.hexhash(native_source)
		}
	}
	assert b.v3_fallback_input_status(changed) == .changed
	assert !b.matches_v3_fallback_inputs(changed)
	assert !b.matches_v3_fallback_inputs(ExternalCErrorBugReport{
		...matching
		input_digests: {
			path:                                               parsed_digest
			shared_builtin_path:                                sha256.hexhash('rewritten shared builtin')
			shared_vlib_path:                                   shared_vlib_digest
			v3_fallback_native_manifest_key:                    sha256.hexhash(v3_fallback_native_manifest_value)
			'${v3_fallback_native_input_prefix}${native_path}': sha256.hexhash(native_source)
		}
	})
	assert !b.matches_v3_fallback_inputs(ExternalCErrorBugReport{
		...matching
		input_digests: {
			path:                                               parsed_digest
			shared_builtin_path:                                shared_builtin_digest
			shared_vlib_path:                                   sha256.hexhash('rewritten shared vlib')
			v3_fallback_native_manifest_key:                    sha256.hexhash(v3_fallback_native_manifest_value)
			'${v3_fallback_native_input_prefix}${native_path}': sha256.hexhash(native_source)
		}
	})
	assert !b.matches_v3_fallback_inputs(ExternalCErrorBugReport{
		...matching
		input_digests: {
			os.join_path(os.dir(path), 'not-parsed.v'): parsed_digest
		}
	})
	mut b_with_extra_input := &Builder{
		compiled_dir: os.dir(native_path)
		pref:         &pref.Preferences{
			vroot: vroot
		}
		table:        ast.new_table()
		parsed_files: b.parsed_files.clone()
	}
	b_with_extra_input.parsed_files << &ast.File{
		path:          os.join_path(os.dir(path), 'added-after-v3-failed.v')
		source_digest: sha256.hexhash('new stable-only input')
	}
	assert !b_with_extra_input.matches_v3_fallback_inputs(matching)
	unavailable := ExternalCErrorBugReport{
		...matching
		input_digests_complete: false
	}
	assert b.v3_fallback_input_status(unavailable) == .unavailable
	assert !b.matches_v3_fallback_inputs(unavailable)
	// Notice-only fallbacks do not claim or submit a V3-only failure report.
	notice_only := ExternalCErrorBugReport{
		kind: external_v3_notice_only_kind
	}
	assert b.v3_fallback_input_status(notice_only) == .unchanged
	assert b.matches_v3_fallback_inputs(notice_only)
}

fn test_export_external_v3_report_bounds_c_output_for_exec() {
	// A huge C diagnostic must be truncated before it goes into a single environment
	// variable, or the retry's os.execvp fails with E2BIG on Linux (a single exec env
	// string is capped near 128 KiB). v_source is already bounded (PR #28131 review).
	clear_v3_report_env()
	huge := 'error: ' + 'x'.repeat(4 * c_error_bug_report_max_env_c_output_bytes)
	export_external_v3_report_to_env(ExternalCErrorBugReport{
		kind:          '' // generated-C compilation error
		ccompiler:     'clang'
		c_output:      huge
		v_file:        'main.v'
		v_source:      'fn main() {}'
		source_inline: true
		tag:           'V3'
	})
	got := take_external_v3_report_from_env() or {
		assert false, 'no report round-tripped'
		return
	}
	// The forwarded diagnostic now fits within the per-string exec limit.
	assert got.c_output.len <= c_error_bug_report_max_env_c_output_bytes
	assert got.c_output.len < huge.len
	assert got.c_output.contains(c_error_bug_report_truncation_notice)
}

fn test_export_external_v3_report_bounds_combined_exec_payload() {
	clear_v3_report_env()
	mut digests := map[string]string{}
	for i in 0 .. 128 {
		digests['/project/${i}/${'p'.repeat(128)}.v'] = sha256.hexhash('source ${i}')
	}
	huge_output := 'error: ' + 'c'.repeat(c_error_bug_report_max_env_c_output_bytes)
	huge_source := 'fn generated() {\n' + 'v'.repeat(c_error_bug_report_max_v_source_bytes) + '\n}'
	export_external_v3_report_to_env(ExternalCErrorBugReport{
		ccompiler:              'clang'
		c_output:               huge_output
		v_file:                 'main.v'
		v_source:               huge_source
		source_inline:          true
		input_digests:          digests
		input_digests_complete: true
		tag:                    'V3'
	})
	environment := os.environ()
	mut payload := map[string]string{}
	for suffix in v3_report_env_suffixes {
		name := '${v3_report_env_prefix}${suffix}'
		if name in environment {
			payload[suffix] = environment[name]
		}
	}
	assert v3_report_env_payload_bytes(payload) <= v3_report_env_budget(environment, os.args)
	assert v3_report_env_payload_bytes(payload) <= v3_report_max_env_payload_bytes
	got := take_external_v3_report_from_env() or {
		assert false, 'no aggregate-bounded report round-tripped'
		return
	}
	// Windows has a smaller effective exec environment budget. The exporter is
	// allowed to preserve only the transport-limited notice when even the bounded
	// report manifest cannot be forwarded safely.
	if got.kind == external_v3_transport_limited_kind {
		assert !got.input_digests_complete
		return
	}
	assert got.input_digests_complete
	assert got.input_digests == digests
	assert got.c_output.len < huge_output.len
	assert got.v_source.len < huge_source.len
	assert got.c_output.contains(c_error_bug_report_truncation_notice)
	assert got.v_source.contains(c_error_v_source_truncation_notice)
	// The excerpt is flagged truncated explicitly through the handoff.
	assert got.v_source_truncated
}

fn test_export_external_v3_report_preserves_failing_line_through_handoff() {
	// A generated-C failure into a file larger than the environment budget must keep the exact
	// failing line. The byte bound is applied once, at the handoff, centered on it, instead of a
	// second head+tail truncation that would drop the middle — including the failing line
	// (PR #28234 review). It is also never forwarded as a silent markerless prefix.
	clear_v3_report_env()
	mut lines := []string{}
	for i in 0 .. 3000 {
		lines << 'fn head_${i}() { println(${i}) }'
	}
	failing_line := lines.len + 1 // 1-based line of the unique failing marker inserted next
	lines << 'fn the_unique_failing_line_marker() {}'
	for i in 0 .. 3000 {
		lines << 'fn tail_${i}() { println(${i}) }'
	}
	source := lines.join('\n')
	// Far larger than the environment value limit, so export must truncate it.
	assert source.len > c_error_bug_report_max_v_source_bytes
	export_external_v3_report_to_env(ExternalCErrorBugReport{
		kind:           '' // generated-C compilation error
		ccompiler:      'clang'
		c_output:       'error: use of undeclared identifier missing'
		v_file:         'main.v'
		v_source:       source
		v_source_focus: failing_line
		source_inline:  true
		tag:            'V3'
	})
	got := take_external_v3_report_from_env() or {
		assert false, 'no report round-tripped'
		return
	}
	// The environment is too small to forward the manifest, so there is nothing to assert.
	if got.kind == external_v3_transport_limited_kind {
		return
	}
	// Either source was omitted (a budget too small to hold even the marker) or it was
	// truncated with the marker present, keeping the failing line — never a markerless prefix.
	if got.v_source != '' {
		assert got.v_source.len < source.len
		assert got.v_source.contains(c_error_v_source_truncation_notice)
		assert got.v_source.contains('the_unique_failing_line_marker')
	}
}

fn test_export_external_v3_report_marks_complete_file_containing_marker_text_as_untruncated() {
	// A complete file that legitimately contains the truncation-marker comment text must NOT be
	// classified as truncated. Truncation is carried explicitly through the handoff, computed
	// from the byte budget, not inferred from (user-controlled) source content (PR #28234 review).
	clear_v3_report_env()
	source := 'fn main() {\n\t${c_error_v_source_truncation_notice}\n\tprintln(1)\n}'
	assert source.contains(c_error_v_source_truncation_notice)
	export_external_v3_report_to_env(ExternalCErrorBugReport{
		kind:          '' // generated-C compilation error
		ccompiler:     'clang'
		c_output:      'error: use of undeclared identifier missing'
		v_file:        'main.v'
		v_source:      source
		source_inline: true
		tag:           'V3'
	})
	got := take_external_v3_report_from_env() or {
		assert false, 'no report round-tripped'
		return
	}
	// The environment is too small to forward the manifest, so there is nothing to assert.
	if got.kind == external_v3_transport_limited_kind {
		return
	}
	// The whole file fit the budget, so it was forwarded complete and reported as untruncated,
	// even though its content contains the marker text.
	assert got.v_source == source
	assert !got.v_source_truncated
}

fn test_export_external_v3_report_preserves_incoming_truncation_flag() {
	// A wasm re-export forwards a report whose source was already bounded by the first handoff.
	// If that excerpt fits the second budget (no further bounding), the exporter must still mark
	// it truncated rather than re-announce it as the complete file (PR #28234 review).
	clear_v3_report_env()
	excerpt := 'module main\n${c_error_v_source_truncation_notice}\nfn main() {}'
	export_external_v3_report_to_env(ExternalCErrorBugReport{
		kind:               '' // generated-C compilation error
		ccompiler:          'clang'
		c_output:           'error: use of undeclared identifier missing'
		v_file:             'main.v'
		v_source:           excerpt
		v_source_truncated: true
		source_inline:      true
		tag:                'V3'
	})
	got := take_external_v3_report_from_env() or {
		assert false, 'no report round-tripped'
		return
	}
	// The environment is too small to forward the manifest, so there is nothing to assert.
	if got.kind == external_v3_transport_limited_kind {
		return
	}
	// The small excerpt fits the budget (forwarded whole), but its incoming truncation flag is
	// preserved through the re-export.
	assert got.v_source == excerpt
	assert got.v_source_truncated
}

fn test_v3_report_env_budget_reserves_existing_argv_and_environment() {
	assert v3_report_env_budget({
		'EXISTING': 'x'.repeat(v3_report_conservative_exec_bytes)
	}, ['v', 'main.v']) == 0
}

fn test_v3_report_env_value_limit_rejects_oversized_values() {
	limit := 1024
	assert v3_report_env_values_fit({
		'VALUE': 'x'.repeat(limit)
	}, limit)
	assert !v3_report_env_values_fit({
		'VALUE': 'x'.repeat(limit + 1)
	}, limit)
	$if windows {
		assert v3_report_env_value_limit() == v3_report_windows_max_env_value_bytes
	} $else {
		assert v3_report_env_value_limit() == v3_report_max_env_payload_bytes
	}
}

fn test_export_external_v3_report_uses_notice_only_when_metadata_cannot_fit() {
	clear_v3_report_env()
	export_external_v3_report_to_env(ExternalCErrorBugReport{
		kind:     external_v3_compiler_error_kind
		v_file:   'f'.repeat(v3_report_max_env_payload_bytes)
		c_output: 'error: V3 failed'
	})
	got := take_external_v3_report_from_env() or {
		assert false, 'no transport-limited notice round-tripped'
		return
	}
	assert got.kind == external_v3_transport_limited_kind
	assert got.c_output == ''
	assert got.v_source == ''
	assert !got.input_digests_complete
}

fn test_build_inline_c_error_report_classifies_and_filters() {
	prefs := pref.Preferences{
		gc_mode: .no_gc
	}
	// An ordinary generated-C diagnostic is classified as a generated-C
	// (`v-c-compiler-error`) report carrying the bounded content — not misreported as an
	// internal V3 error (which would emit `v3-compiler-error`) (PR #28131 review).
	report := build_inline_c_error_report(&prefs, 'clang',
		'main.tmp.c:3:9: error: use of undeclared identifier x', 'main.v', 'fn main() {}', false, 'V3') or {
		assert false, 'an ordinary generated-C diagnostic must be reportable'
		return
	}
	assert report.kind == 'v-c-compiler-error'
	assert report.c_error.contains('undeclared identifier')
	assert report.v_file == 'main.v'
	assert report.v_source == 'fn main() {}'
	// The explicit truncation flag is carried through, not inferred from source content.
	assert !report.v_source_truncated
	assert report.build_options.split(' ').first() == 'V3'
	// An expected missing-library diagnostic is filtered out (not uploaded), exactly as
	// the in-process generated-C path already does.
	if _ := build_inline_c_error_report(&prefs, 'clang', "ld: library 'macos_v3_absent' not found",
		'main.v', 'fn main() {}', false, 'V3')
	{
		assert false, 'a missing-library diagnostic must not be reported'
	}
}

fn test_v_context_covers_whole_file_detects_full_coverage() {
	// A short mapped file: the radius window around a middle line spans every line,
	// so v_context would leak the whole file and must be cleared (PR #28131 review).
	short := ['a', 'b', 'c']
	assert v_context_covers_whole_file(numbered_context_lines(short, 2, c_error_context_radius),
		short)
	// A larger file: the window is a strict subset and is kept.
	mut long := []string{}
	for i in 1 .. 40 {
		long << 'line_${i}'
	}
	ctx := numbered_context_lines(long, 20, c_error_context_radius)
	assert ctx.len < long.len
	assert !v_context_covers_whole_file(ctx, long)
	// An empty context (no mapped line) is not "whole file".
	assert !v_context_covers_whole_file([]CErrorReportLine{}, short)
}

fn test_v_source_and_context_expose_whole_file_checks_the_union() {
	// A 12-line file: v_source (reproducer) holds `main`, and the v_context window
	// holds the unrelated declaration. Neither covers the file alone, but together
	// they expose every nonblank line, reconstructing it (PR #28131 review).
	mapped := ['module main', '', 'fn main() {', '\tx := 1', '\tprintln(x)', '}', '',
		'fn unrelated() {', '\ty := 2', '\tprintln(y)', '}', '']
	v_source := 'fn main() {\n\tx := 1\n\tprintln(x)\n}'
	context := [
		CErrorReportLine{
			line: 1
			text: 'module main'
		},
		CErrorReportLine{
			line: 8
			text: 'fn unrelated() {'
		},
		CErrorReportLine{
			line: 9
			text: '\ty := 2'
		},
		CErrorReportLine{
			line: 10
			text: '\tprintln(y)'
		},
		CErrorReportLine{
			line: 11
			text: '}'
		},
	]
	assert v_source_and_context_expose_whole_file(v_source, context, mapped)
	// A genuine strict subset: the context no longer reaches the unrelated body.
	partial := [
		CErrorReportLine{
			line: 9
			text: '\ty := 2'
		},
	]
	assert !v_source_and_context_expose_whole_file(v_source, partial, mapped)
	// An empty union exposes nothing.
	assert !v_source_and_context_expose_whole_file('', [], mapped)
}

fn test_selected_v_source_only_uploads_mapped_v_source_chunk() {
	// a mapped V file yields a small chunk around the failing line
	lines := ['module main', 'fn a() {}', 'fn b() {}', 'fn c() {}', 'fn bad() { x }']
	chunk := selected_v_source('/tmp/prog.v', lines, 5)
	assert chunk.text.contains('fn bad()')
	assert chunk.focus >= 1

	// a non-V mapped path (an included header) => no source uploaded
	assert selected_v_source('/tmp/foo.h', lines, 5).text == ''
	// no mapping at all => nothing
	assert selected_v_source('', [], 0).text == ''
}

fn test_bounded_v_source_truncates_on_line_boundaries_with_comment_marker() {
	assert bounded_v_source('private source', 0, 0) == ''
	mut lines := []string{}
	for i in 0 .. 400 {
		lines << 'line_${i} = some_value_here'
	}
	source := lines.join('\n')
	max := 2000
	out := bounded_v_source(source, max, 0)
	assert out.len <= max
	// the marker is a V comment on its own line, so the kept source stays parseable
	assert out.contains(c_error_v_source_truncation_notice)
	// the start (declarations) and the end (failing code) are both preserved
	assert out.starts_with('line_0 = some_value_here')
	assert out.ends_with('line_399 = some_value_here')
	// no original line is split across the truncation: every kept line is whole
	for l in out.split('\n') {
		if l == '' || l == c_error_v_source_truncation_notice {
			continue
		}
		assert l.contains(' = some_value_here')
	}
}

fn test_v_source_for_report_focus_points_at_failing_line() {
	mut lines := []string{}
	for i in 0 .. 60 {
		lines << 'stmt_${i}'
	}
	lines << 'bad := missing' // line 61 (0-based index 60)
	for i in 0 .. 20 {
		lines << 'after_${i}'
	}
	chunk := v_source_for_report(lines, 61, 5)
	// focus is the 1-based line of the failing line within the returned window, so bounding can
	// keep a window around it
	text_lines := chunk.text.split('\n')
	assert chunk.focus >= 1
	assert text_lines[chunk.focus - 1] == 'bad := missing'
}

fn test_bounded_v_source_keeps_focus_line_window() {
	mut lines := []string{}
	for i in 0 .. 2000 {
		lines << 'line_${i} = value_${i}'
	}
	source := lines.join('\n')
	// the failing line is #1000 (1-based), in the middle of a block far larger than the budget
	out := bounded_v_source(source, 2000, 1000)
	assert out.len <= 2000
	// the exact failing line is preserved rather than dropped as the middle
	assert out.contains('line_999 = value_999')
	// a window around it is kept (marker present), not the file head/tail
	assert out.contains(c_error_v_source_truncation_notice)
	assert !out.contains('line_0 = value_0')
	assert !out.contains('line_1999 = value_1999')
}

fn test_numbered_context_lines_returns_five_lines_each_side() {
	lines := ['1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12']
	context := numbered_context_lines(lines, 6, 5)
	assert context.len == 11
	assert context.first().line == 1
	assert context.last().line == 11
	assert context[5].line == 6
	assert context[5].text == '6'
}

fn test_v_source_location_mapping_from_line_directives() {
	c_lines := [
		'#line 10 "/tmp/source.v"',
		'int a = 1;',
		'int b = missing;',
		'#line 999 "/tmp/program.tmp.c"',
		'int main(void) { return 0; }',
	]
	loc := v_source_location_for_c_line(c_lines, 3, '/tmp/program.tmp.c') or {
		assert false
		return
	}
	assert loc.file == '/tmp/source.v'
	assert loc.line == 11
	c_line := generated_c_line_for_source_location(c_lines, CErrorReportLocation{
		file: '/tmp/source.v'
		line: 11
	}, '/tmp/program.tmp.c') or {
		assert false
		return
	}
	assert c_line == 3
}

fn test_bounded_v3_fallback_source_maps_generated_c_error() {
	// A generated-C compilation error is mapped back to its V source file (via the #line
	// directives in the trusted staged C) and the full file is returned, so the report is
	// reproducible.
	dir := os.join_path(os.vtmp_dir(), 'v3_gen_c_map_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	v_path := os.join_path(dir, 'source.v')
	mut lines := []string{}
	for i in 0 .. 200 {
		lines << 'fn f${i}() { println(${i}) }'
	}
	whole := lines.join('\n')
	os.write_file(v_path, whole)!
	generated_c := os.join_path(dir, 'program.tmp.c')
	os.write_file(generated_c, '#line 100 "${v_path}"\nint a = 1;\nint b = missing;\n')!
	c_output := '${generated_c}:3:9: error: use of undeclared identifier missing'
	// kind '' selects the generated-C mapping path.
	v_file, v_source, focus := bounded_v3_fallback_source('', c_output, generated_c, {
		v_path: sha256.hexhash(whole)
	})
	assert v_file == 'source.v'
	// The full mapped file is returned, plus the failing line to focus on, so the byte bound is
	// applied once at the handoff without dropping the failing code.
	assert v_source == whole
	assert v_source.contains('fn f100()')
	// `#line 100` makes the next C line map to V line 100, so the error one line later is 101.
	assert focus == 101
}

fn test_bounded_v3_fallback_source_uploads_whole_mapped_file_generated_c() {
	// A generated-C error mapping into a small file uploads the whole file (it is the
	// reproducer), so the report reproduces rather than dropping to metadata-only.
	dir := os.join_path(os.vtmp_dir(), 'v3_gen_c_wholefile_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	v_path := os.join_path(dir, 'source.v')
	mut lines := []string{}
	for i in 0 .. 80 {
		lines << 'fn f${i}() { println(${i}) }'
	}
	// 80 substantive lines plus a whitespace-only final line (81 lines total).
	whole := lines.join('\n') + '\n   '
	os.write_file(v_path, whole)!
	generated_c := os.join_path(dir, 'program.tmp.c')
	os.write_file(generated_c, '#line 40 "${v_path}"\nint b = missing;\n')!
	c_output := '${generated_c}:2:9: error: use of undeclared identifier missing'
	v_file, v_source, _ := bounded_v3_fallback_source('', c_output, generated_c, {
		v_path: sha256.hexhash(whole)
	})
	assert v_file == 'source.v'
	// The full file is uploaded so the failure can be reproduced from the report.
	assert v_source == whole, v_source
}

fn test_bounded_v3_fallback_source_rejects_unparsed_mapped_file() {
	// A project-controlled preinclude can inject a #line directive naming an unrelated
	// local .v file. Only files V3 actually parsed may contribute source to the automatic
	// fallback report (PR #28131 review).
	dir := os.join_path(os.vtmp_dir(), 'v3_gen_c_unparsed_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	parsed_path := os.join_path(dir, 'parsed.v')
	unrelated_path := os.join_path(dir, 'unrelated.v')
	os.write_file(parsed_path, 'fn parsed() {}\n')!
	os.write_file(unrelated_path, 'const private_value = "must not be uploaded"\n')!
	generated_c := os.join_path(dir, 'program.tmp.c')
	os.write_file(generated_c, '#line 1 "${unrelated_path}"\nint exposed = missing;\n')!
	c_output := '${generated_c}:2:15: error: use of undeclared identifier missing'
	v_file, v_source, _ := bounded_v3_fallback_source('', c_output, generated_c, {
		parsed_path: sha256.hexhash(os.read_file(parsed_path)!)
	})
	assert v_file == ''
	assert v_source == ''
}

fn test_bounded_v3_fallback_source_rejects_changed_parsed_source() {
	dir := os.join_path(os.vtmp_dir(), 'v3_gen_c_changed_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	v_path := os.join_path(dir, 'source.v')
	mut original_lines := []string{}
	for i in 0 .. 200 {
		original_lines << 'fn original_${i}() { println(${i}) }'
	}
	original := original_lines.join('\n')
	os.write_file(v_path, original)!
	parsed_digest := sha256.hexhash(original)
	generated_c := os.join_path(dir, 'program.tmp.c')
	os.write_file(generated_c, '#line 100 "${v_path}"\nint b = missing;\n')!
	// Simulate an editor/build watcher replacing the mapped input after V3 parsed it.
	os.write_file(v_path, original.replace('original_', 'new_private_'))!
	c_output := '${generated_c}:2:9: error: use of undeclared identifier missing'
	v_file, v_source, _ := bounded_v3_fallback_source('', c_output, generated_c, {
		v_path: parsed_digest
	})
	assert v_file == 'source.v'
	assert v_source == ''
}

fn test_generated_c_reset_line_is_not_reported_as_v_source() {
	c_lines := [
		'#line 40 "/tmp/program.tmp.c"',
		'int generated = missing;',
	]
	if _ := v_source_location_for_c_line(c_lines, 2, '/tmp/program.tmp.c') {
		assert false
	}
}

fn test_generated_c_debug_marker_is_recognized_as_generated_c() {
	loc := c_error_location_for_generated_c('<generated C>:42:7: error: unknown type name',
		'/tmp/program.random.tmp.c') or {
		assert false
		return
	}
	assert loc.file == cgen.generated_c_debug_path
	assert loc.line == 42
}

fn test_generated_c_line_for_source_location_prefers_non_empty_line() {
	c_lines := [
		'#line 5 "/tmp/source.v"',
		'void main__main(void) {',
		'',
		'#line 6 "/tmp/source.v"',
		'{NoSuchType _ = ((NoSuchType){E_STRUCT});}',
		'}',
	]
	c_line := generated_c_line_for_source_location(c_lines, CErrorReportLocation{
		file: '/tmp/source.v'
		line: 6
	}, '/tmp/program.tmp.c') or {
		assert false
		return
	}
	assert c_line == 5
}

fn test_c_error_bug_report_url_uses_override_without_trailing_slash() {
	assert c_error_bug_report_url(' http://127.0.0.1:19090/bug-report/ ') == 'http://127.0.0.1:19090/bug-report'
}

fn test_c_error_bug_report_url_uses_bugs_domain_by_default() {
	old_url := os.getenv_opt('V_C_ERROR_BUG_REPORT_URL')
	os.unsetenv('V_C_ERROR_BUG_REPORT_URL')
	defer {
		restore_c_error_bug_report_url_env(old_url)
	}
	assert c_error_bug_report_url('') == 'https://bugs.vlang.io/bug-report'
}

fn test_should_submit_c_error_bug_report_allows_default_outside_github_ci() {
	old_github_actions := os.getenv_opt('GITHUB_ACTIONS')
	old_github_job := os.getenv_opt('GITHUB_JOB')
	old_disabled := os.getenv_opt(c_error_bug_report_disabled_env)
	os.unsetenv('GITHUB_ACTIONS')
	os.unsetenv('GITHUB_JOB')
	os.unsetenv(c_error_bug_report_disabled_env)
	defer {
		restore_env_var('GITHUB_ACTIONS', old_github_actions)
		restore_env_var('GITHUB_JOB', old_github_job)
		restore_c_error_bug_report_disabled_env(old_disabled)
	}
	assert should_submit_c_error_bug_report('')
}

fn test_should_submit_c_error_bug_report_skips_bugs_domain_in_github_ci() {
	old_github_actions := os.getenv_opt('GITHUB_ACTIONS')
	old_github_job := os.getenv_opt('GITHUB_JOB')
	old_url := os.getenv_opt('V_C_ERROR_BUG_REPORT_URL')
	old_disabled := os.getenv_opt(c_error_bug_report_disabled_env)
	os.setenv('GITHUB_ACTIONS', 'true', true)
	os.unsetenv('GITHUB_JOB')
	os.unsetenv('V_C_ERROR_BUG_REPORT_URL')
	os.unsetenv(c_error_bug_report_disabled_env)
	defer {
		restore_env_var('GITHUB_ACTIONS', old_github_actions)
		restore_env_var('GITHUB_JOB', old_github_job)
		restore_c_error_bug_report_url_env(old_url)
		restore_c_error_bug_report_disabled_env(old_disabled)
	}
	assert !should_submit_c_error_bug_report('')
	assert !should_submit_c_error_bug_report(' https://bugs.vlang.io/bug-report/ ')
}

fn test_should_submit_c_error_bug_report_uses_custom_url_in_github_ci() {
	old_github_actions := os.getenv_opt('GITHUB_ACTIONS')
	old_github_job := os.getenv_opt('GITHUB_JOB')
	old_url := os.getenv_opt('V_C_ERROR_BUG_REPORT_URL')
	old_disabled := os.getenv_opt(c_error_bug_report_disabled_env)
	os.unsetenv('GITHUB_ACTIONS')
	os.setenv('GITHUB_JOB', 'test', true)
	os.setenv('V_C_ERROR_BUG_REPORT_URL', 'http://127.0.0.1:19090/bug-report', true)
	os.unsetenv(c_error_bug_report_disabled_env)
	defer {
		restore_env_var('GITHUB_ACTIONS', old_github_actions)
		restore_env_var('GITHUB_JOB', old_github_job)
		restore_c_error_bug_report_url_env(old_url)
		restore_c_error_bug_report_disabled_env(old_disabled)
	}
	assert should_submit_c_error_bug_report('')
	assert should_submit_c_error_bug_report('http://127.0.0.1:19091/bug-report')
}

fn test_should_submit_c_error_bug_report_can_be_disabled_by_env() {
	old_github_actions := os.getenv_opt('GITHUB_ACTIONS')
	old_github_job := os.getenv_opt('GITHUB_JOB')
	old_disabled := os.getenv_opt(c_error_bug_report_disabled_env)
	os.unsetenv('GITHUB_ACTIONS')
	os.unsetenv('GITHUB_JOB')
	defer {
		restore_env_var('GITHUB_ACTIONS', old_github_actions)
		restore_env_var('GITHUB_JOB', old_github_job)
		restore_c_error_bug_report_disabled_env(old_disabled)
	}
	for value in ['1', 'true', 'yes', 'on'] {
		os.setenv(c_error_bug_report_disabled_env, value, true)
		assert !should_submit_c_error_bug_report('')
		assert !should_submit_c_error_bug_report('http://127.0.0.1:19090/bug-report')
	}
	os.setenv(c_error_bug_report_disabled_env, '0', true)
	assert should_submit_c_error_bug_report('')
	disable_c_error_bug_reports()
	assert !should_submit_c_error_bug_report('')
}

fn test_bounded_c_error_bug_report_keeps_encoded_body_under_limit() {
	long_output := 'C compiler diagnostic '.repeat(12000)
	long_c_line := 'generated C line '.repeat(1000)
	long_v_line := 'source V line '.repeat(1000)
	report := CErrorBugReport{
		kind:           'v-c-compiler-error'
		v_version:      'V test'
		target_os:      'linux'
		target_backend: 'c'
		ccompiler:      'cc'
		c_error:        long_output
		c_file:         '/tmp/program.tmp.c'
		c_line:         12
		c_context:      [
			CErrorReportLine{
				line: 12
				text: long_c_line
			},
		]
		v_file:         '/tmp/source.v'
		v_line:         4
		v_context:      [
			CErrorReportLine{
				line: 4
				text: long_v_line
			},
		]
	}
	bounded := bounded_c_error_bug_report(report, 4096)
	encoded := c_error_bug_report_json(bounded)
	assert encoded.len <= 4096
	assert bounded.c_error.len < report.c_error.len
	assert bounded.c_context[0].line == 12
	assert bounded.c_context[0].text.len < report.c_context[0].text.len
	assert bounded.v_context[0].line == 4
	assert bounded.v_context[0].text.len < report.v_context[0].text.len
}

fn test_c_error_bug_report_json_escapes_strings() {
	report := CErrorBugReport{
		kind:      'v-c-compiler-error'
		v_version: 'V "test"\n'
		c_context: [
			CErrorReportLine{
				line: 1
				text: 'tab\tslash\\'
			},
		]
	}
	encoded := c_error_bug_report_json(report)
	assert encoded.contains('"v_version":"V \\"test\\"\\n"')
	assert encoded.contains('"text":"tab\\tslash\\\\"')
}

fn test_truncated_report_text_preserves_start_and_end_when_space_allows() {
	text := 'start-' + 'x'.repeat(100) + '-end'
	truncated := truncated_report_text(text, 80)
	assert truncated.len <= 80
	assert truncated.starts_with('start-')
	assert truncated.contains('report truncated before upload')
	assert truncated.ends_with('-end')
}

fn test_new_c_error_bug_report_with_vlines_is_skipped_when_already_vlines() {
	// When the program is already compiled with -g, the original `.tmp.c` already has
	// `#line` directives, so there is nothing to regenerate.
	mut b := Builder{
		pref: &pref.Preferences{
			is_vlines: true
		}
	}
	if _ := b.new_c_error_bug_report_with_vlines('cc', false) {
		assert false, 'expected none when the C source is already #line annotated'
	}
}

fn test_new_c_error_bug_report_with_vlines_is_skipped_without_a_recorded_command() {
	// Without a recorded C compiler command (e.g. -parallel-cc, or a Windows MSVC build),
	// there is no command to rerun, so no V mapping can be produced this way.
	mut b := Builder{
		pref:        &pref.Preferences{}
		last_cc_cmd: ''
	}
	if _ := b.new_c_error_bug_report_with_vlines('cc', false) {
		assert false, 'expected none when no C compiler command was recorded'
	}
}

fn test_new_c_error_bug_report_limits_full_source_to_v3_fallbacks() {
	dir := os.join_path(os.vtmp_dir(), 'c_error_source_scope_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	source := "module main\n\nfn helper() {\n\tprintln('needed')\n}\n\nfn main() {\n\thelper()\n}\n\nfn unrelated() {\n\tprintln('private')\n}"
	v_path := os.join_path(dir, 'source.v')
	os.write_file(v_path, source)!
	generated_c := os.join_path(dir, 'program.tmp.c')
	os.write_file(generated_c, '#line 8 "${v_path}"\nint b = missing;\n')!
	c_output := '${generated_c}:2:9: error: use of undeclared identifier missing'
	mut b := Builder{
		pref:       &pref.Preferences{}
		out_name_c: generated_c
		table:      ast.new_table()
	}
	b.parsed_files = [parser.parse_file(v_path, mut b.table, .skip_comments, b.pref)]
	repro := b.v_source_reproducer(v_path, 8, c_error_bug_report_max_v_source_bytes)
	assert repro != ''
	assert repro.contains('fn helper()')
	assert !repro.contains('fn unrelated()')
	direct_report := b.new_c_error_bug_report('clang', c_output, false)
	assert direct_report.v_source == repro
	assert direct_report.v_source_truncated
	fallback_report := b.new_c_error_bug_report('clang', c_output, true)
	assert fallback_report.v_source == source
	assert !fallback_report.v_source_truncated
}

fn clear_v3_report_env() {
	for suffix in v3_report_env_suffixes {
		os.unsetenv('${v3_report_env_prefix}${suffix}')
	}
}

fn test_external_v3_report_without_present_marker_is_ignored() {
	// A poisoned environment that sets legacy path-style variables (a directory to
	// delete, a file to upload) but no PRESENT marker yields no report, and take must
	// touch nothing on disk. This is the `v -old-compiler -b wasm` poisoning case: the
	// receiver trusts no path from the environment (PR #28131 review).
	clear_v3_report_env()
	victim := os.join_path(os.vtmp_dir(), 'v3_report_victim_${os.getpid()}')
	os.rmdir_all(victim) or {}
	os.mkdir_all(victim) or { panic(err) }
	defer {
		os.rmdir_all(victim) or {}
	}
	secret := os.join_path(victim, 'secret.txt')
	os.write_file(secret, 'top secret')!
	// Variables an earlier path-based design would have honored; the current take ignores
	// them entirely (they are not even in v3_report_env_suffixes).
	os.setenv('${v3_report_env_prefix}DIR', victim, true)
	os.setenv('${v3_report_env_prefix}CFILE', secret, true)
	defer {
		os.unsetenv('${v3_report_env_prefix}DIR')
		os.unsetenv('${v3_report_env_prefix}CFILE')
	}
	if _ := take_external_v3_report_from_env() {
		assert false, 'no PRESENT marker was set, so no report may be returned'
	}
	// take reads no path and deletes no directory: the victim and its secret survive.
	assert os.is_dir(victim)
	assert os.is_file(secret)
}

fn test_external_v3_report_never_carries_a_path_or_directory() {
	// Even a fully forged handoff (PRESENT set, plus legacy DIR/CFILE pointing at a
	// victim) yields an inline, content-only report: c_file and cleanup_dir are empty, so
	// consume can neither read the named file nor delete the named directory. The forged
	// content is at worst attacker-supplied text (PR #28131 review).
	clear_v3_report_env()
	victim := os.join_path(os.vtmp_dir(), 'v3_report_forged_${os.getpid()}')
	os.rmdir_all(victim) or {}
	os.mkdir_all(victim) or { panic(err) }
	defer {
		os.rmdir_all(victim) or {}
	}
	secret := os.join_path(victim, 'secret.txt')
	os.write_file(secret, 'top secret')!
	os.setenv('${v3_report_env_prefix}PRESENT', '1', true)
	os.setenv('${v3_report_env_prefix}KIND', external_v3_compiler_error_kind, true)
	os.setenv('${v3_report_env_prefix}CCOMPILER', 'v3', true)
	os.setenv('${v3_report_env_prefix}COUTPUT', 'error: forged', true)
	os.setenv('${v3_report_env_prefix}VSOURCE', 'attacker supplied text', true)
	os.setenv('${v3_report_env_prefix}DIR', victim, true)
	os.setenv('${v3_report_env_prefix}CFILE', secret, true)
	defer {
		os.unsetenv('${v3_report_env_prefix}DIR')
		os.unsetenv('${v3_report_env_prefix}CFILE')
	}
	got := take_external_v3_report_from_env() or {
		assert false, 'a PRESENT handoff should return its content-only report'
		return
	}
	// The dangerous capabilities are absent regardless of the forged DIR/CFILE.
	assert got.source_inline
	assert got.c_file == ''
	assert got.cleanup_dir == ''
	assert got.v_source == 'attacker supplied text'
	// take never touched the victim.
	assert os.is_dir(victim)
	assert os.is_file(secret)
}
