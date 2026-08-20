module builder

import os
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

fn test_external_v3_report_env_round_trip() {
	// The wasm build path hands the fallback report to the external builder as
	// self-contained content through the environment: export bounds the source in the
	// trusted parent and removes the staged directory, and take returns an inline,
	// path-free report the builder submits on its own build success (PR #28131 review).
	clear_v3_report_env()
	dir := os.join_path(os.vtmp_dir(), 'v3_report_env_round_trip_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	c_file := os.join_path(dir, 'main.v')
	// A program large enough to yield a bounded head+tail snippet (short files upload no
	// source at all, per the no-whole-file guarantee).
	mut lines := []string{}
	for i in 0 .. 4 * c_error_v_source_radius {
		lines << 'fn f${i}() { println(${i}) }'
	}
	os.write_file(c_file, lines.join('\n'))!
	export_external_v3_report_to_env(ExternalCErrorBugReport{
		kind:        external_v3_compiler_error_kind
		ccompiler:   'v3'
		c_output:    'error: v3 failed'
		c_file:      c_file
		tag:         'V3'
		cleanup_dir: dir
	})
	// The parent captured the content and removed the staged directory itself, so no
	// directory path crosses the environment boundary for the builder to delete later.
	assert !os.exists(dir)
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
	assert got.v_source != ''
	assert got.v_source.contains(c_error_v_source_truncation_notice)
	// the variables are cleared, so a second take finds nothing
	if second := take_external_v3_report_from_env() {
		assert false, 'variables were not cleared: ${second.kind}'
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

fn test_v3_report_v_source_bounds_large_files() {
	// A program at or below the window (2 * c_error_v_source_radius lines) cannot be
	// reduced to a strict subset, so no source is uploaded for it at all — the whole
	// file is never sent.
	small := 'fn main() {\n\tprintln(1)\n}\n'
	assert v3_report_v_source(small) == ''
	// A larger program is reduced to a bounded head+tail window; the middle (which
	// could hold unrelated proprietary code) is dropped and never uploaded.
	mut lines := []string{}
	for i in 0 .. 4 * c_error_v_source_radius {
		lines << 'fn f${i}() { println(${i}) }'
	}
	big := lines.join('\n')
	snippet := v3_report_v_source(big)
	assert snippet != ''
	assert snippet.len < big.len
	assert snippet.contains(c_error_v_source_truncation_notice)
	assert snippet.contains('fn f0() ') // head kept
	assert snippet.contains('fn f${4 * c_error_v_source_radius - 1}() ') // tail kept
	assert !snippet.contains('fn f${2 * c_error_v_source_radius}() ') // middle dropped
	assert snippet.split_into_lines().len <= 2 * c_error_v_source_radius + 3
	// A program just over the window whose only dropped (middle) line is blank still
	// exposes every nonblank line via head+tail, so no source is uploaded even though
	// the file is over 80 lines (PR #28131 review).
	mut blank_middle := []string{}
	for i in 0 .. 2 * c_error_v_source_radius + 1 {
		blank_middle << if i == c_error_v_source_radius { '' } else { 'fn g${i}() {}' }
	}
	assert v3_report_v_source(blank_middle.join('\n')) == ''
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

fn test_generated_c_reset_line_is_not_reported_as_v_source() {
	c_lines := [
		'#line 40 "/tmp/program.tmp.c"',
		'int generated = missing;',
	]
	if _ := v_source_location_for_c_line(c_lines, 2, '/tmp/program.tmp.c') {
		assert false
	}
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
	if _ := b.new_c_error_bug_report_with_vlines('cc') {
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
	if _ := b.new_c_error_bug_report_with_vlines('cc') {
		assert false, 'expected none when no C compiler command was recorded'
	}
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
