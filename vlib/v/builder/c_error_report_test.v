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
		cmain:                         'SDL_main'
		force_bounds_checking:         true
		div_by_zero_is_zero:           true
		is_check_overflow:             true
		relaxed_gcc14:                 false
		assert_failure_mode:           .backtraces
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
			'-musl', '-m64', '-cc gcc']
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
	// `-fast-math` and `-no-std` change the C compiler command; `-cmain` changes the entry point
	assert opts.contains('fast_math')
	assert opts.split(' ').any(it == 'no_std')
	assert opts.contains('cmain:SDL_main')
	// `-force-bounds-checking` keeps checks even in `@[direct_array_access]` functions
	assert opts.contains('force_bounds_checking')
	// `-assert backtraces` changes the post-failure C path cgen emits
	assert opts.contains('assert:backtraces')
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

fn test_v_source_for_report_keeps_prefix_and_failing_region() {
	// lines 1..60 (each a column-0 top-level line); the error is on line 50, far past the prefix
	mut lines := []string{}
	for i in 1 .. 61 {
		lines << i.str()
	}
	src := v_source_for_report(lines, 50, 2, 3)
	// leading declarations (imports/types live at the top) are kept
	assert src.contains('1\n2\n3')
	// the failing declaration (line 50) plus `radius` lines after it are kept
	assert src.contains('50\n51\n52')
	// unrelated middle bodies are dropped, with a marker in their place
	assert src.contains(c_error_v_source_omitted_notice)
	assert !src.split('\n').any(it == '25')
}

fn test_v_source_for_report_does_not_cut_prefix_mid_block() {
	mut lines := []string{}
	lines << 'module main' // 1
	lines << 'fn helper() {' // 2 (opens a long block that runs past the prefix)
	for i in 0 .. 40 {
		lines << '\th${i} := ${i}' // 3..42
	}
	lines << '}' // 43 (closes helper)
	lines << 'fn target() {' // 44
	lines << '\tbad := undefined_thing' // 45 (failing)
	lines << '}' // 46
	// prefix_lines=5 lands inside helper's body; it must not keep a half-open `fn helper() {`
	src := v_source_for_report(lines, 45, 2, 5)
	assert src.starts_with('module main')
	// only the brace-balanced leading line survives; the half-open helper body is not included
	assert !src.contains('h0 :=')
	assert !src.contains('fn helper() {')
	// the enclosing block of the failing line is included whole
	assert src.contains('fn target() {')
	assert src.contains('bad := undefined_thing')
	assert src.contains(c_error_v_source_omitted_notice)
}

fn test_v_source_for_report_includes_attributes_above_declaration() {
	mut lines := []string{}
	lines << 'module main' // 1
	for i in 0 .. 40 {
		lines << 'const c${i} = ${i}' // 2..41 (pushes the declaration past the prefix)
	}
	lines << '@[direct_array_access]' // 42 (attribute above the failing fn)
	lines << 'fn hot(a []int) int {' // 43
	lines << '\treturn a[0] + missing' // 44 (failing)
	lines << '}' // 45
	src := v_source_for_report(lines, 44, 2, 5)
	// the attribute is included with the enclosing declaration, since it changes the generated C
	assert src.contains('@[direct_array_access]')
	assert src.contains('fn hot(a []int) int {')
	assert src.contains('return a[0] + missing')
}

fn test_v_source_for_report_includes_enclosing_declaration() {
	mut lines := []string{}
	lines << 'module main' // 1
	lines << 'import os' // 2
	for i in 0 .. 30 {
		lines << 'const c${i} = ${i}' // 3..32 (column-0 declarations)
	}
	lines << 'fn big() {' // 33 (enclosing signature, column 0)
	for i in 0 .. 30 {
		lines << '\tx${i} := ${i}' // 34..63 (indented body)
	}
	lines << '\tbad := undefined_thing' // 64 (failing line, indented)
	lines << '}' // 65
	// error on line 64, radius 5 (window starts at 59, mid-body), prefix 3
	src := v_source_for_report(lines, 64, 5, 3)
	// the region is extended up to the enclosing `fn big() {` signature, not started mid-body
	assert src.contains('fn big() {')
	// it does not resume at an interior statement right after the omission marker
	assert !src.contains('${c_error_v_source_omitted_notice}\n\tx25')
	// the failing line and the leading declarations are both present
	assert src.contains('bad := undefined_thing')
	assert src.starts_with('module main')
	assert src.contains(c_error_v_source_omitted_notice)
}

fn test_v_source_for_report_is_contiguous_when_region_reaches_prefix() {
	lines := ['1', '2', '3', '4', '5', '6', '7']
	// error on line 3 with radius 2 => region reaches the top, so no omission marker
	src := v_source_for_report(lines, 3, 2, 3)
	assert src == '1\n2\n3\n4\n5'
	assert !src.contains(c_error_v_source_omitted_notice)
}

fn test_v_source_for_report_is_empty_without_mapped_line() {
	// no mapped V line (center <= 0) => upload no source at all
	assert v_source_for_report(['a', 'b', 'c'], 0, 40, 40) == ''
}

fn test_selected_v_source_falls_back_to_input_file_without_mapping() {
	// no V mapping (v_file empty), but a single .v input was compiled => keep the whole input,
	// so the report still carries the failing program instead of an empty v_source
	whole := 'module main\nfn main() {}'
	assert selected_v_source('', [], 0, '/tmp/prog.v', whole) == whole

	// no mapping and the input is not a V source file (e.g. a directory target) => nothing
	assert selected_v_source('', [], 0, '/tmp/outdir', 'ignored') == ''

	// a real V mapping uses the failing-line chunk, not the whole input
	lines := ['module main', 'fn a() {}', 'fn b() {}', 'fn c() {}', 'fn bad() { x }']
	chunk := selected_v_source('/tmp/prog.v', lines, 5, '/tmp/prog.v', 'WHOLE_INPUT_IGNORED')
	assert chunk.contains('fn bad()')
	assert !chunk.contains('WHOLE_INPUT_IGNORED')
}

fn test_bounded_v_source_truncates_on_line_boundaries_with_comment_marker() {
	mut lines := []string{}
	for i in 0 .. 400 {
		lines << 'line_${i} = some_value_here'
	}
	source := lines.join('\n')
	max := 2000
	out := bounded_v_source(source, max)
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
