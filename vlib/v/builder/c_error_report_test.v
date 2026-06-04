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
