module builder

import os

fn restore_c_error_bug_report_url_env(old_url ?string) {
	if url := old_url {
		os.setenv('V_C_ERROR_BUG_REPORT_URL', url, true)
	} else {
		os.unsetenv('V_C_ERROR_BUG_REPORT_URL')
	}
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
