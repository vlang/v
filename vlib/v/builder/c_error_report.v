module builder

import json
import net.http
import os
import time
import v.util.version

const default_c_error_bug_report_url = 'https://vlang.io/bug-report'
const c_error_context_radius = 5
const c_error_bug_report_max_body_bytes = 256 * 1024
const c_error_bug_report_truncation_notice = '\n... report truncated before upload ...\n'

struct CErrorReportLine {
pub:
	line int
	text string
}

struct CErrorReportLocation {
pub:
	file string
	line int
}

struct CErrorBugReport {
pub:
	kind           string
	v_version      string
	target_os      string
	target_backend string
	ccompiler      string
	c_error        string
	c_file         string
	c_line         int
	c_context      []CErrorReportLine
	v_file         string
	v_line         int
	v_context      []CErrorReportLine
}

struct CErrorBugReportResponse {
pub:
	id           string
	delete_url   string
	delete_token string
	message      string
}

fn (mut v Builder) submit_c_error_bug_report(ccompiler string, c_output string) {
	raw_report := v.new_c_error_bug_report(ccompiler, c_output)
	report := bounded_c_error_bug_report(raw_report, c_error_bug_report_max_body_bytes)
	report_url := c_error_bug_report_url(v.pref.c_error_bug_report_url)
	response := send_c_error_bug_report(report, report_url) or {
		eprintln('C compiler bug report was not sent to ${report_url}: ${err}')
		return
	}
	println('================== C compiler bug report ==============')
	println('Sent C compiler bug report to ${report_url}.')
	if response.id != '' {
		println('Report id: ${response.id}')
	}
	print_c_error_bug_report_context(report)
	delete_url := if response.delete_url != '' {
		response.delete_url
	} else if response.id != '' && response.delete_token != '' {
		'${report_url}/${response.id}?token=${response.delete_token}'
	} else {
		''
	}
	if delete_url != '' {
		println('Delete this report from the server with:')
		println('  curl -X DELETE ${os.quoted_path(delete_url)}')
	}
	println('='.repeat('================== C compiler bug report =============='.len))
}

fn (mut v Builder) new_c_error_bug_report(ccompiler string, c_output string) CErrorBugReport {
	c_source := os.read_file(v.out_name_c) or { '' }
	c_lines := c_source.split_into_lines()
	mut c_file := v.out_name_c
	mut c_line := 0
	mut v_file := ''
	mut v_line := 0
	if c_loc := c_error_location_for_generated_c(c_output, v.out_name_c) {
		c_file = c_loc.file
		c_line = c_loc.line
		if v_loc := v_source_location_for_c_line(c_lines, c_line, v.out_name_c) {
			v_file = v_loc.file
			v_line = v_loc.line
		}
	} else if source_loc := first_error_source_location(c_output) {
		v_file = source_loc.file
		v_line = source_loc.line
		if found_c_line := generated_c_line_for_source_location(c_lines, source_loc, v.out_name_c) {
			c_line = found_c_line
		}
	}
	v_source := if v_file != '' { os.read_file(v_file) or { '' } } else { '' }
	return CErrorBugReport{
		kind:           'v-c-compiler-error'
		v_version:      version.full_v_version(true)
		target_os:      v.pref.os.str()
		target_backend: v.pref.backend.str()
		ccompiler:      ccompiler
		c_error:        c_output
		c_file:         c_file
		c_line:         c_line
		c_context:      numbered_context_lines(c_lines, c_line, c_error_context_radius)
		v_file:         v_file
		v_line:         v_line
		v_context:      numbered_context_lines(v_source.split_into_lines(), v_line,
			c_error_context_radius)
	}
}

fn c_error_bug_report_url(flag_url string) string {
	trimmed_flag_url := flag_url.trim_space()
	if trimmed_flag_url != '' {
		return trimmed_flag_url.trim_right('/')
	}
	env_url := os.getenv('V_C_ERROR_BUG_REPORT_URL').trim_space()
	if env_url != '' {
		return env_url.trim_right('/')
	}
	return default_c_error_bug_report_url
}

fn send_c_error_bug_report(report CErrorBugReport, report_url string) !CErrorBugReportResponse {
	mut header := http.new_header(key: .content_type, value: 'application/json')
	header.set(.accept, 'application/json')
	response := http.fetch(
		method:        .post
		url:           report_url
		data:          json.encode(report)
		header:        header
		max_retries:   1
		read_timeout:  3 * time.second
		write_timeout: 3 * time.second
	)!
	if response.status_code < 200 || response.status_code >= 300 {
		return error('server responded with HTTP ${response.status_code}')
	}
	return json.decode(CErrorBugReportResponse, response.body)!
}

fn bounded_c_error_bug_report(report CErrorBugReport, max_body_bytes int) CErrorBugReport {
	if max_body_bytes <= 0 || json.encode(report).len <= max_body_bytes {
		return report
	}
	if bounded := report_with_bounded_c_error(report, max_body_bytes, report.c_context,
		report.v_context)
	{
		return bounded
	}
	for context_text_bytes in [4096, 1024, 256, 80, 0] {
		c_context := bounded_report_lines(report.c_context, context_text_bytes)
		v_context := bounded_report_lines(report.v_context, context_text_bytes)
		if bounded := report_with_bounded_c_error(report, max_body_bytes, c_context, v_context) {
			return bounded
		}
	}
	return CErrorBugReport{
		...report
		c_error:   truncated_report_text(report.c_error, 0)
		c_context: []CErrorReportLine{}
		v_context: []CErrorReportLine{}
	}
}

fn report_with_bounded_c_error(report CErrorBugReport, max_body_bytes int, c_context []CErrorReportLine, v_context []CErrorReportLine) ?CErrorBugReport {
	min_report := CErrorBugReport{
		...report
		c_error:   truncated_report_text(report.c_error, 0)
		c_context: c_context
		v_context: v_context
	}
	if json.encode(min_report).len > max_body_bytes {
		return none
	}
	mut low := 0
	mut high := report.c_error.len
	mut best := min_report
	for low <= high {
		mid := (low + high) / 2
		candidate := CErrorBugReport{
			...report
			c_error:   truncated_report_text(report.c_error, mid)
			c_context: c_context
			v_context: v_context
		}
		if json.encode(candidate).len <= max_body_bytes {
			best = candidate
			low = mid + 1
		} else {
			high = mid - 1
		}
	}
	return best
}

fn bounded_report_lines(lines []CErrorReportLine, max_text_bytes int) []CErrorReportLine {
	mut bounded := []CErrorReportLine{cap: lines.len}
	for report_line in lines {
		bounded << CErrorReportLine{
			line: report_line.line
			text: truncated_report_text(report_line.text, max_text_bytes)
		}
	}
	return bounded
}

fn truncated_report_text(text string, max_bytes int) string {
	if max_bytes <= 0 {
		return ''
	}
	if text.len <= max_bytes {
		return text
	}
	if max_bytes <= c_error_bug_report_truncation_notice.len {
		return text[..max_bytes]
	}
	kept_bytes := max_bytes - c_error_bug_report_truncation_notice.len
	head_bytes := kept_bytes / 2
	tail_bytes := kept_bytes - head_bytes
	return text[..head_bytes] + c_error_bug_report_truncation_notice + text[text.len - tail_bytes..]
}

fn print_c_error_bug_report_context(report CErrorBugReport) {
	println('Generated C lines sent from ${report.c_file}:${report.c_line}:')
	print_report_lines(report.c_context, report.c_line)
	if report.v_file != '' {
		println('Corresponding V lines sent from ${report.v_file}:${report.v_line}:')
		print_report_lines(report.v_context, report.v_line)
	} else {
		println('Corresponding V lines sent: no V source mapping was available.')
	}
}

fn print_report_lines(lines []CErrorReportLine, center int) {
	if lines.len == 0 {
		println('  (no source lines available)')
		return
	}
	for line in lines {
		prefix := if line.line == center { '>' } else { ' ' }
		println('${prefix} ${line.line:6} | ${line.text}')
	}
}

fn numbered_context_lines(lines []string, center int, radius int) []CErrorReportLine {
	if center <= 0 || lines.len == 0 {
		return []CErrorReportLine{}
	}
	mut start := center - radius
	if start < 1 {
		start = 1
	}
	mut end := center + radius
	if end > lines.len {
		end = lines.len
	}
	mut context := []CErrorReportLine{cap: end - start + 1}
	for line_nr in start .. end + 1 {
		context << CErrorReportLine{
			line: line_nr
			text: lines[line_nr - 1]
		}
	}
	return context
}

fn c_error_location_for_generated_c(c_output string, generated_c_file string) ?CErrorReportLocation {
	needles := c_error_generated_c_needles(generated_c_file)
	for output_line in c_output.split_into_lines() {
		if !output_line.to_lower_ascii().contains('error') {
			continue
		}
		for needle in needles {
			if loc := parse_error_location_after_needle(output_line, needle) {
				return loc
			}
		}
	}
	return none
}

fn c_error_generated_c_needles(generated_c_file string) []string {
	mut needles := []string{}
	for candidate in [generated_c_file, os.real_path(generated_c_file),
		os.file_name(generated_c_file)] {
		if candidate != '' && candidate !in needles {
			needles << candidate
		}
		normalized := candidate.replace('\\', '/')
		if normalized != '' && normalized !in needles {
			needles << normalized
		}
	}
	return needles
}

fn parse_error_location_after_needle(output_line string, needle string) ?CErrorReportLocation {
	idx := output_line.index(needle) or { return none }
	after := output_line[idx + needle.len..]
	if after.starts_with(':') {
		line_nr := leading_int(after[1..])
		if line_nr > 0 {
			return CErrorReportLocation{
				file: needle
				line: line_nr
			}
		}
	}
	if after.starts_with('(') {
		line_nr := leading_int(after[1..])
		if line_nr > 0 {
			return CErrorReportLocation{
				file: needle
				line: line_nr
			}
		}
	}
	return none
}

fn first_error_source_location(c_output string) ?CErrorReportLocation {
	for output_line in c_output.split_into_lines() {
		if !output_line.to_lower_ascii().contains('error') {
			continue
		}
		if loc := parse_colon_error_location(output_line) {
			return loc
		}
		if loc := parse_msvc_error_location(output_line) {
			return loc
		}
	}
	return none
}

fn parse_colon_error_location(output_line string) ?CErrorReportLocation {
	parts := output_line.split(':')
	if parts.len < 2 {
		return none
	}
	for idx := 1; idx < parts.len; idx++ {
		line_nr := parts[idx].int()
		if line_nr <= 0 {
			continue
		}
		file := parts[..idx].join(':')
		if file == '' {
			continue
		}
		return CErrorReportLocation{
			file: file
			line: line_nr
		}
	}
	return none
}

fn parse_msvc_error_location(output_line string) ?CErrorReportLocation {
	open_idx := output_line.index('(') or { return none }
	close_rel_idx := output_line[open_idx + 1..].index(')') or { return none }
	line_nr := leading_int(output_line[open_idx + 1..open_idx + 1 + close_rel_idx])
	if line_nr <= 0 {
		return none
	}
	return CErrorReportLocation{
		file: output_line[..open_idx]
		line: line_nr
	}
}

fn v_source_location_for_c_line(c_lines []string, c_line int, generated_c_file string) ?CErrorReportLocation {
	if c_line <= 0 || c_lines.len == 0 {
		return none
	}
	mut current := CErrorReportLocation{}
	last_line := if c_line <= c_lines.len { c_line } else { c_lines.len }
	for idx in 0 .. last_line {
		if directive := parse_line_directive(c_lines[idx]) {
			current = directive
			continue
		}
		if idx + 1 == c_line && is_v_source_file(current.file)
			&& !same_path(current.file, generated_c_file) {
			return current
		}
		if current.file != '' {
			current = CErrorReportLocation{
				file: current.file
				line: current.line + 1
			}
		}
	}
	return none
}

fn generated_c_line_for_source_location(c_lines []string, source CErrorReportLocation, generated_c_file string) ?int {
	if source.file == '' || source.line <= 0 {
		return none
	}
	mut current := CErrorReportLocation{}
	mut fallback_line := 0
	for idx, line in c_lines {
		if directive := parse_line_directive(line) {
			current = directive
			continue
		}
		if is_v_source_file(current.file) && !same_path(current.file, generated_c_file)
			&& same_path(current.file, source.file) && current.line == source.line {
			if fallback_line == 0 {
				fallback_line = idx + 1
			}
			if line.trim_space() != '' {
				return idx + 1
			}
		}
		if current.file != '' {
			current = CErrorReportLocation{
				file: current.file
				line: current.line + 1
			}
		}
	}
	if fallback_line > 0 {
		return fallback_line
	}
	return none
}

fn parse_line_directive(line string) ?CErrorReportLocation {
	trimmed := line.trim_space()
	if !trimmed.starts_with('#line ') {
		return none
	}
	rest := trimmed['#line '.len..].trim_space()
	line_nr := leading_int(rest)
	if line_nr <= 0 {
		return none
	}
	first_quote_idx := rest.index('"') or { return none }
	remaining := rest[first_quote_idx + 1..]
	second_quote_idx := remaining.index('"') or { return none }
	return CErrorReportLocation{
		file: remaining[..second_quote_idx]
		line: line_nr
	}
}

fn leading_int(s string) int {
	mut end := 0
	for end < s.len && s[end].is_digit() {
		end++
	}
	if end == 0 {
		return 0
	}
	return s[..end].int()
}

fn is_v_source_file(path string) bool {
	return path.ends_with('.v') || path.ends_with('.vv') || path.ends_with('.vsh')
}

fn same_path(a string, b string) bool {
	if a == b {
		return true
	}
	normalized_a := a.replace('\\', '/')
	normalized_b := b.replace('\\', '/')
	return normalized_a == normalized_b
		|| os.real_path(a).replace('\\', '/') == os.real_path(b).replace('\\', '/')
}
