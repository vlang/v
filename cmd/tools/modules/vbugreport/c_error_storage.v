module vbugreport

import os

pub struct StoredCErrorReport {
pub:
	c_file_name   string
	target_os     string
	ccompiler     string
	v_version     string
	arch          string
	build_options string
	error_string  string
	lines         string
	v_lines       string
	v_source      string
}

// new_stored_c_error_report builds the fields stored by the C error report receiver.
// The generated C context is stored in `lines`, while the corresponding V source
// context (the V line that caused the C error and its surrounding lines) is stored
// separately in `v_lines`. `v_version` (the reporting compiler's version), `arch`,
// `build_options` (the codegen-affecting `v` flags) and `v_source` (the failing file's leading
// declarations plus the block around the error line) are stored so a report can be reproduced,
// and so reports from V versions predating a fix can be filtered out during triage.
pub fn new_stored_c_error_report(c_file string, target_os string, ccompiler string, v_version string, arch string, build_options string, c_error string, c_lines []string, v_lines []string, v_source string) StoredCErrorReport {
	return StoredCErrorReport{
		c_file_name:   normalized_file_name(c_file)
		target_os:     target_os
		ccompiler:     ccompiler
		v_version:     v_version
		arch:          arch
		build_options: build_options
		error_string:  c_error_string(c_error)
		lines:         c_lines.join('\n')
		v_lines:       v_lines.join('\n')
		v_source:      v_source
	}
}

fn normalized_file_name(path string) string {
	return os.file_name(path.replace('\\', '/'))
}

fn c_error_string(c_output string) string {
	for line in c_output.split_into_lines() {
		trimmed := line.trim_space()
		lower := trimmed.to_lower_ascii()
		if lower.starts_with('warning:') {
			continue
		}
		if error_string := normalized_error_string(trimmed, lower) {
			return error_string
		}
	}
	return ''
}

fn normalized_error_string(trimmed string, lower string) ?string {
	if lower.starts_with('fatal error:') || lower.starts_with('fatal error ') {
		return 'error: ${trimmed}'
	}
	if lower.starts_with('error:') {
		return trimmed
	}
	if lower.starts_with('error ') {
		return 'error: ${trimmed['error '.len..]}'
	}
	for needle in [' fatal error:', ' fatal error '] {
		if idx := lower.index(needle) {
			return 'error: ${trimmed[idx + 1..]}'
		}
	}
	if idx := lower.index(' error:') {
		return trimmed[idx + 1..]
	}
	if idx := lower.index(' error ') {
		return 'error: ${trimmed[idx + ' error '.len..]}'
	}
	return none
}
