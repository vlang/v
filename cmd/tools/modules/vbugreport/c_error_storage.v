module vbugreport

import os

pub struct StoredCErrorReport {
pub:
	c_file_name  string
	target_os    string
	ccompiler    string
	error_string string
	lines        string
	v_lines      string
}

// new_stored_c_error_report builds the fields stored by the C error report receiver.
// The generated C context is stored in `lines`, while the corresponding V source
// context (the V line that caused the C error and its surrounding lines) is stored
// separately in `v_lines`.
pub fn new_stored_c_error_report(c_file string, target_os string, ccompiler string, c_error string, c_lines []string, v_lines []string) StoredCErrorReport {
	return StoredCErrorReport{
		c_file_name:  normalized_file_name(c_file)
		target_os:    target_os
		ccompiler:    ccompiler
		error_string: c_error_string(c_error)
		lines:        c_lines.join('\n')
		v_lines:      v_lines.join('\n')
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
