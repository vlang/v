module vbugreport

import os

pub struct StoredCErrorReport {
pub:
	c_file_name  string
	target_os    string
	ccompiler    string
	error_string string
}

// new_stored_c_error_report builds the fields stored by the C error report receiver.
pub fn new_stored_c_error_report(c_file string, target_os string, ccompiler string, c_error string) StoredCErrorReport {
	return StoredCErrorReport{
		c_file_name:  normalized_file_name(c_file)
		target_os:    target_os
		ccompiler:    ccompiler
		error_string: c_error_string(c_error)
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
		if lower.starts_with('error:') {
			return trimmed
		}
		if idx := lower.index('error:') {
			return trimmed[idx..]
		}
	}
	return ''
}
