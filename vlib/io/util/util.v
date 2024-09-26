module util

import os
import rand

const retries = 10000

@[params]
pub struct TempFileOptions {
pub:
	path    string = os.temp_dir()
	pattern string
}

// temp_file returns a uniquely named, open, writable, `os.File` and it's path.
pub fn temp_file(tfo TempFileOptions) !(os.File, string) {
	mut d := tfo.path
	if d == '' {
		d = os.temp_dir()
	}
	os.ensure_folder_is_writable(d) or {
		return error(@FN +
			' could not create temporary file in "${d}". Please ensure write permissions.')
	}
	d = d.trim_right(os.path_separator)
	prefix, suffix := prefix_and_suffix(tfo.pattern) or { return error(@FN + ' ${err.msg()}') }
	for retry := 0; retry < retries; retry++ {
		path := os.join_path(d, prefix + random_number() + suffix)
		mut mode := 'rw+'
		$if windows {
			mode = 'w+'
		}
		mut file := os.open_file(path, mode, 0o600) or { continue }
		if os.exists(path) && os.is_file(path) {
			return file, path
		}
	}
	return error(@FN +
		' could not create temporary file in "${d}". Retry limit (${retries}) exhausted. Please ensure write permissions.')
}

@[params]
pub struct TempDirOptions {
pub:
	path    string = os.temp_dir()
	pattern string
}

fn error_for_temporary_folder(fn_name string, d string) !string {
	return error('${fn_name} could not create temporary directory "${d}". Please ensure you have write permissions for it.')
}

// temp_dir returns a uniquely named, writable, directory path.
pub fn temp_dir(tdo TempFileOptions) !string {
	mut d := tdo.path
	if d == '' {
		d = os.temp_dir()
	}
	os.ensure_folder_is_writable(d) or { return error_for_temporary_folder(@FN, d) }
	d = d.trim_right(os.path_separator)
	prefix, suffix := prefix_and_suffix(tdo.pattern) or { return error(@FN + ' ${err.msg()}') }
	for retry := 0; retry < retries; retry++ {
		path := os.join_path(d, prefix + random_number() + suffix)
		os.mkdir_all(path) or { continue }
		if os.is_dir(path) && os.exists(path) {
			os.ensure_folder_is_writable(path) or { return error_for_temporary_folder(@FN, d) }
			return path
		}
	}
	return error('${@FN} could not create temporary directory "${d}". Retry limit (${retries}) exhausted.')
}

// * Utility functions
fn random_number() string {
	s := (1_000_000_000 + (u32(os.getpid()) + rand.u32n(1_000_000_000) or { 0 })).str()
	return s.substr(1, s.len)
}

fn prefix_and_suffix(pattern string) !(string, string) {
	if pattern.contains(os.path_separator) {
		return error('pattern cannot contain path separators (${os.path_separator}).')
	}
	prefix, suffix := pattern.rsplit_once('*') or { pattern, '' }
	return prefix, suffix
}
