module util

import os
import rand
import rand.seed as rseed

const (
	retries = 10000
)

pub struct TempFileOptions {
	path    string = os.temp_dir()
	pattern string
}

// temp_file returns an uniquely named, open, writable, `os.File` and it's path
pub fn temp_file(tfo TempFileOptions) ?(os.File, string) {
	mut d := tfo.path
	if d == '' {
		d = os.temp_dir()
	}
	os.is_writable_folder(d) or {
		return error(@FN +
			' could not create temporary file in "$d". Please ensure write permissions.')
	}
	d = d.trim_right(os.path_separator)
	mut rng := rand.new_default({})
	prefix, suffix := prefix_and_suffix(tfo.pattern) or { return error(@FN + ' ' + err.msg) }
	for retry := 0; retry < util.retries; retry++ {
		path := os.join_path(d, prefix + random_number(mut rng) + suffix)
		mut mode := 'rw+'
		$if windows {
			mode = 'w+'
		}
		mut file := os.open_file(path, mode, 0o600) or {
			rng.seed(rseed.time_seed_array(2))
			continue
		}
		if os.exists(path) && os.is_file(path) {
			return file, path
		}
	}
	return error(@FN +
		' could not create temporary file in "$d". Retry limit ($util.retries) exhausted. Please ensure write permissions.')
}

pub struct TempDirOptions {
	path    string = os.temp_dir()
	pattern string
}

// temp_dir returns an uniquely named, writable, directory path
pub fn temp_dir(tdo TempFileOptions) ?string {
	mut d := tdo.path
	if d == '' {
		d = os.temp_dir()
	}
	os.is_writable_folder(d) or {
		return error(@FN +
			' could not create temporary directory "$d". Please ensure write permissions.')
	}
	d = d.trim_right(os.path_separator)
	mut rng := rand.new_default({})
	prefix, suffix := prefix_and_suffix(tdo.pattern) or { return error(@FN + ' ' + err.msg) }
	for retry := 0; retry < util.retries; retry++ {
		path := os.join_path(d, prefix + random_number(mut rng) + suffix)
		os.mkdir_all(path) or {
			rng.seed(rseed.time_seed_array(2))
			continue
		}
		if os.is_dir(path) && os.exists(path) {
			os.is_writable_folder(path) or {
				return error(@FN +
					' could not create temporary directory "$d". Please ensure write permissions.')
			}
			return path
		}
	}
	return error(@FN +
		' could not create temporary directory "$d". Retry limit ($util.retries) exhausted. Please ensure write permissions.')
}

// * Utility functions
fn random_number(mut rng rand.PRNG) string {
	s := (u32(1e9) + (u32(os.getpid()) + rng.u32() % u32(1e9))).str()
	return s.substr(1, s.len)
}

fn prefix_and_suffix(pattern string) ?(string, string) {
	mut pat := pattern
	if pat.contains(os.path_separator) {
		return error('pattern cannot contain path separators ($os.path_separator).')
	}
	pos := pat.last_index('*') or { -1 }
	mut prefix := ''
	mut suffix := ''
	if pos != -1 {
		prefix = pat.substr(0, pos)
		suffix = pat.substr(pos + 1, pat.len)
	} else {
		prefix = pat
	}
	return prefix, suffix
}
