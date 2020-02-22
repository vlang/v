module filepath

// ext returns the extension in the file `path`.
pub fn ext(path string) string {
	pos := path.last_index('.') or {
		return ''
	}
	return path[pos..]
}

// is_abs returns true if `path` is absolute.
pub fn is_abs(path string) bool {
	$if windows {
		return path[0] == `/` || // incase we're in MingGW bash
		(path[0].is_letter() && path[1] == `:`)
	}
	return path[0] == `/`
}

// join returns path as string from string parameter(s).
pub fn join(base string, dirs ...string) string {
	mut result := []string
	result << base.trim_right('\\/')
	for d in dirs {
		result << d
	}
	return result.join(separator)
}

// dir returns all but the last element of path, typically the path's directory.
pub fn dir(path string) string {
	pos := path.last_index(separator) or {
		return '.'
	}
	return path[..pos]
}

// basedir returns a directory name from path
pub fn basedir(path string) string {
	pos := path.last_index(separator) or {
		return path
	}
	// NB: *without* terminating /
	return path[..pos]
}

// filename returns a file name from path
pub fn filename(path string) string {
	return path.all_after(separator)
}
