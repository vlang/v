module os

// write_file_array writes the data in `buffer` to a file in `path`.
pub fn write_file_array(path string, buffer array) ? {
	mut f := create(path) ?
	unsafe { f.write_full_buffer(buffer.data, size_t(buffer.len * buffer.element_size)) ? }
	f.close()
}

pub fn glob(patterns ...string) ?[]string {
	mut matches := []string{}
	for pattern in patterns {
		native_glob_pattern(pattern, mut matches) ?
	}
	matches.sort()
	return matches
}

pub const (
	args = []string{}
)
