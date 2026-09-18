fn default_bin_file_for_input(input_file string) string {
	if os.is_dir(input_file) {
		real_input := os.real_path(input_file)
		return os.join_path_single(real_input, os.file_name(real_input))
	}
	resolved_input := if os.exists(input_file) { os.real_path(input_file) } else { input_file }
	if !resolved_input.ends_with('.v') && !resolved_input.ends_with('.vv')
		&& !resolved_input.ends_with('.vsh') {
		return resolved_input
	}
	filename := os.file_name(resolved_input).trim_space()
	mut base := filename.all_before_last('.')
	if os.file_ext(base) in ['.c', '.js', '.wasm'] {
		base = base.all_before_last('.')
	}
	if base == '' {
		base = filename
	}
	if default_bin_file_needs_safe_name(base, filename) {
		base = safe_default_bin_file_name(filename)
	}
	input_dir := os.dir(resolved_input)
	return if input_dir in ['', '.'] { base } else { os.join_path_single(input_dir, base) }
}

fn default_bin_file_needs_safe_name(base string, filename string) bool {
	if base == '' || base in ['.', '..', '-'] {
		return true
	}
	if base == filename && filename.starts_with('.') {
		return true
	}
	if base.ends_with('.c') || base.ends_with('.js') || base.ends_with('.wasm') {
		return true
	}
	for ch in base {
		if ch < ` ` || ch == 127 {
			return true
		}
	}
	return false
}

fn safe_default_bin_file_name(filename string) string {
	mut sanitized := strings.new_builder(filename.len + 4)
	for ch in filename {
		if ch < ` ` || ch == 127 {
			sanitized.write_u8(`_`)
		} else {
			sanitized.write_u8(ch)
		}
	}
	sanitized.write_string('.out')
	return sanitized.str()
}
