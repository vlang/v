module driver

// c_link_input_indices identifies positional native inputs without interpreting
// an option's operand as another option or a source/object filename. Keep indices
// so callers can preserve the original argument bytes and ordering.
fn c_link_input_indices(flags []string) []int {
	mut inputs := []int{}
	mut i := 0
	for i < flags.len {
		flag := flags[i].trim_space()
		if flag == '-x' || c_flag_consumes_next_operand(flag) {
			i += 2
			continue
		}
		if flag.len > 0 && !flag.starts_with('-') {
			inputs << i
		}
		i++
	}
	return inputs
}

// c_source_language_flags pins lowercase .c inputs to C without changing an explicit
// -x selection. Scope the selection to the source so objects and C++ inputs retain
// their own language, even if a toolchain rewrites the C filename to an uppercase alias.
fn c_source_language_flags(flags []string) []string {
	mut result := []string{cap: flags.len}
	mut language := ''
	mut i := 0
	for i < flags.len {
		flag := flags[i]
		clean := flag.trim_space()
		if clean == '-x' || c_flag_consumes_next_operand(clean) {
			result << flag
			if i + 1 < flags.len {
				result << flags[i + 1]
				if clean == '-x' {
					language = flags[i + 1].trim_space()
				}
			}
			i += 2
			continue
		}
		joined_language := c_joined_source_language(clean)
		if joined_language.len > 0 {
			language = joined_language
		}
		if language in ['', 'none'] && !clean.starts_with('-') && clean.ends_with('.c') {
			result << ['-x', 'c', flag, '-x', 'none']
		} else {
			result << flag
		}
		i++
	}
	return result
}

// c_joined_source_language extracts the language from a joined -x selector.
fn c_joined_source_language(flag string) string {
	return if flag.starts_with('-x') && flag.len > 2 { flag[2..] } else { '' }
}
