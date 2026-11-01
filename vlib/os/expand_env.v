module os

import strings

// expand_env replaces ${var} or $var in s with the corresponding
// environment variable values. Mirrors Go's os.ExpandEnv.
// Undefined variables expand to an empty string; values are not expanded recursively.
pub fn expand_env(s string) string {
	mut b := strings.new_builder(s.len)
	mut i := 0
	for i < s.len {
		if s[i] == `$` && i + 1 < s.len {
			// ${var}
			if s[i + 1] == `{` {
				if end := s.index_after('}', i + 2) {
					name := s[i + 2..end]
					// Environment names cannot contain NUL; C getenv would truncate them.
					if name.len > 0 && !name.contains_u8(0) {
						b.write_string(getenv(name))
					}
					i = end + 1
					continue
				}
				// Discard an unterminated expansion's introducer, as Go does.
				i += 2
				continue
			}
			mut j := i + 1
			if is_expand_env_special(s[j]) {
				j++
			} else {
				for j < s.len && is_expand_env_ident_char(s[j]) {
					j++
				}
			}
			if j > i + 1 {
				name := s[i + 1..j]
				b.write_string(getenv(name))
				i = j
				continue
			}
		}
		b.write_byte(s[i])
		i++
	}
	return b.str()
}

fn is_expand_env_ident_char(c u8) bool {
	return (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`) || (c >= `0` && c <= `9`) || c == `_`
}

fn is_expand_env_special(c u8) bool {
	return (c >= `0` && c <= `9`) || c in [`*`, `#`, `$`, `@`, `!`, `?`, `-`]
}
