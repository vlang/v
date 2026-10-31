module os

import strings

// expand_env replaces ${var} or $var in s with the corresponding
// environment variable values. Mirrors Go's os.ExpandEnv.
pub fn expand_env(s string) string {
	mut b := strings.new_builder(s.len)
	mut i := 0
	for i < s.len {
		if s[i] == `$` {
			// ${var}
			if i + 1 < s.len && s[i + 1] == `{` {
				if end := s.index_after('}', i + 2) {
					name := s[i + 2..end]
					b.write_string(getenv(name))
					i = end + 1
					continue
				}
			}
			// $var (alphanumeric + underscore)
			mut j := i + 1
			for j < s.len && (is_ident_char(s[j])) {
				j++
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

fn is_ident_char(c u8) bool {
	return (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`) || (c >= `0` && c <= `9`) || c == `_`
}
