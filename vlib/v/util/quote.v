module util

import strings

const invalid_escapes = r'({$`.'.bytes()

const backslash = 92

const backslash_r = 13

const backslash_n = 10

const double_quote = 34

const double_escape = '\\\\'

[direct_array_access]
pub fn smart_quote(str string, raw bool) string {
	len := str.len
	if len == 0 {
		return ''
	}
	if len < 256 {
		mut is_pure := true
		for i := 0; i < len; i++ {
			ch := u8(str[i])
			if (ch >= 37 && ch <= 90) || (ch >= 95 && ch <= 126)
				|| (ch in [` `, `!`, `#`, `[`, `]`]) {
				// safe punctuation + digits + big latin letters,
				// small latin letters + more safe punctuation,
				// important punctuation exceptions, that are not
				// placed conveniently in a consequitive span in
				// the ASCII table.
				continue
			}
			is_pure = false
			break
		}
		if is_pure {
			return str
		}
	}
	// ensure there is enough space for the potential expansion of several \\ or \n
	mut result := strings.new_builder(len + 10)
	mut pos := -1
	mut last := u8(0)
	mut current := u8(0)
	mut next := u8(0)
	mut skip_next := false
	for {
		pos++
		if skip_next {
			skip_next = false
			pos++
		}
		if pos >= len {
			break
		}
		last = current
		current = str[pos]
		if pos + 1 < len {
			next = str[pos + 1]
		} else {
			next = 0
		}
		if current == util.double_quote {
			current = 0
			result.write_u8(util.backslash)
			result.write_u8(util.double_quote)
			continue
		}
		if current == util.backslash {
			if raw {
				result.write_string(util.double_escape)
				continue
			}
			if next == util.backslash {
				// escaped backslash - keep as is
				skip_next = true
				result.write_string(util.double_escape)
				continue
			}
			if next != 0 {
				if raw {
					skip_next = true
					result.write_string(util.double_escape)
					continue
				}
				if next in util.invalid_escapes {
					skip_next = true
					result.write_u8(next)
					continue
				}
				// keep all valid escape sequences
				skip_next = true
				result.write_u8(current)
				result.write_u8(next)
				continue
			}
		}
		if current == util.backslash_n {
			// keep newlines in string
			current = 0
			result.write_u8(util.backslash)
			result.write_u8(`n`)
			continue
		}
		if current == util.backslash_r && next == util.backslash_n {
			result.write_u8(current)
			result.write_u8(next)
			current = 0
			skip_next = true
			continue
		}
		if !raw {
			if current == `$` {
				if last == util.backslash {
					result.write_u8(last)
					result.write_u8(current)
					continue
				}
			}
			if current == util.backslash_r && next == util.backslash_n {
				// Windows style new line \r\n
				skip_next = true
				result.write_u8(util.backslash)
				result.write_u8(`n`)
				continue
			}
		}
		result.write_u8(current)
	}
	return result.str()
}
