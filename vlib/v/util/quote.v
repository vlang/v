module util

import strings

const invalid_escapes = r'({$`.'.bytes()

const backslash = 92

const backslash_r = 13

const backslash_n = 10

const double_quote = 34

const double_escape = '\\\\'

//[direct_array_access]
pub fn smart_quote(str string, raw bool) string {
	len := str.len
	if len == 0 {
		return ''
	}
	mut result := strings.new_builder(len)
	mut pos := -1
	mut last := byte(0)
	mut current := byte(0)
	mut next := byte(0)
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
		if pos + 1 < len {
			next = str[pos + 1]
		}
		current = str[pos]
		// double quote
		if current == util.double_quote {
			current = 0
			last = current
			result.write_b(util.backslash)
			result.write_b(util.double_quote)
			continue
		}
		if current == util.backslash {
			if raw {
				last = current
				result.write_string(util.double_escape)
				continue
			} else {
				// escaped backslash - keep as is
				if next == util.backslash {
					skip_next = true
					last = current
					result.write_string(util.double_escape)
					continue
				} else if next != 0 {
					if raw {
						skip_next = true
						last = current
						result.write_string(util.double_escape)
						continue
					} else if next in util.invalid_escapes {
						skip_next = true
						last = current
						result.write_b(next)
						continue
					} else {
						// keep all valid escape sequences
						skip_next = true
						last = current
						result.write_b(current)
						result.write_b(next)
						continue
					}
				}
			}
		}
		// keep newlines in string
		if current == util.backslash_n {
			current = 0
			last = current
			result.write_b(util.backslash)
			result.write_b(`n`)
			continue
		} else if current == util.backslash_r && next == util.backslash_n {
			result.write_b(current)
			result.write_b(next)
			current = 0
			skip_next = true
			last = current
			continue
		}
		// Dolar sign
		if !raw && current == `$` {
			if last == util.backslash {
				result.write_b(last)
				result.write_b(current)
				last = current
				continue
			}
		}
		// Windows style new line \r\n
		if !raw && current == util.backslash_r && next == util.backslash_n {
			skip_next = true
			result.write_b(util.backslash)
			result.write_b(`n`)
			last = current
			continue
		}
		last = current
		result.write_b(current)
	}
	return result.str()
}
