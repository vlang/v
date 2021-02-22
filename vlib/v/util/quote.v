module util

import strings

const (
	invalid_escapes = ['(', '{', '$', '`', '.']
)

pub fn smart_quote(str string, raw bool) string {
	len := str.len
	if len == 0 {
		return str
	}
	mut result := strings.new_builder(0)
	mut pos := -1
	mut last := ''
	// TODO: This should be a single char?
	mut next := ''
	mut skip_next := false
	for {
		pos = pos + 1
		if skip_next {
			skip_next = false
			pos = pos + 1
		}
		if pos >= len {
			break
		}
		if pos + 1 < len {
			unsafe {
				next = str.str[pos + 1].ascii_str()
			}
		}
		mut current := str
		mut toadd := str
		if len > 1 {
			unsafe {
				current = str.str[pos].ascii_str()
			}
			toadd = current
		}
		// double quote
		if current == '"' {
			toadd = '\\"'
			current = ''
		}
		if current == '\\' {
			if raw {
				toadd = '\\\\'
			} else {
				// escaped backslash - keep as is
				if next == '\\' {
					toadd = '\\\\'
					skip_next = true
				} else if next != '' {
					if raw {
						toadd = '\\\\' + next
						skip_next = true
					}
					// keep all valid escape sequences
					else if next !in util.invalid_escapes {
						toadd = '\\' + next
						skip_next = true
					} else {
						toadd = next
						skip_next = true
					}
				}
			}
		}
		// keep newlines in string
		if current == '\n' {
			toadd = '\\n'
			current = ''
		} else if current == '\r' && next == '\n' {
			toadd = '\r\n'
			current = ''
			skip_next = true
		}
		// Dolar sign
		if !raw && current == '$' {
			if last == '\\' {
				toadd = r'\$'
			}
		}
		// Windows style new line \r\n
		if !raw && current == '\r' {
			if next == '\n' {
				skip_next = true
				toadd = '\\n'
			}
		}
		result.write_string(toadd)
		last = current
	}
	return result.str()
}
