module yaml

fn (a Any) value_(current Any, key []string) ?Any {
	if key.len == 0 {
		return none
	}
	k, index := parse_array_key(key[0])
	value := match current {
		[]Any {
			if k != '' {
				return none
			}
			current[index] or { return none }
		}
		map[string]Any {
			v := current[k] or { return none }
			if index > -1 {
				if v is []Any {
					v[index] or { return none }
				} else {
					return none
				}
			} else {
				v
			}
		}
		else {
			return none
		}
	}

	if key.len <= 1 {
		return value
	}
	return match value {
		[]Any, map[string]Any { a.value_(value, key[1..]) }
		else { none }
	}
}

fn parse_dotted_key(key string) ![]string {
	mut out := []string{}
	mut buf := ''
	mut in_string := false
	mut delimiter := u8(` `)
	for ch in key {
		if ch in [`"`, `'`] {
			if !in_string {
				delimiter = ch
				in_string = true
				continue
			}
			if ch == delimiter {
				in_string = false
				if buf != '' {
					out << buf
				}
				buf = ''
				delimiter = ` `
				continue
			}
		}
		buf += ch.ascii_str()
		if !in_string && ch == `.` {
			buf = buf[..buf.len - 1]
			if buf != '' {
				out << buf
			}
			buf = ''
		}
	}
	if buf != '' {
		out << buf
	}
	if in_string {
		return error('yaml: missing closing string delimiter `${delimiter.ascii_str()}`')
	}
	return out
}

fn parse_array_key(key string) (string, int) {
	mut index := -1
	mut k := key
	if k.contains('[') {
		index = k.all_after('[').all_before(']').int()
		if k.starts_with('[') {
			k = ''
		} else {
			k = k.all_before('[')
		}
	}
	return k, index
}
