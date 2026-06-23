module c

import strings

const c_reserved_words = ['auto', 'break', 'case', 'char', 'const', 'continue', 'copy', 'default',
	'do', 'double', 'else', 'enum', 'extern', 'float', 'for', 'goto', 'if', 'inline', 'int', 'long',
	'register', 'restrict', 'return', 'short', 'signed', 'sizeof', 'static', 'struct', 'switch',
	'typedef', 'union', 'unsigned', 'void', 'volatile', 'while']

// c_name converts c name data for c.
fn c_name(name string) string {
	if name.starts_with('C.') {
		return name[2..]
	}
	if name == 'malloc' {
		return 'v_malloc'
	}
	// The V builtin `exit` wraps `C.exit`; both lower to the C symbol `exit`.
	// Rename the V function (and its call sites) to `v_exit` so its body's
	// `C.exit(code)` call resolves to libc `exit` instead of recursing forever.
	// `C.exit` itself is handled by the `C.` strip above, so it stays `exit`.
	if name == 'exit' {
		return 'v_exit'
	}
	if c_name_is_plain(name) {
		if name in c_reserved_words {
			return 'v_${name}'
		}
		return name
	}
	n := c_name_sanitize(name)
	if n in c_reserved_words {
		return 'v_${n}'
	}
	return n
}

// c_name_sanitize converts c name sanitize data for c.
fn c_name_sanitize(name string) string {
	mut b := strings.new_builder(name.len + 8)
	mut i := 0
	for i < name.len {
		c := name[i]
		if c == `[` {
			if i + 1 < name.len && name[i + 1] == `]` {
				b.write_string('Array_')
				i += 2
				continue
			}
			b.write_u8(`_`)
		} else if c == `]` {
			i++
			continue
		} else if c == `.` {
			if i + 1 < name.len {
				next := name[i + 1]
				if next == `-` {
					b.write_string('__minus')
					i += 2
					continue
				}
				if next == `+` {
					b.write_string('__plus')
					i += 2
					continue
				}
				if i + 2 < name.len {
					op := name[i + 2]
					if next == `=` && op == `=` {
						b.write_string('__eq')
						i += 3
						continue
					}
					if next == `!` && op == `=` {
						b.write_string('__ne')
						i += 3
						continue
					}
					if next == `<` && op == `=` {
						b.write_string('__le')
						i += 3
						continue
					}
					if next == `>` && op == `=` {
						b.write_string('__ge')
						i += 3
						continue
					}
				}
				if next == `<` {
					b.write_string('__lt')
					i += 2
					continue
				}
				if next == `>` {
					b.write_string('__gt')
					i += 2
					continue
				}
			}
			b.write_string('__')
		} else if c == `&` {
			b.write_string('ptr')
		} else if c == `,` || c == ` ` {
			b.write_u8(`_`)
		} else {
			b.write_u8(c)
		}
		i++
	}
	return b.str()
}

// c_name_is_plain converts c name is plain data for c.
fn c_name_is_plain(name string) bool {
	for i in 0 .. name.len {
		c := name[i]
		if (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`) || (c >= `0` && c <= `9`) || c == `_` {
			continue
		}
		return false
	}
	return true
}

// c_escape supports c escape handling for c.
fn c_escape(s string) string {
	return s.replace('\\', '\\\\').replace('"', '\\"').replace('\n', '\\n').replace('\t', '\\t').replace('\r',
		'\\r')
}
