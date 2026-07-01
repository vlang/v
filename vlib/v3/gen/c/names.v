module c

import strings

// c_reserved_words is a set (not a list) so `name in c_reserved_words` is an O(1) hash
// lookup. c_name() runs on every emitted identifier, so a linear scan here is costly.
const c_reserved_words = {
	'auto':     true
	'break':    true
	'case':     true
	'char':     true
	'asm':      true
	'const':    true
	'continue': true
	'copy':     true
	'default':  true
	'do':       true
	'double':   true
	'else':     true
	'enum':     true
	'extern':   true
	'float':    true
	'for':      true
	'goto':     true
	'if':       true
	'inline':   true
	'int':      true
	'long':     true
	'register': true
	'restrict': true
	'return':   true
	'short':    true
	'signed':   true
	'sizeof':   true
	'static':   true
	'struct':   true
	'switch':   true
	'typedef':  true
	'false':    true
	'typeof':   true
	'stdin':    true
	'stderr':   true
	'stdout':   true
	'true':     true
	'union':    true
	'unsigned': true
	'void':     true
	'volatile': true
	'while':    true
	'unix':     true
}

// c_libc_collisions are libc function names that are not C keywords but clash at
// link/declaration time when a user defines a plain (module `main`) function with
// the same name (e.g. `fn rint(...)` vs libc's `double rint(double)`). They are
// mangled to `v_<name>` consistently at definition and call sites. `C.<name>`
// calls are unaffected (the `C.` prefix is stripped before this check).
const c_libc_collisions = {
	'abort':    true
	'access':   true
	'acos':     true
	'atexit':   true
	'ceil':     true
	'ceilf':    true
	'close':    true
	'cos':      true
	'drem':     true
	'dup2':     true
	'execlp':   true
	'execvp':   true
	'fabs':     true
	'fcntl':    true
	'floor':    true
	'floorf':   true
	'fmod':     true
	'fork':     true
	'getenv':   true
	'j0':       true
	'j1':       true
	'jn':       true
	'ldexp':    true
	'memcmp':   true
	'memcpy':   true
	'memmove':  true
	'memset':   true
	'open':     true
	'pipe':     true
	'pow':      true
	'read':     true
	'realpath': true
	'rint':     true
	'scalb':    true
	'setenv':   true
	'signal':   true
	'snprintf': true
	'sqrt':     true
	'strcmp':   true
	'strlen':   true
	'strncmp':  true
	'strncpy':  true
	'strrchr':  true
	'strstr':   true
	'y0':       true
	'y1':       true
	'yn':       true
}

// c_name converts c name data for c.
fn c_name(name string) string {
	if name.starts_with('C.') {
		return name[2..]
	}
	if name == 'malloc' {
		return 'v_malloc'
	}
	if name == 'int_str' {
		return 'int__str'
	}
	// The V builtin `exit` wraps `C.exit`; both lower to the C symbol `exit`.
	// Rename the V function (and its call sites) to `v_exit` so its body's
	// `C.exit(code)` call resolves to libc `exit` instead of recursing forever.
	// `C.exit` itself is handled by the `C.` strip above, so it stays `exit`.
	if name == 'exit' {
		return 'v_exit'
	}
	if c_name_is_plain(name) {
		if name in c_reserved_words || name in c_libc_collisions {
			return 'v_${name}'
		}
		return name
	}
	n := c_name_sanitize(name)
	if n in c_reserved_words || n in c_libc_collisions {
		if name.contains('@') {
			return '_v_${n}'
		}
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
				if next == `*` {
					b.write_string('__mul')
					i += 2
					continue
				}
				if next == `/` {
					b.write_string('__div')
					i += 2
					continue
				}
				if next == `%` {
					b.write_string('__mod')
					i += 2
					continue
				}
				if next == `&` {
					b.write_string('__and')
					i += 2
					continue
				}
				if next == `|` {
					b.write_string('__or')
					i += 2
					continue
				}
				if next == `^` {
					b.write_string('__xor')
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
					if next == `<` && op == `<` {
						b.write_string('__left_shift')
						i += 3
						continue
					}
					if next == `>` && op == `>` {
						b.write_string('__right_shift')
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
		} else if c == `@` {
			i++
			continue
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

fn c_local_name(name string) string {
	local_name := if name.contains('.') { name.all_after_last('.') } else { name }
	return c_name(local_name)
}

// c_escape supports c escape handling for c.
fn c_escape(s string) string {
	return s.replace('\\', '\\\\').replace('"', '\\"').replace('\n', '\\n').replace('\t', '\\t').replace('\r',
		'\\r')
}
