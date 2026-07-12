module naming

import strings

// reserved_words is a set (not a list) so `name in reserved_words` is an O(1)
// hash lookup. c_name() runs on every emitted identifier, so a linear scan here
// is costly.
const reserved_words = {
	'auto':     true
	'break':    true
	'case':     true
	'char':     true
	'asm':      true
	'const':    true
	'continue': true
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

// libc_collisions are libc function names that are not C keywords but clash at
// link/declaration time when a user defines a plain (module `main`) function with
// the same name (e.g. `fn rint(...)` vs libc's `double rint(double)`). They are
// mangled to `v_<name>` consistently at definition and call sites. `C.<name>`
// calls are unaffected (the `C.` prefix is stripped before this check).
const libc_collisions = {
	'abort':    true
	'abs':      true
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

// c_name returns the C identifier used for a V symbol or type name.
pub fn c_name(name string) string {
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
	if is_plain_identifier(name) {
		if name in reserved_words || name in libc_collisions {
			return 'v_${name}'
		}
		return name
	}
	n := sanitize(name)
	if n in reserved_words || n in libc_collisions {
		if name.contains('@') {
			return '_v_${n}'
		}
		return 'v_${n}'
	}
	return n
}

// sanitize converts a V symbol or type spelling into a C identifier spelling
// without applying reserved-word or libc collision prefixes.
pub fn sanitize(name string) string {
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
			b.write_string('_v_')
		} else if c == `,` || c == ` ` {
			b.write_u8(`_`)
		} else {
			b.write_u8(c)
		}
		i++
	}
	return b.str()
}

// is_plain_identifier reports whether name already contains only C identifier
// characters.
pub fn is_plain_identifier(name string) bool {
	for i in 0 .. name.len {
		c := name[i]
		if (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`) || (c >= `0` && c <= `9`) || c == `_` {
			continue
		}
		return false
	}
	return true
}

// is_reserved_word reports whether name needs a prefix to avoid a C reserved word.
pub fn is_reserved_word(name string) bool {
	return name in reserved_words
}

// is_libc_collision reports whether name needs a prefix to avoid a libc symbol.
pub fn is_libc_collision(name string) bool {
	return name in libc_collisions
}

// type_name_part turns a C type or length expression into a fragment that is safe
// to embed inside a C identifier: `*` becomes `ptr` (so pointer payloads stay
// distinguishable), and every other character that is not a letter, digit, or `_`
// becomes `_`. This keeps const-expression fixed-array lengths (e.g. `segs + 1`)
// and pointer return types (`Foo*`) from producing invalid identifiers such as
// `Array_fixed_f32_segs_+_1` or `__v_thread_arr_wait_Foo*`.
pub fn type_name_part(s string) string {
	mut b := []u8{cap: s.len + 2}
	for i := 0; i < s.len; i++ {
		c := s[i]
		if c == `*` {
			b << `p`
			b << `t`
			b << `r`
		} else if (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`)
			|| (c >= `0` && c <= `9`) || c == `_` {
			b << c
		} else {
			b << `_`
		}
	}
	return b.bytestr()
}
