module naming

import strings

// Keep this marker synchronized with v.flat's static type-method name codec.
const static_type_method_name_marker = '@static@'
const internal_symbol_c_prefix = '__v3_internal_symbol_'

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
	'clock':    true
	'connect':  true
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
	'index':    true
	'j0':       true
	'j1':       true
	'jn':       true
	'ldexp':    true
	'listen':   true
	'log':      true
	'memcmp':   true
	'memcpy':   true
	'memmove':  true
	'memset':   true
	'open':     true
	'pipe':     true
	'pow':      true
	'printf':   true
	'raise':    true
	'read':     true
	'realpath': true
	'rint':     true
	'round':    true
	'scalb':    true
	'select':   true
	'send':     true
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
	'wait':     true
	'y0':       true
	'y1':       true
	'yn':       true
}

// c_name returns the C identifier used for a V symbol or type name.
pub fn c_name(name string) string {
	if name.contains(static_type_method_name_marker) {
		return static_type_method_c_name(name)
	}
	if name.starts_with('C.') {
		if name[2..].contains('.') {
			return sanitize(name)
		}
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
	n := sanitize(name)
	mut result := n
	if n in reserved_words || n in libc_collisions || is_string_literal_symbol(n) {
		if name.contains('@') {
			result = '_v_${n}'
		} else {
			result = 'v_${n}'
		}
	}
	// Keep the C namespace reserved for internal symbols disjoint from every
	// spelling that a source-level name can sanitize to.
	if result.starts_with(internal_symbol_c_prefix) {
		return '${internal_symbol_c_prefix}source_${result}'
	}
	return result
}

// c_name_needs_internal_namespace reports names that cannot use cgen's direct
// identifier fast paths because c_name applies the internal-symbol partition.
pub fn c_name_needs_internal_namespace(name string) bool {
	return name.contains(static_type_method_name_marker)
		|| name.starts_with(internal_symbol_c_prefix)
}

fn static_type_method_c_name(name string) string {
	hex := '0123456789abcdef'
	mut b := strings.new_builder(internal_symbol_c_prefix.len + 7 + name.len * 2)
	b.write_string(internal_symbol_c_prefix)
	b.write_string('static_')
	for c in name.bytes() {
		b.write_u8(hex[c >> 4])
		b.write_u8(hex[c & 15])
	}
	return b.str()
}

fn is_string_literal_symbol(name string) bool {
	if name.len <= 5 || !name.starts_with('_str_') {
		return false
	}
	for i in 5 .. name.len {
		if name[i] < `0` || name[i] > `9` {
			return false
		}
	}
	return true
}

// sanitize converts a V symbol or type spelling into a C identifier spelling
// without applying reserved-word or libc collision prefixes.
@[direct_array_access]
pub fn sanitize(name string) string {
	mut dot_count := 0
	for i in 0 .. name.len {
		c := name[i]
		if (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`) || (c >= `0` && c <= `9`) || c == `_` {
			continue
		}
		if c != `.` {
			return sanitize_complex(name)
		}
		dot_count++
	}
	if dot_count == 0 {
		return name
	}
	out_len := name.len + dot_count
	// The returned string owns a standalone allocation, not managed array storage.
	mut out := unsafe { malloc_noscan(out_len + 1) }
	mut dst := 0
	for i in 0 .. name.len {
		c := name[i]
		if c == `.` {
			unsafe {
				out[dst] = `_`
				out[dst + 1] = `_`
			}
			dst += 2
		} else {
			unsafe { out[dst] = c }
			dst++
		}
	}
	unsafe { out[out_len] = 0 }
	return unsafe { tos(out, out_len) }
}

fn sanitize_complex(name string) string {
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
				if next == `[` && i + 2 < name.len && name[i + 2] == `]` {
					if i + 3 < name.len && name[i + 3] == `=` {
						b.write_string('__op_index_set')
						i += 4
						continue
					}
					b.write_string('__op_index')
					i += 3
					continue
				}
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
		} else if c == `?` {
			b.write_string('Optional_')
		} else if c == `!` {
			b.write_string('Result_')
		} else if c == `@` {
			b.write_string('_v_')
		} else if c == `,` || c == ` ` {
			b.write_u8(`_`)
		} else if (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`)
			|| (c >= `0` && c <= `9`) || c == `_` {
			b.write_u8(c)
		} else {
			b.write_u8(`_`)
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

// local_rename returns the name a local is emitted under when it cannot keep its own.
// A suffix would not be enough: a function may have both `array` and `array__local`
// of its own, and a suffix would bring the two together under one name. Nothing a
// source name sanitizes to begins with this prefix -- c_name pushes anything that
// tries into its `source_` namespace -- so nothing can arrive here by accident.
pub fn local_rename(cname string) string {
	return '${internal_symbol_c_prefix}local_${cname}'
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

// fn_ptr_type_name returns the stable C typedef name for an encoded function-pointer signature.
pub fn fn_ptr_type_name(encoded string) string {
	mut hash := u64(1469598103934665603)
	for c in encoded.bytes() {
		hash = (hash ^ u64(c)) * u64(1099511628211)
	}
	return '_fn_ptr_${hash.hex()}'
}

// fn_ptr_encoded returns the internal `fn_ptr:ret|params` key of a function-pointer
// signature from the C spellings of its return type and parameter types. A part that
// is itself a `fn_ptr:` key is wrapped in parentheses (see fn_ptr_encoded_part), so
// its own `|` and `, ` separators cannot merge into the enclosing signature.
pub fn fn_ptr_encoded(ret string, params []string) string {
	mut sb := strings.new_builder(32)
	sb.write_string('fn_ptr:')
	sb.write_string(fn_ptr_encoded_part(ret))
	sb.write_u8(`|`)
	if params.len == 0 {
		sb.write_string('void')
	}
	for i, param in params {
		if i > 0 {
			sb.write_string(', ')
		}
		sb.write_string(fn_ptr_encoded_part(param))
	}
	return sb.str()
}

// fn_ptr_encoded_part spells a return or parameter C type the way it is embedded in
// an enclosing `fn_ptr:ret|params` key. A nested `fn_ptr:` key is parenthesized:
// spliced in as is, `fn (int, fn (int, int) int)` and `fn (int, fn (int) int, int)`
// would both read `fn_ptr:void|i64, fn_ptr:i64|i64, i64`.
pub fn fn_ptr_encoded_part(ct string) string {
	if ct.starts_with('fn_ptr:') {
		return '(${ct})'
	}
	return ct
}

// fn_ptr_encoded_unwrap undoes fn_ptr_encoded_part for one return or parameter part.
pub fn fn_ptr_encoded_unwrap(part string) string {
	clean := part.trim_space()
	if clean.len > 2 && clean[0] == `(` && clean[clean.len - 1] == `)`
		&& clean[1..].starts_with('fn_ptr:') {
		return clean[1..clean.len - 1]
	}
	return clean
}

// fn_ptr_encoded_split splits a `fn_ptr:ret|params` key into its return type (with a
// nested `fn_ptr:` key unwrapped) and its raw parameter list (see fn_ptr_encoded_params).
// A key without a parameter list yields `void` parameters.
pub fn fn_ptr_encoded_split(encoded string) (string, string) {
	payload := if encoded.starts_with('fn_ptr:') { encoded['fn_ptr:'.len..] } else { encoded }
	mut depth := 0
	for i in 0 .. payload.len {
		match payload[i] {
			`(` {
				depth++
			}
			`)` {
				depth--
			}
			`|` {
				if depth == 0 {
					return fn_ptr_encoded_unwrap(payload[..i]), payload[i + 1..]
				}
			}
			else {}
		}
	}
	return fn_ptr_encoded_unwrap(payload), 'void'
}

// fn_ptr_encoded_params splits the raw parameter list of a `fn_ptr:ret|params` key into
// its parameter C types, keeping each parenthesized nested `fn_ptr:` key in one piece
// and unwrapping it. A `void` (or empty) list yields no parameters.
pub fn fn_ptr_encoded_params(params string) []string {
	clean := params.trim_space()
	if clean.len == 0 || clean == 'void' {
		return []string{}
	}
	mut out := []string{}
	mut depth := 0
	mut start := 0
	for i in 0 .. clean.len {
		match clean[i] {
			`(` {
				depth++
			}
			`)` {
				depth--
			}
			`,` {
				if depth == 0 {
					out << fn_ptr_encoded_unwrap(clean[start..i])
					start = i + 1
				}
			}
			else {}
		}
	}
	out << fn_ptr_encoded_unwrap(clean[start..])
	return out
}
