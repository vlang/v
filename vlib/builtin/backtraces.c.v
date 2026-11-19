module builtin

// print_backtrace shows a backtrace of the current call stack on stdout.
pub fn print_backtrace() {
	// At the time of backtrace_symbols_fd call, the C stack would look something like this:
	// * print_backtrace_skipping_top_frames
	// * print_backtrace itself
	// * the rest of the backtrace frames
	// => top 2 frames should be skipped, since they will not be informative to the developer
	$if !no_backtrace ? {
		$if freestanding {
			println(bare_backtrace())
		} $else $if tinyc && !macos {
			C.tcc_backtrace(c'Backtrace')
		} $else $if use_libbacktrace ?&& !tinyc {
			// TinyCC has no unwind library, so its print_libbacktrace is an empty stub.
			$if openbsd {
				print_backtrace_skipping_top_frames(2)
			} $else {
				// NOTE: TCC doesn't have the unwind library
				print_libbacktrace(1)
			}
		} $else {
			$if windows {
				print_backtrace_skipping_top_frames(2)
			} $else {
				// The nix implementations already skip their own two frames, so only
				// print_backtrace itself is left; the caller of print_backtrace stays.
				print_backtrace_skipping_top_frames(1)
			}
		}
	}
}

// demangle_v_symbol converts a C-mangled V symbol name to a human-readable V name.
// For example: `veb__run_T_main__App_main__Context` => `veb.run[main.App, main.Context]`
@[direct_array_access]
fn demangle_v_symbol(cname string) string {
	mut name := cname
	// Strip builtin__ prefix:
	if name.starts_with('builtin__') {
		name = name[9..]
	}
	// Replace pointer type encoding:
	name = name.replace('__ptr__', '&')
	// Split base name from generic parameters on `_T_`:
	// C mangling: `base_T_type1_type2` where each type is prefixed with `_`
	// and types may contain `__` for module separators.
	t_pos := name.index('_T_') or { -1 }
	if t_pos >= 0 {
		base := name[..t_pos].replace('__', '.')
		generic_suffix := name[t_pos + 3..]
		// Split on single `_` that is not part of `__` (double underscore).
		params := split_generic_params(generic_suffix)
		mut demangled_params := []string{cap: params.len}
		for param in params {
			demangled_params << param.replace('__', '.')
		}
		return base + '[' + demangled_params.join(', ') + ']'
	}
	// No generics - just replace module separator:
	name = name.replace('__', '.')
	// `main.main` is V's internal C mangling, in V it's just `main`:
	if name == 'main.main' {
		return 'main'
	}
	return name
}

// split_generic_params splits a generic suffix like `main__App_main__Context`
// into individual type parameters [`main__App`, `main__Context`].
// Splits on single `_` but not `__` (double underscore used for module separators).
@[direct_array_access]
fn split_generic_params(s string) []string {
	mut params := []string{}
	mut start := 0
	mut i := 0
	for i < s.len {
		if s[i] == `_` {
			if i + 1 < s.len && s[i + 1] == `_` {
				// Part of `__` (module separator), skip both.
				i += 2
			} else {
				// Single `_` is a generic param separator.
				if i > start {
					params << s[start..i]
				}
				i++
				start = i
			}
		} else {
			i++
		}
	}
	if start < s.len {
		params << s[start..]
	}
	return params
}

// eprint_backtrace_frame prints one stack frame the way Go does: the demangled
// V function on one line, then its .v `file:line`, when known, indented below it.
// It demangles like demangle_v_symbol, but into a stack buffer, so that printing
// a backtrace does not allocate.
@[direct_array_access]
fn eprint_backtrace_frame(symbol string, file_line string) {
	mut buf := [256]u8{}
	mut n := 0
	mut i := if symbol.starts_with('builtin__') { 9 } else { 0 }
	mut in_generic_params := false
	for i < symbol.len && n < buf.len - 4 {
		c := symbol[i]
		if c == `_` && backtrace_symbol_has_at(symbol, i, '__ptr__') {
			buf[n] = `&`
			n++
			i += 7
		} else if c == `_` && backtrace_symbol_has_at(symbol, i, '__') {
			buf[n] = `.`
			n++
			i += 2
		} else if c == `_` && !in_generic_params && backtrace_symbol_has_at(symbol, i, '_T_') {
			// `base_T_type1_type2` => `base[type1, type2]`
			buf[n] = `[`
			n++
			i += 3
			in_generic_params = true
		} else if c == `_` && in_generic_params {
			buf[n] = `,`
			buf[n + 1] = ` `
			n += 2
			i++
		} else {
			buf[n] = c
			n++
			i++
		}
	}
	if in_generic_params {
		buf[n] = `]`
		n++
	}
	buf[n] = `(`
	buf[n + 1] = `)`
	n += 2
	// buf holds n initialized bytes of the demangled name.
	eprintln(unsafe { tos(&buf[0], n) })
	if file_line.len > 0 {
		eprint('\t')
		eprintln(file_line)
	}
}

@[direct_array_access]
fn backtrace_symbol_has_at(symbol string, pos int, part string) bool {
	if pos + part.len > symbol.len {
		return false
	}
	for j in 0 .. part.len {
		if symbol[pos + j] != part[j] {
			return false
		}
	}
	return true
}

fn eprint_space_padding(output string, max_len int) {
	padding_len := max_len - output.len
	if padding_len > 0 {
		for _ in 0 .. padding_len {
			eprint(' ')
		}
	}
}
