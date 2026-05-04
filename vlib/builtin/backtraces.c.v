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
		} $else $if tinyc {
			C.tcc_backtrace(c'Backtrace')
		} $else $if use_libbacktrace ? {
			$if openbsd {
				print_backtrace_skipping_top_frames(2)
			} $else {
				// NOTE: TCC doesn't have the unwind library
				print_libbacktrace(1)
			}
		} $else {
			print_backtrace_skipping_top_frames(2)
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

// demangle_backtrace_sym demangles a V symbol within a Linux-style backtrace fragment.
// Linux format: `./executable(symbol_name+0x1a4) `
fn demangle_backtrace_sym(s string) string {
	paren_start := s.index('(') or { return s }
	plus_pos := s.index_after_('+', paren_start)
	if plus_pos < 0 {
		return s
	}
	symbol := s[paren_start + 1..plus_pos]
	if symbol.len == 0 {
		return s
	}
	return s[..paren_start + 1] + demangle_v_symbol(symbol) + s[plus_pos..]
}

fn eprint_space_padding(output string, max_len int) {
	padding_len := max_len - output.len
	if padding_len > 0 {
		for _ in 0 .. padding_len {
			eprint(' ')
		}
	}
}
