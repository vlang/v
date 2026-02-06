// Copyright (c) 2025 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module util

const d_sig = "\$d('"

// resolve_d_value replaces all occurrences of `$d('ident','value')`
// in `str` with either the default `'value'` param or a compile value passed via `-d ident=value`.
pub fn resolve_d_value(compile_values map[string]string, str string) !string {
	start := str.index(d_sig) or { return error('no "${d_sig}...\')" could be found in "${str}"') }
	mut i := 0
	mut ch := u8(`.`)
	mut bd_ident := []u8{cap: 20}
	mut blevel := 1
	for i = start + d_sig.len; i < str.len && ch != `'`; i++ {
		ch = str[i]
		if ch == `)` {
			blevel--
		} else if ch == `(` {
			blevel++
		}
		if ch.is_letter() || ch.is_digit() || ch == `_` {
			bd_ident << ch
		} else {
			if !(ch == `'`) {
				if ch == `$` {
					return error('cannot use string interpolation in compile time \$d() expression')
				}
				return error('invalid `\$d` identifier in "${str}", invalid character `${rune(ch)}`')
			}
		}
	}
	d_ident := bd_ident.bytestr().trim_space()
	if d_ident == '' {
		return error('first argument of `\$d` must be a string identifier')
	}

	// At this point we should have a valid identifier in `d_ident`.
	// Next we parse out the default string value.

	// Advance past the `,` and the opening `'` in second argument, or ... eat whatever is there:
	for ; i < str.len; i++ {
		ch = str[i]
		match ch {
			` `, `,` {
				continue
			}
			`'` {
				i++
			}
			else {}
		}
		break
	}
	// Rinse, repeat for the expected default value string
	ch = `.`
	dv_start := i
	mut dv_end := i
	for i < str.len {
		ch = str[i]
		dv_end++
		i++
		match ch {
			`'` {
				break
			}
			`(` {
				blevel++
			}
			`)` {
				blevel--
				if blevel <= 0 {
					break
				}
			}
			`$` {
				return error('cannot use string interpolation in compile time \$d() expression')
			}
			else {}
		}
	}
	if dv_end - dv_start == 0 {
		return error('second argument of `\$d` must be a pure literal')
	}
	for ; blevel > 0 && i < str.len; i++ {
		if str[i] == `)` {
			i++
			break
		}
	}
	d_default_value := str#[dv_start..dv_end - 1].trim_space() // last character is the closing `)`
	// at this point we have the identifier and the default value.
	// now we need to resolve which one to use from `compile_values`.
	d_value := compile_values[d_ident] or { d_default_value }
	original_expr_to_be_replaced := str#[start..i]
	if original_expr_to_be_replaced[original_expr_to_be_replaced.len - 1] != `)` {
		panic('the last character of `${original_expr_to_be_replaced}` should be `)`')
	}
	rep := str.replace_once(original_expr_to_be_replaced, d_value)
	if original_expr_to_be_replaced.len > 0 && rep.contains(d_sig) {
		// if more `$d()` calls remains, resolve those as well:
		return resolve_d_value(compile_values, rep)
	}
	return rep
}
