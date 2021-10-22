// Copyright (c) 2021 Lars Pontoppidan. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module checker

import toml.ast
import toml.ast.walker
// import toml.util
import toml.token
import toml.scanner

pub const allowed_basic_escape_chars = [`u`, `U`, `b`, `t`, `n`, `f`, `r`, `"`, `\\`]

// Checker checks a tree of TOML `ast.Value`'s for common errors.
pub struct Checker {
	scanner &scanner.Scanner
}

// check checks the `ast.Value` and all it's children
// for common errors.
pub fn (c Checker) check(n &ast.Value) ? {
	walker.walk(c, n) ?
}

fn (c Checker) visit(value &ast.Value) ? {
	match value {
		ast.Bool {
			c.check_boolean(value) ?
		}
		ast.Number {
			c.check_number(value) ?
		}
		ast.Quoted {
			c.check_quoted(value) ?
		}
		else {
			// TODO add more checks to make BurntSushi/toml-test invalid TOML pass
		}
	}
}

// excerpt returns a string of the characters surrounding`
fn (c Checker) excerpt(tp token.Position) string {
	return c.scanner.excerpt(tp.pos, 10)
}

fn is_hex_bin_oct(hbo string) bool {
	return hbo.len > 2 && (hbo.starts_with('0x') || hbo.starts_with('0o') || hbo.starts_with('0b'))
}

fn has_repeating(str string, repeats []rune) bool {
	for i, r in str {
		if r in repeats && i + 1 < str.len {
			if r == str[i + 1] {
				return true
			}
		}
	}
	return false
}

fn (c Checker) check_number(num ast.Number) ? {
	lit := num.text
	if lit in ['0', '0.0', '+0', '-0', '+0.0', '-0.0', '0e0', '+0e0', '-0e0', '0e00'] {
		return
	}

	if lit.contains('_') {
		if lit.starts_with('_') || lit.ends_with('_') {
			return error(@MOD + '.' + @STRUCT + '.' + @FN +
				' numbers like "$lit" can not start or end with `_` in ...${c.excerpt(num.pos)}...')
		}
		if lit.contains('__') {
			return error(@MOD + '.' + @STRUCT + '.' + @FN +
				' numbers like "$lit" can not have more than one underscore (`_`) in ...${c.excerpt(num.pos)}...')
		}
	}

	mut hex_bin_oct := is_hex_bin_oct(lit)
	mut is_bin, mut is_oct, mut is_hex := false, false, false
	is_float := lit.to_lower().all_before('e').contains('.')
	has_exponent_notation := lit.to_lower().contains('e')
	float_decimal_index := lit.index('.') or { -1 }
	// mut is_first_digit := byte(lit[0]).is_digit()
	mut ascii := byte(lit[0]).ascii_str()
	is_sign_prefixed := lit[0] in [`+`, `-`]
	mut lit_sans_sign := lit
	if is_sign_prefixed { // +/- ...
		lit_sans_sign = lit[1..]
		hex_bin_oct = is_hex_bin_oct(lit_sans_sign)
		if hex_bin_oct {
			ascii = byte(lit[0]).ascii_str()
			return error(@MOD + '.' + @STRUCT + '.' + @FN +
				' numbers like "$lit" (hex, octal and binary) can not start with `$ascii` in ...${c.excerpt(num.pos)}...')
		}
		// is_first_digit = byte(lit_sans_sign[0]).is_digit()
		if lit.len > 1 && lit_sans_sign.starts_with('0') {
			ascii = byte(lit_sans_sign[0]).ascii_str()
			return error(@MOD + '.' + @STRUCT + '.' + @FN +
				' numbers like "$lit" can not start with `$ascii` in ...${c.excerpt(num.pos)}...')
		}
	} else {
		if !hex_bin_oct {
			if !is_float && lit[0] == `0` {
				if lit[1] in [`B`, `O`, `X`] {
					return error(@MOD + '.' + @STRUCT + '.' + @FN +
						' numbers like "$lit" only lowercase notation in ...${c.excerpt(num.pos)}...')
				}
				return error(@MOD + '.' + @STRUCT + '.' + @FN +
					' numbers like "$lit" can not start with a zero in ...${c.excerpt(num.pos)}...')
			}

			if is_float && lit[0] == `0` && float_decimal_index > 1 {
				return error(@MOD + '.' + @STRUCT + '.' + @FN +
					' numbers like "$lit" can not start with a zero in ...${c.excerpt(num.pos)}...')
			}
		}
	}

	if has_repeating(lit, [`_`, `.`, `b`, `o`, `x`]) {
		return error(@MOD + '.' + @STRUCT + '.' + @FN +
			' numbers like "$lit" can not have $scanner.digit_extras as repeating characters in ...${c.excerpt(num.pos)}...')
	}

	if hex_bin_oct {
		is_bin = lit_sans_sign.starts_with('0b')
		is_oct = lit_sans_sign.starts_with('0o')
		is_hex = lit_sans_sign.starts_with('0x')

		third := lit[2]
		if third in scanner.digit_extras {
			ascii = byte(third).ascii_str()
			return error(@MOD + '.' + @STRUCT + '.' + @FN +
				' numbers like "$lit" (hex, octal and binary) can not have `$ascii` in ...${c.excerpt(num.pos)}...')
		}
		lit_sans_sign_and_type_prefix := lit_sans_sign[2..]

		if lit_sans_sign_and_type_prefix.starts_with('_')
			|| lit_sans_sign_and_type_prefix.ends_with('_') {
			return error(@MOD + '.' + @STRUCT + '.' + @FN +
				' numbers like "$lit" can not start or end with `_` in ...${c.excerpt(num.pos)}...')
		}

		if is_bin {
			if !c.is_valid_binary_literal(lit_sans_sign_and_type_prefix) {
				return error(@MOD + '.' + @STRUCT + '.' + @FN +
					' "$lit" is not a valid binary number in ...${c.excerpt(num.pos)}...')
			}
		} else if is_oct {
			if !c.is_valid_octal_literal(lit_sans_sign_and_type_prefix) {
				return error(@MOD + '.' + @STRUCT + '.' + @FN +
					' "$lit" is not a valid octal number in ...${c.excerpt(num.pos)}...')
			}
		} else {
			if !c.is_valid_hex_literal(lit_sans_sign_and_type_prefix) {
				return error(@MOD + '.' + @STRUCT + '.' + @FN +
					' "$lit" is not a valid hexadecimal number in ...${c.excerpt(num.pos)}...')
			}
		}
	}

	if has_exponent_notation {
		if lit.to_lower().all_after('e').starts_with('_') {
			return error(@MOD + '.' + @STRUCT + '.' + @FN +
				' the exponent in "$lit" can not start with an underscore in ...${c.excerpt(num.pos)}...')
		}
		if lit.to_lower().all_after('e').contains('.') {
			return error(@MOD + '.' + @STRUCT + '.' + @FN +
				' numbers like "$lit" (with exponent) can not have a decimal point in ...${c.excerpt(num.pos)}...')
		}
		if !is_hex && lit.to_lower().count('e') > 1 {
			return error(@MOD + '.' + @STRUCT + '.' + @FN +
				' numbers like "$lit" (with exponent) can only have one exponent in ...${c.excerpt(num.pos)}...')
		}
	}

	if is_float {
		if lit.count('.') > 1 {
			return error(@MOD + '.' + @STRUCT + '.' + @FN +
				' numbers like "$lit" (float) can only have one decimal point in ...${c.excerpt(num.pos)}...')
		}
		last := lit[lit.len - 1]
		if last in scanner.digit_extras {
			ascii = byte(last).ascii_str()
			return error(@MOD + '.' + @STRUCT + '.' + @FN +
				' numbers like "$lit" (float) can not start with `$ascii` in ...${c.excerpt(num.pos)}...')
		}
		if lit.contains('_.') || lit.contains('._') {
			return error(@MOD + '.' + @STRUCT + '.' + @FN +
				' numbers like "$lit" (float) can not have underscores before or after the decimal point in ...${c.excerpt(num.pos)}...')
		}
		if lit.contains('e.') || lit.contains('.e') || lit.contains('E.') || lit.contains('.E') {
			return error(@MOD + '.' + @STRUCT + '.' + @FN +
				' numbers like "$lit" (float) can not have underscores before or after the decimal point in ...${c.excerpt(num.pos)}...')
		}
	} else {
		if lit.len > 1 && lit.starts_with('0') && lit[1] !in [`b`, `o`, `x`] {
			ascii = byte(lit[0]).ascii_str()
			return error(@MOD + '.' + @STRUCT + '.' + @FN +
				' numbers like "$lit" can not start with `$ascii` in ...${c.excerpt(num.pos)}...')
		}
	}
}

fn (c Checker) is_valid_binary_literal(num string) bool {
	for ch in num {
		if ch == `_` {
			continue
		}
		if !(ch >= `0` && ch <= `1`) {
			return false
		}
	}
	return true
}

fn (c Checker) is_valid_octal_literal(num string) bool {
	for ch in num {
		if ch == `_` {
			continue
		}
		if !(ch >= `0` && ch <= `7`) {
			return false
		}
	}
	return true
}

fn (c Checker) is_valid_hex_literal(num string) bool {
	for ch in num {
		if ch == `_` {
			continue
		}
		if !ch.is_hex_digit() {
			return false
		}
	}
	return true
}

fn (c Checker) check_boolean(b ast.Bool) ? {
	lit := b.text
	if lit in ['true', 'false'] {
		return
	}
	return error(@MOD + '.' + @STRUCT + '.' + @FN +
		' boolean values like "$lit" can only be `true` or `false` literals, not `$lit` in ...${c.excerpt(b.pos)}...')
}

fn (c Checker) check_quoted(q ast.Quoted) ? {
	lit := q.text
	quote := q.quote.ascii_str()
	triple_quote := quote + quote + quote
	if q.is_multiline && lit.ends_with(triple_quote) {
		return error(@MOD + '.' + @STRUCT + '.' + @FN +
			' string values like "$lit" is has unbalanced quote literals `q.quote` in ...${c.excerpt(q.pos)}...')
	}
	c.check_quoted_escapes(q) ?
}

// check_quoted_escapes returns an error for any disallowed escape sequences.
// Delimiters in TOML has significant meaning:
// '/''' delimits *literal* strings (WYSIWYG / What-you-see-is-what-you-get)
// "/""" delimits *basic* strings
// Allowed escapes in *basic* strings are:
// \b         - backspace       (U+0008)
// \t         - tab             (U+0009)
// \n         - linefeed        (U+000A)
// \f         - form feed       (U+000C)
// \r         - carriage return (U+000D)
// \"         - quote           (U+0022)
// \\         - backslash       (U+005C)
// \uXXXX     - unicode         (U+XXXX)
// \UXXXXXXXX - unicode         (U+XXXXXXXX)
fn (c Checker) check_quoted_escapes(q ast.Quoted) ? {
	// Setup a scanner in stack memory for easier navigation.
	mut s := scanner.new_simple(q.text) ?

	is_basic := q.quote == `\"`
	for {
		ch := s.next()
		if ch == -1 {
			break
		}
		ch_byte := byte(ch)
		if ch == `\\` {
			next_ch := byte(s.at())

			if next_ch == `\\` {
				s.next()
				continue
			}
			escape := ch_byte.ascii_str() + next_ch.ascii_str()
			if is_basic {
				if q.is_multiline {
					if next_ch == byte(32) && s.peek(1) == byte(92) {
						st := s.state()
						return error(@MOD + '.' + @STRUCT + '.' + @FN +
							' can not escape whitespaces before escapes in multi-line strings (`\\ \\`) at `$escape` ($st.line_nr,$st.col) in ...${c.excerpt(q.pos)}...')
					}
					if next_ch in [`\t`, `\n`, ` `] {
						s.next()
						continue
					}
				}
				if next_ch !in checker.allowed_basic_escape_chars {
					st := s.state()
					return error(@MOD + '.' + @STRUCT + '.' + @FN +
						' unknown basic string escape character `$next_ch.ascii_str()` in `$escape` ($st.line_nr,$st.col) in ...${c.excerpt(q.pos)}...')
				}
			}
		}
	}
}
