// Copyright (c) 2021 Lars Pontoppidan. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module checker

import toml.ast
import toml.ast.walker
// import toml.util
import toml.token
import toml.scanner

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
		ast.Number {
			c.check_number(value) ?
		}
		ast.Bool {
			c.check_boolean(value) ?
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
	is_hex := lit.contains('0x')
	is_float := lit.to_lower().all_before('e').contains('.')
	has_exponent_notation := lit.to_lower().contains('e')
	float_decimal_index := lit.index('.') or { -1 }
	// mut is_first_digit := byte(lit[0]).is_digit()
	mut ascii := byte(lit[0]).ascii_str()
	is_sign_prefixed := lit[0] in [`+`, `-`]
	if is_sign_prefixed { // +/- ...
		n := lit[1..]
		hex_bin_oct = is_hex_bin_oct(n)
		if hex_bin_oct {
			ascii = byte(lit[0]).ascii_str()
			return error(@MOD + '.' + @STRUCT + '.' + @FN +
				' numbers like "$lit" (hex, octal and binary) can not start with `$ascii` in ...${c.excerpt(num.pos)}...')
		}
		// is_first_digit = byte(n[0]).is_digit()
		if lit.len > 1 && n.starts_with('0') {
			ascii = byte(n[0]).ascii_str()
			return error(@MOD + '.' + @STRUCT + '.' + @FN +
				' numbers like "$lit" can not start with `$ascii` in ...${c.excerpt(num.pos)}...')
		}
	} else {
		if !hex_bin_oct {
			if !is_float && lit[0] == `0` {
				return error(@MOD + '.' + @STRUCT + '.' + @FN +
					' numbers like "$lit" can not start with a zero in ...${c.excerpt(num.pos)}...')
			}

			if is_float && lit[0] == `0` && float_decimal_index > 1 {
				return error(@MOD + '.' + @STRUCT + '.' + @FN +
					' numbers like "$lit" can not start with a zero in ...${c.excerpt(num.pos)}...')
			}
		}
	}

	if has_repeating(lit, [`_`, `.`, `x`, `o`, `b`]) {
		return error(@MOD + '.' + @STRUCT + '.' + @FN +
			' numbers like "$lit" can not have $scanner.digit_extras as repeating characters in ...${c.excerpt(num.pos)}...')
	}

	if hex_bin_oct {
		third := lit[2]
		if third in scanner.digit_extras {
			ascii = byte(third).ascii_str()
			return error(@MOD + '.' + @STRUCT + '.' + @FN +
				' numbers like "$lit" (hex, octal and binary) can not have `$ascii` in ...${c.excerpt(num.pos)}...')
		}
	}

	if has_exponent_notation {
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
		if lit.len > 1 && lit.starts_with('0') && lit[1] !in [`x`, `o`, `b`] {
			ascii = byte(lit[0]).ascii_str()
			return error(@MOD + '.' + @STRUCT + '.' + @FN +
				' numbers like "$lit" can not start with `$ascii` in ...${c.excerpt(num.pos)}...')
		}
	}
}

fn (c Checker) check_boolean(b ast.Bool) ? {
	lit := b.text
	if lit in ['true', 'false'] {
		return
	}
	return error(@MOD + '.' + @STRUCT + '.' + @FN +
		' boolean values like "$lit" can only be `true` or `false` literals, not `$lit` in ...${c.excerpt(b.pos)}...')
}
