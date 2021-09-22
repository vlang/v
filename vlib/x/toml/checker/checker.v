// Copyright (c) 2021 Lars Pontoppidan. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module checker

import x.toml.ast
import x.toml.ast.walker
import x.toml.util
import x.toml.token
import x.toml.scanner

// Checker checks a tree of TOML `ast.Node`'s for common errors.
pub struct Checker {
	scanner &scanner.Scanner
}

pub fn (c Checker) check(n &ast.Node) ? {
	walker.walk(c, n) ?
}

fn (c Checker) visit(node &ast.Node) ? {
	match node {
		ast.Number {
			c.check_number(node) ?
		}
		ast.Bool {
			c.check_boolean(node) ?
		}
		else {
			// println('ok')
		}
	}
	// if node is
	// return error('Hep')
}

// excerpt returns a string of the characters surrounding`
fn (c Checker) excerpt(tp token.Position) string {
	return c.scanner.excerpt(tp.pos, 10)
}

fn (c Checker) check_number(num ast.Number) ? {
	lit := num.text
	if lit in ['0', '0.0', '+0', '-0', '+0.0', '-0.0', '0e0', '+0e0', '-0e0', '0e00'] {
		return
	}
	is_float := lit.contains('.')
	// mut is_first_digit := byte(lit[0]).is_digit()
	mut ascii := byte(lit[0]).ascii_str()
	is_sign_prefixed := lit[0] in [`+`, `-`]
	if is_sign_prefixed { // +/- ...
		n := lit[1..]
		if n.starts_with('0x') || n.starts_with('0o') || n.starts_with('0b') {
			ascii = byte(lit[0]).ascii_str()
			return error(@MOD + '.' + @STRUCT + '.' + @FN +
				' hex, octal and binary numbers can not start with `$ascii` "$lit" in ...${c.excerpt(num.pos)}...')
		}
		// is_first_digit = byte(n[0]).is_digit()
		if lit.len > 1 && n.starts_with('0') {
			ascii = byte(n[0]).ascii_str()
			return error(@MOD + '.' + @STRUCT + '.' + @FN +
				' numbers can not start with `$ascii` "$lit" in ...${c.excerpt(num.pos)}...')
		}
	}

	if is_float {
	} else {
		if lit.len > 1 && lit.starts_with('0') && lit[1] !in [`x`, `o`, `b`] {
			ascii = byte(lit[0]).ascii_str()
			return error(@MOD + '.' + @STRUCT + '.' + @FN +
				' numbers can not start with `$ascii` "$lit" in ...${c.excerpt(num.pos)}...')
		}
	}
}

fn (c Checker) check_boolean(b ast.Bool) ? {
	lit := b.text
	if lit in ['true', 'false'] {
		return
	}
	return error(@MOD + '.' + @STRUCT + '.' + @FN +
		' boolean values can only be `true` or `false` literals, not `$lit` in ...${c.excerpt(b.pos)}...')
}
