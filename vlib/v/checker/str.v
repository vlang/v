// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module checker

import v.ast
import v.token

pub fn (mut c Checker) get_default_fmt(ftyp ast.Type, typ ast.Type) u8 {
	if ftyp.has_flag(.optional) || ftyp.has_flag(.result) {
		return `s`
	} else if typ.is_float() {
		return `g`
	} else if typ.is_signed() || typ.is_int_literal() {
		return `d`
	} else if typ.is_unsigned() {
		return `u`
	} else if typ.is_pointer() {
		return `p`
	} else {
		mut sym := c.table.sym(c.unwrap_generic(ftyp))
		if sym.kind == .alias {
			// string aliases should be printable
			info := sym.info as ast.Alias
			sym = c.table.sym(info.parent_type)
			if info.parent_type == ast.string_type {
				return `s`
			}
		}
		if sym.kind == .function {
			return `s`
		}
		if ftyp in [ast.string_type, ast.bool_type]
			|| sym.kind in [.enum_, .array, .array_fixed, .struct_, .map, .multi_return, .sum_type, .interface_, .none_]
			|| ftyp.has_flag(.optional) || sym.has_method('str') {
			return `s`
		} else {
			return `_`
		}
	}
}

pub fn (mut c Checker) string_inter_lit(mut node ast.StringInterLiteral) ast.Type {
	inside_println_arg_save := c.inside_println_arg
	c.inside_println_arg = true
	for i, expr in node.exprs {
		ftyp := c.expr(expr)
		if ftyp == ast.void_type {
			c.error('expression does not return a value', expr.pos())
		} else if ftyp == ast.char_type && ftyp.nr_muls() == 0 {
			c.error('expression returning type `char` cannot be used in string interpolation directly, print its address or cast it to an integer instead',
				expr.pos())
		}
		c.fail_if_unreadable(expr, ftyp, 'interpolation object')
		node.expr_types << ftyp
		ftyp_sym := c.table.sym(ftyp)
		typ := if ftyp_sym.kind == .alias && !ftyp_sym.has_method('str') {
			c.table.unalias_num_type(ftyp)
		} else {
			ftyp
		}
		mut fmt := node.fmts[i]
		// analyze and validate format specifier
		if fmt !in [`E`, `F`, `G`, `e`, `f`, `g`, `d`, `u`, `x`, `X`, `o`, `c`, `s`, `S`, `p`,
			`b`, `_`] {
			c.error('unknown format specifier `${fmt:c}`', node.fmt_poss[i])
		}
		if fmt == `_` { // set default representation for type if none has been given
			fmt = c.get_default_fmt(ftyp, typ)
			if fmt == `_` {
				if typ != ast.void_type {
					c.error('no known default format for type `${c.table.get_type_name(ftyp)}`',
						node.fmt_poss[i])
				}
			} else {
				node.fmts[i] = fmt
				node.need_fmts[i] = false
			}
		} else { // check if given format specifier is valid for type
			if node.precisions[i] != 987698 && !typ.is_float() {
				c.error('precision specification only valid for float types', node.fmt_poss[i])
			}
			if node.pluss[i] && !typ.is_number() {
				c.error('plus prefix only allowed for numbers', node.fmt_poss[i])
			}
			if (typ.is_unsigned() && fmt !in [`u`, `x`, `X`, `o`, `c`, `b`])
				|| (typ.is_signed() && fmt !in [`d`, `x`, `X`, `o`, `c`, `b`])
				|| (typ.is_int_literal()
				&& fmt !in [`d`, `c`, `x`, `X`, `o`, `u`, `x`, `X`, `o`, `b`])
				|| (typ.is_float() && fmt !in [`E`, `F`, `G`, `e`, `f`, `g`])
				|| (typ.is_pointer() && fmt !in [`p`, `x`, `X`])
				|| (typ.is_string() && fmt !in [`s`, `S`])
				|| (typ.idx() in [ast.i64_type_idx, ast.f64_type_idx] && fmt == `c`) {
				c.error('illegal format specifier `${fmt:c}` for type `${c.table.get_type_name(ftyp)}`',
					node.fmt_poss[i])
			}
			node.need_fmts[i] = fmt != c.get_default_fmt(ftyp, typ)
		}
		// check recursive str
		if c.table.cur_fn.is_method && c.table.cur_fn.name == 'str'
			&& c.table.cur_fn.receiver.name == expr.str() {
			c.error('cannot call `str()` method recursively', expr.pos())
		}
	}
	c.inside_println_arg = inside_println_arg_save
	return ast.string_type
}

const unicode_lit_overflow_message = 'unicode character exceeds max allowed value of 0x10ffff, consider using a unicode literal (\\u####)'

// unicode character literals are limited to a maximum value of 0x10ffff
// https://stackoverflow.com/questions/52203351/why-unicode-is-restricted-to-0x10ffff
pub fn (mut c Checker) string_lit(mut node ast.StringLiteral) ast.Type {
	mut idx := 0
	for idx < node.val.len {
		match node.val[idx] {
			`\\` {
				mut start_pos := token.Pos{
					...node.pos
					col: node.pos.col + 1 + idx
				}
				start_idx := idx
				idx++
				next_ch := node.val[idx] or { return ast.string_type }
				if next_ch == `u` {
					idx++
					mut ch := node.val[idx] or { return ast.string_type }
					mut hex_char_count := 0
					for ch.is_hex_digit() {
						hex_char_count++
						end_pos := token.Pos{
							...start_pos
							len: idx + 1 - start_idx
						}
						match hex_char_count {
							1...5 {}
							6 {
								first_digit := node.val[idx - 5] - 48
								second_digit := node.val[idx - 4] - 48
								if first_digit > 1 {
									c.error(checker.unicode_lit_overflow_message, end_pos)
								} else if first_digit == 1 && second_digit > 0 {
									c.error(checker.unicode_lit_overflow_message, end_pos)
								}
							}
							else {
								c.error(checker.unicode_lit_overflow_message, end_pos)
							}
						}
						idx++
						ch = node.val[idx] or { return ast.string_type }
					}
				}
			}
			else {
				idx++
			}
		}
	}
	return ast.string_type
}

pub fn (mut c Checker) int_lit(mut node ast.IntegerLiteral) ast.Type {
	if node.val.len < 17 {
		// can not be a too large number, no need for more expensive checks
		return ast.int_literal_type
	}
	lit := node.val.replace('_', '').all_after('-')
	is_neg := node.val.starts_with('-')
	limit := if is_neg { '9223372036854775808' } else { '18446744073709551615' }
	message := 'integer literal $node.val overflows int'

	if lit.len > limit.len {
		c.error(message, node.pos)
	} else if lit.len == limit.len {
		for i, digit in lit {
			if digit > limit[i] {
				c.error(message, node.pos)
			} else if digit < limit[i] {
				break
			}
		}
	}

	return ast.int_literal_type
}
