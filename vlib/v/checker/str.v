// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module checker

import v.ast
import v.token
import encoding.utf8.validate

fn (mut c Checker) get_default_fmt(ftyp ast.Type, typ ast.Type) u8 {
	if ftyp.has_option_or_result() {
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
			|| sym.kind in [.enum, .array, .array_fixed, .struct, .map, .multi_return, .sum_type, .interface, .aggregate, .none]
			|| ftyp.has_option_or_result() || sym.has_method('str') {
			return `s`
		} else {
			return `_`
		}
	}
}

fn (mut c Checker) string_inter_lit(mut node ast.StringInterLiteral) ast.Type {
	inside_interface_deref_save := c.inside_interface_deref
	c.inside_interface_deref = true
	for i, mut expr in node.exprs {
		expected_type := c.expected_type
		c.expected_type = ast.string_type
		mut ftyp := c.expr(mut expr)
		c.expected_type = expected_type
		ftyp = c.type_resolver.get_type_or_default(expr, c.check_expr_option_or_result_call(expr,
			ftyp))
		if ftyp == ast.void_type || ftyp == 0 {
			c.error('expression does not return a value', expr.pos())
		} else if ftyp == ast.char_type && ftyp.nr_muls() == 0 {
			c.error('expression returning type `char` cannot be used in string interpolation directly, print its address or cast it to an integer instead',
				expr.pos())
		}
		if ftyp == 0 {
			return ast.void_type
		}
		c.markused_string_inter_lit(mut node, ftyp)
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
			`b`, `_`, `r`, `R`] {
			c.error('unknown format specifier `${fmt:c}`', node.fmt_poss[i])
		}
		if fmt == `_` { // set default representation for type if none has been given
			fmt = c.get_default_fmt(ftyp, typ)
			if fmt == `_` {
				if typ != ast.void_type && !(c.inside_lambda && typ.has_flag(.generic)) {
					c.error('no known default format for type `${c.table.get_type_name(ftyp)}`',
						node.fmt_poss[i])
				}
			} else if c.comptime.is_comptime(expr)
				&& c.type_resolver.get_type_or_default(expr, ast.void_type) != ast.void_type {
				// still `_` placeholder for comptime variable without specifier
				node.need_fmts[i] = false
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
			if ((typ.is_unsigned() && fmt !in [`u`, `x`, `X`, `o`, `c`, `b`])
				|| (typ.is_signed() && fmt !in [`d`, `x`, `X`, `o`, `c`, `b`])
				|| (typ.is_int_literal() && fmt !in [`d`, `c`, `x`, `X`, `o`, `u`, `b`])
				|| (typ.is_float() && fmt !in [`E`, `F`, `G`, `e`, `f`, `g`])
				|| (typ.is_pointer() && fmt !in [`p`, `x`, `X`])
				|| (typ.is_string() && fmt !in [`s`, `S`, `r`, `R`])
				|| (typ.idx() in [ast.i64_type_idx, ast.f64_type_idx] && fmt == `c`))
				&& !(typ.is_ptr() && fmt in [`p`, `x`, `X`]) {
				c.error('illegal format specifier `${fmt:c}` for type `${c.table.get_type_name(ftyp)}`',
					node.fmt_poss[i])
			}
			if c.table.final_sym(typ).kind in [.array, .array_fixed, .struct, .interface, .none, .map, .sum_type]
				&& fmt in [`E`, `F`, `G`, `e`, `f`, `g`, `d`, `u`, `x`, `X`, `o`, `c`, `p`, `b`, `r`, `R`]
				&& !(typ.is_ptr() && fmt in [`p`, `x`, `X`]) {
				c.error('illegal format specifier `${fmt:c}` for type `${c.table.get_type_name(ftyp)}`',
					node.fmt_poss[i])
			}
			node.need_fmts[i] = fmt != c.get_default_fmt(ftyp, typ)
		}
		// check recursive str
		if c.table.cur_fn != unsafe { nil } && c.table.cur_fn.is_method
			&& c.table.cur_fn.name == 'str' && c.table.cur_fn.receiver.name == '${expr}' {
			c.error('cannot call `str()` method recursively', expr.pos())
		}
	}
	c.inside_interface_deref = inside_interface_deref_save
	if c.pref.warn_about_allocs {
		c.warn_alloc('string interpolation', node.pos)
	}
	return ast.string_type
}

const unicode_lit_overflow_message = 'unicode character exceeds max allowed value of 0x10ffff, consider using a unicode literal (\\u####)'

// unicode character literals are limited to a maximum value of 0x10ffff
// https://stackoverflow.com/questions/52203351/why-unicode-is-restricted-to-0x10ffff
@[direct_array_access]
fn (mut c Checker) string_lit(mut node ast.StringLiteral) ast.Type {
	valid_utf8 := validate.utf8_string(node.val)
	if !valid_utf8 {
		c.note("invalid utf8 string, please check your file's encoding is utf8", node.pos)
	}
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
				if next_ch == `\\` {
					// ignore escaping char
					idx++
				} else if next_ch == `u` {
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
									c.error(unicode_lit_overflow_message, end_pos)
								} else if first_digit == 1 && second_digit > 0 {
									c.error(unicode_lit_overflow_message, end_pos)
								}
							}
							else {
								c.error(unicode_lit_overflow_message, end_pos)
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

struct LoHiLimit {
	lower  string
	higher string
}

const iencoding_map = {
	`B`: LoHiLimit{'1000000000000000000000000000000000000000000000000000000000000000', '1111111111111111111111111111111111111111111111111111111111111111'}
	`O`: LoHiLimit{'1000000000000000000000', '1777777777777777777777'}
	`_`: LoHiLimit{'9223372036854775808', '18446744073709551615'}
	`X`: LoHiLimit{'8000000000000000', 'FFFFFFFFFFFFFFFF'}
}

fn (mut c Checker) int_lit(mut node ast.IntegerLiteral) ast.Type {
	if node.val.len < 17 {
		// can not be a too large number, no need for more expensive checks
		return ast.int_literal_type
	}
	lit := node.val.replace('_', '').all_after('-').to_upper_ascii()
	is_neg := node.val.starts_with('-')
	if lit.len > 2 && lit[0] == `0` && lit[1] in [`B`, `X`, `O`] {
		if lohi := iencoding_map[lit[1]] {
			c.check_num_literal(lohi, is_neg, lit[2..]) or { c.num_lit_overflow_error(node) }
		}
	} else {
		lohi := iencoding_map[`_`]
		c.check_num_literal(lohi, is_neg, lit) or { c.num_lit_overflow_error(node) }
	}
	return ast.int_literal_type
}

@[direct_array_access]
fn (mut c Checker) check_num_literal(lohi LoHiLimit, is_neg bool, lit string) ! {
	limit := if is_neg { lohi.lower } else { lohi.higher }
	if lit.len < limit.len {
		return
	}
	if lit.len > limit.len {
		return error('length overflow')
	}
	if lit.len == limit.len {
		for i, digit in lit {
			if digit > limit[i] {
				return error('value overflow at i: ${i}')
			} else if digit < limit[i] {
				break
			}
		}
	}
}

fn (mut c Checker) num_lit_overflow_error(node &ast.IntegerLiteral) {
	if c.inside_integer_literal_cast {
		return
	}
	c.error('integer literal ${node.val} overflows int', node.pos)
}
