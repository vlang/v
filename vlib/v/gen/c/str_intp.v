/*
str_intp.v

Copyright (c) 2019-2022 Dario Deledda. All rights reserved.
Use of this source code is governed by an MIT license
that can be found in the LICENSE file.

This file contains string interpolation V functions
*/
module c

import v.ast
import v.util

fn (mut g Gen) str_format(node ast.StringInterLiteral, i int) (u64, string) {
	mut base := 0 // numeric base
	mut upper_case := false // set upercase for the result string
	mut typ := g.unwrap_generic(node.expr_types[i])
	sym := g.table.sym(typ)
	if sym.kind == .alias {
		typ = (sym.info as ast.Alias).parent_type
	}
	mut remove_tail_zeros := false
	fspec := node.fmts[i]
	mut fmt_type := StrIntpType{}

	// upper cases
	if (fspec - `A`) <= (`Z` - `A`) {
		upper_case = true
	}

	if fspec in [`s`, `S`] {
		/*
		if node.fwidths[i] == 0 {
			fmt_type = .si_s
		} else {
			fmt_type = .si_s
		}
		*/
		fmt_type = .si_s
	} else if typ.is_float() {
		if fspec in [`g`, `G`] {
			match typ {
				ast.f32_type { fmt_type = .si_g32 }
				// ast.f64_type { fmt_type = .si_g64 }
				else { fmt_type = .si_g64 }
			}
			remove_tail_zeros = true
		} else if fspec in [`e`, `E`] {
			match typ {
				ast.f32_type { fmt_type = .si_e32 }
				// ast.f64_type { fmt_type = .si_e64 }
				else { fmt_type = .si_e64 }
			}
		} else if fspec in [`f`, `F`] {
			match typ {
				ast.f32_type { fmt_type = .si_f32 }
				// ast.f64_type { fmt_type = .si_f64 }
				else { fmt_type = .si_f64 }
			}
		}
	} else if typ.is_pointer() {
		if fspec in [`x`, `X`] {
			base = 16 - 2 // our base start from 2
		}
		if fspec in [`p`, `x`, `X`] {
			fmt_type = .si_p
		} else {
			fmt_type = .si_vp
		}
	} else if typ.is_int() {
		if fspec in [`x`, `X`] {
			base = 16 - 2 // our base start from 2
		}
		// if fspec in [`o`] {
		if fspec == `o` {
			base = 8 - 2 // our base start from 2
		}
		// binary format
		if fspec == `b` {
			base = 1 // our base start from 2 we use 1 for binary
		}
		if fspec == `c` {
			fmt_type = .si_c
		} else {
			match typ {
				ast.i8_type { fmt_type = .si_i8 }
				ast.byte_type { fmt_type = .si_u8 }
				ast.i16_type { fmt_type = .si_i16 }
				ast.u16_type { fmt_type = .si_u16 }
				ast.i64_type { fmt_type = .si_i64 }
				ast.u64_type { fmt_type = .si_u64 }
				ast.u32_type { fmt_type = .si_u32 }
				ast.usize_type { fmt_type = .si_u64 }
				ast.isize_type { fmt_type = .si_i64 }
				else { fmt_type = .si_i32 }
			}
		}
	} else {
		// TODO: better check this case
		fmt_type = .si_p
	}

	/*
	// pad filling 64bit format
	mut pad_ch := u8(0)
	if node.fills[i] {
		pad_ch = u8(`0`)
	}
	res := get_str_intp_u64_format(fmt_type, node.fwidths[i], node.precisions[i], remove_tail_zeros, node.pluss[i], pad_ch, base, upper_case)
	*/

	// pad filling 32bit format
	mut pad_ch := 0
	if node.fills[i] {
		pad_ch = 1
	}
	res := get_str_intp_u32_format(fmt_type, node.fwidths[i], node.precisions[i], remove_tail_zeros,
		node.pluss[i], byte(pad_ch), base, upper_case)
	//
	return res, fmt_type.str()
}

fn (mut g Gen) str_val(node ast.StringInterLiteral, i int) {
	expr := node.exprs[i]

	typ := g.unwrap_generic(node.expr_types[i])
	typ_sym := g.table.sym(typ)
	if typ == ast.string_type && g.comptime_for_method.len == 0 {
		if g.inside_vweb_tmpl {
			g.write('vweb__filter(')
			if expr.is_auto_deref_var() {
				g.write('*')
			}
			g.expr(expr)
			g.write(')')
		} else {
			if expr.is_auto_deref_var() {
				g.write('*')
			}
			g.expr(expr)
		}
	} else if typ_sym.kind == .interface_ && (typ_sym.info as ast.Interface).defines_method('str') {
		rec_type_name := util.no_dots(g.cc_type(typ, false))
		g.write('${c_name(rec_type_name)}_name_table[')
		g.expr(expr)
		dot := if typ.is_ptr() { '->' } else { '.' }
		g.write('${dot}_typ]._method_str(')
		g.expr(expr)
		g.write('${dot}_object')
		g.write(')')
	} else if node.fmts[i] == `s` || typ.has_flag(.variadic) {
		mut exp_typ := typ
		if expr is ast.Ident {
			if expr.obj is ast.Var {
				if g.comptime_var_type_map.len > 0 || g.comptime_for_method.len > 0 {
					exp_typ = expr.obj.typ
				} else if expr.obj.smartcasts.len > 0 {
					exp_typ = expr.obj.smartcasts.last()
					cast_sym := g.table.sym(exp_typ)
					if cast_sym.info is ast.Aggregate {
						exp_typ = cast_sym.info.types[g.aggregate_type_idx]
					}
				}
			}
		}
		g.gen_expr_to_string(expr, exp_typ)
	} else if typ.is_number() || typ.is_pointer() || node.fmts[i] == `d` {
		if typ.is_signed() && node.fmts[i] in [`x`, `X`, `o`] {
			// convert to unsigned first befors C's integer propagation strikes
			if typ == ast.i8_type {
				g.write('(byte)(')
			} else if typ == ast.i16_type {
				g.write('(u16)(')
			} else if typ == ast.int_type {
				g.write('(u32)(')
			} else {
				g.write('(u64)(')
			}
			if expr.is_auto_deref_var() {
				g.write('*')
			}
			g.expr(expr)
			g.write(')')
		} else {
			if expr.is_auto_deref_var() {
				g.write('*')
			}
			g.expr(expr)
		}
	} else {
		if expr.is_auto_deref_var() {
			g.write('*')
		}
		g.expr(expr)
	}
}

fn (mut g Gen) string_inter_literal(node ast.StringInterLiteral) {
	// fn (mut g Gen) str_int2(node ast.StringInterLiteral) {
	if g.inside_comptime_for_field {
		mut node_ := unsafe { node }
		for i, expr in node_.exprs {
			if mut expr is ast.Ident {
				if mut expr.obj is ast.Var {
					node_.expr_types[i] = expr.obj.typ
				}
			}
		}
	}
	g.write(' str_intp($node.vals.len, ')
	g.write('_MOV((StrIntpData[]){')
	for i, val in node.vals {
		escaped_val := util.smart_quote(val, false)

		if escaped_val.len > 0 {
			g.write('{_SLIT("$escaped_val"), ')
		} else {
			g.write('{_SLIT0, ')
		}

		if i >= node.exprs.len {
			// last part of the string
			g.write('0, { .d_c = 0 }}')
			break
		}

		ft_u64, ft_str := g.str_format(node, i)
		g.write('0x$ft_u64.hex(), {.d_$ft_str = ')

		// for pointers we need a void* cast
		if unsafe { ft_str.str[0] } == `p` {
			g.write('(void*)(')
			g.str_val(node, i)
			g.write(')')
		} else {
			g.str_val(node, i)
		}

		g.write('}}')
		if i < (node.vals.len - 1) {
			g.write(', ')
		}
	}
	g.write('}))')
}
