/*
str_intp.v

Copyright (c) 2019-2024 Dario Deledda. All rights reserved.
Use of this source code is governed by an MIT license
that can be found in the LICENSE file.

This file contains string interpolation V functions
*/
module c

import v.ast
import v.util

fn (mut g Gen) get_default_fmt(ftyp ast.Type, typ ast.Type) u8 {
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
		mut sym := g.table.sym(g.unwrap_generic(ftyp))
		if sym.kind == .alias {
			// string aliases should be printable
			info := sym.info as ast.Alias
			sym = g.table.sym(info.parent_type)
			if info.parent_type == ast.string_type {
				return `s`
			}
		}
		if sym.kind == .function {
			return `s`
		}
		if ftyp in [ast.string_type, ast.bool_type]
			|| sym.kind in [.enum, .array, .array_fixed, .struct, .map, .multi_return, .sum_type, .interface, .none]
			|| ftyp.has_option_or_result() || sym.has_method('str') {
			return `s`
		} else {
			return `_`
		}
	}
}

fn (mut g Gen) str_format(node ast.StringInterLiteral, i int, fmts []u8) (u64, string) {
	mut base := 0 // numeric base
	mut upper_case := false // set uppercase for the result string
	mut typ := g.unwrap_generic(node.expr_types[i])
	if node.exprs[i].is_auto_deref_var() {
		typ = typ.deref()
	}
	typ = g.table.final_type(typ)
	mut remove_tail_zeros := false
	fspec := fmts[i]
	mut fmt_type := StrIntpType.si_no_str
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
	} else if fspec in [`r`, `R`] {
		fmt_type = .si_r
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
				ast.u8_type { fmt_type = .si_u8 }
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
		node.pluss[i], u8(pad_ch), base, upper_case)

	return res, fmt_type.str()
}

fn (mut g Gen) str_val(node ast.StringInterLiteral, i int, fmts []u8) {
	expr := node.exprs[i]
	fmt := fmts[i]
	typ := g.unwrap_generic(node.expr_types[i])
	typ_sym := g.table.sym(typ)
	if typ == ast.string_type && g.comptime.comptime_for_method == unsafe { nil } {
		if g.inside_vweb_tmpl {
			g.write('${g.vweb_filter_fn_name}(')
			if expr.is_auto_deref_var() && fmt != `p` {
				g.write('*')
			}
			g.expr(expr)
			g.write(')')
		} else {
			if expr.is_auto_deref_var() && fmt != `p` {
				g.write('*')
			}
			g.expr(expr)
		}
	} else if typ_sym.kind == .interface && (typ_sym.info as ast.Interface).defines_method('str') {
		rec_type_name := util.no_dots(g.cc_type(typ, false))
		g.write('${c_name(rec_type_name)}_name_table[')
		g.expr(expr)
		dot := if typ.is_ptr() { '->' } else { '.' }
		g.write('${dot}_typ]._method_str(')
		g.expr(expr)
		g.write2('${dot}_object', ')')
	} else if fmt == `s` || typ.has_flag(.variadic) {
		mut exp_typ := typ
		if expr is ast.Ident {
			if g.comptime.get_ct_type_var(expr) == .smartcast {
				exp_typ = g.type_resolver.get_type(expr)
			} else if expr.obj is ast.Var {
				if expr.obj.smartcasts.len > 0 {
					exp_typ = g.unwrap_generic(expr.obj.smartcasts.last())
					cast_sym := g.table.sym(exp_typ)
					if cast_sym.info is ast.Aggregate {
						exp_typ = cast_sym.info.types[g.aggregate_type_idx]
					}
				}
			}
		}
		g.gen_expr_to_string(expr, exp_typ)
	} else if typ.is_number() || typ.is_pointer() || fmt == `d` {
		if typ.is_signed() && fmt in [`x`, `X`, `o`] {
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
			if expr.is_auto_deref_var() && fmt != `p` {
				g.write('*')
			}
			g.expr(expr)
		}
	} else {
		if expr.is_auto_deref_var() && fmt != `p` {
			g.write('*')
		}
		g.expr(expr)
	}
}

fn (mut g Gen) string_inter_literal(node ast.StringInterLiteral) {
	inside_interface_deref_old := g.inside_interface_deref
	g.inside_interface_deref = true
	defer {
		g.inside_interface_deref = inside_interface_deref_old
	}
	mut node_ := unsafe { node }
	mut fmts := node_.fmts.clone()
	for i, mut expr in node_.exprs {
		if g.comptime.is_comptime(expr) {
			ctyp := g.type_resolver.get_type_or_default(expr, node_.expr_types[i])
			if ctyp != ast.void_type {
				node_.expr_types[i] = ctyp
				if node_.fmts[i] == `_` {
					ftyp_sym := g.table.sym(ctyp)
					typ := if ftyp_sym.kind == .alias && !ftyp_sym.has_method('str') {
						g.table.unalias_num_type(ctyp)
					} else {
						ctyp
					}
					fmts[i] = g.get_default_fmt(ctyp, typ)
				}
			}
		}
	}
	g.write2('str_intp(', node.vals.len.str())
	g.write(', _MOV((StrIntpData[]){')
	for i, val in node.vals {
		mut escaped_val := cescape_nonascii(util.smart_quote(val, false))
		escaped_val = escaped_val.replace('\0', '\\0')

		if escaped_val.len > 0 {
			g.write2('{_S("', escaped_val)
			g.write('"), ')
		} else {
			g.write('{_SLIT0, ')
		}

		if i >= node.exprs.len {
			// last part of the string
			g.write('0, { .d_c = 0 }}')
			break
		}

		ft_u64, ft_str := g.str_format(node, i, fmts)
		g.write2('0x', ft_u64.hex())
		g.write2(', {.d_', ft_str)
		g.write(' = ')

		// for pointers we need a void* cast
		if unsafe { ft_str.str[0] } == `p` {
			g.write('(void*)(')
			g.str_val(node, i, fmts)
			g.write(')')
		} else {
			g.str_val(node, i, fmts)
		}

		g.write('}}')
		if i < (node.vals.len - 1) {
			g.write(', ')
		}
	}
	g.write('}))')
}
