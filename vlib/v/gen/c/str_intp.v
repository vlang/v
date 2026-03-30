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

// is_or_block_var_unwrapped checks if a variable was initialized from an
// expression with an `or {}` block (e.g. `x := opt_val or { default }`),
// which means the variable's actual type has the option flag cleared.
fn (g Gen) is_or_block_var_unwrapped(obj ast.Var) bool {
	init_expr := obj.expr
	return match init_expr {
		ast.CallExpr { init_expr.or_block.kind != .absent }
		ast.Ident { init_expr.or_expr.kind != .absent }
		ast.IndexExpr { init_expr.or_expr.kind != .absent }
		ast.SelectorExpr { init_expr.or_block.kind != .absent }
		ast.PrefixExpr { init_expr.or_block.kind != .absent }
		else { false }
	}
}

// should_clear_option_flag checks if an expression's option flag should be cleared
// because the variable was unwrapped via smartcast or `or {}` block.
fn (g Gen) should_clear_option_flag(expr ast.Expr) bool {
	ident := match expr {
		ast.Ident { expr }
		else { return false }
	}
	match ident.obj {
		ast.Var {
			if ident.obj.is_unwrapped {
				return true
			}
			if g.is_or_block_var_unwrapped(ident.obj) {
				return true
			}
			if !ident.obj.typ.has_flag(.option) && ident.obj.ct_type_var == .no_comptime {
				return true
			}
		}
		else {}
	}
	return false
}

fn int_ref_interpolates_as_value(expr ast.Expr, typ ast.Type, fmt u8) bool {
	if fmt == `p` || !(typ.is_int_valptr() || typ.is_float_valptr()) {
		return false
	}
	if expr.is_auto_deref_var() {
		return true
	}
	return match expr {
		ast.Ident {
			if expr.obj is ast.Var {
				expr.obj.is_arg || (expr.obj.expr is ast.PrefixExpr && expr.obj.expr.op == .amp)
			} else {
				false
			}
		}
		ast.PrefixExpr {
			expr.op == .amp
		}
		else {
			false
		}
	}
}

fn (mut g Gen) should_resolve_str_intp_expr_type(expr ast.Expr, typ ast.Type) bool {
	if typ == 0 || typ.has_flag(.generic) || g.type_has_unresolved_generic_parts(typ) {
		return true
	}
	// In generic contexts, always resolve expression types since AST types
	// may be stale from a previous checker instantiation
	if g.cur_fn != unsafe { nil } && g.cur_concrete_types.len > 0 {
		return true
	}
	return match expr {
		ast.CallExpr, ast.ComptimeSelector, ast.Ident, ast.IndexExpr, ast.InfixExpr {
			true
		}
		ast.SelectorExpr {
			expr.expr is ast.TypeOf
		}
		else {
			false
		}
	}
}

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
			|| sym.kind in [.enum, .array, .array_fixed, .struct, .generic_inst, .map, .multi_return, .sum_type, .interface, .none]
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
	expr := node.exprs[i]
	mut typ := if i < node.expr_types.len {
		g.unwrap_generic(node.expr_types[i])
	} else {
		ast.string_type
	}
	if g.should_resolve_str_intp_expr_type(expr, typ) {
		resolved_expr_typ := g.resolved_expr_type(expr, typ)
		if resolved_expr_typ != 0 {
			typ = g.unwrap_generic(g.recheck_concrete_type(resolved_expr_typ))
		}
	}
	// Resolve aggregate types (from multi-branch match arms) to the
	// concrete variant type for the current iteration.
	typ_sym_fmt := g.table.sym(typ)
	if typ_sym_fmt.info is ast.Aggregate {
		typ = typ_sym_fmt.info.types[g.aggregate_type_idx]
	}
	if expr.is_auto_deref_var() && typ.is_ptr() {
		typ = typ.deref()
	}
	if int_ref_interpolates_as_value(expr, typ, fmts[i]) && typ.is_ptr() {
		typ = typ.deref()
	}
	typ = g.table.final_type(typ)
	if typ.has_flag(.shared_f) && typ.is_ptr() {
		typ = typ.clear_flag(.shared_f).deref()
	}
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
				ast.i8_type {
					fmt_type = .si_i8
				}
				ast.u8_type {
					fmt_type = .si_u8
				}
				ast.i16_type {
					fmt_type = .si_i16
				}
				ast.u16_type {
					fmt_type = .si_u16
				}
				ast.i64_type {
					fmt_type = .si_i64
				}
				ast.u64_type {
					fmt_type = .si_u64
				}
				ast.i32_type {
					fmt_type = .si_i32
				}
				ast.u32_type {
					fmt_type = .si_u32
				}
				ast.int_type {
					$if new_int ? && x64 {
						fmt_type = .si_i64
					} $else {
						fmt_type = .si_i32
					}
				}
				ast.usize_type {
					fmt_type = .si_u64
				}
				ast.isize_type {
					fmt_type = .si_i64
				}
				else {
					fmt_type = .si_i32
				}
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
	static_width := if i < node.fwidth_exprs.len && node.fwidth_exprs[i] !is ast.EmptyExpr {
		0
	} else {
		node.fwidths[i]
	}
	static_precision := if i < node.precision_exprs.len && node.precision_exprs[i] !is ast.EmptyExpr {
		987698
	} else {
		node.precisions[i]
	}
	res := get_str_intp_u32_format(fmt_type, static_width, static_precision, remove_tail_zeros,
		node.pluss[i], u8(pad_ch), base, upper_case)

	return res, fmt_type.str()
}

fn (mut g Gen) str_val(node ast.StringInterLiteral, i int, fmts []u8) {
	expr := node.exprs[i]
	fmt := fmts[i]
	mut orig_typ := if i < node.expr_types.len {
		g.unwrap_generic(node.expr_types[i])
	} else {
		ast.string_type
	}
	if g.should_resolve_str_intp_expr_type(expr, orig_typ) {
		resolved_expr_typ := g.resolved_expr_type(expr, orig_typ)
		if resolved_expr_typ != 0 {
			orig_typ = g.unwrap_generic(g.recheck_concrete_type(resolved_expr_typ))
		}
	}
	// Resolve aggregate types (from multi-branch match arms) to the
	// concrete variant type for the current iteration.
	orig_typ_sym := g.table.sym(orig_typ)
	if orig_typ_sym.info is ast.Aggregate {
		orig_typ = orig_typ_sym.info.types[g.aggregate_type_idx]
	}
	is_int_valptr := int_ref_interpolates_as_value(expr, orig_typ, fmt)
	typ := if is_int_valptr { orig_typ.deref() } else { orig_typ }
	typ_sym := g.table.sym(typ)
	if g.comptime.inside_comptime_for && expr is ast.SelectorExpr && expr.field_name == 'name'
		&& expr.expr is ast.TypeOf {
		g.expr(expr)
		return
	}
	if typ == ast.string_type && g.comptime.comptime_for_method == unsafe { nil } {
		if g.inside_vweb_tmpl {
			g.write('${g.vweb_filter_fn_name}(')
			if expr.is_auto_deref_var() && fmt != `p` {
				g.write('*')
			}
			g.expr(expr)
			g.write(')')
		} else {
			if g.is_autofree_tmp && g.is_autofree
				&& expr !in [ast.Ident, ast.StringLiteral, ast.SelectorExpr, ast.ComptimeSelector] {
				if expr is ast.CallExpr {
					old_is_autofree_tmp := g.is_autofree_tmp
					g.autofree_call_pregen(expr)
					g.is_autofree_tmp = old_is_autofree_tmp
				}
				tmp := g.new_tmp_var()
				tmp_pos := expr.pos()
				mut scope := g.file.scope.innermost(tmp_pos.pos)
				scope.register(ast.Var{
					name:            tmp
					typ:             ast.string_type
					is_autofree_tmp: true
					pos:             tmp_pos
				})
				pos_before := g.out.len
				if expr.is_auto_deref_var() && fmt != `p` {
					g.write('*')
				}
				g.expr(expr)
				expr_code := g.out.cut_to(pos_before).trim_space()
				g.strs_to_free0 << 'string ${tmp} = ${expr_code};'
				g.write(tmp)
				return
			}
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
		is_comptime_for_var := expr is ast.Ident && g.is_comptime_for_var(expr)
		if !is_comptime_for_var && expr is ast.Ident {
			if g.comptime.get_ct_type_var(expr) == .smartcast {
				exp_typ = g.type_resolver.get_type(expr)
			} else if expr.obj is ast.Var {
				if expr.obj.smartcasts.len > 0 {
					exp_typ = g.unwrap_generic(expr.obj.smartcasts.last())
					cast_sym := g.table.sym(exp_typ)
					if cast_sym.info is ast.Aggregate {
						exp_typ = cast_sym.info.types[g.aggregate_type_idx]
					}
					if exp_typ.has_flag(.option) && expr.obj.is_unwrapped {
						exp_typ = exp_typ.clear_flag(.option)
					}
				} else if expr.obj.is_unwrapped && exp_typ.has_flag(.option) {
					exp_typ = exp_typ.clear_flag(.option)
				}
			}
		}
		if exp_typ.has_flag(.option) && expr is ast.Ident && g.is_comptime_for_var(expr) {
			str_fn_name := g.get_str_fn(exp_typ.clear_flag(.option))
			g.write('${str_fn_name}(*(${g.base_type(exp_typ)}*)(')
			old_inside_opt_or_res := g.inside_opt_or_res
			g.inside_opt_or_res = true
			g.expr(expr)
			g.inside_opt_or_res = old_inside_opt_or_res
			g.write('.data))')
		} else {
			g.gen_expr_to_string(expr, exp_typ)
		}
	} else if typ.is_number() || typ.is_pointer() || fmt == `d` {
		if typ.is_signed() && fmt in [`x`, `X`, `o`] {
			// convert to unsigned first befors C's integer propagation strikes
			if typ == ast.i8_type {
				g.write('(byte)(')
			} else if typ == ast.i16_type {
				g.write('(u16)(')
			} else if typ == ast.i32_type {
				g.write('(u32)(')
			} else if typ == ast.int_type {
				$if new_int ? && x64 {
					g.write('(u64)(')
				} $else {
					g.write('(u32)(')
				}
			} else {
				g.write('(u64)(')
			}
			if expr.is_auto_deref_var() || is_int_valptr {
				g.write('*')
			}
			g.expr(expr)
			if typ.has_flag(.shared_f) {
				g.write('->val')
			}
			g.write(')')
		} else {
			if (expr.is_auto_deref_var() || is_int_valptr) && fmt != `p` {
				g.write('*')
			}
			g.expr(expr)
			if typ.has_flag(.shared_f) {
				g.write('->val')
			}
		}
	} else {
		if expr.is_auto_deref_var() && fmt != `p` {
			g.write('*')
		}
		g.expr(expr)
		if typ.has_flag(.shared_f) {
			g.write('->val')
		}
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
		mut field_typ := if mut expr is ast.Ident && g.is_comptime_for_var(expr) {
			g.comptime.comptime_for_field_type
		} else if i < node_.expr_types.len {
			node_.expr_types[i]
		} else {
			ast.void_type
		}
		if g.comptime.inside_comptime_for && mut expr is ast.SelectorExpr {
			if expr.expr is ast.TypeOf && expr.field_name == 'name' {
				field_typ = ast.string_type
			}
		}
		if g.comptime.is_comptime(expr) || (g.comptime.inside_comptime_for && expr is ast.Ident) {
			mut ctyp := g.type_resolver.get_type_or_default(expr, field_typ)
			// In generic contexts, comptime type may be stale from a previous
			// checker instantiation. Prefer resolved_expr_type when available.
			if g.cur_fn != unsafe { nil } && g.cur_concrete_types.len > 0 {
				resolved_ct := g.resolved_expr_type(expr, ctyp)
				if resolved_ct != ast.void_type && resolved_ct != 0 {
					ctyp = g.unwrap_generic(g.recheck_concrete_type(resolved_ct))
				}
			}
			if ctyp != ast.void_type {
				// Clear option flag for variables unwrapped via `or {}` blocks
				if ctyp.has_flag(.option) && g.should_clear_option_flag(expr) {
					ctyp = ctyp.clear_flag(.option)
				}
				node_.expr_types[i] = ctyp
				if node_.fmts[i] == `_`
					|| (g.cur_fn != unsafe { nil } && g.cur_concrete_types.len > 0) {
					ftyp_sym := g.table.sym(ctyp)
					typ := if ftyp_sym.kind == .alias && !ftyp_sym.has_method('str') {
						g.table.unalias_num_type(ctyp)
					} else {
						ctyp
					}
					fmts[i] = g.get_default_fmt(ctyp, typ)
				}
			}
		} else {
			if g.should_resolve_str_intp_expr_type(expr, field_typ) {
				resolved_field_typ := g.resolved_expr_type(expr, field_typ)
				if resolved_field_typ != ast.void_type {
					field_typ = g.unwrap_generic(g.recheck_concrete_type(resolved_field_typ))
				}
			}
			// Resolve aggregate types (from multi-branch match arms) to the
			// concrete variant type for the current iteration.
			field_sym := g.table.sym(field_typ)
			if field_sym.info is ast.Aggregate {
				field_typ = field_sym.info.types[g.aggregate_type_idx]
			}
			// Clear option flag for variables unwrapped via `or {}` blocks
			if field_typ.has_flag(.option) && g.should_clear_option_flag(expr) {
				field_typ = field_typ.clear_flag(.option)
			}
			if i >= node_.expr_types.len {
				node_.expr_types << field_typ
			} else {
				node_.expr_types[i] = field_typ
			}
			// Update format specifier if it was auto-determined and the type changed
			if !node_.need_fmts[i] {
				ftyp_sym := g.table.sym(field_typ)
				new_typ := if ftyp_sym.kind == .alias && !ftyp_sym.has_method('str') {
					g.table.unalias_num_type(field_typ)
				} else {
					field_typ
				}
				fmts[i] = g.get_default_fmt(field_typ, new_typ)
			}
		}
		expr_ := expr
		match expr_ {
			ast.Ident {
				if expr_.obj is ast.Var && g.table.is_interface_smartcast(expr_.obj) {
					expr_var := expr_.obj
					if field_typ.is_ptr() && !expr_var.orig_type.is_ptr()
						&& g.table.final_sym(expr_var.orig_type).kind == .interface
						&& g.table.final_sym(field_typ).kind != .interface {
						field_typ = field_typ.deref()
						node_.expr_types[i] = field_typ
						if !node_.need_fmts[i] {
							fmts[i] = g.get_default_fmt(field_typ, field_typ)
						}
					}
				}
			}
			else {}
		}
	}
	g.write2('builtin__str_intp(', node.vals.len.str())
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
			g.write('0, { .d_c = 0 }, 0, 0, 0}')
			break
		}

		ft_u64, ft_str := g.str_format(node, i, fmts)
		$if trace_ci_fixes ? {
			if g.file.path.contains('comptime_for_in_options_struct_test.v')
				|| g.file.path.contains('comptime_map_fields_decode_test.v') {
				g.write('/*trace_str_intp expr=')
				g.write(node.exprs[i].str().replace('*/', '* /'))
				g.write(' typ=')
				g.write(g.table.type_to_str(node.expr_types[i]).replace('*/', '* /'))
				g.write(' fmt=')
				g.write(ft_str)
				g.write('*/')
			}
		}
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

		g.write('}')
		has_dynamic_width := i < node.fwidth_exprs.len && node.fwidth_exprs[i] !is ast.EmptyExpr
		has_dynamic_precision := i < node.precision_exprs.len
			&& node.precision_exprs[i] !is ast.EmptyExpr
		if has_dynamic_width || has_dynamic_precision {
			g.write(', ')
			if has_dynamic_width {
				g.expr(node.fwidth_exprs[i])
			} else {
				g.write('0')
			}
			g.write(', ')
			if has_dynamic_precision {
				g.expr(node.precision_exprs[i])
			} else {
				g.write('0')
			}
			g.write(', ')
			g.write(if has_dynamic_width && has_dynamic_precision {
				'3'
			} else if has_dynamic_width {
				'1'
			} else {
				'2'
			})
		} else {
			g.write(', 0, 0, 0')
		}
		g.write('}')
		if i < (node.vals.len - 1) {
			g.write(', ')
		}
	}
	g.write('}))')
}
