// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module c

import v.ast
import v.util

fn (mut g Gen) string_literal(node ast.StringLiteral) {
	escaped_val := cescape_nonascii(util.smart_quote(node.val, node.is_raw))
	if node.language == .c {
		g.write2('"', escaped_val)
		g.write('"')
	} else {
		g.write2('_S("', escaped_val)
		g.write('")')
	}
}

// optimize string interpolation in string builders:
// `sb.writeln('a=$a')` =>
// `sb.writeln('a='); sb.writeln(a.str())`
fn (mut g Gen) string_inter_literal_sb_optimized(call_expr ast.CallExpr) {
	node := call_expr.args[0].expr as ast.StringInterLiteral
	g.writeln('// sb inter opt')
	is_nl := call_expr.name == 'writeln'
	for i, val in node.vals {
		escaped_val := cescape_nonascii(util.smart_quote(val, false))
		g.write('strings__Builder_write_string(&')
		g.expr(call_expr.left)
		g.write2(', _S("', escaped_val)
		g.writeln('"));')
		if i >= node.exprs.len {
			break
		}
		if is_nl && i == node.exprs.len - 1 {
			g.write('strings__Builder_writeln(&')
		} else {
			g.write('strings__Builder_write_string(&')
		}
		g.expr(call_expr.left)
		g.write(', ')
		typ := node.expr_types[i]
		g.write2(g.styp(typ), '_str(')
		sym := g.table.sym(typ)
		if sym.kind != .function {
			g.expr(node.exprs[i])
		}
		g.writeln('));')
	}
	g.writeln('')
	return
}

fn (mut g Gen) gen_expr_to_string(expr ast.Expr, etype ast.Type) {
	old_inside_opt_or_res := g.inside_opt_or_res
	g.inside_opt_or_res = true
	g.expected_fixed_arr = true
	defer {
		g.inside_opt_or_res = old_inside_opt_or_res
		g.expected_fixed_arr = false
	}
	is_shared := etype.has_flag(.shared_f)
	mut typ := etype
	if is_shared {
		typ = typ.clear_flag(.shared_f).set_nr_muls(0)
	}
	// original is_ptr for the typ (aliased type could overwrite it)
	is_ptr := typ.is_ptr()
	mut sym := g.table.sym(typ)
	// when type is non-option alias and doesn't has `str()`, print the aliased value
	if mut sym.info is ast.Alias && !sym.has_method('str') && !etype.has_flag(.option) {
		parent_sym := g.table.sym(sym.info.parent_type)
		if parent_sym.has_method('str') {
			typ = sym.info.parent_type
			sym = unsafe { parent_sym }
		}
	}
	sym_has_str_method, str_method_expects_ptr, _ := sym.str_method_info()
	if typ.has_flag(.variadic) {
		str_fn_name := g.get_str_fn(typ)
		g.write('${str_fn_name}(')
		g.expr(expr)
		g.write(')')
	} else if typ == ast.string_type {
		if etype.is_ptr() {
			g.write('*')
		}
		g.expr(expr)
	} else if typ == ast.bool_type {
		g.expr(expr)
		g.write(' ? _S("true") : _S("false")')
	} else if sym.kind == .none || typ == ast.void_type.set_flag(.option) {
		if expr is ast.CallExpr {
			stmt_str := g.go_before_last_stmt()
			g.expr(expr)
			g.writeln(';')
			g.write(stmt_str)
		}
		g.write('_S("<none>")')
	} else if sym.kind == .enum {
		if expr !is ast.EnumVal || sym.has_method('str') {
			str_fn_name := g.get_str_fn(typ)
			g.write('${str_fn_name}(')
			if typ.nr_muls() > 0 {
				g.write('*'.repeat(typ.nr_muls()))
			}
			if expr is ast.EnumVal {
				g.write2(sym.cname, '__')
			}
			g.enum_expr(expr)
			g.write(')')
		} else {
			g.write('_S("')
			g.enum_expr(expr)
			g.write('")')
		}
	} else if sym_has_str_method
		|| sym.kind in [.array, .array_fixed, .map, .struct, .multi_return, .sum_type, .interface] {
		unwrap_option := expr is ast.Ident && expr.or_expr.kind == .propagate_option
		exp_typ := if unwrap_option { typ.clear_flag(.option) } else { typ }
		is_dump_expr := expr is ast.DumpExpr
		is_var_mut := expr.is_auto_deref_var()
		str_fn_name := g.get_str_fn(exp_typ)
		temp_var_needed := expr is ast.CallExpr
			&& (expr.return_type.is_ptr() || g.table.sym(expr.return_type).is_c_struct())
		mut tmp_var := ''
		if temp_var_needed {
			tmp_var = g.new_tmp_var()
			ret_typ := g.styp(exp_typ)
			line := g.go_before_last_stmt().trim_space()
			g.empty_line = true
			g.write('${ret_typ} ${tmp_var} = ')
			g.expr(expr)
			g.writeln(';')
			g.write(line)
		}
		if is_ptr && !is_var_mut {
			ref_str := '&'.repeat(typ.nr_muls())
			g.write('str_intp(1, _MOV((StrIntpData[]){{_S("${ref_str}"), ${si_s_code} ,{.d_s = isnil(')
			if typ.has_flag(.option) {
				g.write('*(${g.base_type(exp_typ)}*)&')
				if temp_var_needed {
					g.write(tmp_var)
				} else {
					g.expr(expr)
				}
				g.write('.data) ? _S("Option(&nil)") : ')
			} else {
				inside_interface_deref_old := g.inside_interface_deref
				g.inside_interface_deref = false
				defer {
					g.inside_interface_deref = inside_interface_deref_old
				}
				if temp_var_needed {
					g.write(tmp_var)
				} else {
					g.expr(expr)
				}
				g.write(') ? _S("nil") : ')
			}
		}
		g.write2(str_fn_name, '(')
		if str_method_expects_ptr && !is_ptr {
			if is_dump_expr || (g.pref.ccompiler_type != .tinyc && expr is ast.CallExpr) {
				g.write('ADDR(${g.styp(typ)}, ')
				defer {
					g.write(')')
				}
			} else {
				g.write('&')
			}
		} else if is_ptr && typ.has_flag(.option) {
			if typ.has_flag(.option_mut_param_t) {
				g.write('*')
			} else {
				g.write('*(${g.styp(typ)}*)&')
			}
		} else if !str_method_expects_ptr && !is_shared && (is_ptr || is_var_mut) {
			if sym.is_c_struct() {
				g.write(c_struct_ptr(sym, typ, str_method_expects_ptr))
			} else {
				g.write('*'.repeat(etype.nr_muls()))
			}
		} else if sym.is_c_struct() {
			g.write(c_struct_ptr(sym, typ, str_method_expects_ptr))
		}
		if expr is ast.ArrayInit {
			if expr.is_fixed {
				s := g.styp(expr.typ)
				if !expr.has_index {
					g.write('(${s})')
				}
			}
		}
		if unwrap_option {
			g.expr(expr)
		} else {
			if temp_var_needed {
				g.write(tmp_var)
			} else {
				g.expr_with_cast(expr, typ, typ)
			}
		}

		if is_shared {
			g.write('->val')
		}
		g.write(')')
		if is_ptr && !is_var_mut {
			g.write('}}}))')
		}
	} else {
		is_var_mut := expr.is_auto_deref_var()
		str_fn_name := g.get_str_fn(typ)
		g.write('${str_fn_name}(')
		if sym.kind != .function {
			unwrap_option := expr is ast.Ident && expr.or_expr.kind == .propagate_option
			exp_typ := if unwrap_option { typ.clear_flag(.option) } else { typ }
			temp_var_needed := expr is ast.CallExpr
				&& (expr.return_type.is_ptr() || g.table.sym(expr.return_type).is_c_struct())
			mut tmp_var := ''
			if temp_var_needed {
				tmp_var = g.new_tmp_var()
				ret_typ := g.styp(exp_typ)
				line := g.go_before_last_stmt().trim_space()
				g.empty_line = true
				g.write('${ret_typ} ${tmp_var} = ')
				g.expr(expr)
				g.writeln(';')
				g.write(line)
			}
			if str_method_expects_ptr && !is_ptr && !typ.has_flag(.option) {
				g.write('&')
			} else if (!str_method_expects_ptr && is_ptr && !is_shared) || is_var_mut {
				g.write('*')
			} else {
				if sym.is_c_struct() {
					g.write(c_struct_ptr(sym, typ, str_method_expects_ptr))
				}
			}
			if temp_var_needed {
				g.write(tmp_var)
			} else {
				if expr is ast.StructInit && g.table.final_sym(expr.typ).is_primitive_fixed_array() {
					s := g.styp(expr.typ)
					g.write('(${s})')
				}
				g.expr_with_cast(expr, typ, typ)
			}
		} else if typ.has_flag(.option) {
			// only Option fn receive argument
			g.expr_with_cast(expr, typ, typ)
		}
		g.write(')')
	}
}
