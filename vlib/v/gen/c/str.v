// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module c

import v.ast
import v.util

fn (mut g Gen) string_literal(node ast.StringLiteral) {
	escaped_val := cescape_nonascii(util.smart_quote(node.val, node.is_raw))
	if node.language == .c {
		g.write(cescaped_string_literal(escaped_val))
	} else {
		g.write('_S(${cescaped_string_literal(escaped_val)})')
	}
}

// optimize string interpolation in string builders:
// `sb.writeln('a=${a}')` =>
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

fn (g &Gen) option_mut_param_surface_type(expr ast.Expr) ast.Type {
	ident := match expr {
		ast.Ident { expr }
		else { return 0 }
	}

	mut typ := ast.Type(0)
	if ident.obj is ast.Var {
		typ = g.option_mut_param_surface_type_from_var(ident.name, ident.obj)
	}
	if scope_var := ident.scope.find_var(ident.name) {
		scope_typ := g.option_mut_param_surface_type_from_var(ident.name, scope_var)
		if scope_typ != 0 {
			typ = scope_typ
		}
	}
	if typ == 0 && ident.obj is ast.Var {
		if ident.obj.is_arg && ident.obj.orig_type.has_flag(.option) {
			typ = ident.obj.orig_type
		}
	}
	if typ == 0 || !typ.has_flag(.option) {
		return 0
	}
	return typ
}

fn (g &Gen) option_mut_param_surface_type_from_var(name string, var ast.Var) ast.Type {
	if !var.is_arg || var.is_unwrapped || !var.typ.has_flag(.option_mut_param_t) {
		return 0
	}
	mut typ := var.typ.clear_flag(.option_mut_param_t)
	if g.mut_option_param_assigned_directly(name) {
		inner := typ.clear_option_and_result()
		if inner.is_ptr() {
			typ = inner.deref().set_flag(.option)
		}
	}
	return typ
}

fn (mut g Gen) gen_expr_to_string(expr ast.Expr, etype ast.Type) {
	old_inside_opt_or_res := g.inside_opt_or_res
	g.inside_opt_or_res = true
	g.expected_fixed_arr = true
	defer {
		g.inside_opt_or_res = old_inside_opt_or_res
		g.expected_fixed_arr = false
	}
	mut expr_type := etype
	if expr is ast.Ident && g.resolved_ident_is_by_value_auto_deref_capture(expr) {
		resolved_scope_type := g.resolved_scope_var_type(expr)
		if resolved_scope_type != 0 {
			expr_type = resolved_scope_type
		}
	}
	is_shared := expr_type.has_flag(.shared_f)
	mut typ := expr_type
	if is_shared {
		typ = typ.clear_flag(.shared_f).set_nr_muls(0)
	}
	if expr is ast.Ident && g.cur_fn != unsafe { nil }
		&& g.mut_option_param_assigned_directly(expr.name) {
		for param in g.cur_fn.params {
			if param.name == expr.name && param.typ.has_flag(.option_mut_param_t) {
				mut opt_typ := param.typ.clear_flag(.option_mut_param_t)
				if opt_typ.is_ptr() {
					opt_typ = opt_typ.deref()
				}
				g.write('${g.get_str_fn(opt_typ)}(*')
				g.expr(expr)
				g.write(')')
				return
			}
		}
	}
	mut_arg_option_type := g.option_mut_param_surface_type(expr)
	if mut_arg_option_type != 0 {
		typ = mut_arg_option_type
	}
	if mut_arg_option_type != 0 && expr is ast.Ident
		&& g.mut_option_param_assigned_directly(expr.name) {
		g.write('${g.get_str_fn(mut_arg_option_type)}(*')
		g.expr(expr)
		g.write(')')
		return
	}
	if mut_arg_option_type == 0 && expr is ast.Ident && g.expr_is_auto_deref_var(expr)
		&& typ.has_flag(.option) {
		g.write('${g.get_str_fn(typ)}(*')
		g.expr(expr)
		g.write(')')
		return
	}
	if expr is ast.Ident && expr.obj is ast.Var && expr.obj.is_inherited {
		inherited_typ := g.resolved_scope_var_type(expr)
		if inherited_typ != 0 {
			typ = inherited_typ
		}
	}
	// `mut ?T` params are passed by pointer in C, but should still stringify as
	// option values rather than as raw `&...` pointers.
	is_ptr := typ.is_ptr() || (typ.has_flag(.option_mut_param_t) && !typ.has_flag(.option))
	mut sym := g.table.sym(typ)
	// when type is non-option alias and doesn't has `str()`, print the aliased value
	if mut sym.info is ast.Alias && !sym.has_method('str') && !expr_type.has_flag(.option) {
		parent_sym := g.table.sym(sym.info.parent_type)
		if parent_sym.has_method('str') {
			typ = sym.info.parent_type
			sym = unsafe { parent_sym }
		}
	}
	sym_has_str_method, str_method_expects_ptr, _ := sym.str_method_info()
	// When interface smartcast expr produces a pointer in C but type was already dereffed,
	// we need to dereference the generated expression.
	is_interface_smartcast_to_nonptr := !is_ptr && expr is ast.Ident && expr.obj is ast.Var
		&& (expr.obj as ast.Var).smartcasts.len > 0
		&& (expr.obj as ast.Var).smartcasts.last().is_ptr()
		&& g.table.final_sym(g.unwrap_generic((expr.obj as ast.Var).orig_type)).kind == .interface
		&& g.table.final_sym(g.unwrap_generic((expr.obj as ast.Var).smartcasts.last())).kind != .interface
	use_raw_interface_smartcast_expr := is_ptr && expr is ast.Ident && expr.obj is ast.Var
		&& (expr.obj as ast.Var).smartcasts.len > 0
		&& (expr.obj as ast.Var).smartcasts.last().is_ptr()
		&& (g.table.final_sym(g.unwrap_generic((expr.obj as ast.Var).typ)).kind == .interface
		|| ((expr.obj as ast.Var).orig_type != 0
		&& g.table.final_sym(g.unwrap_generic((expr.obj as ast.Var).orig_type)).kind == .interface))
	if typ.has_flag(.variadic) {
		str_fn_name := g.get_str_fn(typ)
		g.write('${str_fn_name}(')
		g.expr(expr)
		g.write(')')
	} else if typ == ast.string_type {
		if expr_type.is_ptr() {
			g.write('*')
		}
		g.expr(expr)
	} else if typ == ast.bool_type {
		g.write('(')
		g.expr(expr)
		g.write(' ? _S("true") : _S("false"))')
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
		unwrap_opt_or_res := match expr {
			ast.CallExpr, ast.ComptimeCall, ast.ComptimeSelector, ast.InfixExpr, ast.PrefixExpr,
			ast.SelectorExpr {
				expr.or_block.kind != .absent
			}
			ast.Ident, ast.IndexExpr {
				expr.or_expr.kind != .absent
			}
			else {
				false
			}
		}

		exp_typ := if unwrap_opt_or_res { typ.clear_option_and_result() } else { typ }
		if unwrap_opt_or_res {
			typ = exp_typ
		}
		is_dump_expr := expr is ast.DumpExpr
		is_var_mut := g.expr_is_auto_deref_var(expr) && !typ.has_flag(.option)
		str_fn_name := if mut_arg_option_type != 0 {
			g.get_str_fn(mut_arg_option_type)
		} else {
			g.get_str_fn(exp_typ)
		}
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
			g.write('builtin__str_intp(1, _MOV((StrIntpData[]){{_S("${ref_str}"), ${si_s_code}, {.d_s = builtin__isnil(')
			if typ.has_flag(.option) || mut_arg_option_type != 0 {
				if mut_arg_option_type != 0 {
					if temp_var_needed {
						g.write(tmp_var)
					} else {
						g.expr(expr)
					}
					g.write(') ? _S("nil") : ')
				} else {
					g.write('*(${g.base_type(exp_typ)}*)&')
					if temp_var_needed {
						g.write(tmp_var)
					} else {
						g.expr(expr)
					}
					g.write('.data) ? _S("Option(&nil)") : ')
				}
			} else {
				inside_interface_deref_old := g.inside_interface_deref
				g.inside_interface_deref = false
				defer(fn) {
					g.inside_interface_deref = inside_interface_deref_old
				}
				if temp_var_needed {
					g.write(tmp_var)
				} else if use_raw_interface_smartcast_expr {
					old_inside_selector_lhs := g.inside_selector_lhs
					g.inside_selector_lhs = true
					g.expr(expr)
					g.inside_selector_lhs = old_inside_selector_lhs
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
				defer(fn) {
					g.write(')')
				}
			} else {
				g.write('&')
			}
		} else if mut_arg_option_type != 0 {
			g.write('*')
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
				g.write('*'.repeat(expr_type.nr_muls()))
			}
		} else if !str_method_expects_ptr && is_interface_smartcast_to_nonptr {
			g.write('*')
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
		if unwrap_opt_or_res {
			g.expr(expr)
		} else {
			if temp_var_needed {
				g.write(tmp_var)
			} else if use_raw_interface_smartcast_expr {
				old_inside_selector_lhs := g.inside_selector_lhs
				g.inside_selector_lhs = true
				g.expr_with_cast(expr, typ, typ)
				g.inside_selector_lhs = old_inside_selector_lhs
			} else {
				g.expr_with_cast(expr, typ, typ)
			}
		}

		if is_shared {
			g.write('->val')
		}
		g.write(')')
		if is_ptr && !is_var_mut {
			g.write('}, 0, 0, 0}}))')
		}
	} else {
		is_var_mut := g.expr_is_auto_deref_var(expr) && !typ.has_flag(.option)
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
			} else if typ.has_flag(.option_mut_param_t) {
				g.write('*')
			} else if (!str_method_expects_ptr && is_ptr && !is_shared) || is_var_mut {
				g.write('*'.repeat(typ.nr_muls()))
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
