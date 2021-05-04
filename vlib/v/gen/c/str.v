// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module c

import v.ast
import v.util

fn (mut g Gen) write_str_fn_definitions() {
	g.writeln(c_str_fn_defs)
}

fn (mut g Gen) string_literal(node ast.StringLiteral) {
	if node.is_raw {
		escaped_val := util.smart_quote(node.val, true)
		g.write('_SLIT("$escaped_val")')
		return
	}
	escaped_val := util.smart_quote(node.val, false)
	if g.is_c_call || node.language == .c {
		// In C calls we have to generate C strings
		// `C.printf("hi")` => `printf("hi");`
		g.write('"$escaped_val"')
	} else {
		// TODO calculate the literal's length in V, it's a bit tricky with all the
		// escape characters.
		// Clang and GCC optimize `strlen("lorem ipsum")` to `11`
		// g.write('tos4("$escaped_val", strlen("$escaped_val"))')
		// g.write('tos4("$escaped_val", $it.val.len)')
		// g.write('_SLIT("$escaped_val")')
		g.write('_SLIT("$escaped_val")')
	}
}

// optimize string interpolation in string builders:
// `sb.writeln('a=$a')` =>
// `sb.writeln('a='); sb.writeln(a.str())`
fn (mut g Gen) string_inter_literal_sb_optimized(call_expr ast.CallExpr) {
	node := call_expr.args[0].expr as ast.StringInterLiteral
	// sb_name := g.cur_call_expr.left
	// g.go_before_stmt(0)
	g.writeln('// sb inter opt')
	is_nl := call_expr.name == 'writeln'
	// println('optimize sb $call_expr.name')
	for i, val in node.vals {
		escaped_val := util.smart_quote(val, false)
		// if val == '' {
		// break
		// continue
		// }
		g.write('strings__Builder_write_string(&')
		g.expr(call_expr.left)
		g.write(', _SLIT("')
		g.write(escaped_val)
		g.writeln('"));')
		//
		if i >= node.exprs.len {
			break
		}
		// if node.expr_types.len <= i || node.exprs.len <= i {
		// continue
		// }
		if is_nl && i == node.exprs.len - 1 {
			g.write('strings__Builder_writeln(&')
		} else {
			g.write('strings__Builder_write_string(&')
		}
		g.expr(call_expr.left)
		g.write(', ')
		typ := node.expr_types[i]
		g.write(g.typ(typ))
		g.write('_str(')
		sym := g.table.get_type_symbol(typ)
		if sym.kind != .function {
			g.expr(node.exprs[i])
		}
		g.writeln('));')
	}
	g.writeln('')
	// println(node.vals)
	return
}

fn (mut g Gen) string_inter_literal(node ast.StringInterLiteral) {
	g.write('_STR("')
	// Build the string with %
	mut end_string := false
	for i, val in node.vals {
		mut escaped_val := val.replace_each(['%', '%%'])
		escaped_val = util.smart_quote(escaped_val, false)
		if i >= node.exprs.len {
			if escaped_val.len > 0 {
				end_string = true
				g.write('\\000')
				g.write(escaped_val)
			}
			break
		}
		g.write(escaped_val)
		mut typ := g.unwrap_generic(node.expr_types[i])
		sym := g.table.get_type_symbol(typ)
		if sym.kind == .alias {
			typ = (sym.info as ast.Alias).parent_type
		}
		// write correct format specifier to intermediate string
		g.write('%')
		fspec := node.fmts[i]
		mut fmt := if node.pluss[i] { '+' } else { '' }
		if node.fills[i] && node.fwidths[i] >= 0 {
			fmt = '${fmt}0'
		}
		if node.fwidths[i] != 0 {
			fmt = '$fmt${node.fwidths[i]}'
		}
		if node.precisions[i] != 987698 {
			fmt = '${fmt}.${node.precisions[i]}'
		}
		if fspec == `s` {
			if node.fwidths[i] == 0 {
				g.write('.*s')
			} else {
				g.write('*.*s')
			}
		} else if typ.is_float() {
			g.write('$fmt${fspec:c}')
		} else if typ.is_pointer() {
			if fspec == `p` {
				g.write('${fmt}p')
			} else {
				g.write('$fmt"PRI${fspec:c}PTR"')
			}
		} else if typ.is_int() {
			if fspec == `c` {
				g.write('${fmt}c')
			} else {
				g.write('$fmt"PRI${fspec:c}')
				if typ in [ast.i8_type, ast.byte_type] {
					g.write('8')
				} else if typ in [ast.i16_type, ast.u16_type] {
					g.write('16')
				} else if typ in [ast.i64_type, ast.u64_type] {
					g.write('64')
				} else {
					g.write('32')
				}
				g.write('"')
			}
		} else {
			// TODO: better check this case
			g.write('$fmt"PRId32"')
		}
		if i < node.exprs.len - 1 {
			g.write('\\000')
		}
	}
	num_string_parts := if end_string { node.exprs.len + 1 } else { node.exprs.len }
	g.write('", $num_string_parts, ')
	// Build args
	for i, expr in node.exprs {
		typ := g.unwrap_generic(node.expr_types[i])
		if typ == ast.string_type {
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
		} else if node.fmts[i] == `s` || typ.has_flag(.variadic) {
			g.gen_expr_to_string(expr, typ)
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
		if node.fmts[i] == `s` && node.fwidths[i] != 0 {
			g.write(', ${node.fwidths[i]}')
		}
		if i < node.exprs.len - 1 {
			g.write(', ')
		}
	}
	g.write(')')
}

fn (mut g Gen) gen_expr_to_string(expr ast.Expr, etype ast.Type) {
	is_shared := etype.has_flag(.shared_f)
	mut typ := etype
	if is_shared {
		typ = typ.clear_flag(.shared_f).set_nr_muls(0)
	}
	mut sym := g.table.get_type_symbol(typ)
	// when type is alias, print the aliased value
	if mut sym.info is ast.Alias {
		parent_sym := g.table.get_type_symbol(sym.info.parent_type)
		if parent_sym.has_method('str') {
			typ = sym.info.parent_type
			sym = parent_sym
		}
	}
	sym_has_str_method, str_method_expects_ptr, _ := sym.str_method_info()
	if typ.has_flag(.variadic) {
		str_fn_name := g.gen_str_for_type(typ)
		g.write('${str_fn_name}(')
		g.expr(expr)
		g.write(')')
	} else if typ == ast.string_type {
		g.expr(expr)
	} else if typ == ast.bool_type {
		g.expr(expr)
		g.write(' ? _SLIT("true") : _SLIT("false")')
	} else if sym.kind == .none_ {
		g.write('_SLIT("<none>")')
	} else if sym.kind == .enum_ {
		is_var := match expr {
			ast.SelectorExpr, ast.Ident { true }
			else { false }
		}
		if is_var {
			str_fn_name := g.gen_str_for_type(typ)
			g.write('${str_fn_name}(')
			g.enum_expr(expr)
			g.write(')')
		} else {
			g.write('_SLIT("')
			g.enum_expr(expr)
			g.write('")')
		}
	} else if sym_has_str_method
		|| sym.kind in [.array, .array_fixed, .map, .struct_, .multi_return, .sum_type, .interface_] {
		is_ptr := typ.is_ptr()
		is_var_mut := expr.is_auto_deref_var()
		str_fn_name := g.gen_str_for_type(typ)
		if is_ptr && !is_var_mut {
			g.write('_STR("&%.*s\\000", 2, ')
		}
		g.write('${str_fn_name}(')
		if str_method_expects_ptr && !is_ptr {
			g.write('&')
		} else if (!str_method_expects_ptr && is_ptr && !is_shared) || is_var_mut {
			g.write('*')
		}
		if expr is ast.ArrayInit {
			if expr.is_fixed {
				s := g.typ(expr.typ)
				g.write('($s)')
			}
		}
		g.expr_with_cast(expr, typ, typ)
		if is_shared {
			g.write('->val')
		}
		g.write(')')
		if is_ptr && !is_var_mut {
			g.write(')')
		}
	} else {
		str_fn_name := g.gen_str_for_type(typ)
		g.write('${str_fn_name}(')
		if sym.kind != .function {
			g.expr_with_cast(expr, typ, typ)
		}
		g.write(')')
	}
}
