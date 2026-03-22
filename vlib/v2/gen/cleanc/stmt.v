// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module cleanc

import v2.ast

fn (mut g Gen) set_file_module(file ast.File) {
	g.cur_file_name = file.name
	for stmt in file.stmts {
		if stmt is ast.ModuleStmt {
			g.cur_module = stmt.name.replace('.', '_')
			return
		}
	}
	// Files without a module declaration are in the 'main' module
	g.cur_module = 'main'
}

fn (mut g Gen) gen_stmts(stmts []ast.Stmt) {
	for i in 0 .. stmts.len {
		g.gen_stmt(stmts[i])
	}
}

fn (mut g Gen) gen_stmt(node ast.Stmt) {
	if !stmt_has_valid_data(node) {
		return
	}
	match node {
		ast.FnDecl {
			g.gen_fn_decl(node)
		}
		ast.AssignStmt {
			g.gen_assign_stmt(node)
		}
		ast.ExprStmt {
			if !expr_has_valid_data(node.expr) {
				return
			}
			if node.expr is ast.UnsafeExpr {
				unsafe_expr := node.expr as ast.UnsafeExpr
				if unsafe_expr.stmts.len > 1 {
					g.write_indent()
					g.sb.writeln('{')
					g.indent++
				}
				for stmt in unsafe_expr.stmts {
					g.gen_stmt(stmt)
				}
				if unsafe_expr.stmts.len > 1 {
					g.indent--
					g.write_indent()
					g.sb.writeln('}')
				}
				return
			}
			if node.expr is ast.IfExpr {
				g.write_indent()
				if_expr := node.expr as ast.IfExpr
				g.gen_if_expr_stmt(&if_expr)
				return
			}
			g.write_indent()
			g.expr(node.expr)
			g.sb.writeln(';')
		}
		ast.ReturnStmt {
			g.write_indent()
			if g.is_tuple_alias(g.cur_fn_ret_type) {
				if node.exprs.len == 1 {
					expr := node.exprs[0]
					if g.get_expr_type(expr) == g.cur_fn_ret_type {
						g.sb.write_string('return ')
						g.expr(expr)
						g.sb.writeln(';')
						return
					}
				}
				mut tuple_exprs := shallow_copy_exprs(node.exprs)
				if node.exprs.len == 1 && node.exprs[0] is ast.Tuple {
					tuple_expr := node.exprs[0] as ast.Tuple
					tuple_exprs = shallow_copy_exprs(tuple_expr.exprs)
				}
				field_types := g.tuple_aliases[g.cur_fn_ret_type] or { []string{} }
				g.sb.write_string('return ((${g.cur_fn_ret_type}){')
				for i, field_type in field_types {
					if i > 0 {
						g.sb.write_string(', ')
					}
					g.sb.write_string('.arg${i} = ')
					if i < tuple_exprs.len {
						// If the field type is an interface and the expression is a
						// concrete type (e.g., from smartcast narrowing), wrap in interface.
						if g.is_interface_type(field_type) {
							expr_type := g.get_expr_type(tuple_exprs[i]).trim_right('*')
							if expr_type != '' && expr_type != field_type
								&& !g.is_interface_type(expr_type) {
								if g.gen_interface_cast(field_type, tuple_exprs[i]) {
								} else {
									g.expr(tuple_exprs[i])
								}
							} else {
								g.expr(tuple_exprs[i])
							}
						} else {
							g.expr(tuple_exprs[i])
						}
					} else {
						g.sb.write_string(zero_value_for_type(field_type))
					}
				}
				g.sb.writeln('});')
				return
			}
			if node.exprs.len == 1 && node.exprs[0] is ast.IfExpr {
				if_expr := node.exprs[0] as ast.IfExpr
				g.gen_return_if_expr(if_expr, false)
				return
			}
			if g.cur_fn_ret_type.starts_with('Array_fixed_') {
				if node.exprs.len == 0 {
					g.sb.writeln('return (${g.cur_fn_c_ret_type}){0};')
					return
				}
				expr := node.exprs[0]
				g.sb.write_string('return ({ ${g.cur_fn_c_ret_type} _ret = (${g.cur_fn_c_ret_type}){0}; ')
				if expr is ast.CallExpr {
					if call_ret := g.get_call_return_type(expr.lhs, expr.args) {
						if call_ret == g.cur_fn_ret_type {
							g.sb.write_string('${g.cur_fn_c_ret_type} _tmp = ')
							g.expr(expr)
							g.sb.writeln('; memcpy(_ret.ret_arr, _tmp.ret_arr, sizeof(${g.cur_fn_ret_type})); _ret; });')
							return
						}
					}
				}
				if expr is ast.Ident || expr is ast.SelectorExpr || expr is ast.IndexExpr {
					g.sb.write_string('memcpy(_ret.ret_arr, ')
					g.expr(expr)
					g.sb.writeln(', sizeof(${g.cur_fn_ret_type})); _ret; });')
					return
				}
				g.sb.write_string('${g.cur_fn_ret_type} _arr = ')
				g.expr(expr)
				g.sb.writeln('; memcpy(_ret.ret_arr, _arr, sizeof(${g.cur_fn_ret_type})); _ret; });')
				return
			}
			if g.cur_fn_ret_type.starts_with('_option_') {
				if node.exprs.len == 0 {
					g.sb.writeln('return (${g.cur_fn_ret_type}){ .state = 2 };')
					return
				}
				expr := node.exprs[0]
				if expr is ast.BasicLiteral && expr.value == '0' {
					g.sb.writeln('return (${g.cur_fn_ret_type}){ .state = 2 };')
					return
				}
				if is_none_expr(expr) || expr is ast.Type {
					g.sb.writeln('return (${g.cur_fn_ret_type}){ .state = 2 };')
					return
				}
				mut expr_type := g.get_expr_type(expr)
				if (expr_type == '' || expr_type == 'int') && expr is ast.Ident {
					expr_type = g.get_local_var_c_type(expr.name) or { expr_type }
				}
				if expr is ast.Ident && expr.name == 'err' {
					g.sb.write_string('return (${g.cur_fn_ret_type}){ .is_error=true, .err=')
					g.expr(expr)
					g.sb.writeln(' };')
					return
				}
				if expr_type == 'IError' {
					g.sb.write_string('return (${g.cur_fn_ret_type}){ .is_error=true, .err=')
					g.expr(expr)
					g.sb.writeln(' };')
					return
				}
				if expr_type == g.cur_fn_ret_type {
					g.sb.write_string('return ')
					g.expr(expr)
					g.sb.writeln(';')
					return
				}
				value_type := option_value_type(g.cur_fn_ret_type)
				if value_type == '' || value_type == 'void' {
					g.sb.writeln('return (${g.cur_fn_ret_type}){ .state = 2 };')
					return
				}
				if value_type in g.tuple_aliases {
					field_types := g.tuple_aliases[value_type]
					mut tuple_exprs := shallow_copy_exprs(node.exprs)
					if node.exprs.len == 1 && node.exprs[0] is ast.Tuple {
						tuple_expr := node.exprs[0] as ast.Tuple
						tuple_exprs = shallow_copy_exprs(tuple_expr.exprs)
					}
					g.sb.write_string('return ({ ${g.cur_fn_ret_type} _opt = (${g.cur_fn_ret_type}){ .state = 2 }; ${value_type} _val = (${value_type}){')
					for i, field_type in field_types {
						if i > 0 {
							g.sb.write_string(', ')
						}
						g.sb.write_string('.arg${i} = ')
						if i < tuple_exprs.len {
							g.expr(tuple_exprs[i])
						} else {
							g.sb.write_string(zero_value_for_type(field_type))
						}
					}
					g.sb.writeln('}; _option_ok(&_val, (_option*)&_opt, sizeof(_val)); _opt; });')
					return
				}
				g.sb.write_string('return ({ ${g.cur_fn_ret_type} _opt = (${g.cur_fn_ret_type}){ .state = 2 }; ${value_type} _val = ')
				if value_type in g.sum_type_variants {
					g.gen_type_cast_expr(value_type, expr)
				} else {
					g.expr(expr)
				}
				g.sb.writeln('; _option_ok(&_val, (_option*)&_opt, sizeof(_val)); _opt; });')
				return
			}
			if g.cur_fn_ret_type.starts_with('_result_') {
				if node.exprs.len == 0 {
					g.sb.writeln('return (${g.cur_fn_ret_type}){ .is_error=false };')
					return
				}
				expr := node.exprs[0]
				if g.is_error_call_expr(expr) {
					g.sb.write_string('return (${g.cur_fn_ret_type}){ .is_error=true, .err=')
					g.expr(expr)
					g.sb.writeln(' };')
					return
				}
				// `return err` propagates the error from the or-block
				if expr is ast.Ident && expr.name == 'err' {
					g.sb.write_string('return (${g.cur_fn_ret_type}){ .is_error=true, .err=')
					g.expr(expr)
					g.sb.writeln(' };')
					return
				}
				value_type := g.result_value_type(g.cur_fn_ret_type)
				if value_type in g.tuple_aliases && node.exprs.len > 1 {
					field_types := g.tuple_aliases[value_type]
					g.sb.write_string('return ({ ${g.cur_fn_ret_type} _res = (${g.cur_fn_ret_type}){0}; ${value_type} _val = (${value_type}){')
					for i, field_type in field_types {
						if i > 0 {
							g.sb.write_string(', ')
						}
						g.sb.write_string('.arg${i} = ')
						if i < node.exprs.len {
							g.expr(node.exprs[i])
						} else {
							g.sb.write_string(zero_value_for_type(field_type))
						}
					}
					g.sb.writeln('}; _result_ok(&_val, (_result*)&_res, sizeof(_val)); _res; });')
					return
				}
				// `return T(x)` in `!T` functions can be an unlowered propagate path where `x` is already `_result_T`.
				// In that case return `x` directly, instead of casting to `T` and re-wrapping.
				if value_type != '' && expr is ast.CastExpr
					&& g.expr_type_to_c(expr.typ) == value_type {
					inner_type := g.get_expr_type(expr.expr)
					if inner_type == g.cur_fn_ret_type {
						g.sb.write_string('return ')
						g.expr(expr.expr)
						g.sb.writeln(';')
						return
					}
				}
				mut expr_type := g.get_expr_type(expr)
				if (expr_type == '' || expr_type == 'int') && expr is ast.Ident {
					expr_type = g.get_local_var_c_type(expr.name) or { expr_type }
				}
				if expr_type == g.cur_fn_ret_type {
					g.sb.write_string('return ')
					g.expr(expr)
					g.sb.writeln(';')
					return
				}
				// For CallExpr in result-returning function, check if the called
				// function also returns the same result type (passthrough).
				if expr is ast.CallExpr {
					if call_ret := g.get_call_return_type(expr.lhs, expr.args) {
						if call_ret == g.cur_fn_ret_type {
							g.sb.write_string('return ')
							g.expr(expr)
							g.sb.writeln(';')
							return
						}
					}
				}
				if value_type == '' || value_type == 'void' {
					g.sb.writeln('return (${g.cur_fn_ret_type}){ .is_error=false };')
					return
				}
				g.sb.write_string('return ({ ${g.cur_fn_ret_type} _res = (${g.cur_fn_ret_type}){0}; ${value_type} _val = ')
				if value_type in g.sum_type_variants {
					g.gen_type_cast_expr(value_type, expr)
				} else if g.is_interface_type(value_type) {
					// Mut params are pointers — dereference when returning as value
					if expr is ast.Ident && expr.name in g.cur_fn_mut_params {
						g.sb.write_string('(*')
						g.expr(expr)
						g.sb.write_string(')')
					} else if !g.gen_interface_cast(value_type, expr) {
						g.expr(expr)
					}
				} else {
					// Dereference mut parameters when returning by value in result-wrapping
					// (mut params are pointers, but _val expects a value)
					if expr is ast.Ident && expr.name in g.cur_fn_mut_params {
						g.sb.write_string('(*')
						g.expr(expr)
						g.sb.write_string(')')
					} else {
						g.expr(expr)
					}
				}
				g.sb.writeln('; _result_ok(&_val, (_result*)&_res, sizeof(_val)); _res; });')
				return
			}
			g.sb.write_string('return')
			if node.exprs.len > 0 {
				g.sb.write_string(' ')
				expr := node.exprs[0]
				if g.cur_fn_ret_type in g.sum_type_variants {
					g.gen_type_cast_expr(g.cur_fn_ret_type, expr)
				} else if g.is_interface_type(g.cur_fn_ret_type) {
					if g.get_expr_type(expr) == g.cur_fn_ret_type {
						g.expr(expr)
					} else {
						g.gen_type_cast_expr(g.cur_fn_ret_type, expr)
					}
				} else if g.cur_fn_ret_type.ends_with('*') && expr is ast.ParenExpr
					&& expr.expr is ast.PrefixExpr && (expr.expr as ast.PrefixExpr).op == .mul {
					// Return type is a pointer but the expression dereferences a pointer
					// (e.g., interface smartcast: *(T*)(obj._object)). Strip the deref
					// to return the pointer directly.
					deref := expr.expr as ast.PrefixExpr
					g.expr(deref.expr)
				} else {
					g.expr(expr)
				}
			} else if g.cur_fn_name == 'main' {
				g.sb.write_string(' 0')
			}
			g.sb.writeln(';')
		}
		ast.ForStmt {
			g.gen_for_stmt(node)
		}
		ast.FlowControlStmt {
			g.write_indent()
			if node.op == .key_break {
				g.sb.writeln('break;')
			} else if node.op == .key_continue {
				g.sb.writeln('continue;')
			} else if node.op == .key_goto {
				g.sb.writeln('goto ${node.label};')
			}
		}
		ast.ModuleStmt {
			g.cur_module = node.name.replace('.', '_')
		}
		ast.ImportStmt {}
		ast.ConstDecl {
			g.gen_const_decl(node)
		}
		ast.StructDecl {
			g.gen_struct_decl(node)
		}
		ast.EnumDecl {
			g.gen_enum_decl(node)
		}
		ast.TypeDecl {
			if node.variants.len > 0 {
				g.gen_sum_type_decl(node)
			} else if node.base_type !is ast.EmptyExpr {
				g.gen_type_alias(node)
			}
		}
		ast.InterfaceDecl {
			g.gen_interface_decl(node)
		}
		ast.GlobalDecl {
			g.gen_global_decl(node)
		}
		ast.Directive {
			// C directives are collected and emitted in the preamble.
			_ = node
		}
		ast.ForInStmt {
			panic('bug in v2 compiler: ForInStmt should have been lowered in v2.transformer')
		}
		ast.DeferStmt {
			panic('bug in v2 compiler: DeferStmt should have been lowered in v2.transformer (${g.cur_file_name}:${g.cur_fn_name})')
		}
		ast.AssertStmt {
			panic('bug in v2 compiler: AssertStmt should have been lowered in v2.transformer')
		}
		ast.ComptimeStmt {
			panic('bug in v2 compiler: ComptimeStmt should have been handled in v2.transformer')
		}
		ast.BlockStmt {
			for bs in node.stmts {
				g.gen_stmt(bs)
			}
		}
		ast.LabelStmt {
			g.write_indent()
			g.sb.writeln('${node.name}:')
			if node.stmt !is ast.EmptyStmt {
				g.gen_stmt(node.stmt)
			}
		}
		ast.AsmStmt {
			g.write_indent()
			g.sb.writeln('/* [TODO] AsmStmt */')
		}
		[]ast.Attribute {}
		ast.EmptyStmt {}
		// else {}
	}
}
