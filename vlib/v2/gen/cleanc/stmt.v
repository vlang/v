// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module cleanc

import v2.ast
import v2.types

fn (mut g Gen) set_file_module(file ast.File) {
	g.cur_file_name = file.name
	g.cur_import_modules.clear()
	for imp in file.imports {
		mod_name := if imp.name.contains('.') { imp.name.all_after_last('.') } else { imp.name }
		if imp.alias != '' {
			g.cur_import_modules[imp.alias] = mod_name
		}
		if !imp.is_aliased && mod_name != '' {
			g.cur_import_modules[mod_name] = mod_name
		}
	}
	g.is_module_ident_cache.clear()
	g.resolved_module_names.clear()
	for stmt in file.stmts {
		if stmt is ast.ModuleStmt {
			g.cur_module = stmt.name.replace('.', '_')
			return
		}
	}
	g.cur_module = if file.mod != '' { file.mod.replace('.', '_') } else { 'main' }
}

fn (mut g Gen) gen_stmts(stmts []ast.Stmt) {
	saved_file_name := g.cur_file_name
	saved_module := g.cur_module
	saved_import_modules := g.cur_import_modules.clone()
	for i in 0 .. stmts.len {
		g.cur_file_name = saved_file_name
		g.cur_module = saved_module
		g.cur_import_modules = saved_import_modules.clone()
		g.is_module_ident_cache.clear()
		g.resolved_module_names.clear()
		g.gen_stmt(stmts[i])
	}
	g.cur_file_name = saved_file_name
	g.cur_module = saved_module
	g.cur_import_modules = saved_import_modules.clone()
	g.is_module_ident_cache.clear()
	g.resolved_module_names.clear()
}

fn (mut g Gen) gen_scoped_stmts(stmts []ast.Stmt) {
	saved_runtime_local_types := g.runtime_local_types.clone()
	saved_runtime_decl_types := g.runtime_decl_types.clone()
	saved_not_local_var_cache := g.not_local_var_cache.clone()
	g.gen_stmts(stmts)
	g.runtime_local_types = saved_runtime_local_types.clone()
	g.runtime_decl_types = saved_runtime_decl_types.clone()
	g.not_local_var_cache = saved_not_local_var_cache.clone()
}

fn (mut g Gen) gen_scoped_expr_stmts(expr ast.Expr) {
	saved_runtime_local_types := g.runtime_local_types.clone()
	saved_runtime_decl_types := g.runtime_decl_types.clone()
	saved_not_local_var_cache := g.not_local_var_cache.clone()
	g.gen_stmts_from_expr(expr)
	g.runtime_local_types = saved_runtime_local_types.clone()
	g.runtime_decl_types = saved_runtime_decl_types.clone()
	g.not_local_var_cache = saved_not_local_var_cache.clone()
}

fn tuple_field_types_need_stmt_expr(field_types []string) bool {
	for field_type in field_types {
		if field_type.starts_with('Array_fixed_') && !field_type.ends_with('*') {
			return true
		}
	}
	return false
}

fn (mut g Gen) gen_tuple_field_expr_value(field_type string, expr ast.Expr) {
	if g.is_interface_type(field_type) {
		expr_type := g.get_expr_type(expr).trim_right('*')
		if expr_type != '' && expr_type != field_type && !g.is_interface_type(expr_type) {
			if g.gen_interface_cast(field_type, expr) {
				return
			}
		}
	}
	g.expr(expr)
}

fn (mut g Gen) gen_tuple_field_stmt_expr_assign(tuple_name string, idx int, field_type string, expr ast.Expr) {
	field_name := '${tuple_name}.arg${idx}'
	if field_type.starts_with('Array_fixed_') && !field_type.ends_with('*') {
		if expr is ast.CallExpr {
			if call_ret := g.get_call_return_type(expr.lhs, expr.args) {
				if call_ret == field_type {
					wrapper_type := g.c_fn_return_type_from_v(field_type)
					tmp_name := '_tuple_arr_${g.tmp_counter}'
					g.tmp_counter++
					g.sb.write_string('{ ${wrapper_type} ${tmp_name} = ')
					g.expr(expr)
					g.sb.write_string('; memcpy(${field_name}, ${tmp_name}.ret_arr, sizeof(${field_type})); } ')
					return
				}
			}
		}
		if expr is ast.IfExpr {
			g.gen_decl_if_expr(field_name, field_type, &expr)
			return
		}
		g.sb.write_string('memcpy(${field_name}, ')
		if expr is ast.ArrayInitExpr {
			g.sb.write_string('((${field_type})')
			g.expr(expr)
			g.sb.write_string(')')
		} else {
			g.expr(expr)
		}
		g.sb.write_string(', sizeof(${field_type})); ')
		return
	}
	g.sb.write_string('${field_name} = ')
	g.gen_tuple_field_expr_value(field_type, expr)
	g.sb.write_string('; ')
}

fn (mut g Gen) gen_tuple_value_stmt_expr(tuple_type string, tuple_exprs []ast.Expr, field_types []string) {
	tmp_name := '_tuple_ret_${g.tmp_counter}'
	g.tmp_counter++
	g.sb.write_string('({ ${tuple_type} ${tmp_name} = (${tuple_type}){0}; ')
	for i, field_type in field_types {
		if i < tuple_exprs.len {
			g.gen_tuple_field_stmt_expr_assign(tmp_name, i, field_type, tuple_exprs[i])
		} else if field_type.starts_with('Array_fixed_') && !field_type.ends_with('*') {
			g.sb.write_string('memset(${tmp_name}.arg${i}, 0, sizeof(${field_type})); ')
		} else {
			g.sb.write_string('${tmp_name}.arg${i} = ${zero_value_for_type(field_type)}; ')
		}
	}
	g.sb.write_string('${tmp_name}; })')
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
			if node.expr is ast.ComptimeExpr {
				comptime_expr := node.expr as ast.ComptimeExpr
				if comptime_expr.expr is ast.IfExpr {
					g.gen_comptime_if_stmt(comptime_expr.expr as ast.IfExpr)
					return
				}
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
			g.emit_scheduled_drops_at_return()
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
				if tuple_field_types_need_stmt_expr(field_types) {
					g.sb.write_string('return ')
					g.gen_tuple_value_stmt_expr(g.cur_fn_ret_type, tuple_exprs, field_types)
					g.sb.writeln(';')
					return
				}
				g.sb.write_string('return ((${g.cur_fn_ret_type}){')
				for i, field_type in field_types {
					if i > 0 {
						g.sb.write_string(', ')
					}
					g.sb.write_string('.arg${i} = ')
					if i < tuple_exprs.len {
						g.gen_tuple_field_expr_value(field_type, tuple_exprs[i])
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
				if (expr_type == '' || expr_type == 'int') && expr is ast.CallExpr {
					expr_type = g.get_call_return_type(expr.lhs, expr.args) or { expr_type }
				}
				value_type := option_value_type(g.cur_fn_ret_type)
				if expr is ast.Ident && expr.name == 'err' && (g.get_local_var_c_type(expr.name) or {
					'IError'
				}) in ['IError', 'builtin__IError'] {
					g.sb.write_string('return (${g.cur_fn_ret_type}){ .is_error=true, .err=')
					g.expr(expr)
					g.sb.writeln(' };')
					return
				}
				if expr_type == 'IError' {
					if value_type != '' && value_type != 'void' {
						if expr is ast.CastExpr
							&& g.expr_type_to_c(expr.typ) in ['IError', 'builtin__IError'] {
							concrete_type := g.concrete_type_for_interface_value('IError',
								expr.expr)
							if concrete_type == value_type {
								g.sb.write_string('return ({ ${g.cur_fn_ret_type} _opt = (${g.cur_fn_ret_type}){ .state = 2 }; ${value_type} _val = ')
								g.expr(expr.expr)
								g.sb.writeln('; _option_ok(&_val, (_option*)&_opt, sizeof(_val)); _opt; });')
								return
							}
						} else if expr is ast.CallOrCastExpr
							&& g.expr_type_to_c(expr.lhs) in ['IError', 'builtin__IError'] {
							concrete_type := g.concrete_type_for_interface_value('IError',
								expr.expr)
							if concrete_type == value_type {
								g.sb.write_string('return ({ ${g.cur_fn_ret_type} _opt = (${g.cur_fn_ret_type}){ .state = 2 }; ${value_type} _val = ')
								g.expr(expr.expr)
								g.sb.writeln('; _option_ok(&_val, (_option*)&_opt, sizeof(_val)); _opt; });')
								return
							}
						}
					}
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
				if value_type == '' || value_type == 'void' {
					g.sb.writeln('return (${g.cur_fn_ret_type}){ .state = 2 };')
					return
				}
				if value_type in g.tuple_aliases {
					if node.exprs.len == 1 && expr_type == value_type && node.exprs[0] !is ast.Tuple {
						g.sb.write_string('return ({ ${g.cur_fn_ret_type} _opt = (${g.cur_fn_ret_type}){ .state = 2 }; ${value_type} _val = ')
						g.expr(expr)
						g.sb.writeln('; _option_ok(&_val, (_option*)&_opt, sizeof(_val)); _opt; });')
						return
					}
					field_types := g.tuple_aliases[value_type]
					mut tuple_exprs := shallow_copy_exprs(node.exprs)
					if node.exprs.len == 1 && node.exprs[0] is ast.Tuple {
						tuple_expr := node.exprs[0] as ast.Tuple
						tuple_exprs = shallow_copy_exprs(tuple_expr.exprs)
					}
					if tuple_field_types_need_stmt_expr(field_types) {
						g.sb.write_string('return ({ ${g.cur_fn_ret_type} _opt = (${g.cur_fn_ret_type}){ .state = 2 }; ${value_type} _val = ')
						g.gen_tuple_value_stmt_expr(value_type, tuple_exprs, field_types)
						g.sb.writeln('; _option_ok(&_val, (_option*)&_opt, sizeof(_val)); _opt; });')
						return
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
				} else if g.gen_auto_deref_value_param_arg(value_type, expr) {
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
				// `return err` propagates the error from the or-block. A user local
				// named `err` can still be a concrete error value and must be wrapped.
				if expr is ast.Ident && expr.name == 'err' && (g.get_local_var_c_type(expr.name) or {
					'IError'
				}) in ['IError', 'builtin__IError'] {
					g.sb.write_string('return (${g.cur_fn_ret_type}){ .is_error=true, .err=')
					g.expr(expr)
					g.sb.writeln(' };')
					return
				}
				value_type := g.result_value_c_type(g.cur_fn_ret_type)
				if value_type in g.tuple_aliases && node.exprs.len > 1 {
					field_types := g.tuple_aliases[value_type]
					if tuple_field_types_need_stmt_expr(field_types) {
						g.sb.write_string('return ({ ${g.cur_fn_ret_type} _res = (${g.cur_fn_ret_type}){0}; ${value_type} _val = ')
						g.gen_tuple_value_stmt_expr(value_type, node.exprs, field_types)
						g.sb.writeln('; _result_ok(&_val, (_result*)&_res, sizeof(_val)); _res; });')
						return
					}
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
				if is_ierror_c_type(expr_type) {
					g.sb.write_string('return (${g.cur_fn_ret_type}){ .is_error=true, .err=')
					g.expr(expr)
					g.sb.writeln(' };')
					return
				}
				if g.concrete_ierror_base_for_c_type(expr_type) != '' {
					g.sb.write_string('return (${g.cur_fn_ret_type}){ .is_error=true, .err=')
					g.gen_ierror_from_concrete_expr(expr, expr_type)
					g.sb.writeln(' };')
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
				if value_type.starts_with('Array_fixed_') {
					g.sb.write_string('return ({ ${g.cur_fn_ret_type} _res = (${g.cur_fn_ret_type}){0}; ${value_type} _val = {0}; ')
					is_zero_fixed_array := expr is ast.ArrayInitExpr && expr.exprs.len == 0
						&& expr.init is ast.EmptyExpr
					if !is_zero_fixed_array {
						g.sb.write_string('memcpy(_val, ')
						g.expr(expr)
						g.sb.write_string(', sizeof(_val)); ')
					}
					g.sb.writeln('_result_ok(&_val, (_result*)&_res, sizeof(_val)); _res; });')
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
					if !g.gen_auto_deref_value_param_arg(value_type, expr) {
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
				if g.gen_return_mut_param_value(expr) {
				} else if g.cur_fn_ret_type in g.sum_type_variants {
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
				} else if vector_elem_type_for_name(g.cur_fn_ret_type) != '' && expr is ast.InitExpr
					&& g.gen_simd_vector_init_expr(g.cur_fn_ret_type, expr.fields) {
				} else if g.gen_auto_deref_value_param_arg(g.cur_fn_ret_type, expr) {
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
				if g.comptime_continue_label != '' {
					g.sb.writeln('goto ${g.comptime_continue_label};')
				} else {
					g.sb.writeln('continue;')
				}
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
			g.gen_comptime_stmt(node)
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

fn (mut g Gen) gen_comptime_stmt(node ast.ComptimeStmt) {
	inner := node.stmt
	if inner is ast.ForStmt {
		g.gen_comptime_for(inner)
		return
	}
	if inner is ast.ExprStmt {
		if inner.expr is ast.ComptimeExpr {
			if inner.expr.expr is ast.IfExpr {
				g.gen_comptime_if_stmt(inner.expr.expr)
				return
			}
		}
		g.write_indent()
		g.expr(inner.expr)
		g.sb.writeln(';')
		return
	}
	g.write_indent()
	g.sb.writeln('/* [TODO] ComptimeStmt */')
}

fn (mut g Gen) gen_comptime_if_stmt(node ast.IfExpr) {
	result := g.eval_comptime_cond(node.cond)
	if result {
		g.gen_stmts(node.stmts)
		return
	}
	if node.else_expr is ast.IfExpr {
		else_if := node.else_expr as ast.IfExpr
		if else_if.cond is ast.EmptyExpr {
			g.gen_stmts(else_if.stmts)
		} else {
			g.gen_comptime_if_stmt(else_if)
		}
	}
}

fn (mut g Gen) gen_comptime_for(node ast.ForStmt) {
	// Extract ForInStmt from the ForStmt's init field
	if node.init !is ast.ForInStmt {
		g.write_indent()
		g.sb.writeln('/* [TODO] ComptimeFor non-for-in */')
		return
	}
	for_in := node.init as ast.ForInStmt
	// The expression should be T.fields (a SelectorExpr)
	if for_in.expr !is ast.SelectorExpr {
		g.write_indent()
		g.sb.writeln('/* [TODO] ComptimeFor non-selector */')
		return
	}
	sel := for_in.expr as ast.SelectorExpr
	kind := sel.rhs.name
	if kind !in ['fields', 'methods'] {
		g.write_indent()
		g.sb.writeln('/* [TODO] ComptimeFor ${kind} */')
		return
	}
	type_name := sel.lhs.name()
	concrete := g.active_generic_types[type_name] or {
		c_name := g.expr_type_to_c(sel.lhs).trim_space()
		g.concrete_type_from_c_name(c_name) or {
			g.write_indent()
			g.sb.writeln('/* [TODO] ComptimeFor unknown type ${type_name} */')
			return
		}
	}
	if concrete !is types.Struct {
		g.write_indent()
		g.sb.writeln('/* ComptimeFor: ${type_name} is not a struct */')
		return
	}
	struct_type := g.comptime_for_struct_type(concrete, concrete as types.Struct)
	if kind == 'methods' {
		g.gen_comptime_for_methods(node, for_in, type_name, struct_type)
		return
	}
	field_var := for_in.value.name()
	// Save comptime state
	prev_field_var := g.comptime_field_var
	prev_field_name := g.comptime_field_name
	prev_field_type := g.comptime_field_type
	prev_field_raw_type := g.comptime_field_raw_type
	prev_field_attrs := g.comptime_field_attrs
	prev_field_idx := g.comptime_field_idx
	prev_field_is_embed := g.comptime_field_is_embed
	prev_continue_label := g.comptime_continue_label
	g.comptime_field_var = field_var
	g.write_indent()
	g.sb.writeln('{ /* comptime for ${field_var} in ${type_name}.fields */')
	g.indent++
	for i, field in struct_type.fields {
		g.comptime_field_name = field.name
		g.comptime_field_type = g.types_type_to_c(field.typ)
		g.comptime_field_raw_type = field.typ
		g.comptime_field_attrs = g.comptime_field_attribute_strings(struct_type.name, field)
		g.comptime_field_idx = i
		g.comptime_field_is_embed = g.comptime_field_is_embedded(struct_type, field)
		g.tmp_counter++
		g.comptime_continue_label = '__v_ctf_continue_${g.tmp_counter}_${i}'
		g.write_indent()
		g.sb.writeln('{ /* field ${i}: ${field.name} */')
		g.indent++
		// Use ForStmt.stmts for the loop body
		g.gen_stmts(node.stmts)
		g.write_indent()
		g.sb.writeln('${g.comptime_continue_label}:;')
		g.indent--
		g.write_indent()
		g.sb.writeln('}')
	}
	g.indent--
	g.write_indent()
	g.sb.writeln('}')
	// Restore comptime state
	g.comptime_field_var = prev_field_var
	g.comptime_field_name = prev_field_name
	g.comptime_field_type = prev_field_type
	g.comptime_field_raw_type = prev_field_raw_type
	g.comptime_field_attrs = prev_field_attrs
	g.comptime_field_idx = prev_field_idx
	g.comptime_field_is_embed = prev_field_is_embed
	g.comptime_continue_label = prev_continue_label
}

fn (g &Gen) comptime_field_is_embedded(struct_type types.Struct, field types.Field) bool {
	for embedded in struct_type.embedded {
		embedded_name := embedded.name.all_after_last('__')
		if field.name == embedded.name || field.name == embedded_name {
			return true
		}
	}
	return false
}

fn (mut g Gen) comptime_for_struct_type(concrete types.Type, fallback types.Struct) types.Struct {
	struct_c_name := g.types_type_to_c(concrete).trim_space().trim_right('*')
	if struct_c_name == '' {
		return fallback
	}
	full_struct_type := g.lookup_struct_type_by_c_name(struct_c_name)
	if full_struct_type.fields.len > 0
		&& !type_contains_generic_placeholder(types.Type(full_struct_type)) {
		return full_struct_type
	}
	return fallback
}

// gen_comptime_for_methods emits the body of `$for method in T.methods` by
// looping over the AST FnDecls whose receiver C-type matches T, setting the
// `g.comptime_method_*` state per iteration, and recursively generating the
// loop body so per-method selectors (method.name, method.attrs, etc.) and
// `app.$method(...)` dispatch can be resolved by expr.v.
fn (mut g Gen) gen_comptime_for_methods(node ast.ForStmt, for_in ast.ForInStmt, type_name string, struct_type types.Struct) {
	concrete_struct_type := types.Type(struct_type)
	struct_c_name := g.types_type_to_c(concrete_struct_type).trim_space().trim_right('*')
	method_decls := g.collect_method_fndecls(struct_type.name, struct_c_name)
	method_var := for_in.value.name()
	prev_method_var := g.comptime_method_var
	prev_method_name := g.comptime_method_name
	prev_method_attrs := g.comptime_method_attrs
	prev_method_return_type := g.comptime_method_return_type
	prev_method_args := g.comptime_method_args
	prev_method_idx := g.comptime_method_idx
	prev_method_receiver_type := g.comptime_method_receiver_type
	prev_method_struct_name := g.comptime_method_struct_name
	prev_continue_label := g.comptime_continue_label
	g.comptime_method_var = method_var
	g.comptime_method_receiver_type = struct_c_name
	g.comptime_method_struct_name = struct_type.name
	g.write_indent()
	g.sb.writeln('{ /* comptime for ${method_var} in ${type_name}.methods */')
	g.indent++
	for i, decl in method_decls {
		g.comptime_method_name = decl.name
		g.comptime_method_attrs = comptime_attribute_strings(decl.attributes)
		g.comptime_method_return_type = decl.typ.return_type
		g.comptime_method_args = decl.typ.params.clone()
		g.comptime_method_idx = i
		g.tmp_counter++
		g.comptime_continue_label = '__v_ctm_continue_${g.tmp_counter}_${i}'
		g.write_indent()
		g.sb.writeln('{ /* method ${i}: ${decl.name} */')
		g.indent++
		g.gen_stmts(node.stmts)
		g.write_indent()
		g.sb.writeln('${g.comptime_continue_label}:;')
		g.indent--
		g.write_indent()
		g.sb.writeln('}')
	}
	g.indent--
	g.write_indent()
	g.sb.writeln('}')
	g.comptime_method_var = prev_method_var
	g.comptime_method_name = prev_method_name
	g.comptime_method_attrs = prev_method_attrs
	g.comptime_method_return_type = prev_method_return_type
	g.comptime_method_args = prev_method_args
	g.comptime_method_idx = prev_method_idx
	g.comptime_method_receiver_type = prev_method_receiver_type
	g.comptime_method_struct_name = prev_method_struct_name
	g.comptime_continue_label = prev_continue_label
}

// collect_method_fndecls walks all loaded files and returns FnDecls whose
// receiver type matches either the struct V-name or its C-mangled name.
fn (mut g Gen) collect_method_fndecls(struct_v_name string, struct_c_name string) []ast.FnDecl {
	mut out := []ast.FnDecl{}
	for file in g.files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl {
				if !stmt.is_method {
					continue
				}
				if stmt.receiver.typ is ast.EmptyExpr {
					continue
				}
				receiver_v_name := stmt.receiver.typ.name()
				if receiver_v_name == struct_v_name || receiver_v_name == struct_c_name
					|| short_type_name(struct_c_name) == receiver_v_name {
					out << stmt
					continue
				}
				// Compare resolved C name as fallback (handles module-qualified receivers).
				receiver_c_name := g.expr_type_to_c(stmt.receiver.typ).trim_space().trim_right('*')
				if receiver_c_name == struct_c_name {
					out << stmt
				}
			}
		}
	}
	return out
}

fn comptime_attribute_strings(attrs []ast.Attribute) []string {
	mut out := []string{cap: attrs.len}
	for attr in attrs {
		if attr.name != '' {
			if attr.value is ast.EmptyExpr {
				out << attr.name
			} else {
				out << '${attr.name}: ${attr.value.name().trim("'")}'
			}
		} else if attr.value !is ast.EmptyExpr {
			out << attr.value.name().trim("'")
		}
	}
	return out
}

fn (mut g Gen) gen_return_mut_param_value(expr ast.Expr) bool {
	if expr !is ast.Ident || g.cur_fn_ret_type.ends_with('*') {
		return false
	}
	ident := expr as ast.Ident
	if ident.name !in g.cur_fn_mut_params {
		return false
	}
	local_type := (g.get_local_var_c_type(ident.name) or { '' }).trim_space()
	if !local_type.ends_with('*') {
		return false
	}
	local_base := local_type.trim_right('*')
	ret_base := g.cur_fn_ret_type.trim_right('*')
	if local_base == '' || ret_base == '' {
		return false
	}
	if local_base != ret_base && short_type_name(local_base) != short_type_name(ret_base) {
		return false
	}
	g.sb.write_string('(*')
	g.expr(expr)
	g.sb.write_string(')')
	return true
}

fn (mut g Gen) comptime_field_attribute_strings(struct_name string, field types.Field) []string {
	for file in g.files {
		for stmt in file.stmts {
			if stmt is ast.StructDecl {
				if stmt.name != struct_name && !struct_name.ends_with('__${stmt.name}') {
					continue
				}
				for ast_field in stmt.fields {
					if ast_field.name == field.name {
						return comptime_attribute_strings(ast_field.attributes)
					}
				}
			}
		}
	}
	return comptime_attribute_strings(field.attributes)
}
