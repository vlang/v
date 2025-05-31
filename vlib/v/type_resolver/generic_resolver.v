// Copyright (c) 2019-2024 Felipe Pena All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module type_resolver

import v.ast

// unwrap_generic_expr retrieves the concrete type from a generic expr
pub fn (mut ct TypeResolver) unwrap_generic_expr(expr ast.Expr, default_typ ast.Type) ast.Type {
	match expr {
		ast.StringLiteral, ast.StringInterLiteral {
			return ast.string_type
		}
		ast.ParExpr {
			return ct.unwrap_generic_expr(expr.expr, default_typ)
		}
		ast.CastExpr {
			return expr.typ
		}
		ast.InfixExpr {
			if expr.left_ct_expr {
				return ct.resolver.unwrap_generic(ct.get_type(expr.left))
			}
			if expr.right_ct_expr {
				return ct.resolver.unwrap_generic(ct.get_type(expr.right))
			}
			return default_typ
		}
		ast.Ident {
			return if expr.ct_expr {
				ct.resolver.unwrap_generic(ct.get_type(expr))
			} else {
				default_typ
			}
		}
		ast.AsCast {
			return ct.resolver.unwrap_generic(expr.typ)
		}
		else {
			return default_typ
		}
	}
}

// is_generic_param_var checks if the var is related to generic parameter
@[inline]
pub fn (t &TypeResolver) is_generic_param_var(node ast.Expr) bool {
	return node is ast.Ident && node.info is ast.IdentVar && node.obj is ast.Var
		&& (node.obj as ast.Var).ct_type_var == .generic_param
}

// is_generic_expr checks if the expr relies on fn generic argument
pub fn (t &TypeResolver) is_generic_expr(node ast.Expr) bool {
	return match node {
		ast.Ident {
			// variable declared as generic type
			t.is_generic_param_var(node)
		}
		ast.IndexExpr {
			// generic_var[N]
			t.is_generic_param_var(node.left)
		}
		ast.CallExpr {
			// fn which has any generic dependent expr
			if node.args.any(t.is_generic_param_var(it.expr)) {
				return true
			}
			if node.is_static_method && node.left_type.has_flag(.generic) {
				return true
			}
			if node.return_type_generic != 0 && node.return_type_generic.has_flag(.generic) {
				return true
			}
			// fn[T]() or generic_var.fn[T]()
			node.concrete_types.any(it.has_flag(.generic))
		}
		ast.SelectorExpr {
			// generic_var.property
			t.is_generic_param_var(node.expr)
		}
		ast.AsCast {
			// var as T
			node.typ.has_flag(.generic)
		}
		else {
			false
		}
	}
}

// get_generic_array_fixed_element_type retrieves the plain element type from a nested fixed array [N][N]T -> T
pub fn (t &TypeResolver) get_generic_array_fixed_element_type(array ast.ArrayFixed) ast.Type {
	mut cparam_elem_info := array
	mut cparam_elem_sym := t.table.sym(array.elem_type)
	for {
		if mut cparam_elem_sym.info is ast.ArrayFixed {
			cparam_elem_info = cparam_elem_sym.info
			cparam_elem_sym = t.table.sym(cparam_elem_info.elem_type)
		} else {
			return cparam_elem_info.elem_type.set_nr_muls(0)
		}
	}
	return ast.void_type
}

// get_generic_array_element_type retrieves the plain element type from a nested array [][]T -> T
pub fn (t &TypeResolver) get_generic_array_element_type(array ast.Array) ast.Type {
	mut cparam_elem_info := array
	mut cparam_elem_sym := t.table.sym(array.elem_type)
	for {
		if mut cparam_elem_sym.info is ast.Array {
			cparam_elem_info = cparam_elem_sym.info
			cparam_elem_sym = t.table.sym(cparam_elem_info.elem_type)
		} else {
			return cparam_elem_info.elem_type.set_nr_muls(0)
		}
	}
	return ast.void_type
}

// resolve_args resolves the ast node types dynamically depending on its special meaning
pub fn (mut t TypeResolver) resolve_args(cur_fn &ast.FnDecl, func &ast.Fn, mut node_ ast.CallExpr, concrete_types []ast.Type) map[int]ast.Type {
	mut comptime_args := map[int]ast.Type{}
	has_dynamic_vars := (cur_fn != unsafe { nil } && cur_fn.generic_names.len > 0)
		|| t.info.comptime_for_field_var != ''
		|| func.generic_names.len != node_.raw_concrete_types.len
	if !has_dynamic_vars {
		return comptime_args
	}
	offset := if func.is_method { 1 } else { 0 }
	mut k := -1
	for i, mut call_arg in node_.args {
		param := if func.is_variadic && i >= func.params.len - (offset + 1) {
			func.params.last()
		} else {
			func.params[offset + i]
		}
		if !param.typ.has_flag(.generic) {
			continue
		}
		k++
		param_typ := param.typ
		if mut call_arg.expr is ast.Ident {
			if mut call_arg.expr.obj is ast.Var {
				node_.args[i].typ = call_arg.expr.obj.typ
				if call_arg.expr.obj.ct_type_var !in [.generic_var, .generic_param, .no_comptime] {
					mut ctyp := t.get_type(call_arg.expr)
					if ctyp != ast.void_type {
						arg_sym := t.table.sym(ctyp)
						param_sym := t.table.final_sym(param_typ)
						if arg_sym.info is ast.Array && param_sym.kind == .array {
							ctyp = arg_sym.info.elem_type
						} else if arg_sym.info is ast.Map && param_sym.info is ast.Map {
							if call_arg.expr.obj.ct_type_var == .value_var {
								ctyp = arg_sym.info.value_type
								if param_sym.info.value_type.nr_muls() > 0 && ctyp.nr_muls() > 0 {
									ctyp = ctyp.set_nr_muls(0)
								}
							} else if call_arg.expr.obj.ct_type_var == .key_var {
								ctyp = arg_sym.info.key_type
								if param_sym.info.key_type.nr_muls() > 0 && ctyp.nr_muls() > 0 {
									ctyp = ctyp.set_nr_muls(0)
								}
							} else {
								key_is_generic := param_sym.info.key_type.has_flag(.generic)
								if key_is_generic {
									ctyp = t.resolver.unwrap_generic(arg_sym.info.key_type)
								}
								if param_sym.info.value_type.has_flag(.generic) {
									if key_is_generic {
										comptime_args[k] = ctyp
										k++
									}
									ctyp = t.resolver.unwrap_generic(arg_sym.info.value_type)
								}
							}
						} else if arg_sym.kind == .any {
							cparam_type_sym := t.table.sym(t.resolver.unwrap_generic(ctyp))
							if param_sym.info is ast.Map && cparam_type_sym.info is ast.Map {
								if param_sym.info.key_type.has_flag(.generic) {
									comptime_args[k] = cparam_type_sym.info.key_type
									if param_sym.info.value_type.has_flag(.generic) {
										k++
										ctyp = cparam_type_sym.info.value_type
									}
								} else if param_sym.info.value_type.has_flag(.generic) {
									ctyp = cparam_type_sym.info.value_type
								}
							}
						}
						comptime_args[k] = ctyp
					}
				} else if call_arg.expr.obj.ct_type_var == .generic_param {
					mut ctyp := t.get_type(call_arg.expr)
					if ctyp != ast.void_type {
						arg_sym := t.table.final_sym(call_arg.typ)
						param_typ_sym := t.table.sym(param_typ)
						if param_typ.has_flag(.variadic) {
							ctyp = ast.mktyp(ctyp)
							comptime_args[k] = ctyp
						} else if arg_sym.info is ast.Array && param_typ_sym.kind == .array {
							ctyp = t.get_generic_array_element_type(arg_sym.info)
							comptime_args[k] = ctyp
						} else if arg_sym.kind in [.struct, .interface, .sum_type] {
							mut generic_types := []ast.Type{}
							match arg_sym.info {
								ast.Struct, ast.Interface, ast.SumType {
									if param_typ_sym.generic_types.len > 0 {
										generic_types = param_typ_sym.generic_types.clone()
									} else {
										generic_types = arg_sym.info.generic_types.clone()
									}
								}
								else {}
							}
							generic_names := generic_types.map(t.table.sym(it).name)
							for _, gt_name in cur_fn.generic_names {
								if gt_name in generic_names
									&& generic_types.len == concrete_types.len {
									idx := generic_names.index(gt_name)
									comptime_args[k] = concrete_types[idx]
									break
								}
							}
						} else if arg_sym.kind == .any {
							cparam_type_sym := t.table.sym(t.resolver.unwrap_generic(ctyp))
							if param_typ_sym.kind == .array && cparam_type_sym.info is ast.Array {
								comptime_args[k] = cparam_type_sym.info.elem_type
							} else if param_typ_sym.info is ast.Map
								&& cparam_type_sym.info is ast.Map {
								if param_typ_sym.info.key_type.has_flag(.generic) {
									comptime_args[k] = cparam_type_sym.info.key_type
									if param_typ_sym.info.value_type.has_flag(.generic) {
										k++
										comptime_args[k] = cparam_type_sym.info.value_type
									}
								} else if param_typ_sym.info.value_type.has_flag(.generic) {
									comptime_args[k] = cparam_type_sym.info.value_type
								}
							} else {
								if node_.args[i].expr.is_auto_deref_var() {
									ctyp = ctyp.deref()
								}
								if ctyp.nr_muls() > 0 && param_typ.nr_muls() > 0 {
									ctyp = ctyp.set_nr_muls(0)
								}
								comptime_args[k] = ctyp
							}
						} else {
							comptime_args[k] = ctyp
						}
					}
				} else if call_arg.expr.obj.ct_type_var == .generic_var {
					mut ctyp := t.resolver.unwrap_generic(t.get_type(call_arg.expr))
					cparam_type_sym := t.table.sym(t.resolver.unwrap_generic(ctyp))
					param_typ_sym := t.table.sym(param_typ)
					if param_typ_sym.kind == .array && cparam_type_sym.info is ast.Array {
						ctyp = cparam_type_sym.info.elem_type
					}
					if node_.args[i].expr.is_auto_deref_var() {
						ctyp = ctyp.deref()
					}
					if ctyp.nr_muls() > 0 && param_typ.nr_muls() > 0 {
						ctyp = ctyp.set_nr_muls(0)
					}
					comptime_args[k] = ctyp
				}
			}
		} else if mut call_arg.expr is ast.PrefixExpr {
			if call_arg.expr.right is ast.ComptimeSelector {
				comptime_args[k] = t.info.comptime_for_field_type
				comptime_args[k] = comptime_args[k].deref()
				if param_typ.nr_muls() > 0 && comptime_args[k].nr_muls() > 0 {
					comptime_args[k] = comptime_args[k].set_nr_muls(0)
				}
			} else if mut call_arg.expr.right is ast.Ident {
				if t.info.get_ct_type_var(call_arg.expr.right) != .generic_var {
					mut ctyp := t.get_type(call_arg.expr.right)
					if ctyp != ast.void_type {
						comptime_args[k] = ctyp
						if param_typ.nr_muls() > 0 && comptime_args[k].nr_muls() > 0 {
							comptime_args[k] = comptime_args[k].set_nr_muls(0)
						}
					}
				}
			}
		} else if mut call_arg.expr is ast.ComptimeSelector {
			comptime_args[k] = t.info.comptime_for_field_type
			arg_sym := t.table.final_sym(call_arg.typ)
			param_sym := t.table.sym(param_typ)
			if arg_sym.kind == .array && param_sym.kind == .array {
				comptime_sym := t.table.sym(comptime_args[k])
				comptime_args[k] = t.get_generic_array_element_type(comptime_sym.info as ast.Array)
			} else if arg_sym.info is ast.Map && param_sym.info is ast.Map {
				comptime_sym := t.table.sym(comptime_args[k])
				if comptime_sym.info is ast.Map {
					if param_sym.info.key_type.has_flag(.generic) {
						comptime_args[k] = comptime_sym.info.key_type
						if param_sym.info.value_type.has_flag(.generic) {
							k++
							comptime_args[k] = comptime_sym.info.value_type
						}
					} else if param_sym.info.value_type.has_flag(.generic) {
						comptime_args[k] = comptime_sym.info.value_type
					}
				}
			}
			if param_typ.nr_muls() > 0 && comptime_args[k].nr_muls() > 0 {
				comptime_args[k] = comptime_args[k].set_nr_muls(0)
			}
		} else if mut call_arg.expr is ast.ComptimeCall {
			if call_arg.expr.method_name == 'method' {
				sym := t.table.sym(t.resolver.unwrap_generic(call_arg.expr.left_type))
				// `app.$method()`
				if m := sym.find_method(t.info.comptime_for_method.name) {
					comptime_args[k] = m.return_type
				}
			}
		} else if mut call_arg.expr is ast.CastExpr && call_arg.expr.typ.has_flag(.generic) {
			cparam_type_sym := t.table.sym(t.resolver.unwrap_generic(call_arg.expr.typ))
			param_typ_sym := t.table.sym(param_typ)
			if param_typ_sym.kind == .map && cparam_type_sym.info is ast.Map {
				comptime_args[k] = cparam_type_sym.info.key_type
				comptime_args[k + 1] = cparam_type_sym.info.value_type
			}
		} else if call_arg.expr is ast.IndexExpr && t.info.is_comptime(call_arg.expr) {
			mut ctyp := t.get_type(call_arg.expr)
			param_typ_sym := t.table.sym(param_typ)
			cparam_type_sym := t.table.sym(t.resolver.unwrap_generic(ctyp))
			if param_typ_sym.kind == .array && cparam_type_sym.info is ast.Array {
				ctyp = cparam_type_sym.info.elem_type
			}
			comptime_args[k] = ctyp
		} else if mut call_arg.expr is ast.StructInit && call_arg.expr.typ.has_flag(.generic) {
			mut ctyp := t.resolver.unwrap_generic(call_arg.expr.typ)
			param_typ_sym := t.table.sym(param_typ)
			cparam_type_sym := t.table.sym(ctyp)
			if param_typ_sym.kind == .array && cparam_type_sym.info is ast.Array {
				comptime_args[k] = cparam_type_sym.info.elem_type
			} else if param_typ_sym.kind == .map && cparam_type_sym.info is ast.Map {
				comptime_args[k] = cparam_type_sym.info.key_type
				comptime_args[k + 1] = cparam_type_sym.info.value_type
			} else {
				comptime_args[k] = ctyp
			}
		}
	}
	return comptime_args
}

pub fn (mut t TypeResolver) resolve_fn_generic_args(cur_fn &ast.FnDecl, func &ast.Fn, mut node ast.CallExpr) (bool, []ast.Type) {
	mut concrete_types := node.concrete_types.map(t.resolver.unwrap_generic(it))
	mut need_recheck := false
	// dynamic values from comptime and generic parameters
	// overwrite concrete_types[ receiver_concrete_type + arg number ]
	if concrete_types.len > 0 {
		mut rec_len := 0
		// discover receiver concrete_type len
		if func.is_method && node.left_type.has_flag(.generic) {
			rec_sym := t.table.final_sym(t.resolver.unwrap_generic(node.left_type))
			match rec_sym.info {
				ast.Struct, ast.Interface, ast.SumType {
					rec_len += rec_sym.info.generic_types.len
				}
				else {}
			}
		}
		mut comptime_args := t.resolve_args(cur_fn, func, mut node, concrete_types)
		if comptime_args.len > 0 {
			for k, v in comptime_args {
				if (rec_len + k) < concrete_types.len {
					concrete_types[rec_len + k] = t.resolver.unwrap_generic(v)
				}
			}
			if t.table.register_fn_concrete_types(func.fkey(), concrete_types) {
				need_recheck = true
			}
		}
	}

	return need_recheck, concrete_types
}
