// Copyright (c) 2019-2024 Felipe Pena All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module type_resolver

import ast
import v.token
import v.util

@[minify]
pub struct ResolverInfo {
pub mut:
	saved_type_map map[string]ast.Type

	// loop id for loop distinction
	comptime_loop_id int
	// $for
	inside_comptime_for bool
	// $if
	inside_comptime_if  bool
	has_different_types bool
	// .variants
	comptime_for_variant_var string
	// .fields
	comptime_for_field_var   string
	comptime_for_field_type  ast.Type
	comptime_for_field_value ast.StructField
	// .values
	comptime_for_enum_var string
	// .attributes
	comptime_for_attr_var string
	// .methods
	comptime_for_method_var      string
	comptime_for_method          &ast.Fn = unsafe { nil }
	comptime_for_method_ret_type ast.Type
	// .args
	comptime_for_method_param_var string
}

// Interface for cgen / checker instance
pub interface IResolverType {
mut:
	file &ast.File
	unwrap_generic(t ast.Type) ast.Type
}

pub struct DummyResolver {
mut:
	file &ast.File = unsafe { nil }
}

fn (d DummyResolver) unwrap_generic(t ast.Type) ast.Type {
	return t
}

@[heap]
pub struct TypeResolver {
pub mut:
	resolver   IResolverType = DummyResolver{}
	table      &ast.Table    = unsafe { nil }
	info       ResolverInfo        // current info
	info_stack []ResolverInfo      // stores the values from the above on each $for loop, to make nesting them easier
	type_map   map[string]ast.Type // map for storing dynamic resolved types on checker/gen phase
}

@[inline]
pub fn TypeResolver.new(table &ast.Table, resolver &IResolverType) &TypeResolver {
	return &TypeResolver{
		table:    table
		resolver: resolver
	}
}

// update_ct_type updates current type for specific key (comptime vars)
// `var` iteration vars, comptime vars
// `var.typ` => for comptime $for variables
// `var.unaliased_typ` => for comptime $for variables
// `var.return_type` => for .method return type
@[inline]
pub fn (mut t TypeResolver) update_ct_type(key string, var_type ast.Type) {
	t.type_map[key] = var_type
}

// get_ct_type_or_default retrieves a comptime variable value on type map or default_type otherwise
@[inline]
pub fn (t &TypeResolver) get_ct_type_or_default(key string, default_type ast.Type) ast.Type {
	return t.type_map[key] or { default_type }
}

@[noreturn]
fn (t &TypeResolver) error(s string, pos token.Pos) {
	util.show_compiler_message('cgen error:', pos: pos, file_path: t.resolver.file.path, message: s)
	exit(1)
}

// promote_type resolves the final type of different generic/comptime operand types
pub fn (t &TypeResolver) promote_type(left_type ast.Type, right_type ast.Type) ast.Type {
	if left_type == ast.f32_type && right_type == ast.f64_type {
		return right_type
	}
	return left_type
}

// get_type_or_default retries the comptime value if the AST node is related to comptime otherwise default_typ is returned
@[inline]
pub fn (mut t TypeResolver) get_type_or_default(node ast.Expr, default_typ ast.Type) ast.Type {
	match node {
		ast.Ident {
			if node.ct_expr {
				ctyp := t.get_type(node)
				return if ctyp != ast.void_type {
					if node.or_expr.kind == .absent {
						ctyp
					} else {
						ctyp.clear_flag(.option)
					}
				} else {
					default_typ
				}
			}
		}
		ast.SelectorExpr {
			if node.expr is ast.Ident && node.expr.ct_expr {
				ctyp := t.get_type(node)
				return if ctyp != ast.void_type { ctyp } else { default_typ }
			}
			return default_typ
		}
		ast.ParExpr {
			return t.get_type_or_default(node.expr, default_typ)
		}
		ast.InfixExpr {
			if !node.left.is_literal() && node.op in [.plus, .minus, .mul, .div, .mod] {
				return t.get_type_or_default(node.left, default_typ)
			}
			if !node.right.is_literal() && node.op in [.plus, .minus, .mul, .div, .mod] {
				return t.get_type_or_default(node.right, default_typ)
			}
		}
		ast.IndexExpr {
			if node.left is ast.Ident && node.left.ct_expr {
				ctyp := t.get_type(node)
				if ctyp != ast.void_type {
					return ctyp
				}
			}
		}
		ast.ComptimeSelector {
			// val.$(field.name)
			return t.get_comptime_selector_type(node, ast.void_type)
		}
		ast.CastExpr {
			if node.typ.has_flag(.generic) {
				return t.resolver.unwrap_generic(node.typ)
			}
		}
		ast.PostfixExpr {
			if node.op == .question && node.expr is ast.Ident && node.expr.ct_expr {
				ctyp := t.get_type(node)
				return if ctyp != ast.void_type { ctyp } else { default_typ }
			}
		}
		else {
			return default_typ
		}
	}
	return default_typ
}

// get_type retrieves the actual type from a comptime related ast node
@[inline]
pub fn (mut t TypeResolver) get_type(node ast.Expr) ast.Type {
	if node is ast.Ident {
		if node.obj is ast.Var {
			return match node.obj.ct_type_var {
				.generic_param {
					// generic parameter from generic function
					node.obj.typ
				}
				.generic_var {
					// generic var used on fn call assignment
					if node.obj.smartcasts.len > 0 {
						node.obj.smartcasts.last()
					} else {
						node.obj.typ
					}
				}
				.smartcast {
					ctyp := t.get_ct_type_or_default('${t.info.comptime_for_variant_var}.typ',
						node.obj.typ)
					return if (node.obj as ast.Var).is_unwrapped {
						ctyp.clear_flag(.option)
					} else {
						ctyp
					}
				}
				.aggregate {
					t.get_ct_type_or_default(node.name, if node.obj.smartcasts.len > 0 {
						node.obj.smartcasts.last()
					} else {
						node.obj.typ
					})
				}
				.key_var, .value_var {
					// key and value variables from normal for stmt
					t.get_ct_type_or_default(node.name, ast.void_type)
				}
				.field_var {
					// field var from $for loop
					if node.obj.ct_type_unwrapped {
						t.info.comptime_for_field_type.clear_flag(.option)
					} else {
						t.info.comptime_for_field_type
					}
				}
				else {
					ast.void_type
				}
			}
		}
	} else if node is ast.ComptimeSelector {
		// val.$(field.name)
		ctyp := t.get_comptime_selector_type(node, ast.void_type)
		if node.or_block.kind == .propagate_option {
			return ctyp.clear_flag(.option)
		}
		return ctyp
	} else if node is ast.SelectorExpr {
		if node.is_field_typ {
			return t.get_type_from_comptime_var(node.expr as ast.Ident)
		}
		if node.expr is ast.Ident && node.expr.ct_expr {
			struct_typ := t.resolver.unwrap_generic(t.get_type(node.expr))
			struct_sym := t.table.final_sym(struct_typ)
			// Struct[T] can have field with generic type
			if struct_sym.info is ast.Struct && struct_sym.info.generic_types.len > 0 {
				if field := t.table.find_field(struct_sym, node.field_name) {
					f_unwrap := node.scope.find_struct_field(ast.Expr(node.expr).str(),
						t.get_type_or_default(node.expr, node.expr_type), node.field_name)
					if f_unwrap != unsafe { nil } {
						return f_unwrap.smartcasts.last()
					}
					return field.typ
				}
			} else {
				sym := t.table.sym(t.resolver.unwrap_generic(node.expr_type))
				if f := t.table.find_field_with_embeds(sym, node.field_name) {
					return f.typ
				}
			}
		}
		return node.typ
	} else if node is ast.ComptimeCall {
		method_name := t.info.comptime_for_method.name
		left_sym := t.table.sym(t.resolver.unwrap_generic(node.left_type))
		f := left_sym.find_method(method_name) or {
			t.error('could not find method `${method_name}` on compile-time resolution',
				node.method_pos)
			return ast.void_type
		}
		return f.return_type
	} else if node is ast.IndexExpr && t.info.is_comptime(node.left) {
		nltype := t.get_type(node.left)
		nltype_unwrapped := t.resolver.unwrap_generic(nltype)
		return t.table.value_type(nltype_unwrapped)
	} else if node is ast.ParExpr && t.info.is_comptime(node.expr) {
		return t.get_type(node.expr)
	} else if node is ast.InfixExpr {
		if node.left_ct_expr {
			return t.get_type(node.left)
		} else if node.right_ct_expr {
			return t.get_type(node.right)
		}
	} else if node is ast.CastExpr && node.typ.has_flag(.generic) {
		// T(expr)
		return t.resolver.unwrap_generic(node.typ)
	} else if node is ast.PostfixExpr && node.op == .question
		&& node.expr in [ast.Ident, ast.ComptimeSelector] {
		// var?
		// f.$(field.name)?
		ctyp := t.get_type(node.expr)
		return ctyp.clear_flag(.option)
	}
	return ast.void_type
}
