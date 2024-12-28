module type_resolver

import ast
import v.token
import v.util

pub struct ResolverInfo {
pub mut:
	saved_type_map map[string]ast.Type

	// loop id for loop distinction
	comptime_loop_id int
	// $for
	inside_comptime_for bool
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

pub interface IResolverType {
mut:
	file &ast.File
	unwrap_generic(t ast.Type) ast.Type
	// resolve_return_type(node ast.CallExpr) ast.Type
}

pub struct DummyResolver {
mut:
	file &ast.File = unsafe { nil }
}

fn (d DummyResolver) unwrap_generic(t ast.Type) ast.Type {
	return t
}

fn (d DummyResolver) resolve_return_type(node ast.CallExpr) ast.Type {
	return ast.void_type
}

@[heap]
pub struct TypeResolver {
pub mut:
	resolver   IResolverType = DummyResolver{}
	table      &ast.Table    = unsafe { nil }
	info       ResolverInfo   // infoent info
	info_stack []ResolverInfo // stores the values from the above on each $for loop, to make nesting them easier
	type_map   map[string]ast.Type
}

pub fn TypeResolver.new(table &ast.Table, resolver &IResolverType) &TypeResolver {
	return &TypeResolver{
		table:    table
		resolver: resolver
	}
}

@[noreturn]
fn (mut ct TypeResolver) error(s string, pos token.Pos) {
	util.show_compiler_message('cgen error:', pos: pos, file_path: ct.resolver.file.path, message: s)
	exit(1)
}

pub fn (mut ct TypeResolver) get_comptime_selector_var_type(node ast.ComptimeSelector) (ast.StructField, string) {
	field_name := ct.info.comptime_for_field_value.name
	left_sym := ct.table.sym(ct.resolver.unwrap_generic(node.left_type))
	field := ct.table.find_field_with_embeds(left_sym, field_name) or {
		ct.error('`${node.left}` has no field named `${field_name}`', node.left.pos())
	}
	return field, field_name
}

// get_type_or_default retries the comptime value if the AST node is related to comptime otherwise default_typ is returned
@[inline]
pub fn (mut ct TypeResolver) get_type_or_default(node ast.Expr, default_typ ast.Type) ast.Type {
	match node {
		ast.Ident {
			if node.ct_expr {
				ctyp := ct.get_type(node)
				return if ctyp != ast.void_type { ctyp } else { default_typ }
			}
		}
		ast.SelectorExpr {
			if node.expr is ast.Ident && node.expr.ct_expr {
				struct_typ := ct.resolver.unwrap_generic(ct.get_type(node.expr))
				struct_sym := ct.table.final_sym(struct_typ)
				// Struct[T] can have field with generic type
				if struct_sym.info is ast.Struct && struct_sym.info.generic_types.len > 0 {
					if field := ct.table.find_field(struct_sym, node.field_name) {
						return field.typ
					}
				}
			}
		}
		ast.ParExpr {
			return ct.get_type_or_default(node.expr, default_typ)
		}
		ast.InfixExpr {
			if node.op in [.plus, .minus, .mul, .div, .mod] {
				return ct.get_type_or_default(node.left, default_typ)
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
pub fn (mut ct TypeResolver) get_type(node ast.Expr) ast.Type {
	if node is ast.Ident {
		if node.obj is ast.Var {
			return match node.obj.ct_type_var {
				.generic_param {
					// generic parameter from infoent function
					node.obj.typ
				}
				.generic_var {
					// generic var used on fn call assignment
					if node.obj.smartcasts.len > 0 {
						node.obj.smartcasts.last()
					} else {
						ct.type_map['g.${node.name}.${node.obj.pos.pos}'] or { node.obj.typ }
					}
				}
				.smartcast {
					ctyp := ct.type_map['${ct.info.comptime_for_variant_var}.typ'] or {
						node.obj.typ
					}
					return if (node.obj as ast.Var).is_unwrapped {
						ctyp.clear_flag(.option)
					} else {
						ctyp
					}
				}
				.key_var, .value_var {
					// key and value variables from normal for stmt
					ct.type_map[node.name] or { ast.void_type }
				}
				.field_var {
					// field var from $for loop
					ct.info.comptime_for_field_type
				}
				else {
					ast.void_type
				}
			}
		}
	} else if node is ast.ComptimeSelector {
		// val.$(field.name)
		return ct.get_comptime_selector_type(node, ast.void_type)
	} else if node is ast.SelectorExpr && ct.info.is_comptime_selector_type(node) {
		return ct.get_type_from_comptime_var(node.expr as ast.Ident)
	} else if node is ast.ComptimeCall {
		method_name := ct.info.comptime_for_method.name
		left_sym := ct.table.sym(ct.resolver.unwrap_generic(node.left_type))
		f := left_sym.find_method(method_name) or {
			ct.error('could not find method `${method_name}` on compile-time resolution',
				node.method_pos)
			return ast.void_type
		}
		return f.return_type
	} else if node is ast.IndexExpr && ct.info.is_comptime(node.left) {
		nltype := ct.get_type(node.left)
		nltype_unwrapped := ct.resolver.unwrap_generic(nltype)
		return ct.table.value_type(nltype_unwrapped)
	}
	return ast.void_type
}

@[inline]
pub fn (mut g TypeResolver) is_generic_param_var(node ast.Expr) bool {
	return node is ast.Ident && node.info is ast.IdentVar && node.obj is ast.Var
		&& (node.obj as ast.Var).ct_type_var == .generic_param
}

// is_generic_expr checks if the expr relies on fn generic argument
pub fn (mut g TypeResolver) is_generic_expr(node ast.Expr) bool {
	return match node {
		ast.Ident {
			// variable declared as generic type
			g.is_generic_param_var(node)
		}
		ast.IndexExpr {
			// generic_var[N]
			g.is_generic_param_var(node.left)
		}
		ast.CallExpr {
			// fn which has any generic dependent expr
			if node.args.any(g.is_generic_param_var(it.expr)) {
				return true
			}
			if node.is_static_method && node.left_type.has_flag(.generic) {
				return true
			}
			// fn[T]() or generic_var.fn[T]()
			node.concrete_types.any(it.has_flag(.generic))
		}
		ast.SelectorExpr {
			// generic_var.property
			g.is_generic_param_var(node.expr)
		}
		else {
			false
		}
	}
}
