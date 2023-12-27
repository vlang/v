// Copyright (c) 2019-2023 V devs. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module comptime

import v.ast
import v.token
import v.util

pub interface IResolverType {
mut:
	file &ast.File
	unwrap_generic(t ast.Type) ast.Type
}

@[noreturn]
fn (mut ct ComptimeInfo) error(s string, pos token.Pos) {
	util.show_compiler_message('cgen error:', pos: pos, file_path: ct.resolver.file.path, message: s)
	exit(1)
}

@[inline]
pub fn (mut ct ComptimeInfo) get_comptime_selector_key_type(val ast.ComptimeSelector) string {
	if val.field_expr is ast.SelectorExpr {
		if val.field_expr.expr is ast.Ident {
			return '${val.field_expr.expr.name}.typ'
		}
	}
	return ''
}

// is_comptime_var checks if the node is related to a comptime variable
@[inline]
pub fn (mut ct ComptimeInfo) is_comptime_var(node ast.Expr) bool {
	return ct.get_ct_type_var(node) != .no_comptime
}

// get_ct_type_var gets the comptime type of the variable (.generic_param, .key_var, etc)
@[inline]
pub fn (mut ct ComptimeInfo) get_ct_type_var(node ast.Expr) ast.ComptimeVarKind {
	return if node is ast.Ident && node.obj is ast.Var {
		(node.obj as ast.Var).ct_type_var
	} else {
		.no_comptime
	}
}

// get_comptime_var_type retrieves the actual type from a comptime related ast node
@[inline]
pub fn (mut ct ComptimeInfo) get_comptime_var_type(node ast.Expr) ast.Type {
	if node is ast.Ident && node.obj is ast.Var {
		return match (node.obj as ast.Var).ct_type_var {
			.generic_param {
				// generic parameter from current function
				node.obj.typ
			}
			.key_var, .value_var {
				// key and value variables from normal for stmt
				ct.type_map[node.name] or { ast.void_type }
			}
			.field_var {
				// field var from $for loop
				ct.comptime_for_field_type
			}
			else {
				ast.void_type
			}
		}
	} else if node is ast.ComptimeSelector {
		// val.$(field.name)
		return ct.get_comptime_selector_type(node, ast.void_type)
	} else if node is ast.SelectorExpr && ct.is_comptime_selector_type(node) {
		if node.expr is ast.Ident {
			match node.expr.name {
				ct.comptime_for_variant_var {
					return ct.type_map['${ct.comptime_for_variant_var}.typ']
				}
				ct.comptime_for_enum_var {
					return ct.type_map['${ct.comptime_for_enum_var}.typ']
				}
				else {
					// field_var.typ from $for field
					return ct.comptime_for_field_type
				}
			}
		}
	} else if node is ast.ComptimeCall {
		method_name := ct.comptime_for_method
		left_sym := ct.table.sym(ct.resolver.unwrap_generic(node.left_type))
		f := left_sym.find_method(method_name) or {
			ct.error('could not find method `${method_name}` on compile-time resolution',
				node.method_pos)
			return ast.void_type
		}
		return f.return_type
	}
	return ast.void_type
}

pub fn (mut ct ComptimeInfo) get_comptime_selector_var_type(node ast.ComptimeSelector) (ast.StructField, string) {
	field_name := ct.comptime_for_field_value.name
	left_sym := ct.table.sym(ct.resolver.unwrap_generic(node.left_type))
	field := ct.table.find_field_with_embeds(left_sym, field_name) or {
		ct.error('`${node.left}` has no field named `${field_name}`', node.left.pos())
	}
	return field, field_name
}

// get_comptime_selector_type retrieves the var.$(field.name) type when field_name is 'name' otherwise default_type is returned
@[inline]
pub fn (mut ct ComptimeInfo) get_comptime_selector_type(node ast.ComptimeSelector, default_type ast.Type) ast.Type {
	if node.field_expr is ast.SelectorExpr && ct.check_comptime_is_field_selector(node.field_expr)
		&& node.field_expr.field_name == 'name' {
		return ct.resolver.unwrap_generic(ct.comptime_for_field_type)
	}
	return default_type
}

// is_comptime_selector_field_name checks if the SelectorExpr is related to $for variable accessing specific field name provided by `field_name`
@[inline]
pub fn (mut ct ComptimeInfo) is_comptime_selector_field_name(node ast.SelectorExpr, field_name string) bool {
	return ct.comptime_for_field_var != '' && node.expr is ast.Ident
		&& node.expr.name == ct.comptime_for_field_var && node.field_name == field_name
}

// is_comptime_selector_type checks if the SelectorExpr is related to $for variable accessing .typ field
@[inline]
pub fn (mut ct ComptimeInfo) is_comptime_selector_type(node ast.SelectorExpr) bool {
	if ct.inside_comptime_for && node.expr is ast.Ident {
		return
			node.expr.name in [ct.comptime_for_enum_var, ct.comptime_for_variant_var, ct.comptime_for_field_var]
			&& node.field_name == 'typ'
	}
	return false
}

// check_comptime_is_field_selector checks if the SelectorExpr is related to $for variable
@[inline]
pub fn (mut ct ComptimeInfo) check_comptime_is_field_selector(node ast.SelectorExpr) bool {
	if ct.comptime_for_field_var != '' && node.expr is ast.Ident {
		return node.expr.name == ct.comptime_for_field_var
	}
	return false
}

// check_comptime_is_field_selector_bool checks if the SelectorExpr is related to field.is_* boolean fields
@[inline]
pub fn (mut ct ComptimeInfo) check_comptime_is_field_selector_bool(node ast.SelectorExpr) bool {
	if ct.check_comptime_is_field_selector(node) {
		return node.field_name in ['is_mut', 'is_pub', 'is_shared', 'is_atomic', 'is_option',
			'is_array', 'is_map', 'is_chan', 'is_struct', 'is_alias', 'is_enum']
	}
	return false
}

// get_comptime_selector_bool_field evaluates the bool value for field.is_* fields
pub fn (mut ct ComptimeInfo) get_comptime_selector_bool_field(field_name string) bool {
	field := ct.comptime_for_field_value
	field_typ := ct.comptime_for_field_type
	field_sym := ct.table.sym(ct.resolver.unwrap_generic(ct.comptime_for_field_type))

	match field_name {
		'is_pub' { return field.is_pub }
		'is_mut' { return field.is_mut }
		'is_shared' { return field_typ.has_flag(.shared_f) }
		'is_atomic' { return field_typ.has_flag(.atomic_f) }
		'is_option' { return field.typ.has_flag(.option) }
		'is_array' { return field_sym.kind in [.array, .array_fixed] }
		'is_map' { return field_sym.kind == .map }
		'is_chan' { return field_sym.kind == .chan }
		'is_struct' { return field_sym.kind == .struct_ }
		'is_alias' { return field_sym.kind == .alias }
		'is_enum' { return field_sym.kind == .enum_ }
		else { return false }
	}
}

pub fn (mut ct ComptimeInfo) is_comptime_type(x ast.Type, y ast.ComptimeType) bool {
	x_kind := ct.table.type_kind(x)
	match y.kind {
		.unknown {
			return false
		}
		.map_ {
			return x_kind == .map
		}
		.string {
			return x_kind == .string
		}
		.int {
			return x_kind in [.i8, .i16, .int, .i64, .u8, .u16, .u32, .u64, .usize, .isize,
				.int_literal]
		}
		.float {
			return x_kind in [.f32, .f64, .float_literal]
		}
		.struct_ {
			return x_kind == .struct_
		}
		.iface {
			return x_kind == .interface_
		}
		.array {
			return x_kind in [.array, .array_fixed]
		}
		.array_dynamic {
			return x_kind == .array
		}
		.array_fixed {
			return x_kind == .array_fixed
		}
		.sum_type {
			return x_kind == .sum_type
		}
		.enum_ {
			return x_kind == .enum_
		}
		.alias {
			return x_kind == .alias
		}
		.function {
			return x_kind == .function
		}
		.option {
			return x.has_flag(.option)
		}
	}
}

// comptime_get_kind_var identifies the comptime variable kind (i.e. if it is about .values, .fields, .methods  etc)
fn (mut ct ComptimeInfo) comptime_get_kind_var(var ast.Ident) ?ast.ComptimeForKind {
	if ct.inside_comptime_for {
		return none
	}

	match var.name {
		ct.comptime_for_variant_var {
			return .variants
		}
		ct.comptime_for_field_var {
			return .fields
		}
		ct.comptime_for_enum_var {
			return .values
		}
		ct.comptime_for_method_var {
			return .methods
		}
		ct.comptime_for_attr_var {
			return .attributes
		}
		else {
			return none
		}
	}
}

pub struct DummyResolver {
mut:
	file &ast.File = unsafe { nil }
}

fn (d DummyResolver) unwrap_generic(t ast.Type) ast.Type {
	return t
}

pub struct ComptimeInfo {
pub mut:
	// variable type resolver
	resolver IResolverType = DummyResolver{}
	// symbol table resolver
	table &ast.Table = unsafe { nil }
	// $for
	inside_comptime_for bool
	type_map            map[string]ast.Type
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
	comptime_for_method          string
	comptime_for_method_ret_type ast.Type
}
