// Copyright (c) 2019-2024 V devs. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module type_resolver

import v.ast

pub fn (mut t TypeResolver) get_comptime_selector_var_type(node ast.ComptimeSelector) (ast.StructField, string) {
	field_name := t.info.comptime_for_field_value.name
	left_sym := t.table.sym(t.resolver.unwrap_generic(node.left_type))
	field := t.table.find_field_with_embeds(left_sym, field_name) or {
		t.error('`${node.left}` has no field named `${field_name}`', node.left.pos())
	}
	return field, field_name
}

// has_comptime_expr checks if the expr contains some comptime expr
@[inline]
pub fn (t &ResolverInfo) has_comptime_expr(node ast.Expr) bool {
	return (node is ast.Ident && node.ct_expr)
		|| (node is ast.IndexExpr && t.has_comptime_expr(node.left))
		|| node is ast.ComptimeSelector || (node is ast.StructInit
		&& node.init_fields.any(it.expr is ast.AnonFn && it.expr.decl.generic_names.len > 0))
		|| (node is ast.PostfixExpr && t.has_comptime_expr(node.expr))
		|| (node is ast.SelectorExpr && t.has_comptime_expr(node.expr))
		|| (node is ast.InfixExpr && (t.has_comptime_expr(node.left)
		|| t.has_comptime_expr(node.right)))
}

// is_comptime checks if the node is related to a comptime marked variable
@[inline]
pub fn (t &ResolverInfo) is_comptime(node ast.Expr) bool {
	return match node {
		ast.Ident {
			node.ct_expr
		}
		ast.IndexExpr {
			if node.left is ast.Ident {
				node.left.ct_expr
			} else {
				false
			}
		}
		ast.SelectorExpr {
			return node.expr is ast.Ident && node.expr.ct_expr
		}
		ast.InfixExpr {
			return node.left_ct_expr || node.right_ct_expr
		}
		ast.ParExpr {
			return t.is_comptime(node.expr)
		}
		ast.ComptimeSelector {
			return true
		}
		ast.PostfixExpr {
			return t.is_comptime(node.expr)
		}
		else {
			false
		}
	}
}

// is_comptime_variant_var checks if the node is related to a comptime variant variable
@[inline]
pub fn (t &ResolverInfo) is_comptime_variant_var(node ast.Ident) bool {
	return node.name == t.comptime_for_variant_var
}

// typeof_type resolves type for typeof() expr where field.typ is resolved to real type instead of int type to make type(field.typ).name working
pub fn (mut t TypeResolver) typeof_type(node ast.Expr, default_type ast.Type) ast.Type {
	if t.info.is_comptime(node) {
		return t.get_type(node)
	} else if node is ast.SelectorExpr && node.expr_type != 0 {
		if node.expr is ast.Ident && node.is_field_typ {
			return t.get_type_from_comptime_var(node.expr)
		}
		field := node.scope.find_struct_field(node.expr.str(), node.expr_type, node.field_name)
		if field != unsafe { nil } {
			if field.smartcasts.len > 0 {
				return field.smartcasts.last()
			}
		}
		sym := t.table.sym(t.resolver.unwrap_generic(node.expr_type))
		if f := t.table.find_field_with_embeds(sym, node.field_name) {
			return f.typ
		}
	} else if node is ast.SelectorExpr && node.name_type != 0 {
		if node.field_name in ['value_type', 'element_type'] {
			return t.table.value_type(t.resolver.unwrap_generic(node.name_type))
		} else if node.field_name == 'key_type' {
			sym := t.table.sym(t.resolver.unwrap_generic(node.name_type))
			if sym.info is ast.Map {
				return t.resolver.unwrap_generic(sym.info.key_type)
			}
		}
	}
	return default_type
}

// typeof_field_type resolves the T.<field_name> and typeof[T]().<field_name> type
pub fn (mut t TypeResolver) typeof_field_type(typ ast.Type, field_name string) ast.Type {
	match field_name {
		'name' {
			return ast.string_type
		}
		'idx' {
			return t.resolver.unwrap_generic(typ)
		}
		'unaliased_typ' {
			return t.table.unaliased_type(t.resolver.unwrap_generic(typ))
		}
		'indirections' {
			return ast.int_type
		}
		'key_type' {
			sym := t.table.final_sym(t.resolver.unwrap_generic(typ))
			if sym.info is ast.Map {
				return t.resolver.unwrap_generic(sym.info.key_type)
			}
			return ast.no_type
		}
		'value_type', 'element_type' {
			return t.table.value_type(t.resolver.unwrap_generic(typ))
		}
		else {
			return typ
		}
	}
}

// get_ct_type_var gets the comptime type of the variable (.generic_param, .key_var, etc)
@[inline]
pub fn (t &ResolverInfo) get_ct_type_var(node ast.Expr) ast.ComptimeVarKind {
	if node is ast.Ident && node.obj is ast.Var {
		return node.obj.ct_type_var
	} else if node is ast.IndexExpr {
		return t.get_ct_type_var(node.left)
	} else if node is ast.InfixExpr {
		return t.get_ct_type_var(node.left)
	} else if node is ast.ParExpr {
		return t.get_ct_type_var(node.expr)
	} else if node is ast.SelectorExpr {
		return t.get_ct_type_var(node.expr)
	}
	return .no_comptime
}

// get_type_from_comptime_var retrieves the comptime type related to $for variable
@[inline]
pub fn (t &TypeResolver) get_type_from_comptime_var(var ast.Ident) ast.Type {
	match var.name {
		t.info.comptime_for_variant_var {
			return t.get_ct_type_or_default('${t.info.comptime_for_variant_var}.typ',
				ast.void_type)
		}
		t.info.comptime_for_method_param_var {
			return t.get_ct_type_or_default('${t.info.comptime_for_method_param_var}.typ',
				ast.void_type)
		}
		else {
			// field_var.typ from $for field
			return t.info.comptime_for_field_type
		}
	}
}

// get_comptime_selector_type retrieves the var.$(field.name) type when field_name is 'name' otherwise default_type is returned
@[inline]
pub fn (mut t TypeResolver) get_comptime_selector_type(node ast.ComptimeSelector, default_type ast.Type) ast.Type {
	if node.is_name && node.field_expr is ast.SelectorExpr
		&& t.info.check_comptime_is_field_selector(node.field_expr) {
		return t.resolver.unwrap_generic(t.info.comptime_for_field_type)
	}
	return default_type
}

// is_comptime_selector_field_name checks if the SelectorExpr is related to $for variable or generic letter accessing specific field name provided by `field_name`
@[inline]
pub fn (t &ResolverInfo) is_comptime_selector_field_name(node ast.SelectorExpr, field_name string) bool {
	return ((t.comptime_for_field_var != '' && node.expr is ast.Ident
		&& node.expr.name == t.comptime_for_field_var) || node.name_type != 0)
		&& node.field_name == field_name
}

// is_comptime_selector_type checks if the SelectorExpr is related to $for variable accessing .typ field
@[inline]
pub fn (t &ResolverInfo) is_comptime_selector_type(node ast.SelectorExpr) bool {
	if t.inside_comptime_for && node.expr is ast.Ident {
		return
			node.expr.name in [t.comptime_for_enum_var, t.comptime_for_variant_var, t.comptime_for_field_var, t.comptime_for_method_param_var]
			&& node.field_name == 'typ'
	}
	return false
}

// check_comptime_is_field_selector checks if the SelectorExpr is related to $for variable
@[inline]
pub fn (t &ResolverInfo) check_comptime_is_field_selector(node ast.SelectorExpr) bool {
	if t.comptime_for_field_var != '' && node.expr is ast.Ident {
		return node.expr.name == t.comptime_for_field_var
	}
	return false
}

// check_comptime_is_field_selector_bool checks if the SelectorExpr is related to field.is_* boolean fields
@[inline]
pub fn (t &ResolverInfo) check_comptime_is_field_selector_bool(node ast.SelectorExpr) bool {
	if t.check_comptime_is_field_selector(node) {
		return node.field_name in ['is_mut', 'is_pub', 'is_shared', 'is_atomic', 'is_option',
			'is_array', 'is_map', 'is_chan', 'is_struct', 'is_alias', 'is_enum']
	}
	return false
}

// get_comptime_selector_bool_field evaluates the bool value for field.is_* fields
pub fn (mut t TypeResolver) get_comptime_selector_bool_field(field_name string) bool {
	field := t.info.comptime_for_field_value
	field_typ := t.info.comptime_for_field_type
	field_sym := t.table.sym(t.resolver.unwrap_generic(t.info.comptime_for_field_type))

	match field_name {
		'is_pub' { return field.is_pub }
		'is_mut' { return field.is_mut }
		'is_shared' { return field_typ.has_flag(.shared_f) }
		'is_atomic' { return field_typ.has_flag(.atomic_f) }
		'is_option' { return field.typ.has_flag(.option) }
		'is_array' { return field_sym.kind in [.array, .array_fixed] }
		'is_map' { return field_sym.kind == .map }
		'is_chan' { return field_sym.kind == .chan }
		'is_struct' { return field_sym.kind == .struct }
		'is_alias' { return field_sym.kind == .alias }
		'is_enum' { return field_sym.kind == .enum }
		else { return false }
	}
}

// is_comptime_type check if the type is compatible with the supplied ComptimeType
pub fn (t &TypeResolver) is_comptime_type(x ast.Type, y ast.ComptimeType) bool {
	x_kind := t.table.type_kind(x)
	match y.kind {
		.unknown {
			return false
		}
		.map {
			return x_kind == .map
		}
		.string {
			return x_kind == .string
		}
		.voidptr {
			return x.is_voidptr()
		}
		.pointer {
			return x.is_any_kind_of_pointer()
		}
		.int {
			return x_kind in [.i8, .i16, .i32, .int, .i64, .u8, .u16, .u32, .u64, .usize, .isize,
				.int_literal]
		}
		.float {
			return x_kind in [.f32, .f64, .float_literal]
		}
		.struct {
			return x_kind == .struct
		}
		.iface {
			return x_kind == .interface
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
		.enum {
			return x_kind == .enum
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

// comptime_get_kind_var identifies the comptime variable kind (i.e. if it is about .values, .fields, .methods, .args etc)
fn (t &ResolverInfo) comptime_get_kind_var(var ast.Ident) ?ast.ComptimeForKind {
	if t.inside_comptime_for {
		return none
	}

	match var.name {
		t.comptime_for_variant_var {
			return .variants
		}
		t.comptime_for_field_var {
			return .fields
		}
		t.comptime_for_enum_var {
			return .values
		}
		t.comptime_for_method_var {
			return .methods
		}
		t.comptime_for_attr_var {
			return .attributes
		}
		t.comptime_for_method_param_var {
			return .params
		}
		else {
			return none
		}
	}
}
