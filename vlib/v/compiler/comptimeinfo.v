module compiler

import v.ast

pub interface IResolverType {
mut:
	unwrap_generic(ast.Type) ast.Type
}

// get_comptime_selector_type retrieves the var.$(field.name) type when field_name is 'name' otherwise default_type is returned
@[inline]
pub fn (mut ct ComptimeInfo) get_comptime_selector_type(node ast.ComptimeSelector, default_type ast.Type) ast.Type {
	if node.field_expr is ast.SelectorExpr && ct.check_comptime_is_field_selector(node.field_expr)
		&& node.field_expr.field_name == 'name' {
		return ct.resolver.unwrap_generic(ct.comptime_fields_default_type)
	}
	return default_type
}

// is_comptime_selector_field_name checks if the SelectorExpr is related to $for variable accessing specific field name provided by `field_name`
@[inline]
pub fn (mut ct ComptimeInfo) is_comptime_selector_field_name(node ast.SelectorExpr, field_name string) bool {
	return ct.inside_comptime_for_field && node.expr is ast.Ident
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
	if ct.inside_comptime_for_field && node.expr is ast.Ident {
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
	field_typ := ct.comptime_fields_default_type
	field_sym := ct.table.sym(ct.resolver.unwrap_generic(ct.comptime_fields_default_type))

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

pub struct ComptimeInfo {
pub mut:
	resolver &IResolverType = unsafe { nil }
	table    &ast.Table     = unsafe { nil }
	// $for
	inside_comptime_for bool
	// .variants
	comptime_for_variant_var string
	// .fields
	inside_comptime_for_field    bool
	comptime_for_field_var       string
	comptime_fields_default_type ast.Type
	comptime_fields_type         map[string]ast.Type
	comptime_for_field_value     ast.StructField
	// .values
	comptime_for_enum_var     string
	comptime_enum_field_value string
	// .attributes
	comptime_for_attr_var string
	// .methods
	comptime_for_method_var      string
	comptime_for_method          string
	comptime_for_method_ret_type ast.Type
}
