// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module transformer

import v2.ast
import v2.token
import v2.types

struct ConcreteSumtypeWrapInfo {
	name     string
	variants []string
}

// get_fn_return_type gets the return type for a function
fn (t &Transformer) get_fn_return_type(fn_name string) ?types.Type {
	if fn_name.contains('__') {
		module_name := fn_name.all_before_last('__')
		short_name := fn_name.all_after_last('__')
		mut method_lookup_names := []string{}
		t.append_method_lookup_type_name(mut method_lookup_names, module_name)
		if ret_type := t.lookup_method_return_type(method_lookup_names, short_name) {
			return ret_type
		}
		if ret_type := t.cached_fn_return_type_index['${module_name}#${short_name}'] {
			return ret_type
		}
		short_module := short_module_name(module_name)
		if short_module != module_name {
			if ret_type := t.cached_fn_return_type_index['${short_module}#${short_name}'] {
				return ret_type
			}
		}
	}
	// First try the current module scope.
	if ret_type := t.cached_fn_return_type_index['${t.cur_module}#${fn_name}'] {
		return ret_type
	}
	// Try builtin scope directly (many common functions are here).
	if t.cur_module != 'builtin' {
		if ret_type := t.cached_fn_return_type_index['builtin#${fn_name}'] {
			return ret_type
		}
	}
	// Fallback: scan all module scopes for local/private functions.
	if ret_type := t.cached_fn_return_type_index['*#${fn_name}'] {
		return ret_type
	}
	return none
}

fn (t &Transformer) type_from_param_type_expr(expr ast.Expr, generic_params []string) ?types.Type {
	generic_name := direct_generic_param_name_from_expr(expr, generic_params)
	if generic_name != '' {
		return types.Type(types.NamedType(generic_name))
	}
	if typ := t.generic_aware_type_from_param_type_expr(expr, generic_params) {
		return typ
	}
	// Resolve from the AST directly so nested type shapes like `&map[K]V`
	// don't lose structure round-tripping through C-style name strings
	// (which are ambiguous for pointer-of-map vs. map-of-pointer).
	if typ := t.lookup_type_from_expr(expr) {
		return typ
	}
	type_name := t.expr_to_type_name(expr)
	if type_name == '' {
		return none
	}
	if typ := t.c_name_to_type(type_name) {
		return typ
	}
	c_name := t.v_type_name_to_c_name(type_name)
	if c_name != '' && c_name != type_name {
		if typ := t.c_name_to_type(c_name) {
			return typ
		}
	}
	if typ := t.concrete_generic_sumtype_base_type(type_name) {
		return typ
	}
	if c_name != '' && c_name != type_name {
		if typ := t.concrete_generic_sumtype_base_type(c_name) {
			return typ
		}
	}
	return none
}

fn (t &Transformer) concrete_generic_sumtype_base_type(type_name string) ?types.Type {
	generic_idx := type_name.index('_T_') or { return none }
	base_name := type_name[..generic_idx]
	if base_name == '' {
		return none
	}
	typ := t.lookup_concrete_generic_sumtype_base_name(base_name) or { return none }
	if typ is types.SumType {
		return typ
	}
	return none
}

fn (t &Transformer) lookup_concrete_generic_sumtype_base_name(base_name string) ?types.Type {
	last_dunder := base_name.last_index('__') or { return t.lookup_sumtype_by_name(base_name) }
	module_part := base_name[..last_dunder]
	short_name := base_name[last_dunder + 2..]
	if module_part == '' || short_name == '' {
		return none
	}
	if module_part.contains('__') {
		if typ := t.lookup_sumtype_in_nested_module_path(module_part, short_name) {
			return typ
		}
		if typ := t.lookup_sumtype_by_name('${module_call_c_prefix(module_part)}__${short_name}') {
			return typ
		}
		if typ := t.lookup_sumtype_by_name(base_name) {
			return typ
		}
		return none
	}
	if typ := t.lookup_sumtype_by_name(base_name) {
		return typ
	}
	if typ := t.lookup_sumtype_in_nested_module_path(module_part, short_name) {
		return typ
	}
	return none
}

fn (t &Transformer) lookup_sumtype_by_name(name string) ?types.Type {
	typ := t.lookup_type(name) or { return none }
	if typ is types.SumType {
		return typ
	}
	return none
}

fn (t &Transformer) lookup_sumtype_in_module(module_name string, short_name string) ?types.Type {
	scope := t.get_module_scope(module_name) or { return none }
	typ := scope.lookup_type(short_name) or { return none }
	if typ is types.SumType {
		return typ
	}
	return none
}

fn (t &Transformer) lookup_sumtype_in_nested_module_path(module_part string, short_name string) ?types.Type {
	if typ := t.lookup_sumtype_in_module(module_part, short_name) {
		return typ
	}
	dotted_module := module_part.replace('__', '.')
	if dotted_module != module_part {
		if typ := t.lookup_sumtype_in_module(dotted_module, short_name) {
			return typ
		}
	}
	return none
}

fn fixed_array_len_from_type_expr(expr ast.Expr) int {
	if expr is ast.BasicLiteral {
		return expr.value.int()
	}
	if expr is ast.Ident {
		return expr.name.int()
	}
	return 0
}

fn (t &Transformer) generic_aware_type_from_param_type_expr(expr ast.Expr, generic_params []string) ?types.Type {
	if expr is ast.Ident {
		if expr.name in generic_params {
			return types.Type(types.NamedType(expr.name))
		}
		return none
	}
	if expr is ast.ModifierExpr {
		return t.generic_aware_type_from_param_type_expr(expr.expr, generic_params)
	}
	if expr is ast.PrefixExpr && expr.op == .amp {
		base := t.type_from_param_type_expr(expr.expr, generic_params) or { return none }
		return types.Type(types.Pointer{
			base_type: base
		})
	}
	if expr is ast.Type {
		match expr {
			ast.ArrayType {
				elem := t.type_from_param_type_expr(expr.elem_type, generic_params) or {
					return none
				}
				return types.Type(types.Array{
					elem_type: elem
				})
			}
			ast.ArrayFixedType {
				elem := t.type_from_param_type_expr(expr.elem_type, generic_params) or {
					return none
				}
				return types.Type(types.ArrayFixed{
					len:       fixed_array_len_from_type_expr(expr.len)
					elem_type: elem
				})
			}
			ast.MapType {
				key := t.type_from_param_type_expr(expr.key_type, generic_params) or { return none }
				value := t.type_from_param_type_expr(expr.value_type, generic_params) or {
					return none
				}
				return types.Type(types.Map{
					key_type:   key
					value_type: value
				})
			}
			ast.PointerType {
				base := t.type_from_param_type_expr(expr.base_type, generic_params) or {
					return none
				}
				return types.Type(types.Pointer{
					base_type: base
				})
			}
			ast.OptionType {
				base := t.type_from_param_type_expr(expr.base_type, generic_params) or {
					return none
				}
				return types.Type(types.OptionType{
					base_type: base
				})
			}
			ast.ResultType {
				base := t.type_from_param_type_expr(expr.base_type, generic_params) or {
					return none
				}
				return types.Type(types.ResultType{
					base_type: base
				})
			}
			else {}
		}
	}
	return none
}

fn (mut t Transformer) seed_fallback_fn_param_scope(params []ast.Parameter, generic_params []string) {
	for param in params {
		if param.name == '' || param.name == '_' {
			continue
		}
		if typ := t.type_from_param_type_expr(param.typ, generic_params) {
			t.remember_local_decl_type(param.name, typ)
			t.register_local_var_type(param.name, typ)
		}
	}
}

fn (mut t Transformer) seed_fn_param_decl_types(params []ast.Parameter, generic_params []string) {
	for param in params {
		if param.name == '' || param.name == '_' {
			continue
		}
		if typ := t.type_from_param_type_expr(param.typ, generic_params) {
			t.remember_local_decl_type(param.name, typ)
			continue
		}
		if typ := t.lookup_var_type(param.name) {
			t.remember_local_decl_type(param.name, typ)
		}
	}
}

fn (mut t Transformer) seed_fn_pointer_param_return_types(params []ast.Parameter, generic_params []string) {
	for param in params {
		if param.name == '' || param.name == '_' {
			continue
		}
		if ret_type := t.fn_type_expr_return_type(param.typ, generic_params) {
			t.local_fn_pointer_return_types[param.name] = ret_type
		}
	}
}

fn (t &Transformer) fn_type_expr_return_type(expr ast.Expr, generic_params []string) ?types.Type {
	mut fn_type := ast.FnType{}
	mut ok := false
	if expr is ast.Type && expr is ast.FnType {
		fn_type = expr as ast.FnType
		ok = true
	}
	if !ok || fn_type.return_type is ast.EmptyExpr {
		return none
	}
	return t.type_from_param_type_expr(fn_type.return_type, generic_params)
}

// fn_returns_result checks if a function returns a Result type
fn (t &Transformer) fn_returns_result(fn_name string) bool {
	ret_type := t.get_fn_return_type(fn_name) or { return false }
	return ret_type is types.ResultType
}

// fn_returns_option checks if a function returns an Option type
fn (t &Transformer) fn_returns_option(fn_name string) bool {
	ret_type := t.get_fn_return_type(fn_name) or { return false }
	return ret_type is types.OptionType
}

// get_fn_return_base_type gets the base type name for a function returning Result/Option
fn (t &Transformer) get_fn_return_base_type(fn_name string) string {
	ret_type := t.get_fn_return_type(fn_name) or { return '' }
	match ret_type {
		types.ResultType {
			return ret_type.base_type.name()
		}
		types.OptionType {
			return ret_type.base_type.name()
		}
		else {
			return ''
		}
	}
}

fn (t &Transformer) channel_receive_wrapper_type(expr ast.Expr) ?types.Type {
	if expr !is ast.PrefixExpr {
		return none
	}
	prefix_expr := expr as ast.PrefixExpr
	if prefix_expr.op != .arrow {
		return none
	}
	mut recv_type := types.Type(types.void_)
	if typ := t.get_expr_type(prefix_expr.expr) {
		recv_type = typ
	} else if prefix_expr.expr is ast.SelectorExpr {
		recv_type = t.get_struct_field_type(prefix_expr.expr) or { return none }
	} else {
		return none
	}
	if elem_type := recv_type.channel_elem_type() {
		return types.Type(types.OptionType{
			base_type: elem_type
		})
	}
	return none
}

fn (t &Transformer) expr_wrapper_type_for_or(expr ast.Expr) ?types.Type {
	if !expr_has_valid_data(expr) {
		return none
	}
	if typ := t.get_expr_type(expr) {
		if typ is types.OptionType || typ is types.ResultType {
			return typ
		}
	}
	if typ := t.fn_pointer_call_return_type(expr) {
		if typ is types.OptionType || typ is types.ResultType {
			return typ
		}
	}
	if typ := t.resolve_call_return_type(expr) {
		if typ is types.OptionType || typ is types.ResultType {
			return typ
		}
	}
	if wrapper_type := t.channel_receive_wrapper_type(expr) {
		return wrapper_type
	}
	return none
}

// extract_return_sumtype_name extracts the base sumtype name from a return type AST node.
// For ?SumType (OptionType) or !SumType (ResultType), returns the base type name.
fn (t &Transformer) extract_return_sumtype_name(return_type ast.Expr) string {
	if concrete_name := t.extract_concrete_generic_return_sumtype_name(return_type) {
		return concrete_name
	}
	if return_type is ast.Type {
		return t.extract_base_type_name_from_type(return_type)
	}
	return ''
}

fn (t &Transformer) extract_concrete_generic_return_sumtype_name(return_type ast.Expr) ?string {
	match return_type {
		ast.GenericArgs {
			return t.concrete_generic_return_sumtype_name_from_parts(return_type.lhs,
				return_type.args)
		}
		ast.GenericArgOrIndexExpr {
			return t.concrete_generic_return_sumtype_name_from_parts(return_type.lhs, [
				return_type.expr,
			])
		}
		ast.Type {
			match return_type {
				ast.GenericType {
					return t.concrete_generic_return_sumtype_name_from_parts(return_type.name,
						return_type.params)
				}
				ast.OptionType {
					return t.extract_concrete_generic_return_sumtype_name(return_type.base_type)
				}
				ast.ResultType {
					return t.extract_concrete_generic_return_sumtype_name(return_type.base_type)
				}
				else {}
			}
		}
		else {}
	}

	return none
}

fn (t &Transformer) concrete_generic_return_sumtype_name_from_parts(lhs ast.Expr, args []ast.Expr) ?string {
	base := t.lookup_type_from_expr(lhs) or {
		base_full := t.type_expr_name_full(lhs)
		if base_full != '' {
			t.lookup_type(base_full) or {
				base_name := t.type_expr_name(lhs)
				if base_name == '' {
					return none
				}
				t.lookup_type(base_name) or { return none }
			}
		} else {
			base_name := t.type_expr_name(lhs)
			if base_name == '' {
				return none
			}
			t.lookup_type(base_name) or { return none }
		}
	}
	if base !is types.SumType {
		return none
	}
	generic_params := generic_template_type_param_names_from_type(base)
	bindings := t.generic_type_arg_bindings(generic_params, args) or { return none }
	suffix := t.generic_specialization_suffix_from_bindings(generic_params, bindings)
	if suffix == '' {
		return none
	}
	return t.type_to_c_name(base) + suffix
}

fn (t &Transformer) extract_base_type_name_from_type(typ ast.Type) string {
	if typ is ast.OptionType {
		return t.extract_type_name_from_expr(typ.base_type)
	}
	if typ is ast.ResultType {
		return t.extract_type_name_from_expr(typ.base_type)
	}
	return ''
}

fn (t &Transformer) extract_type_name_from_expr(expr ast.Expr) string {
	if expr is ast.Ident {
		return t.return_type_context_name(expr.name)
	}
	if expr is ast.SelectorExpr {
		if expr.lhs is ast.Ident {
			lhs_ident := expr.lhs as ast.Ident
			qualified := '${lhs_ident.name}__${expr.rhs.name}'
			if _ := t.lookup_type(qualified) {
				return qualified
			}
		}
		return expr.rhs.name
	}
	return ''
}

fn (t &Transformer) return_type_context_name(name string) string {
	if name == '' || name.contains('__') {
		return name
	}
	if t.cur_module != '' && t.cur_module != 'main' && t.cur_module != 'builtin' {
		qualified := '${t.cur_module}__${name}'
		if _ := t.lookup_type(qualified) {
			return qualified
		}
	}
	return name
}

// seed_scope_with_fn_params inserts the function's receiver (for methods)
// and parameters into the current scope by name → resolved type. Used when
// the checker did not cache a scope for this function (e.g. generic functions
// whose only callsite is inside another function body).
fn (mut t Transformer) seed_scope_with_fn_params(decl ast.FnDecl) {
	if t.scope == unsafe { nil } {
		return
	}
	if decl.is_method {
		recv_name := decl.receiver.name
		if recv_name != '' && t.scope.lookup_var_type(recv_name) == none {
			if recv_type := t.lookup_type_from_expr(decl.receiver.typ) {
				mut typ := recv_type
				if decl.receiver.is_mut {
					typ = types.Type(types.Pointer{
						base_type: recv_type
					})
				}
				t.scope.insert(recv_name, typ)
			}
		}
	}
	for param in decl.typ.params {
		if param.name == '' {
			continue
		}
		if t.scope.lookup_var_type(param.name) != none {
			continue
		}
		if param_type := t.lookup_type_from_expr(param.typ) {
			mut typ := param_type
			if param.is_mut {
				typ = types.Type(types.Pointer{
					base_type: param_type
				})
			}
			t.scope.insert(param.name, typ)
		}
	}
}

// lookup_type_from_expr resolves a type-expression AST node (Ident, SelectorExpr,
// or ast.Type wrapping PointerType/OptionType/ResultType/etc.) to a `types.Type`.
// Tries module-qualified names before short names so `http.Request` resolves to
// `http__Request`.
fn (t &Transformer) lookup_type_from_expr(expr ast.Expr) ?types.Type {
	if expr is ast.EmptyExpr || !expr_has_valid_data(expr) {
		return none
	}
	if typ := t.get_synth_type(expr.pos()) {
		return typ
	}
	if expr is ast.Ident {
		if typ := t.lookup_type(expr.name) {
			return typ
		}
		if typ := t.c_name_to_type(expr.name) {
			return typ
		}
		c_name := t.v_type_name_to_c_name(expr.name)
		if c_name != '' && c_name != expr.name {
			if typ := t.c_name_to_type(c_name) {
				return typ
			}
		}
		return none
	}
	if expr is ast.SelectorExpr {
		if expr.lhs is ast.Ident {
			lhs_ident := expr.lhs as ast.Ident
			mut module_names := []string{}
			if full_name := t.cur_import_aliases[lhs_ident.name] {
				module_names << module_call_c_prefix(full_name)
				module_names << full_name.replace('.', '__')
			}
			if prefix := t.resolve_module_call_prefix(lhs_ident.name) {
				module_names << prefix
			}
			module_names << lhs_ident.name.replace('.', '__')
			mut seen := map[string]bool{}
			for module_name in module_names {
				if module_name == '' || module_name in seen {
					continue
				}
				seen[module_name] = true
				qualified := '${module_name}__${expr.rhs.name}'
				if typ := t.lookup_type(qualified) {
					return typ
				}
			}
		}
		return t.lookup_type(expr.rhs.name)
	}
	if expr is ast.PrefixExpr && expr.op == .amp {
		base := t.lookup_type_from_expr(expr.expr) or { return none }
		return types.Type(types.Pointer{
			base_type: base
		})
	}
	if expr is ast.ModifierExpr {
		return t.lookup_type_from_expr(expr.expr)
	}
	if expr is ast.GenericArgs {
		return t.lookup_generic_type_from_expr(expr.lhs, expr.args)
	}
	if expr is ast.GenericArgOrIndexExpr {
		return t.lookup_generic_type_from_expr(expr.lhs, [expr.expr])
	}
	if expr is ast.Type {
		return t.lookup_type_from_ast_type(expr)
	}
	return none
}

fn (t &Transformer) lookup_type_from_ast_type(typ ast.Type) ?types.Type {
	if typ is ast.PointerType {
		base := t.lookup_type_from_expr(typ.base_type) or { return none }
		return types.Type(types.Pointer{
			base_type: base
		})
	}
	if typ is ast.OptionType {
		base := t.lookup_type_from_expr(typ.base_type) or { return none }
		return types.Type(types.OptionType{
			base_type: base
		})
	}
	if typ is ast.ResultType {
		base := t.lookup_type_from_expr(typ.base_type) or { return none }
		return types.Type(types.ResultType{
			base_type: base
		})
	}
	if typ is ast.MapType {
		key := t.lookup_type_from_expr(typ.key_type) or { return none }
		value := t.lookup_type_from_expr(typ.value_type) or { return none }
		return types.Type(types.Map{
			key_type:   key
			value_type: value
		})
	}
	if typ is ast.ArrayType {
		elem := t.lookup_type_from_expr(typ.elem_type) or { return none }
		return types.Type(types.Array{
			elem_type: elem
		})
	}
	if typ is ast.GenericType {
		return t.lookup_generic_type_from_expr(typ.name, typ.params)
	}
	// For other ast.Type variants (ArrayFixedType, FnType, etc.), the
	// caller's name extraction logic may not need them. Returning none lets
	// callers fall back to other resolution paths.
	return none
}

fn (t &Transformer) lookup_generic_type_from_expr(lhs ast.Expr, args []ast.Expr) ?types.Type {
	base := t.lookup_type_from_expr(lhs) or { return none }
	return t.instantiate_generic_type(base, args)
}

fn (t &Transformer) instantiate_generic_type(base types.Type, args []ast.Expr) ?types.Type {
	if base is types.Struct {
		bindings := t.generic_type_arg_bindings(base.generic_params, args) or { return base }
		return substitute_type(base, bindings)
	}
	return base
}

fn (t &Transformer) instantiate_generic_sumtype_variant(variant types.Type, bindings map[string]types.Type) types.Type {
	variant_params := generic_template_type_param_names_from_type(variant)
	substituted := substitute_type(variant, bindings)
	suffix := t.generic_specialization_suffix_from_bindings(variant_params, bindings)
	if suffix == '' {
		return substituted
	}
	return specialize_generic_sumtype_variant_type(variant, substituted, suffix)
}

fn specialize_generic_sumtype_variant_type(template types.Type, substituted types.Type, suffix string) types.Type {
	if template is types.Struct && substituted is types.Struct {
		return types.Type(types.Struct{
			name:           template.name + suffix
			generic_params: substituted.generic_params
			implements:     substituted.implements
			embedded:       substituted.embedded
			fields:         substituted.fields
			is_soa:         substituted.is_soa
		})
	}
	if template is types.Pointer && substituted is types.Pointer {
		return types.Type(types.Pointer{
			lifetime:  substituted.lifetime
			base_type: specialize_generic_sumtype_variant_type(template.base_type,
				substituted.base_type, suffix)
		})
	}
	if template is types.Array && substituted is types.Array {
		return types.Type(types.Array{
			elem_type: specialize_generic_sumtype_variant_type(template.elem_type,
				substituted.elem_type, suffix)
		})
	}
	if template is types.ArrayFixed && substituted is types.ArrayFixed {
		return types.Type(types.ArrayFixed{
			len:       substituted.len
			elem_type: specialize_generic_sumtype_variant_type(template.elem_type,
				substituted.elem_type, suffix)
		})
	}
	if template is types.Map && substituted is types.Map {
		return types.Type(types.Map{
			key_type:   specialize_generic_sumtype_variant_type(template.key_type,
				substituted.key_type, suffix)
			value_type: specialize_generic_sumtype_variant_type(template.value_type,
				substituted.value_type, suffix)
		})
	}
	if template is types.OptionType && substituted is types.OptionType {
		return types.Type(types.OptionType{
			base_type: specialize_generic_sumtype_variant_type(template.base_type,
				substituted.base_type, suffix)
		})
	}
	if template is types.ResultType && substituted is types.ResultType {
		return types.Type(types.ResultType{
			base_type: specialize_generic_sumtype_variant_type(template.base_type,
				substituted.base_type, suffix)
		})
	}
	return substituted
}

fn (t &Transformer) concrete_sumtype_wrap_info_from_lhs(lhs ast.Expr) ?ConcreteSumtypeWrapInfo {
	match lhs {
		ast.GenericArgs {
			return t.concrete_sumtype_wrap_info_from_generic_parts(lhs.lhs, lhs.args)
		}
		ast.GenericArgOrIndexExpr {
			return t.concrete_sumtype_wrap_info_from_generic_parts(lhs.lhs, [lhs.expr])
		}
		else {}
	}

	return none
}

fn (t &Transformer) concrete_sumtype_wrap_info_from_return_type(return_type ast.Expr) ?ConcreteSumtypeWrapInfo {
	match return_type {
		ast.GenericArgs {
			return t.concrete_sumtype_wrap_info_from_generic_parts(return_type.lhs,
				return_type.args)
		}
		ast.GenericArgOrIndexExpr {
			return t.concrete_sumtype_wrap_info_from_generic_parts(return_type.lhs, [
				return_type.expr,
			])
		}
		ast.Type {
			match return_type {
				ast.GenericType {
					return t.concrete_sumtype_wrap_info_from_generic_parts(return_type.name,
						return_type.params)
				}
				ast.OptionType {
					return t.concrete_sumtype_wrap_info_from_return_type(return_type.base_type)
				}
				ast.ResultType {
					return t.concrete_sumtype_wrap_info_from_return_type(return_type.base_type)
				}
				else {}
			}
		}
		else {}
	}

	return none
}

fn (t &Transformer) concrete_sumtype_wrap_info_from_generic_parts(lhs ast.Expr, args []ast.Expr) ?ConcreteSumtypeWrapInfo {
	base := t.lookup_type_from_expr(lhs) or { return none }
	if base !is types.SumType {
		return none
	}
	generic_params := generic_template_type_param_names_from_type(types.Type(base))
	bindings := t.generic_type_arg_bindings(generic_params, args) or { return none }
	suffix := t.generic_specialization_suffix_from_bindings(generic_params, bindings)
	if suffix == '' {
		return none
	}
	mut variants := []string{cap: base.variants.len}
	for variant in base.variants {
		concrete_variant := t.instantiate_generic_sumtype_variant(variant, bindings)
		variant_name := t.type_to_c_name(concrete_variant)
		variants << if variant_name != '' { variant_name } else { concrete_variant.name() }
	}
	return ConcreteSumtypeWrapInfo{
		name:     t.type_to_c_name(types.Type(base)) + suffix
		variants: variants
	}
}

fn (t &Transformer) sumtype_wrap_info_for_name(type_name string) ?ConcreteSumtypeWrapInfo {
	if type_name == '' {
		return none
	}
	if t.is_sum_type(type_name) {
		return ConcreteSumtypeWrapInfo{
			name:     type_name
			variants: t.get_sum_type_variants(type_name)
		}
	}
	base := t.concrete_generic_sumtype_base_type(type_name) or { return none }
	if base !is types.SumType {
		return none
	}
	generic_params := generic_template_type_param_names_from_type(base)
	if generic_params.len == 0 || t.cur_monomorphized_fn_bindings.len == 0 {
		return none
	}
	mut bindings := map[string]types.Type{}
	for param in generic_params {
		concrete := t.cur_monomorphized_fn_bindings[param] or { return none }
		if clone_type_contains_generic_placeholder(concrete) {
			return none
		}
		bindings[param] = concrete
	}
	suffix := t.generic_specialization_suffix_from_bindings(generic_params, bindings)
	if suffix == '' {
		return none
	}
	expected_name := t.type_to_c_name(base) + suffix
	if !generic_concrete_type_names_match(expected_name, type_name) {
		return none
	}
	mut variants := []string{cap: base.variants.len}
	for variant in base.variants {
		concrete_variant := t.instantiate_generic_sumtype_variant(variant, bindings)
		variant_name := t.type_to_c_name(concrete_variant)
		variants << if variant_name != '' { variant_name } else { concrete_variant.name() }
	}
	return ConcreteSumtypeWrapInfo{
		name:     type_name
		variants: variants
	}
}

fn (t &Transformer) current_return_sumtype_wrap_info() ?ConcreteSumtypeWrapInfo {
	if t.cur_fn_return_sumtype_info.name != '' {
		return t.cur_fn_return_sumtype_info
	}
	return t.sumtype_wrap_info_for_name(t.cur_fn_ret_type_name)
}

fn generic_concrete_type_names_match(expected string, actual string) bool {
	if expected == actual {
		return true
	}
	if expected == '' || actual == '' {
		return false
	}
	expected_short := if expected.contains('__') { expected.all_after_last('__') } else { expected }
	actual_short := if actual.contains('__') { actual.all_after_last('__') } else { actual }
	return expected_short == actual_short
}

fn (t &Transformer) generic_type_arg_bindings(generic_params []string, args []ast.Expr) ?map[string]types.Type {
	if generic_params.len == 0 || args.len == 0 {
		return none
	}
	mut bindings := map[string]types.Type{}
	for i, param_name in generic_params {
		if i >= args.len {
			break
		}
		arg := args[i]
		if arg is ast.Ident {
			if concrete := t.cur_monomorphized_fn_bindings[arg.name] {
				bindings[param_name] = t.qualify_generic_concrete_type_from_expr(concrete, arg)
				continue
			}
		}
		if concrete := t.get_synth_type(arg.pos()) {
			bindings[param_name] = t.qualify_generic_concrete_type_from_expr(concrete, arg)
			continue
		}
		if concrete := t.lookup_type_from_expr(arg) {
			bindings[param_name] = t.qualify_generic_concrete_type_from_expr(concrete, arg)
			continue
		}
		if concrete := t.get_expr_type(arg) {
			bindings[param_name] = t.qualify_generic_concrete_type_from_expr(concrete, arg)
			continue
		}
	}
	if bindings.len == 0 {
		return none
	}
	return bindings
}

fn (t &Transformer) qualify_generic_concrete_type_from_expr(concrete types.Type, arg ast.Expr) types.Type {
	match concrete {
		types.Struct {
			if name := t.generic_concrete_type_arg_c_name(types.Type(concrete), arg) {
				return types.Type(types.Struct{
					name:           name
					generic_params: concrete.generic_params
					implements:     concrete.implements
					embedded:       concrete.embedded
					fields:         concrete.fields
					is_soa:         concrete.is_soa
				})
			}
		}
		types.Enum {
			if name := t.generic_concrete_type_arg_c_name(types.Type(concrete), arg) {
				return types.Type(types.Enum{
					is_flag: concrete.is_flag
					name:    name
					fields:  concrete.fields
				})
			}
		}
		types.Interface {
			if name := t.generic_concrete_type_arg_c_name(types.Type(concrete), arg) {
				return types.Type(types.Interface{
					name:   name
					fields: concrete.fields
				})
			}
		}
		types.SumType {
			if name := t.generic_concrete_type_arg_c_name(types.Type(concrete), arg) {
				return types.Type(types.SumType{
					name:           name
					generic_params: concrete.generic_params
					variants:       concrete.variants
				})
			}
		}
		types.Alias {
			if name := t.generic_concrete_type_arg_c_name(types.Type(concrete), arg) {
				return types.Type(types.Alias{
					name:      name
					base_type: concrete.base_type
				})
			}
		}
		types.NamedType {
			if name := t.generic_concrete_type_arg_c_name(types.Type(concrete), arg) {
				return types.Type(types.NamedType(name))
			}
		}
		types.Pointer {
			if base_arg := pointer_generic_type_arg_base(arg) {
				return types.Type(types.Pointer{
					lifetime:  concrete.lifetime
					base_type: t.qualify_generic_concrete_type_from_expr(concrete.base_type,
						base_arg)
				})
			}
		}
		types.Array {
			if elem_arg := array_generic_type_arg_elem(arg) {
				return types.Type(types.Array{
					elem_type: t.qualify_generic_concrete_type_from_expr(concrete.elem_type,
						elem_arg)
				})
			}
		}
		types.ArrayFixed {
			if elem_arg := array_fixed_generic_type_arg_elem(arg) {
				return types.Type(types.ArrayFixed{
					len:       concrete.len
					elem_type: t.qualify_generic_concrete_type_from_expr(concrete.elem_type,
						elem_arg)
				})
			}
		}
		types.Map {
			if parts := map_generic_type_arg_parts(arg) {
				return types.Type(types.Map{
					key_type:   t.qualify_generic_concrete_type_from_expr(concrete.key_type,
						parts.key)
					value_type: t.qualify_generic_concrete_type_from_expr(concrete.value_type,
						parts.value)
				})
			}
		}
		types.OptionType {
			if base_arg := option_generic_type_arg_base(arg) {
				return types.Type(types.OptionType{
					base_type: t.qualify_generic_concrete_type_from_expr(concrete.base_type,
						base_arg)
				})
			}
		}
		types.ResultType {
			if base_arg := result_generic_type_arg_base(arg) {
				return types.Type(types.ResultType{
					base_type: t.qualify_generic_concrete_type_from_expr(concrete.base_type,
						base_arg)
				})
			}
		}
		else {}
	}

	return concrete
}

fn (t &Transformer) generic_concrete_type_arg_c_name(concrete types.Type, arg ast.Expr) ?string {
	if arg is ast.Ident {
		if bound_type := t.cur_monomorphized_fn_bindings[arg.name] {
			name := t.type_to_c_name(bound_type)
			if name != '' {
				return name
			}
		}
		if arg.name.contains('__') {
			return arg.name
		}
		if concrete.name() == arg.name {
			return t.generic_ident_type_arg_c_name(arg.name)
		}
	}
	if arg is ast.Type {
		match arg {
			ast.GenericType {
				base_name := t.expr_to_type_name(arg.name)
				suffix := t.generic_specialization_suffix(arg.params)
				if base_name != '' && suffix != '' {
					return base_name + suffix
				}
			}
			else {}
		}
	}
	if name := t.generic_init_type_name(arg) {
		return name
	}
	name := t.expr_to_type_name(arg)
	if name == '' {
		return none
	}
	return name
}

fn (t &Transformer) generic_ident_type_arg_c_name(name string) string {
	if name == '' || name.contains('__') || t.is_builtin_type_name(name) {
		return name
	}
	if t.cur_module != '' && t.cur_module != 'main' && t.cur_module != 'builtin' {
		if builtin_scope := t.cached_scopes['builtin'] {
			if obj := builtin_scope.objects[name] {
				if _ := transformer_object_type(obj) {
					return name
				}
			}
		}
		if module_scope := t.get_module_scope(t.cur_module) {
			if obj := module_scope.objects[name] {
				if _ := transformer_object_type(obj) {
					return '${t.cur_module}__${name}'
				}
			}
		}
	}
	return name
}

fn pointer_generic_type_arg_base(arg ast.Expr) ?ast.Expr {
	match arg {
		ast.PrefixExpr {
			if arg.op in [.amp, .mul] {
				return arg.expr
			}
		}
		ast.Type {
			if arg is ast.PointerType {
				return arg.base_type
			}
		}
		else {}
	}

	return none
}

fn array_generic_type_arg_elem(arg ast.Expr) ?ast.Expr {
	if arg is ast.Type {
		match arg {
			ast.ArrayType {
				return arg.elem_type
			}
			else {}
		}
	}
	return none
}

fn array_fixed_generic_type_arg_elem(arg ast.Expr) ?ast.Expr {
	if arg is ast.Type {
		match arg {
			ast.ArrayFixedType {
				return arg.elem_type
			}
			else {}
		}
	}
	return none
}

struct GenericMapTypeArgParts {
	key   ast.Expr
	value ast.Expr
}

fn map_generic_type_arg_parts(arg ast.Expr) ?GenericMapTypeArgParts {
	if arg is ast.Type {
		match arg {
			ast.MapType {
				return GenericMapTypeArgParts{
					key:   arg.key_type
					value: arg.value_type
				}
			}
			else {}
		}
	}
	return none
}

fn option_generic_type_arg_base(arg ast.Expr) ?ast.Expr {
	if arg is ast.Type {
		match arg {
			ast.OptionType {
				return arg.base_type
			}
			else {}
		}
	}
	return none
}

fn result_generic_type_arg_base(arg ast.Expr) ?ast.Expr {
	if arg is ast.Type {
		match arg {
			ast.ResultType {
				return arg.base_type
			}
			else {}
		}
	}
	return none
}

// get_method_return_type tries to get the return type for a method call.
// Returns the return type if found, none otherwise.
fn (t &Transformer) get_method_return_type(expr ast.Expr) ?types.Type {
	// Check if this is a method call (CallExpr or CallOrCastExpr with SelectorExpr lhs)
	mut sel_expr := ast.SelectorExpr{}
	mut has_sel := false
	if expr is ast.CallExpr {
		call_lhs := t.unwrap_call_target_lhs(expr.lhs)
		if call_lhs is ast.SelectorExpr {
			sel_expr = call_lhs as ast.SelectorExpr
			has_sel = true
		}
	} else if expr is ast.CallOrCastExpr {
		call_lhs := t.unwrap_call_target_lhs(expr.lhs)
		if call_lhs is ast.SelectorExpr {
			sel_expr = call_lhs as ast.SelectorExpr
			has_sel = true
		}
	}
	if has_sel {
		method_name := sel_expr.rhs.name
		mut lookup_type_names := []string{}
		// Get the receiver type from the checker's stored types
		if receiver_type := t.resolve_expr_type(sel_expr.lhs) {
			t.append_method_lookup_type_name(mut lookup_type_names, receiver_type.name())
			base_type := t.unwrap_alias_and_pointer_type(receiver_type)
			t.append_method_lookup_type_name(mut lookup_type_names, base_type.name())
		}
		if receiver_type := t.get_expr_type(sel_expr.lhs) {
			t.append_method_lookup_type_name(mut lookup_type_names, receiver_type.name())
			base_type := t.unwrap_alias_and_pointer_type(receiver_type)
			t.append_method_lookup_type_name(mut lookup_type_names, base_type.name())
		}
		if t.is_string_expr(sel_expr.lhs) {
			if ret_type := builtin_string_method_return_type(method_name) {
				return ret_type
			}
		}
		if sel_expr.lhs is ast.SelectorExpr {
			selector_type_name := t.get_selector_type_name(sel_expr.lhs as ast.SelectorExpr)
			if selector_type_name != '' {
				t.append_method_lookup_type_name(mut lookup_type_names, selector_type_name)
			}
		} else if sel_expr.lhs is ast.Ident {
			var_type_name := t.get_var_type_name(sel_expr.lhs.name)
			t.append_method_lookup_type_name(mut lookup_type_names, var_type_name)
		}
		if receiver_type := t.resolve_expr_type(sel_expr.lhs) {
			if ret_type := t.interface_method_return_type(receiver_type, method_name) {
				return ret_type
			}
		}
		if receiver_type := t.get_expr_type(sel_expr.lhs) {
			if ret_type := t.interface_method_return_type(receiver_type, method_name) {
				return ret_type
			}
		}
		if ret_type := t.lookup_method_return_type(lookup_type_names, method_name) {
			return ret_type
		}
		if ret_type := t.unique_cached_method_return_type(method_name) {
			return ret_type
		}
		if ret_type := t.unique_scope_method_return_type(method_name) {
			return ret_type
		}
	}
	return none
}

fn builtin_string_method_return_type(method_name string) ?types.Type {
	match method_name {
		'index', 'last_index', 'index_after' {
			return types.Type(types.OptionType{
				base_type: types.Type(types.int_)
			})
		}
		else {}
	}

	return none
}

fn (t &Transformer) interface_method_return_type(receiver_type types.Type, method_name string) ?types.Type {
	base_type := t.unwrap_alias_and_pointer_type(receiver_type)
	if base_type !is types.Interface {
		return none
	}
	fields := t.resolved_interface_fields(base_type as types.Interface)
	for field in fields {
		if field.name != method_name || !field.is_interface_method {
			continue
		}
		field_type := t.unwrap_alias_type(field.typ)
		if field_type is types.FnType {
			return field_type.get_return_type()
		}
	}
	return none
}

fn (t &Transformer) resolved_interface_fields(iface types.Interface) []types.Field {
	if iface.fields.len > 0 {
		return iface.fields
	}
	if iface.name != '' {
		if live_type := t.lookup_type(iface.name) {
			if live_type is types.Interface && live_type.fields.len > 0 {
				return live_type.fields
			}
		}
		short_name := iface.name.all_after_last('__')
		if short_name != iface.name {
			if live_type := t.lookup_type(short_name) {
				if live_type is types.Interface && live_type.fields.len > 0 {
					return live_type.fields
				}
			}
		}
	}
	return iface.fields
}

fn (t &Transformer) expr_can_be_call_target(expr ast.Expr) bool {
	if lhs_type := t.resolve_expr_type(expr) {
		return t.is_callable_type(lhs_type)
	}
	match expr {
		ast.Ident {
			if t.scope != unsafe { nil } {
				if obj := t.scope.lookup_parent(expr.name, 0) {
					return obj is types.Fn
				}
			}
			return t.get_fn_return_type(expr.name) != none
		}
		ast.SelectorExpr {
			if expr.lhs is ast.Ident {
				return t.get_module_scope((expr.lhs as ast.Ident).name) != none
			}
			return false
		}
		else {
			return false
		}
	}
}

fn (t &Transformer) unwrap_call_target_lhs(lhs ast.Expr) ast.Expr {
	match lhs {
		ast.GenericArgs {
			if t.expr_can_be_call_target(lhs.lhs) {
				return t.unwrap_call_target_lhs(lhs.lhs)
			}
			return lhs
		}
		ast.GenericArgOrIndexExpr {
			if t.expr_can_be_call_target(lhs.lhs) {
				return t.unwrap_call_target_lhs(lhs.lhs)
			}
			return lhs
		}
		else {
			return lhs
		}
	}
}

fn module_call_c_prefix(module_name string) string {
	if module_name.contains('.') {
		return module_name.all_after_last('.')
	}
	if module_name.contains('__') {
		return module_name.all_after_last('__')
	}
	return module_name
}

fn (t &Transformer) resolve_module_call_prefix(module_ident string) ?string {
	if module_ident == '' {
		return none
	}
	if resolved_mod := t.resolve_module_name(module_ident) {
		return module_call_c_prefix(resolved_mod)
	}
	if t.get_module_scope(module_ident) != none {
		return module_call_c_prefix(module_ident)
	}
	return none
}

fn (mut t Transformer) transform_generic_module_call_from_parts(lhs ast.Expr, raw_args []ast.Expr, pos token.Pos) ?ast.Expr {
	mut sel := ast.SelectorExpr{}
	mut type_args := []ast.Expr{}
	match lhs {
		ast.GenericArgOrIndexExpr {
			if lhs.lhs !is ast.SelectorExpr {
				return none
			}
			sel = lhs.lhs as ast.SelectorExpr
			type_args << lhs.expr
		}
		ast.GenericArgs {
			if lhs.lhs !is ast.SelectorExpr {
				return none
			}
			sel = lhs.lhs as ast.SelectorExpr
			type_args = lhs.args.clone()
		}
		else {
			return none
		}
	}

	if sel.lhs !is ast.Ident {
		return none
	}
	module_ident := (sel.lhs as ast.Ident).name
	call_prefix := t.resolve_module_call_prefix(module_ident) or { return none }
	suffix := t.generic_specialization_suffix(type_args)
	if suffix == '' {
		return none
	}
	call_name := '${call_prefix}__${sel.rhs.name}${suffix}'
	args := t.transform_call_args_for_lhs(ast.Expr(ast.SelectorExpr{
		lhs: sel.lhs
		rhs: sel.rhs
		pos: sel.pos
	}), raw_args)
	return ast.Expr(ast.CallExpr{
		lhs:  ast.Expr(ast.Ident{
			name: call_name
			pos:  pos
		})
		args: args
		pos:  pos
	})
}

fn (mut t Transformer) transform_generic_module_call(expr ast.CallExpr) ?ast.Expr {
	return t.transform_generic_module_call_from_parts(expr.lhs, expr.args, expr.pos)
}

fn (mut t Transformer) transform_generic_selector_method_call(sel ast.SelectorExpr, type_args []ast.Expr, raw_args []ast.Expr, pos token.Pos) ?ast.Expr {
	is_module_call := sel.lhs is ast.Ident && t.lookup_var_type(sel.lhs.name) == none
		&& (t.is_module_ident(sel.lhs.name) || t.get_module_scope(sel.lhs.name) != none)
	if is_module_call {
		return none
	}
	suffix := t.generic_specialization_suffix(type_args)
	if suffix == '' {
		return none
	}
	generic_lhs := ast.Expr(ast.GenericArgs{
		lhs:  ast.Expr(sel)
		args: type_args
		pos:  pos
	})
	call_args := t.lower_missing_call_args(generic_lhs, raw_args)
	fn_info := t.lookup_call_fn_info(generic_lhs)
	mut args := []ast.Expr{cap: call_args.len + 1}
	args << t.transform_expr(sel.lhs)
	for i, arg in call_args {
		args << t.transform_call_arg_with_sumtype_check(arg, fn_info, i)
	}
	recv_is_self := t.cur_fn_recv_param != '' && sel.lhs is ast.Ident
		&& (sel.lhs as ast.Ident).name == t.cur_fn_recv_param && t.get_expr_type(sel.lhs) == none
	if recv_is_self && t.cur_fn_recv_prefix != '' {
		return ast.Expr(ast.CallExpr{
			lhs:  ast.Ident{
				name: '${t.cur_fn_recv_prefix}__${sel.rhs.name}${suffix}'
			}
			args: args
			pos:  pos
		})
	}
	method_name := sel.rhs.name + suffix
	if !t.resolves_to_embedded_method(sel.lhs, method_name) {
		if resolved := t.resolve_method_call_name(sel.lhs, method_name) {
			return ast.Expr(ast.CallExpr{
				lhs:  ast.Ident{
					name: resolved
				}
				args: args
				pos:  pos
			})
		}
	}
	if !t.resolves_to_embedded_method(sel.lhs, sel.rhs.name) {
		if resolved := t.resolve_method_call_name(sel.lhs, sel.rhs.name) {
			return ast.Expr(ast.CallExpr{
				lhs:  ast.Ident{
					name: resolved + suffix
				}
				args: args
				pos:  pos
			})
		}
	}
	return none
}

fn (t &Transformer) resolve_call_return_type(expr ast.Expr) ?types.Type {
	if ret_type := t.generic_call_concrete_return_type(expr) {
		return ret_type
	}
	mut call_lhs := ast.empty_expr
	if expr is ast.CallExpr {
		call_lhs = t.unwrap_call_target_lhs(expr.lhs)
	} else if expr is ast.CallOrCastExpr {
		call_lhs = t.unwrap_call_target_lhs(expr.lhs)
	} else {
		return none
	}
	match call_lhs {
		ast.Ident {
			ident := call_lhs as ast.Ident
			return t.get_fn_return_type(ident.name)
		}
		ast.SelectorExpr {
			sel := call_lhs as ast.SelectorExpr
			if sel.lhs is ast.Ident {
				mod_name := (sel.lhs as ast.Ident).name
				mut module_names := []string{cap: 4}
				module_names << mod_name
				if resolved_mod := t.resolve_module_name(mod_name) {
					if resolved_mod !in module_names {
						module_names << resolved_mod
					}
					short_mod := if resolved_mod.contains('.') {
						resolved_mod.all_after_last('.')
					} else if resolved_mod.contains('__') {
						resolved_mod.all_after_last('__')
					} else {
						resolved_mod
					}
					if short_mod !in module_names {
						module_names << short_mod
					}
				}
				for module_name in module_names {
					if fn_type := t.lookup_fn_cached(module_name, sel.rhs.name) {
						if ret_type := fn_type.get_return_type() {
							return ret_type
						}
					}
				}
			}
			return t.get_method_return_type(expr)
		}
		else {
			return none
		}
	}
}

fn (t &Transformer) generic_call_concrete_return_type(expr ast.Expr) ?types.Type {
	mut lhs := ast.empty_expr
	mut args := []ast.Expr{}
	match expr {
		ast.CallExpr {
			lhs = expr.lhs
			args = expr.args.clone()
		}
		ast.CallOrCastExpr {
			lhs = expr.lhs
			if expr.expr !is ast.EmptyExpr {
				args << expr.expr
			}
		}
		else {
			return none
		}
	}

	base_name := t.generic_call_base_name(lhs) or { return none }
	decl := t.generic_fn_decl_for_call_info(base_name) or { return none }
	generic_params := decl_generic_param_names(decl)
	if generic_params.len == 0 || decl.typ.return_type is ast.EmptyExpr {
		return none
	}
	info := t.generic_aware_call_fn_info(lhs, base_name) or { return none }
	bindings := t.generic_bindings_from_call_args(info, args) or { return none }
	ret_type := t.type_from_param_type_expr(decl.typ.return_type, generic_params) or { return none }
	return substitute_type(ret_type, bindings)
}

fn (t &Transformer) append_method_lookup_type_name(mut names []string, raw_name string) {
	normalized := normalized_method_lookup_type_name(raw_name)
	if normalized == '' {
		return
	}
	names << normalized
	dunder := last_double_underscore(normalized)
	if dunder >= 0 {
		short_name := normalized[dunder + 2..]
		if short_name != '' && short_name != normalized {
			names << short_name
		}
	}
}

fn normalized_method_lookup_type_name(raw_name string) string {
	if raw_name.len == 0 || !transformer_string_has_valid_data(raw_name) {
		return ''
	}
	mut normalized := if raw_name.index_u8(`.`) >= 0 {
		raw_name.replace('.', '__')
	} else {
		raw_name
	}
	if normalized.starts_with('&') {
		normalized = normalized[1..]
	}
	if normalized.ends_with('*') {
		normalized = normalized[..normalized.len - 1]
	}
	return normalized
}

fn transformer_string_has_valid_data(s string) bool {
	if s.len == 0 {
		return true
	}
	if s.len < 0 || s.len > 1024 {
		return false
	}
	ptr := unsafe { u64(s.str) }
	return transformer_data_ptr_has_valid_address(ptr)
}

fn (t &Transformer) method_key_matches_type_name(method_key string, type_name string) bool {
	if method_key.len == 0 || type_name.len == 0 || method_key.len > 1024 || type_name.len > 1024
		|| !transformer_string_has_valid_data(method_key)
		|| !transformer_string_has_valid_data(type_name) {
		return false
	}
	// Avoid .replace/.contains here: replace always allocates and contains builds
	// a KMP failure table per call. This runs inside O(method_keys) fallback loops
	// per call site, so those per-call allocations were a large transform cost.
	// Only normalize when a '.' is actually present (index_u8 does not allocate),
	// and locate `__` with a hand-rolled scan.
	normalized_key := if method_key.index_u8(`.`) >= 0 {
		method_key.replace('.', '__')
	} else {
		method_key
	}
	normalized_type := if type_name.index_u8(`.`) >= 0 {
		type_name.replace('.', '__')
	} else {
		type_name
	}
	if normalized_key == normalized_type {
		return true
	}
	key_dunder := last_double_underscore(normalized_key)
	type_dunder := last_double_underscore(normalized_type)
	if key_dunder >= 0 && type_dunder >= 0 {
		return false
	}
	short_type := if type_dunder >= 0 { normalized_type[type_dunder + 2..] } else { normalized_type }
	short_key := if key_dunder >= 0 { normalized_key[key_dunder + 2..] } else { normalized_key }
	if short_key == short_type {
		return true
	}
	if normalized_key.len > short_type.len + 2
		&& normalized_key[normalized_key.len - short_type.len - 2] == `_`
		&& normalized_key[normalized_key.len - short_type.len - 1] == `_`
		&& normalized_key.ends_with(short_type) {
		return true
	}
	if normalized_type.len > short_key.len + 2
		&& normalized_type[normalized_type.len - short_key.len - 2] == `_`
		&& normalized_type[normalized_type.len - short_key.len - 1] == `_`
		&& normalized_type.ends_with(short_key) {
		return true
	}
	return false
}

// candidate_method_keys returns the cached method keys that could fuzzy-match any
// of `names` — i.e. those sharing a receiver short name. A method_key_matches_type_name
// match always implies equal short names, so the fuzzy fallback loops can scan
// these candidates instead of every method key (O(all_keys) per call site).
fn (t &Transformer) candidate_method_keys(names []string) []string {
	mut cand := []string{}
	mut shorts_done := []string{}
	for name in names {
		if name == '' {
			continue
		}
		sh := method_short_name(name)
		if sh in shorts_done {
			continue
		}
		shorts_done << sh
		keys := t.cached_method_keys_by_short[sh] or { continue }
		cand << keys
	}
	return cand
}

fn (t &Transformer) lookup_method_return_type(type_names []string, method_name string) ?types.Type {
	if method_name == '' {
		return none
	}
	mut seen := []string{}
	for raw_name in type_names {
		if raw_name == '' {
			continue
		}
		if raw_name in seen {
			continue
		}
		seen << raw_name
		if fn_type := t.lookup_method_cached(raw_name, method_name) {
			if ret_type := fn_type.get_return_type() {
				return ret_type
			}
		}
	}
	for key in t.candidate_method_keys(seen) {
		mut matches_receiver := false
		for type_name in seen {
			if t.method_key_matches_type_name(key, type_name) {
				matches_receiver = true
				break
			}
		}
		if !matches_receiver {
			continue
		}
		methods_for_type := t.cached_methods[key] or { continue }
		for method in methods_for_type {
			if method.get_name() != method_name {
				continue
			}
			method_typ := method.get_typ()
			if method_typ is types.FnType {
				if ret_type := method_typ.get_return_type() {
					return ret_type
				}
			}
		}
	}
	return none
}

fn (t &Transformer) unique_cached_method_return_type(method_name string) ?types.Type {
	if method_name == '' {
		return none
	}
	mut found := types.Type(types.void_)
	mut found_any := false
	for key in t.cached_method_keys {
		methods_for_type := t.cached_methods[key] or { continue }
		for method in methods_for_type {
			if method.get_name() != method_name {
				continue
			}
			method_typ := method.get_typ()
			if method_typ is types.FnType {
				ret_type := method_typ.get_return_type() or { continue }
				if found_any && ret_type.name() != found.name() {
					return none
				}
				found = ret_type
				found_any = true
			}
		}
	}
	if found_any {
		return found
	}
	return none
}

fn (t &Transformer) unique_scope_method_return_type(method_name string) ?types.Type {
	if method_name == '' {
		return none
	}
	mut found := types.Type(types.void_)
	mut found_any := false
	for _, scope in t.cached_scopes {
		for key, obj in scope.objects {
			if key != method_name && !key.ends_with('__${method_name}')
				&& !key.ends_with('___${method_name}') && !key.ends_with('.${method_name}') {
				continue
			}
			if obj is types.Fn {
				fn_typ := obj.get_typ()
				if fn_typ is types.FnType {
					ret_type := fn_typ.get_return_type() or { continue }
					if found_any && ret_type.name() != found.name() {
						return none
					}
					found = ret_type
					found_any = true
				}
			}
		}
	}
	if found_any {
		return found
	}
	return none
}

fn (t &Transformer) lookup_method_exists(type_names []string, method_name string) bool {
	if method_name == '' {
		return false
	}
	mut seen := []string{}
	for raw_name in type_names {
		if raw_name == '' || raw_name in seen {
			continue
		}
		seen << raw_name
		if _ := t.lookup_method_cached(raw_name, method_name) {
			return true
		}
	}
	for key in t.candidate_method_keys(seen) {
		mut matches_receiver := false
		for type_name in seen {
			if t.method_key_matches_type_name(key, type_name) {
				matches_receiver = true
				break
			}
		}
		if !matches_receiver {
			continue
		}
		methods_for_type := t.cached_methods[key] or { continue }
		for method in methods_for_type {
			if method.get_name() == method_name {
				return true
			}
		}
	}
	return false
}

fn (t &Transformer) type_has_cached_method(typ types.Type, method_name string) bool {
	if t.type_name_has_cached_method(typ.name(), method_name) {
		return true
	}
	base_type := t.unwrap_alias_and_pointer_type(typ)
	return t.type_name_has_cached_method(base_type.name(), method_name)
}

fn (t &Transformer) type_name_has_cached_method(raw_name string, method_name string) bool {
	normalized := normalized_method_lookup_type_name(raw_name)
	if normalized == '' {
		return false
	}
	if _ := t.lookup_method_cached(normalized, method_name) {
		return true
	}
	dunder := last_double_underscore(normalized)
	if dunder >= 0 {
		short_name := normalized[dunder + 2..]
		if short_name != '' && short_name != normalized {
			return t.lookup_method_cached(short_name, method_name) != none
		}
	}
	return false
}

fn (t &Transformer) receiver_has_cached_method(receiver ast.Expr, method_name string) bool {
	if typ := t.get_expr_type(receiver) {
		return t.type_has_cached_method(typ, method_name)
	}
	if receiver is ast.SelectorExpr {
		selector_type_name := t.get_selector_type_name(receiver)
		if t.type_name_has_cached_method(selector_type_name, method_name) {
			return true
		}
		mut lookup_names := []string{}
		t.append_method_lookup_type_name(mut lookup_names, selector_type_name)
		return t.lookup_method_exists(lookup_names, method_name)
	} else if receiver is ast.Ident {
		var_type_name := t.get_var_type_name(receiver.name)
		if t.type_name_has_cached_method(var_type_name, method_name) {
			return true
		}
		mut lookup_names := []string{}
		t.append_method_lookup_type_name(mut lookup_names, var_type_name)
		return t.lookup_method_exists(lookup_names, method_name)
	}
	return false
}

fn (t &Transformer) smartcast_source_has_cached_method(ctx SmartcastContext, method_name string) bool {
	if ctx.sumtype == '' {
		return false
	}
	if typ := t.c_name_to_type(ctx.sumtype) {
		return t.type_has_cached_method(typ, method_name)
	}
	if t.type_name_has_cached_method(ctx.sumtype, method_name) {
		return true
	}
	mut lookup_names := []string{}
	t.append_method_lookup_type_name(mut lookup_names, ctx.sumtype)
	return t.lookup_method_exists(lookup_names, method_name)
}

fn (t &Transformer) smartcast_variant_method_name(ctx SmartcastContext, method_name string) ?string {
	mut lookup_names := []string{cap: 4}
	if ctx.variant_full != '' {
		lookup_names << ctx.variant_full
	}
	if ctx.variant != '' && ctx.variant != ctx.variant_full {
		lookup_names << ctx.variant
	}
	if ctx.variant_full != '' && !ctx.variant_full.contains('__') && t.cur_module != ''
		&& t.cur_module != 'main' && t.cur_module != 'builtin'
		&& ctx.variant_full !in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'byte', 'rune', 'f32', 'f64', 'usize', 'isize', 'bool', 'string', 'voidptr', 'charptr', 'byteptr'] {
		lookup_names << '${t.cur_module.replace('.', '__')}__${ctx.variant_full}'
	}
	for lookup_name in lookup_names {
		if t.lookup_method_cached(lookup_name, method_name) != none {
			return '${lookup_name}__${method_name}'
		}
	}
	return none
}

fn (mut t Transformer) smartcast_method_receiver(receiver ast.Expr, ctx SmartcastContext) ast.Expr {
	is_interface_ctx := ctx.sumtype.starts_with('__iface__')
	if is_interface_ctx || t.is_interface_type(ctx.sumtype) || t.is_interface_receiver(receiver) {
		iface_object := t.synth_selector(receiver, '_object', types.Type(types.voidptr_))
		mut concrete_type := ctx.variant_full
		if concrete_type == '' {
			concrete_type = ctx.variant
		}
		if concrete_type != '' {
			return ast.CastExpr{
				typ:  ast.Ident{
					name: '${concrete_type}*'
				}
				expr: iface_object
			}
		}
	}
	return t.apply_smartcast_receiver_ctx(receiver, ctx)
}

struct SmartcastMethodReceiver {
	ctx      SmartcastContext
	receiver ast.Expr
}

fn (t &Transformer) explicit_cast_inner_expr(expr ast.Expr) ?ast.Expr {
	if expr is ast.AsCastExpr {
		return expr.expr
	}
	if expr is ast.CastExpr {
		return expr.expr
	}
	if expr is ast.ParenExpr {
		return t.explicit_cast_inner_expr(expr.expr)
	}
	return none
}

fn (t &Transformer) method_receiver_without_lhs_explicit_cast(receiver ast.Expr) ?ast.Expr {
	if receiver is ast.SelectorExpr {
		uncasted_lhs := t.explicit_cast_inner_expr(receiver.lhs) or { return none }
		return ast.SelectorExpr{
			lhs: uncasted_lhs
			rhs: receiver.rhs
			pos: receiver.pos
		}
	}
	return none
}

fn (t &Transformer) smartcast_method_receiver_context(receiver ast.Expr) ?SmartcastMethodReceiver {
	receiver_str := t.expr_to_string(receiver)
	if receiver_str != '' {
		if ctx := t.find_smartcast_for_expr(receiver_str) {
			return SmartcastMethodReceiver{
				ctx:      ctx
				receiver: receiver
			}
		}
	}
	normalized_receiver := t.method_receiver_without_lhs_explicit_cast(receiver) or { return none }
	normalized_str := t.expr_to_string(normalized_receiver)
	if normalized_str == '' {
		return none
	}
	ctx := t.find_smartcast_for_expr(normalized_str) or { return none }
	return SmartcastMethodReceiver{
		ctx:      ctx
		receiver: normalized_receiver
	}
}

// resolve_expr_type resolves the type of an expression, falling back to scope
// lookup when the checker didn't store a type at the expression's position.
fn (t &Transformer) resolve_expr_type(expr ast.Expr) ?types.Type {
	if !expr_has_valid_data(expr) {
		return none
	}
	// First try the environment (checker stored type)
	pos := expr.pos()
	if pos.is_valid() {
		if typ := t.env.get_expr_type(pos.id) {
			return typ
		}
	}
	// Fallback: resolve based on expression structure
	if expr is ast.Ident {
		if resolved_type := t.lookup_var_type(expr.name) {
			return resolved_type
		}
		return none
	}
	if expr is ast.IndexExpr {
		// For slices (a[x..y]), the result type is the same as the container
		if expr.expr is ast.RangeExpr {
			if resolved_type := t.resolve_expr_type(expr.lhs) {
				return resolved_type
			}
		}
	}
	return none
}

fn (t &Transformer) fn_pointer_call_return_type(expr ast.Expr) ?types.Type {
	lhs := match expr {
		ast.CallExpr { expr.lhs }
		ast.CallOrCastExpr { expr.lhs }
		else { return none }
	}

	if lhs is ast.Ident {
		if lhs.name in t.local_fn_pointer_return_types {
			ret := t.local_fn_pointer_return_types[lhs.name] or { return none }
			return ret
		}
		fn_type := t.lookup_fn_pointer_var_type(lhs.name) or { return none }
		if ret := fn_type.get_return_type() {
			return ret
		}
	}
	return none
}

fn (t &Transformer) lookup_fn_pointer_var_type(name string) ?types.FnType {
	if typ := t.lookup_var_type(name) {
		return fn_type_from_transformer_type(typ)
	}
	if t.scope == unsafe { nil } {
		return none
	}
	mut scope := unsafe { t.scope }
	for ; scope != unsafe { nil }; scope = scope.parent {
		obj := scope.objects[name] or { continue }
		return fn_type_from_transformer_type(obj.typ())
	}
	return none
}

fn fn_type_from_transformer_type(typ types.Type) ?types.FnType {
	if !types.type_has_valid_payload(typ) {
		return none
	}
	match typ {
		types.FnType {
			return typ
		}
		types.Alias {
			if types.type_has_valid_payload(typ.base_type) && typ.base_type is types.FnType {
				return typ.base_type as types.FnType
			}
		}
		types.Pointer {
			if !types.type_has_valid_payload(typ.base_type) {
				return none
			}
			if typ.base_type is types.FnType {
				return typ.base_type as types.FnType
			}
			if typ.base_type is types.Alias && types.type_has_valid_payload(typ.base_type.base_type)
				&& typ.base_type.base_type is types.FnType {
				return typ.base_type.base_type as types.FnType
			}
		}
		else {}
	}

	return none
}

// expr_returns_option checks if an expression returns an Option type by looking up
// its type from the checker's environment. Works for both function and method calls.
fn (t &Transformer) expr_returns_option(expr ast.Expr) bool {
	if !expr_has_valid_data(expr) {
		return false
	}
	if wrapper_type := t.expr_wrapper_type_for_or(expr) {
		return wrapper_type is types.OptionType
	}
	if typ := t.get_expr_type(expr) {
		if typ is types.OptionType || typ.name().starts_with('?') {
			return true
		}
	}
	if ret := t.fn_pointer_call_return_type(expr) {
		return ret is types.OptionType || ret.name().starts_with('?')
	}
	if ret := t.get_method_return_type(expr) {
		return ret is types.OptionType
	}
	// Fallback: check if the call target is a function pointer variable with Option return type.
	if expr is ast.CallExpr || expr is ast.CallOrCastExpr {
		mut call_lhs := ast.empty_expr
		if expr is ast.CallExpr {
			call_lhs = expr.lhs
		} else if expr is ast.CallOrCastExpr {
			call_lhs = expr.lhs
		}
		if call_lhs is ast.Ident {
			lhs_ident := call_lhs as ast.Ident
			if var_type := t.lookup_var_type(lhs_ident.name) {
				return t.fn_type_returns_option(var_type)
			}
		}
	}
	return false
}

// expr_returns_result checks if an expression returns a Result type by looking up
// its type from the checker's environment. Works for both function and method calls.
fn (t &Transformer) expr_returns_result(expr ast.Expr) bool {
	if !expr_has_valid_data(expr) {
		return false
	}
	if typ := t.get_expr_type(expr) {
		if typ is types.ResultType || typ.name().starts_with('!') {
			return true
		}
	}
	if ret := t.fn_pointer_call_return_type(expr) {
		return ret is types.ResultType || ret.name().starts_with('!')
	}
	if ret := t.get_method_return_type(expr) {
		return ret is types.ResultType
	}
	// Fallback: check if the call target is a function pointer variable with Result return type.
	// This handles cases like `if r := fn_ptr_var(args)` where the type checker didn't
	// annotate the call expression but the variable's FnType has the return type info.
	if expr is ast.CallExpr || expr is ast.CallOrCastExpr {
		mut call_lhs := ast.empty_expr
		if expr is ast.CallExpr {
			call_lhs = expr.lhs
		} else if expr is ast.CallOrCastExpr {
			call_lhs = expr.lhs
		}
		if call_lhs is ast.Ident {
			lhs_ident := call_lhs as ast.Ident
			if var_type := t.lookup_var_type(lhs_ident.name) {
				return t.fn_type_returns_result(var_type)
			}
		}
	}
	return false
}

fn (t &Transformer) fn_type_returns_result(typ types.Type) bool {
	match typ {
		types.FnType {
			if ret := typ.get_return_type() {
				return ret is types.ResultType
			}
		}
		types.Alias {
			return t.fn_type_returns_result(typ.base_type)
		}
		types.Pointer {
			return t.fn_type_returns_result(typ.base_type)
		}
		else {}
	}

	return false
}

fn (t &Transformer) fn_type_returns_option(typ types.Type) bool {
	match typ {
		types.FnType {
			if ret := typ.get_return_type() {
				return ret is types.OptionType
			}
		}
		types.Alias {
			return t.fn_type_returns_option(typ.base_type)
		}
		types.Pointer {
			return t.fn_type_returns_option(typ.base_type)
		}
		else {}
	}

	return false
}

// get_expr_base_type gets the base type name for an expression returning Result/Option
fn (t &Transformer) get_expr_base_type(expr ast.Expr) string {
	if !expr_has_valid_data(expr) {
		return ''
	}
	if wrapper_type := t.expr_wrapper_type_for_or(expr) {
		match wrapper_type {
			types.ResultType {
				return wrapper_type.base_type.name()
			}
			types.OptionType {
				return wrapper_type.base_type.name()
			}
			else {}
		}
	}
	return ''
}

fn (t &Transformer) contains_call_expr(expr ast.Expr) bool {
	return t.contains_call_expr_depth(0, expr)
}

fn (t &Transformer) contains_call_expr_depth(depth int, expr ast.Expr) bool {
	if depth > max_runtime_const_dep_expr_depth || !expr_has_valid_data(expr) {
		return false
	}
	return match expr {
		ast.CallExpr {
			true
		}
		ast.CastExpr {
			t.contains_call_expr_depth(depth + 1, expr.expr)
		}
		ast.ParenExpr {
			t.contains_call_expr_depth(depth + 1, expr.expr)
		}
		ast.CallOrCastExpr {
			t.contains_call_expr_depth(depth + 1, expr.expr)
		}
		ast.PrefixExpr {
			t.contains_call_expr_depth(depth + 1, expr.expr)
		}
		ast.PostfixExpr {
			t.contains_call_expr_depth(depth + 1, expr.expr)
		}
		ast.InfixExpr {
			t.contains_call_expr_depth(depth + 1, expr.lhs)
				|| t.contains_call_expr_depth(depth + 1, expr.rhs)
		}
		ast.ArrayInitExpr {
			mut has_call := false
			for e in expr.exprs {
				if t.contains_call_expr_depth(depth + 1, e) {
					has_call = true
					break
				}
			}
			has_call = has_call
				|| (expr.init !is ast.EmptyExpr && t.contains_call_expr_depth(depth + 1, expr.init))
			has_call = has_call
				|| (expr.len !is ast.EmptyExpr && t.contains_call_expr_depth(depth + 1, expr.len))
			has_call = has_call
				|| (expr.cap !is ast.EmptyExpr && t.contains_call_expr_depth(depth + 1, expr.cap))
			has_call
		}
		ast.InitExpr {
			mut has_call := false
			for field in expr.fields {
				if t.contains_call_expr_depth(depth + 1, field.value) {
					has_call = true
					break
				}
			}
			has_call
		}
		ast.MapInitExpr {
			mut has_call := false
			for key in expr.keys {
				if t.contains_call_expr_depth(depth + 1, key) {
					has_call = true
					break
				}
			}
			if !has_call {
				for val in expr.vals {
					if t.contains_call_expr_depth(depth + 1, val) {
						has_call = true
						break
					}
				}
			}
			has_call
		}
		ast.SelectorExpr {
			t.contains_call_expr_depth(depth + 1, expr.lhs)
		}
		ast.IndexExpr {
			t.contains_call_expr_depth(depth + 1, expr.lhs)
				|| t.contains_call_expr_depth(depth + 1, expr.expr)
		}
		else {
			false
		}
	}
}

// get_call_fn_name extracts the function name from a call expression
fn (t &Transformer) get_call_fn_name(expr ast.Expr) string {
	if expr is ast.CallExpr {
		return t.call_lhs_name(expr.lhs)
	}
	if expr is ast.CallOrCastExpr {
		return t.call_lhs_name(expr.lhs)
	}
	return ''
}

// is_method_call_expr returns true when `expr` is a call whose lhs is a method
// receiver (`recv.method(...)`), as opposed to a module function call
// (`mod.fn(...)`) or a direct identifier call (`fn(...)`).
fn (t &Transformer) is_method_call_expr(expr ast.Expr) bool {
	mut lhs := ast.empty_expr
	if expr is ast.CallExpr {
		lhs = t.unwrap_call_target_lhs(expr.lhs)
	} else if expr is ast.CallOrCastExpr {
		lhs = t.unwrap_call_target_lhs(expr.lhs)
	} else {
		return false
	}
	if lhs !is ast.SelectorExpr {
		return false
	}
	sel := lhs as ast.SelectorExpr
	// If sel.lhs is an Ident that resolves to a module, treat as module call.
	if sel.lhs is ast.Ident {
		mod_ident := (sel.lhs as ast.Ident).name
		if t.get_module_scope(mod_ident) != none {
			return false
		}
		if t.resolve_module_name(mod_ident) != none {
			return false
		}
	}
	return true
}

fn (t &Transformer) call_lhs_name(lhs ast.Expr) string {
	unwrapped_lhs := t.unwrap_call_target_lhs(lhs)
	match unwrapped_lhs {
		ast.Ident {
			ident := unwrapped_lhs as ast.Ident
			return ident.name
		}
		ast.SelectorExpr {
			sel := unwrapped_lhs as ast.SelectorExpr
			return sel.rhs.name
		}
		ast.GenericArgs {
			ga := unwrapped_lhs as ast.GenericArgs
			return t.call_lhs_name(ga.lhs)
		}
		ast.GenericArgOrIndexExpr {
			gai := unwrapped_lhs as ast.GenericArgOrIndexExpr
			return t.call_lhs_name(gai.lhs)
		}
		else {
			return ''
		}
	}
}

// is_void_call_expr checks if an expression is a function call that returns void.
// Used to detect or-blocks that end with a void call (e.g. error_with_pos()).
fn (mut t Transformer) is_void_call_expr(expr ast.Expr) bool {
	fn_name := t.get_call_fn_name(expr)
	if fn_name == '' {
		return false
	}
	if ret := t.get_expr_type(expr) {
		ret_name := ret.name()
		return ret_name == '' || ret_name == 'void' || ret_name == 'Void'
	}
	// Check using method return type lookup
	if ret := t.get_method_return_type(expr) {
		// Check if the return type is actually void (Void, Nil, or Primitive with empty name)
		ret_name := ret.name()
		if ret_name != '' && ret_name != 'void' && ret_name != 'Void' {
			return false // Has a non-void return type
		}
		return true // Return type is void
	}
	// For method calls (SelectorExpr LHS), don't fall back to fn_return_type lookup
	// since the short method name may conflict with a builtin function.
	// e.g. `logger.error(...)` has fn_name='error' which matches builtin `error()`.
	mut is_method_call := false
	if expr is ast.CallExpr && expr.lhs is ast.SelectorExpr {
		is_method_call = true
	} else if expr is ast.CallOrCastExpr && expr.lhs is ast.SelectorExpr {
		is_method_call = true
	}
	if !is_method_call {
		// Check using fn return type lookup
		if ret := t.get_fn_return_type(fn_name) {
			ret_name := ret.name()
			if ret_name != '' && ret_name != 'void' && ret_name != 'Void' {
				return false
			}
			return true
		}
		// Fallback for well-known noreturn / void builtins that may not have a
		// registered Fn entry (e.g. when the transformer runs against synthetic
		// AST in unit tests). Real code with a registered signature still hits
		// the get_fn_return_type branch above.
		short_name := fn_name.all_after_last('__')
		if short_name in ['panic', 'exit', 'eprintln_exit', 'unreachable'] {
			return true
		}
	}
	return false
}

fn (mut t Transformer) transform_fn_decl(decl ast.FnDecl) ast.FnDecl {
	lowered_decl := t.fn_decl_with_implicit_veb_context_param(decl)
	attrs, stmts := t.transform_fn_decl_parts(lowered_decl)
	return ast.FnDecl{
		attributes: attrs
		is_public:  lowered_decl.is_public
		is_method:  lowered_decl.is_method
		is_static:  lowered_decl.is_static
		receiver:   lowered_decl.receiver
		language:   lowered_decl.language
		name:       lowered_decl.name
		typ:        lowered_decl.typ
		stmts:      stmts
		pos:        lowered_decl.pos
	}
}

fn (mut t Transformer) fn_decl_with_implicit_veb_context_param(decl ast.FnDecl) ast.FnDecl {
	if decl.receiver.name == 'ctx' {
		return decl
	}
	for param in decl.typ.params {
		if param.name == 'ctx' {
			return decl
		}
	}
	scope_fn_name, fn_scope_key := t.fn_scope_names_for_decl(decl)
	if !t.fn_decl_returns_veb_result(decl, scope_fn_name, fn_scope_key) {
		return decl
	}
	ctx_type := t.implicit_veb_context_type(scope_fn_name, fn_scope_key) or { return decl }
	ctx_base_type := if ctx_type is types.Pointer { ctx_type.base_type } else { ctx_type }
	ctx_param := ast.Parameter{
		name:   'ctx'
		typ:    t.type_to_ast_expr(ctx_base_type, decl.pos)
		is_mut: true
		pos:    decl.pos
	}
	mut params := []ast.Parameter{cap: decl.typ.params.len + 1}
	params << ctx_param
	for param in decl.typ.params {
		params << param
	}
	return ast.FnDecl{
		attributes: decl.attributes
		is_public:  decl.is_public
		is_method:  decl.is_method
		is_static:  decl.is_static
		receiver:   decl.receiver
		language:   decl.language
		name:       decl.name
		typ:        ast.FnType{
			generic_params: decl.typ.generic_params
			params:         params
			return_type:    decl.typ.return_type
		}
		stmts:      decl.stmts
		pos:        decl.pos
	}
}

fn (mut t Transformer) implicit_veb_context_type(scope_fn_name string, fn_scope_key string) ?types.Type {
	if fn_scope := t.cached_fn_scopes[fn_scope_key] {
		if typ := fn_scope.lookup_var_type('ctx') {
			return typ
		}
	}
	if fn_scope := t.env.get_fn_scope(t.cur_module, scope_fn_name) {
		if typ := fn_scope.lookup_var_type('ctx') {
			return typ
		}
	}
	return none
}

fn (mut t Transformer) fn_decl_returns_veb_result(decl ast.FnDecl, scope_fn_name string, fn_scope_key string) bool {
	if t.return_expr_is_veb_result(decl.typ.return_type) {
		return true
	}
	if ret := t.get_fn_return_type(scope_fn_name) {
		if ret.name() == 'veb__Result' || ret.name() == 'veb.Result' {
			return true
		}
	}
	if ret := t.get_fn_return_type(fn_scope_key) {
		if ret.name() == 'veb__Result' || ret.name() == 'veb.Result' {
			return true
		}
	}
	return false
}

fn (t &Transformer) return_expr_is_veb_result(expr ast.Expr) bool {
	match expr {
		ast.Ident {
			return expr.name == 'veb__Result' || expr.name == 'veb.Result'
				|| (expr.name == 'Result' && t.cur_module == 'veb')
		}
		ast.SelectorExpr {
			if expr.lhs is ast.Ident {
				lhs := expr.lhs as ast.Ident
				return lhs.name == 'veb' && expr.rhs.name == 'Result'
			}
		}
		else {}
	}

	return false
}

fn (t &Transformer) fn_scope_names_for_decl(decl ast.FnDecl) (string, string) {
	scope_fn_name := if decl.is_method {
		// Match checker scope keys: receiver base type name WITHOUT current module prefix,
		// then `__method_name`. The module name is already part of env.get_fn_scope key.
		mut recv_name := t.get_receiver_type_name(decl.receiver.typ)
		if t.cur_module != '' {
			prefix := '${t.cur_module}__'
			if recv_name.starts_with(prefix) {
				recv_name = recv_name[prefix.len..]
			}
		}
		'${recv_name}__${decl.name}'
	} else {
		decl.name
	}
	fn_scope_key := if t.cur_module == '' {
		scope_fn_name
	} else {
		'${t.cur_module}__${scope_fn_name}'
	}
	return scope_fn_name, fn_scope_key
}

// transform_fn_decl_parts_to_flat is the flat-builder mirror of
// `transform_fn_decl_parts`. Returns the final attribute list and the body
// stmts already encoded as FlatNodeIds in `out`, ready for the FnDecl arm to
// wrap via `emit_fn_decl_by_ids`. Currently a literal pass-through (delegates
// to the legacy `_parts` helper, then leaf-encodes each stmt via
// `out.emit_stmt`) — same observable behavior as the previous FnDecl arm's
// explicit per-stmt loop, just moved behind a named seam. The point of the
// seam is to enable progressive porting of `transform_stmts` body-stmt
// expansion sites in follow-up sessions: each future session can replace
// part of the pass-through with direct-emit logic that skips intermediate
// `ast.Stmt` wrappers. Bit-equal scaffolding — zero memory savings on its
// own; the wins materialise in the per-site ports.
fn (mut t Transformer) transform_fn_decl_parts_to_flat(decl ast.FnDecl, mut out ast.FlatBuilder) ([]ast.Attribute, []ast.FlatNodeId) {
	attrs, stmts := t.transform_fn_decl_parts(decl)
	mut stmt_ids := []ast.FlatNodeId{cap: stmts.len}
	for s in stmts {
		stmt_ids << out.emit_stmt(s)
	}
	return attrs, stmt_ids
}

// transform_fn_decl_parts is the body-work driver behind `transform_fn_decl`.
// It returns the two variable parts of the lowered FnDecl — final attribute
// list (possibly augmented with `noinline` for `@[live]`) and the final
// transformed + defer-lowered stmt list — leaving the immutable
// is_public/is_method/is_static/receiver/language/name/typ/pos fields to be
// re-attached by the caller. The flat-write port's FnDecl arm calls
// `transform_fn_decl_parts_to_flat` (above) which delegates here and then
// leaf-encodes. Future sessions fork the body work into direct-emit paths
// inside the `_to_flat` seam without touching this legacy helper or its
// non-flat callers.
struct FnBodyTransformCtx {
mut:
	live_fn_detected                    bool
	scope_fn_name                       string
	fn_scope_key                        string
	has_return_type                     bool
	fn_return_type                      types.Type
	old_scope                           &types.Scope = unsafe { nil }
	old_fn_root_scope                   &types.Scope = unsafe { nil }
	old_local_decl_types                map[string]types.Type
	old_fn_ret_type_name                string
	old_fn_return_sumtype_info          ConcreteSumtypeWrapInfo
	old_fn_returns_option               bool
	old_fn_returns_result               bool
	old_fn_name_str                     string
	old_fn_recv_prefix                  string
	old_fn_recv_param                   string
	old_fn_recv_is_ptr                  bool
	old_monomorphized_bindings          map[string]types.Type
	old_fn_generic_params               []string
	old_local_fn_pointer_return_types   map[string]types.Type
	old_local_receiver_generic_bindings map[string]map[string]types.Type
	old_generic_var_type_params         map[string]string
	old_smartcast_stack                 []SmartcastContext
	old_smartcast_expr_counts           map[string]int
}

fn (mut t Transformer) enter_fn_body_transform(decl ast.FnDecl) ?FnBodyTransformCtx {
	mut ctx := FnBodyTransformCtx{}
	// and they will never be called, so emit an empty body.
	if has_non_lifetime_generic_params(decl.typ.generic_params) {
		mut has_generic_types := decl.is_static || decl.name in t.env.generic_types
		if !has_generic_types {
			for key, _ in t.env.generic_types {
				if key.starts_with('${decl.name}[') || key.contains('.${decl.name}[')
					|| key.ends_with('.${decl.name}') || key.contains('__${decl.name}[')
					|| key.ends_with('__${decl.name}') {
					has_generic_types = true
					break
				}
			}
		}
		// Generic function values (`handler[T]`) are specialized later by cgen, not
		// through the normal checked call path, so keep their bodies for that pass.
		if !has_generic_types && decl.name !in t.generic_fn_value_names {
			return none
		}
	}

	// Check for conditional compilation attributes (e.g., @[if verbose ?])
	// Skip functions whose conditions evaluate to false, and mark them for call elision
	for attr in decl.attributes {
		if attr.comptime_cond !is ast.EmptyExpr {
			if !t.eval_comptime_cond(attr.comptime_cond) {
				t.elided_fns[decl.name] = true
				return none
			}
		}
	}

	// Detect @[live] functions for hot code reloading
	if t.pref != unsafe { nil }
		&& (t.pref.backend == .arm64 || t.pref.backend == .x64 || t.pref.backend == .cleanc) {
		if decl.attributes.has('live') {
			mangled := if decl.is_method {
				recv_name := t.get_receiver_type_name(decl.receiver.typ)
				'${recv_name}__${decl.name}'
			} else {
				decl.name
			}
			recv_type := if decl.is_method {
				t.get_receiver_type_name(decl.receiver.typ)
			} else {
				''
			}
			t.live_fns << LiveFn{
				decl_name:    decl.name
				mangled_name: mangled
				is_method:    decl.is_method
				recv_type:    recv_type
			}
			if t.cur_file_name.len > 0 {
				t.live_source_file = t.cur_file_name
			}
			ctx.live_fn_detected = true
		}
	}
	// Save current scope and fn_root_scope
	ctx.old_scope = t.scope
	ctx.old_fn_root_scope = t.fn_root_scope

	// Get the function's scope from the environment (populated by checker)
	// This contains parameter types, receiver type, and local variables
	// For methods, include receiver type in the key (e.g., "SortedMap__set").
	// The key must match how the checker generates it (using resolved/base type).
	scope_fn_name, fn_scope_key := t.fn_scope_names_for_decl(decl)
	ctx.fn_scope_key = fn_scope_key
	fn_generic_params := generic_param_names(decl.typ.generic_params)
	ctx.old_local_decl_types = t.local_decl_types.move()
	t.local_decl_types = map[string]types.Type{}
	if fn_scope := t.cached_fn_scopes[fn_scope_key] {
		t.scope = types.new_scope(fn_scope)
		t.fn_root_scope = t.scope
	} else {
		// Fallback: create a new scope if function scope not found.
		// Generic function bodies are skipped by the checker when no
		// specialization is recorded at body-check time, so no scope is
		// cached. Seed the scope from the AST params/receiver so method
		// return-type lookups in or-expr lowering keep working.
		t.open_scope()
		t.fn_root_scope = t.scope
		t.seed_fallback_fn_param_scope(decl.typ.params, fn_generic_params)
		// Cache the seeded scope locally and also publish directly to env.fn_scopes
		// (lock-protected). Worker→main merge of cached_fn_scopes can silently drop
		// keys inserted into a worker's map after clone, so we publish the entry
		// through the shared environment as well — that's what cleanc reads at
		// emit time via env.get_fn_scope(cur_module, fn_name).
		t.cached_fn_scopes[fn_scope_key] = t.fn_root_scope
		t.env.set_fn_scope(t.cur_module, scope_fn_name, t.fn_root_scope)
	}
	if decl.is_method && decl.receiver.name != '' && decl.receiver.name != '_' {
		if typ := t.type_from_param_type_expr(decl.receiver.typ, fn_generic_params) {
			t.remember_local_decl_type(decl.receiver.name, typ)
			t.register_local_var_type(decl.receiver.name, typ)
		}
	}
	t.seed_fn_param_decl_types(decl.typ.params, fn_generic_params)
	t.seed_fn_pointer_param_return_types(decl.typ.params, fn_generic_params)
	// Ensure params/receiver are present in scope. The checker may cache an
	// empty scope for generic function bodies (no specialization recorded),
	// so seed missing entries from the AST so method-return-type lookups in
	// or-expr lowering (e.g. `req.header.get(.host) or { ... }`) work.
	t.seed_scope_with_fn_params(decl)
	ctx.old_monomorphized_bindings = t.cur_monomorphized_fn_bindings.move()
	t.cur_monomorphized_fn_bindings = t.lookup_monomorphized_fn_bindings(t.cur_module,
		scope_fn_name) or {
		t.lookup_monomorphized_fn_bindings(t.cur_module, decl.name) or {
			map[string]types.Type{}
		}
	}

	// Set current function return type for sum type wrapping in returns
	// and enum shorthand resolution
	ctx.old_fn_ret_type_name = t.cur_fn_ret_type_name
	ctx.old_fn_return_sumtype_info = t.cur_fn_return_sumtype_info
	ctx.old_fn_returns_option = t.cur_fn_returns_option
	ctx.old_fn_returns_result = t.cur_fn_returns_result
	t.cur_fn_return_sumtype_info = ConcreteSumtypeWrapInfo{}
	t.cur_fn_returns_option = false
	t.cur_fn_returns_result = false
	if decl.typ.return_type is ast.Type {
		t.cur_fn_returns_option = decl.typ.return_type is ast.OptionType
		t.cur_fn_returns_result = decl.typ.return_type is ast.ResultType
	}
	if decl.typ.return_type is ast.Ident {
		ret_name := decl.typ.return_type.name
		// Qualify with module prefix for enum shorthand resolution
		// (e.g., Token → token__Token so resolve_enum_shorthand produces token__Token__member)
		// Skip qualification if the type is a builtin type (e.g., ChanState is defined in
		// vlib/builtin, so functions in the sync module returning ChanState should NOT
		// produce sync__ChanState__member — just ChanState__member).
		mut is_builtin_ret_type := false
		if !ret_name.contains('__') {
			if scope := t.get_module_scope('builtin') {
				if obj := scope.lookup_parent(ret_name, 0) {
					is_builtin_ret_type = obj is types.Type
				}
			}
			// Fallback: check if module-qualified name does NOT exist as a type.
			// If `sync__ChanState` is not a real type but `ChanState` is (builtin),
			// then don't add the module prefix.
			if !is_builtin_ret_type && t.cur_module != '' {
				qualified := '${t.cur_module}__${ret_name}'
				qualified_exists := t.lookup_type(qualified) != none
				if !qualified_exists {
					is_builtin_ret_type = true
				}
			}
		}
		if t.cur_module != '' && t.cur_module != 'main' && t.cur_module != 'builtin'
			&& !ret_name.contains('__') && !is_builtin_ret_type {
			t.cur_fn_ret_type_name = '${t.cur_module}__${ret_name}'
		} else {
			t.cur_fn_ret_type_name = ret_name
		}
	} else if decl.typ.return_type is ast.SelectorExpr {
		// Handle module-qualified return types like token.Token
		sel := decl.typ.return_type as ast.SelectorExpr
		if sel.lhs is ast.Ident {
			t.cur_fn_ret_type_name = '${sel.lhs.name}__${sel.rhs.name}'
		}
	} else {
		t.cur_fn_ret_type_name = t.extract_return_sumtype_name(decl.typ.return_type)
	}
	t.cur_fn_return_sumtype_info = t.concrete_sumtype_wrap_info_from_return_type(decl.typ.return_type) or {
		ConcreteSumtypeWrapInfo{}
	}

	// Transform function body
	// Clear per-function state: array_elem_type_overrides tracks .map() result types
	// and must not leak across function boundaries (e.g., variable 'a' in one function
	// must not affect variable 'a' in another function).
	t.array_elem_type_overrides = map[string]string{}
	t.interface_concrete_types = map[string]string{}
	ctx.old_fn_name_str = t.cur_fn_name_str
	ctx.old_fn_recv_prefix = t.cur_fn_recv_prefix
	ctx.old_fn_recv_param = t.cur_fn_recv_param
	ctx.old_fn_recv_is_ptr = t.cur_fn_recv_is_ptr
	t.cur_fn_name_str = decl.name
	if decl.is_method {
		recv_name := t.get_receiver_type_name(decl.receiver.typ)
		if t.cur_module != '' && t.cur_module != 'main' && t.cur_module != 'builtin'
			&& !recv_name.contains('__') {
			t.cur_fn_recv_prefix = '${t.cur_module}__${recv_name}'
		} else {
			t.cur_fn_recv_prefix = recv_name
		}
		t.cur_fn_recv_param = decl.receiver.name
		mut recv_is_ptr := decl.receiver.is_mut
		if recv_type := t.type_from_param_type_expr(decl.receiver.typ, fn_generic_params) {
			recv_is_ptr = recv_is_ptr || t.is_pointer_type(recv_type)
		}
		t.cur_fn_recv_is_ptr = recv_is_ptr
	} else {
		t.cur_fn_recv_prefix = ''
		t.cur_fn_recv_param = ''
		t.cur_fn_recv_is_ptr = false
	}
	ctx.old_fn_generic_params = t.cur_fn_generic_params
	ctx.old_local_fn_pointer_return_types = t.local_fn_pointer_return_types.move()
	ctx.old_local_receiver_generic_bindings = t.local_receiver_generic_bindings.move()
	ctx.old_generic_var_type_params = t.generic_var_type_params.move()
	t.cur_fn_generic_params = fn_generic_params
	t.local_fn_pointer_return_types = map[string]types.Type{}
	t.local_receiver_generic_bindings = map[string]map[string]types.Type{}
	t.seed_fn_pointer_param_return_types(decl.typ.params, fn_generic_params)
	if t.generic_var_type_params.len == 0 {
		t.generic_var_type_params = map[string]string{}
	}
	if t.cur_fn_generic_params.len > 0 {
		for param in decl.typ.params {
			if placeholder := t.generic_placeholder_from_type_expr(param.typ) {
				t.generic_var_type_params[param.name] = placeholder
			}
		}
	}
	ctx.old_smartcast_stack = t.smartcast_stack
	ctx.old_smartcast_expr_counts = t.smartcast_expr_counts.move()
	t.smartcast_stack = []SmartcastContext{cap: 4}
	t.smartcast_expr_counts = map[string]int{}
	ctx.has_return_type = decl.typ.return_type !is ast.EmptyExpr
	ctx.fn_return_type = t.get_fn_return_type(scope_fn_name) or {
		t.get_fn_return_type(fn_scope_key) or { types.Type(types.void_) }
	}
	return ctx
}

fn (mut t Transformer) restore_fn_body_transform_state(mut ctx FnBodyTransformCtx) {
	t.smartcast_stack = ctx.old_smartcast_stack
	t.smartcast_expr_counts = ctx.old_smartcast_expr_counts.move()
	t.cur_fn_generic_params = ctx.old_fn_generic_params
	t.local_fn_pointer_return_types = ctx.old_local_fn_pointer_return_types.move()
	t.local_receiver_generic_bindings = ctx.old_local_receiver_generic_bindings.move()
	t.generic_var_type_params = ctx.old_generic_var_type_params.move()
	t.cur_fn_name_str = ctx.old_fn_name_str
	t.cur_fn_recv_prefix = ctx.old_fn_recv_prefix
	t.cur_fn_recv_param = ctx.old_fn_recv_param
	t.cur_fn_recv_is_ptr = ctx.old_fn_recv_is_ptr
	t.cur_monomorphized_fn_bindings = ctx.old_monomorphized_bindings.move()
	t.cur_fn_ret_type_name = ctx.old_fn_ret_type_name
	t.cur_fn_return_sumtype_info = ctx.old_fn_return_sumtype_info
	t.cur_fn_returns_option = ctx.old_fn_returns_option
	t.cur_fn_returns_result = ctx.old_fn_returns_result
}

fn (mut t Transformer) finish_fn_body_transform(decl ast.FnDecl, mut ctx FnBodyTransformCtx) []ast.Attribute {
	if t.fn_root_scope != unsafe { nil } {
		t.cached_fn_scopes[ctx.fn_scope_key] = t.fn_root_scope
	}
	t.local_decl_types = ctx.old_local_decl_types.move()

	// Restore previous scope and fn_root_scope
	t.scope = ctx.old_scope
	t.fn_root_scope = ctx.old_fn_root_scope

	// For @[live] functions, force @[noinline] so the function gets its own
	// symbol in the binary (required for -hot-fn extraction).
	if ctx.live_fn_detected && !decl.attributes.has('noinline') {
		mut new_attrs := []ast.Attribute{cap: decl.attributes.len + 1}
		new_attrs << ast.Attribute{
			value: ast.Expr(ast.Ident{
				name: 'noinline'
			})
		}
		for a in decl.attributes {
			new_attrs << a
		}
		return new_attrs
	}

	return decl.attributes
}

fn (mut t Transformer) transform_fn_decl_parts(decl ast.FnDecl) ([]ast.Attribute, []ast.Stmt) {
	mut ctx := t.enter_fn_body_transform(decl) or { return decl.attributes, []ast.Stmt{} }
	transformed_stmts := t.transform_stmts(decl.stmts)
	t.restore_fn_body_transform_state(mut ctx)
	final_stmts := t.lower_defer_stmts(transformed_stmts, ctx.has_return_type, ctx.fn_return_type)
	attrs := t.finish_fn_body_transform(decl, mut ctx)
	return attrs, final_stmts
}

fn has_non_lifetime_generic_params(params []ast.Expr) bool {
	for param in params {
		if param !is ast.LifetimeExpr {
			return true
		}
	}
	return false
}

fn generic_param_names(params []ast.Expr) []string {
	mut names := []string{cap: params.len}
	for param in params {
		match param {
			ast.Ident {
				names << param.name
			}
			ast.LifetimeExpr {
				continue
			}
			ast.Type {
				name := param.name()
				if name != '' {
					names << name
				}
			}
			else {}
		}
	}
	return names
}

fn (t &Transformer) generic_call_lhs_from_index_expr(lhs ast.Expr) ?ast.Expr {
	if lhs !is ast.IndexExpr {
		return none
	}
	index_expr := lhs as ast.IndexExpr
	if index_expr.is_gated || !t.expr_looks_like_type_arg(index_expr.expr) {
		return none
	}
	if index_expr.lhs !is ast.Ident && index_expr.lhs !is ast.SelectorExpr {
		return none
	}
	return ast.Expr(ast.GenericArgOrIndexExpr{
		lhs:  index_expr.lhs
		expr: index_expr.expr
		pos:  index_expr.pos
	})
}

fn (t &Transformer) expr_looks_like_type_arg(expr ast.Expr) bool {
	match expr {
		ast.Ident {
			if expr.name in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'f32',
				'f64', 'bool', 'rune', 'byte', 'string', 'isize', 'usize', 'voidptr', 'charptr',
				'byteptr', 'T', 'U', 'V', 'K', 'W'] {
				return true
			}
			if expr.name.len > 0 && expr.name[0] >= `A` && expr.name[0] <= `Z` {
				return true
			}
			return t.lookup_type(expr.name) != none
		}
		ast.SelectorExpr {
			if expr.rhs.name.len > 0 && expr.rhs.name[0] >= `A` && expr.rhs.name[0] <= `Z` {
				return true
			}
			return t.lookup_type(expr.name().replace('.', '__')) != none
		}
		ast.PrefixExpr {
			return expr.op == .amp && t.expr_looks_like_type_arg(expr.expr)
		}
		ast.Type, ast.GenericArgs {
			return true
		}
		else {
			return false
		}
	}
}

fn (mut t Transformer) transform_call_expr(expr ast.CallExpr) ast.Expr {
	// Resolve $d('key', default) comptime define calls to their default value.
	// $d reads from compile-time environment; we just use the default.
	if expr.lhs is ast.Ident && expr.lhs.name == 'd' && expr.args.len == 2 {
		return t.transform_expr(expr.args[1])
	}
	if transformed_embed := t.transform_embed_file_chain_lhs(ast.Expr(expr), expr.pos) {
		return t.transform_expr(transformed_embed)
	}
	// Inline generic math functions (abs[T], min[T], max[T], maxof[T], minof[T]).
	// Generic function declarations are not instantiated by the compiler, so these
	// become unresolved symbols unless inlined here.
	if inlined := t.try_inline_generic_math_call(expr) {
		return inlined
	}
	// Expand .filter() / .map() calls to hoisted statements + temp variable
	if expanded := t.try_expand_filter_or_map_expr(expr) {
		return expanded
	}
	if generic_lhs := t.generic_call_lhs_from_index_expr(expr.lhs) {
		return t.transform_call_expr(ast.CallExpr{
			lhs:  generic_lhs
			args: expr.args
			pos:  expr.pos
		})
	}
	// Array literal lowering already builds:
	// builtin__new_array_from_c_array_noscan(len, cap, sizeof(T), [values...])
	// Re-transforming that 4th ArrayInitExpr argument causes nested lowering and
	// corrupt AST payloads in later cleanc/codegen stages.
	if expr.lhs is ast.Ident && expr.lhs.name == 'builtin__new_array_from_c_array_noscan'
		&& expr.args.len == 4 && expr.args[3] is ast.ArrayInitExpr {
		mut args := []ast.Expr{cap: expr.args.len}
		for i, arg in expr.args {
			if i == 3 {
				args << arg
			} else {
				args << t.transform_expr(arg)
			}
		}
		return ast.CallExpr{
			lhs:  ast.Expr(expr.lhs)
			args: args
			pos:  expr.pos
		}
	}

	// Some enum flag calls are lowered early to array__has/array__all.
	// Recover them here and rewrite back to bitwise flag checks.
	if expr.lhs is ast.Ident && expr.args.len == 2 {
		if expr.lhs.name in ['array__has', 'array__all'] {
			method_name := if expr.lhs.name == 'array__has' { 'has' } else { 'all' }
			receiver_type := t.get_enum_type(expr.args[0])
			mut should_rewrite := t.is_flag_enum_receiver(expr.args[0], receiver_type)
			if !should_rewrite {
				if recv_type := t.get_expr_type(expr.args[0]) {
					base := t.unwrap_alias_and_pointer_type(recv_type)
					if base is types.Enum {
						should_rewrite = base.is_flag
					} else if base !is types.Array && base !is types.ArrayFixed {
						// array__has/array__all are array-only helpers. If lowering produced
						// them for a non-array receiver, this is the enum-flag path.
						should_rewrite = true
					}
				}
			}
			if should_rewrite {
				return t.transform_flag_enum_method(expr.args[0], method_name, [expr.args[1]],
					receiver_type)
			}
		}
	}
	if expr.lhs is ast.Ident && expr.args.len == 2 {
		method_name := match expr.lhs.name {
			'array__contains' { 'contains' }
			'array__index' { 'index' }
			'array__last_index' { 'last_index' }
			else { '' }
		}

		if method_name != '' {
			if info := t.get_array_method_info(expr.args[0]) {
				fn_name := t.register_needed_array_method(info, method_name)
				mut value_arg := expr.args[1]
				if value_arg is ast.PrefixExpr {
					prefix_arg := value_arg as ast.PrefixExpr
					if prefix_arg.op == .amp {
						value_arg = prefix_arg.expr
					}
				}
				return ast.CallExpr{
					lhs:  ast.Ident{
						name: fn_name
					}
					args: [
						t.transform_array_receiver_expr(expr.args[0]),
						t.transform_expr(value_arg),
					]
					pos:  expr.pos
				}
			}
		}
	}
	// Check if this is a flag enum method call: receiver.has(arg) or receiver.all(arg)
	if expr.lhs is ast.SelectorExpr {
		sel := expr.lhs as ast.SelectorExpr
		if t.is_native_be {
			if concrete := t.get_native_default_interface_concrete_type(sel.lhs, sel.rhs.name) {
				call_args := t.lower_missing_call_args(expr.lhs, expr.args)
				mut native_args := []ast.Expr{cap: call_args.len + 1}
				native_args << t.transform_expr(sel.lhs)
				for arg in call_args {
					native_args << t.transform_expr(arg)
				}
				return ast.CallExpr{
					lhs:  ast.Ident{
						name: '${concrete}__${sel.rhs.name}'
					}
					args: native_args
					pos:  expr.pos
				}
			}
		}
		// Skip calls to conditionally compiled functions (e.g., @[if verbose ?])
		if sel.rhs.name in t.elided_fns {
			return ast.Expr(ast.BasicLiteral{
				kind:  .number
				value: '0'
			})
		}
		if receiver_type := t.resolve_expr_type(sel.lhs) {
			if is_embed_file_helper_type(receiver_type) {
				mut args := []ast.Expr{cap: expr.args.len}
				for arg in expr.args {
					args << t.transform_expr(arg)
				}
				return ast.CallExpr{
					lhs:  ast.Expr(ast.SelectorExpr{
						lhs: t.transform_expr(sel.lhs)
						rhs: sel.rhs
						pos: expr.pos
					})
					args: args
					pos:  expr.pos
				}
			}
		}
		// arr.sort() or arr.sort(a < b) - generate comparator and use sort_with_compare
		if sel.rhs.name in ['sort', 'sorted'] {
			if expr.args.len == 0 {
				// .sort() with no args: default ascending
				if result := t.transform_sort_call(sel.lhs, sel.rhs.name, [], expr.pos) {
					return result
				}
			} else if expr.args.len == 1 && t.is_sort_compare_lambda_expr(expr.args[0]) {
				// .sort(a < b) with lambda comparator
				if result := t.transform_sort_call(sel.lhs, sel.rhs.name, [expr.args[0]], expr.pos) {
					return result
				}
			}
		}
		method_name := sel.rhs.name
		if method_name == 'zero' && expr.args.len == 0 {
			receiver_type := t.get_enum_type(sel.lhs)
			if t.is_flag_enum(receiver_type) {
				return ast.CastExpr{
					typ:  ast.Ident{
						name: receiver_type
					}
					expr: ast.BasicLiteral{
						kind:  .number
						value: '0'
					}
					pos:  expr.pos
				}
			}
		}
		if method_name in ['has', 'all'] {
			if expr.args.len == 1 {
				arg0 := expr.args[0]
				is_string_arg := arg0 is ast.StringLiteral
					|| (arg0 is ast.BasicLiteral && arg0.kind == .string)
				if !is_string_arg {
					// Try to detect if receiver is a flag enum
					receiver_type := t.get_enum_type(sel.lhs)
					if t.is_flag_enum_receiver(sel.lhs, receiver_type) {
						// Transform the method call
						return t.transform_flag_enum_method(sel.lhs, method_name, expr.args,
							receiver_type)
					}
				}
			}
		}
		if method_name in ['contains', 'index', 'last_index'] && expr.args.len == 1
			&& t.specific_array_method_c_name(sel.lhs, method_name) == none {
			if info := t.get_array_method_info(sel.lhs) {
				fn_name := t.register_needed_array_method(info, method_name)
				return ast.CallExpr{
					lhs:  ast.Ident{
						name: fn_name
					}
					args: [
						t.transform_array_receiver_expr(sel.lhs),
						t.transform_expr(expr.args[0]),
					]
					pos:  expr.pos
				}
			}
		}
		// Transform direct .str() calls on arrays/maps to specialized function calls
		// e.g., a.str() where a is []int -> Array_int_str(a)
		if method_name == 'str' && expr.args.len == 0 {
			// Keep explicit user-defined/declared str() methods (e.g. strings.Builder).
			// Only lower to helper calls when there is no real method on the receiver type.
			mut should_lower_str := !t.receiver_has_cached_method(sel.lhs, method_name)
			if !should_lower_str {
				if recv_type := t.get_expr_type(sel.lhs) {
					base_type := t.unwrap_alias_and_pointer_type(recv_type)
					if base_type is types.Array || base_type is types.ArrayFixed
						|| base_type is types.Map {
						has_alias_str := if recv_type is types.Alias {
							t.resolve_alias_receiver_method_name(recv_type, method_name) != none
						} else {
							false
						}
						should_lower_str = !has_alias_str
					}
				}
			}
			if _ := t.specific_array_method_c_name(sel.lhs, method_name) {
				should_lower_str = false
			}
			if should_lower_str {
				str_fn_info := t.get_str_fn_info_for_expr(sel.lhs)
				// Skip Array_u8_str: []u8 = strings.Builder which has its own .str() method
				// with different semantics (finalizes builder vs formatting array contents).
				if str_fn_info.str_fn_name != '' && str_fn_info.str_fn_name != 'Array_u8_str' {
					t.needed_str_fns[str_fn_info.str_fn_name] = str_fn_info.elem_type
					// Also register enum types so the generator produces
					// the proper if-else variant chain instead of a struct stub.
					if typ := t.get_expr_type(sel.lhs) {
						if typ is types.Enum {
							t.needed_enum_str_fns[str_fn_info.str_fn_name] = typ
						}
					}
					return ast.CallExpr{
						lhs:  ast.Ident{
							name: str_fn_info.str_fn_name
						}
						args: [
							t.transform_expr(sel.lhs),
						]
						pos:  expr.pos
					}
				}
			}
		}
		// Sum type .type_name() - lower to match on _tag
		if method_name == 'type_name' && expr.args.len == 0 {
			if lowered := t.transform_sumtype_type_name(sel.lhs) {
				return lowered
			}
		}
		// Check for smart-casted method call: se.lhs.method() when se.lhs is smartcast to Type
		if t.has_active_smartcast() {
			if smartcast_receiver := t.smartcast_method_receiver_context(sel.lhs) {
				if resolved_fn := t.smartcast_variant_method_name(smartcast_receiver.ctx,
					sel.rhs.name)
				{
					casted_receiver := t.smartcast_method_receiver(smartcast_receiver.receiver,
						smartcast_receiver.ctx)
					mut args := []ast.Expr{cap: expr.args.len + 1}
					args << casted_receiver
					for arg in expr.args {
						args << t.transform_expr(arg)
					}
					return ast.CallExpr{
						lhs:  ast.Ident{
							name: resolved_fn
						}
						args: args
						pos:  expr.pos
					}
				}
			}
		}
		// Check for interface method call: iface.method(args...)
		if native_call := t.try_transform_native_interface_concrete_call(sel, expr.args, expr.pos,
			expr.lhs)
		{
			return native_call
		}
		if t.is_interface_receiver(sel.lhs) {
			call_args := t.lower_missing_call_args(expr.lhs, expr.args)
			iface_fn_info := t.lookup_call_fn_info(expr.lhs)
			mut transformed_iface_args := []ast.Expr{cap: call_args.len}
			for i, arg in call_args {
				transformed_iface_args << t.transform_call_arg_with_sumtype_check(arg,
					iface_fn_info, i)
			}
			transformed_iface_args = t.lower_variadic_args(expr.lhs, transformed_iface_args)
			// Native backends (arm64/x64): resolve to direct concrete method call.
			// `iface.method(args...)` → `ConcreteType__method(iface, args...)`
			if t.is_native_be {
				if concrete := t.get_interface_concrete_type_for_expr(sel.lhs) {
					resolved_method := '${concrete}__${sel.rhs.name}'
					mut native_args := []ast.Expr{cap: transformed_iface_args.len + 1}
					native_receiver := t.native_interface_receiver_arg(sel.lhs, concrete)
					native_args << t.transform_expr(native_receiver)
					native_args << transformed_iface_args
					return ast.CallExpr{
						lhs:  ast.Ident{
							name: resolved_method
						}
						args: native_args
						pos:  expr.pos
					}
				}
				if concrete := t.get_native_default_interface_concrete_type(sel.lhs, sel.rhs.name) {
					resolved_method := '${concrete}__${sel.rhs.name}'
					mut native_args := []ast.Expr{cap: transformed_iface_args.len + 1}
					native_args << t.transform_expr(sel.lhs)
					native_args << transformed_iface_args
					return ast.CallExpr{
						lhs:  ast.Ident{
							name: resolved_method
						}
						args: native_args
						pos:  expr.pos
					}
				}
			}
			// C/cleanc backends: Transform to vtable dispatch
			// Prepend iface._object to the args list
			mut new_args := []ast.Expr{cap: transformed_iface_args.len + 1}
			new_args << t.synth_selector(sel.lhs, '_object', types.Type(types.voidptr_))
			new_args << transformed_iface_args
			return ast.CallExpr{
				lhs:  ast.Expr(expr.lhs) // Keep the selector: iface.method
				args: new_args
				pos:  expr.pos
			}
		}
	}
	// Check for println/eprintln with non-string argument
	// Transform: println(arr) -> println(Array_int_str(arr))
	if expr.lhs is ast.Ident {
		fn_name := expr.lhs.name
		if fn_name in ['println', 'eprintln', 'print', 'eprint'] && expr.args.len == 1 {
			arg := expr.args[0]
			if arg is ast.StringInterLiteral {
				return ast.CallExpr{
					lhs:  ast.Expr(expr.lhs)
					args: [t.transform_expr(arg)]
					pos:  expr.pos
				}
			}
			if !t.is_string_expr(arg) {
				// Get the str function name and record it for generation
				str_fn_info := t.get_str_fn_info_for_expr(arg)
				if str_fn_info.str_fn_name != '' {
					t.needed_str_fns[str_fn_info.str_fn_name] = str_fn_info.elem_type
					if typ := t.get_expr_type(arg) {
						if typ is types.Enum {
							t.needed_enum_str_fns[str_fn_info.str_fn_name] = typ
						}
					}
					mut str_call_args := []ast.Expr{cap: 1}
					str_call_args << t.transform_expr(arg)
					// Transform to println(Type_str(arg))
					return ast.CallExpr{
						lhs:  ast.Expr(expr.lhs)
						args: [
							ast.Expr(ast.CallExpr{
								lhs:  ast.Ident{
									name: str_fn_info.str_fn_name
								}
								args: str_call_args
								pos:  expr.pos
							}),
						]
						pos:  expr.pos
					}
				}
			}
		}
	}
	if generic_module_call := t.transform_generic_module_call(expr) {
		return generic_module_call
	}
	if expr.lhs is ast.GenericArgs {
		ga := expr.lhs as ast.GenericArgs
		if ga.lhs is ast.Ident {
			lhs := t.specialize_generic_callable_expr(ga.lhs, ga.args, ga.pos)
			return ast.CallExpr{
				lhs:  lhs
				args: t.transform_call_args_for_lhs(lhs, expr.args)
				pos:  expr.pos
			}
		}
	}
	if expr.lhs is ast.GenericArgOrIndexExpr {
		gai := expr.lhs as ast.GenericArgOrIndexExpr
		if gai.lhs is ast.Ident {
			lhs := t.specialize_generic_callable_expr(gai.lhs, [gai.expr], gai.pos)
			return ast.CallExpr{
				lhs:  lhs
				args: t.transform_call_args_for_lhs(lhs, expr.args)
				pos:  expr.pos
			}
		}
	}
	if expr.lhs is ast.GenericArgs {
		ga := expr.lhs as ast.GenericArgs
		if ga.lhs is ast.SelectorExpr {
			if transformed := t.transform_generic_selector_method_call(ga.lhs as ast.SelectorExpr,
				ga.args, expr.args, expr.pos)
			{
				return transformed
			}
		}
	}
	if expr.lhs is ast.IndexExpr {
		idx := expr.lhs as ast.IndexExpr
		if idx.lhs is ast.SelectorExpr {
			if transformed := t.transform_generic_selector_method_call(idx.lhs as ast.SelectorExpr, [
				idx.expr,
			], expr.args, expr.pos)
			{
				return transformed
			}
		}
	}
	// Method call resolution: rewrite receiver.method(args) -> Type__method(receiver, args)
	if expr.lhs is ast.SelectorExpr {
		sel := expr.lhs as ast.SelectorExpr
		if resolved_static := t.resolve_static_type_method_call(sel.lhs, sel.rhs.name) {
			call_args := t.lower_missing_call_args(expr.lhs, expr.args)
			fn_info := t.generic_aware_call_fn_info(expr.lhs, resolved_static)
			mut args := []ast.Expr{cap: call_args.len}
			for i, arg in call_args {
				args << t.transform_call_arg_with_sumtype_check(arg, fn_info, i)
			}
			args = t.lower_variadic_args(expr.lhs, args)
			mut call_name := resolved_static
			if info := fn_info {
				if inferred := t.inferred_generic_call_name(call_name, info, call_args) {
					call_name = inferred
				}
			}
			return ast.CallExpr{
				lhs:  ast.Ident{
					name: call_name
				}
				args: args
				pos:  expr.pos
			}
		}
		// Nested module call: rand.seed.time_seed_array() -> seed__time_seed_array()
		if sel.lhs is ast.SelectorExpr {
			inner := sel.lhs as ast.SelectorExpr
			if inner.lhs is ast.Ident && t.is_module_ident(inner.lhs.name) {
				sub_mod := inner.rhs.name
				fn_name := sel.rhs.name
				// Check if sub_mod is actually a module scope (not a variable like os.args)
				mut resolved_name := ''
				if t.get_module_scope(sub_mod) != none {
					resolved_name = '${sub_mod}__${fn_name}'
				} else {
					full_mod := '${inner.lhs.name}__${sub_mod}'
					if t.get_module_scope(full_mod) != none {
						resolved_name = '${full_mod}__${fn_name}'
					}
				}
				if resolved_name != '' {
					args := t.transform_call_args_for_lhs(ast.Expr(ast.Ident{
						name: resolved_name
					}), expr.args)
					return ast.CallExpr{
						lhs:  ast.Ident{
							name: resolved_name
						}
						args: args
						pos:  expr.pos
					}
				}
			}
		}
		mut resolved_module_call_name := ''
		if sel.lhs is ast.Ident {
			lhs_name := (sel.lhs as ast.Ident).name
			if t.lookup_var_type(lhs_name) == none {
				if resolved_mod := t.resolve_module_name(lhs_name) {
					mut module_names := []string{cap: 2}
					module_names << resolved_mod
					if lhs_name != resolved_mod {
						module_names << lhs_name
					}
					for mod_name in module_names {
						if _ := t.lookup_fn_cached(mod_name, sel.rhs.name) {
							call_mod := if mod_name.contains('.') {
								mod_name.all_after_last('.')
							} else if mod_name.contains('__') {
								mod_name.all_after_last('__')
							} else {
								mod_name
							}
							resolved_module_call_name = '${call_mod}__${sel.rhs.name}'
							break
						}
					}
				}
				if resolved_module_call_name == '' {
					if call_prefix := t.resolve_module_call_prefix(lhs_name) {
						if _ := t.lookup_fn_cached(call_prefix, sel.rhs.name) {
							resolved_module_call_name = '${call_prefix}__${sel.rhs.name}'
						}
					}
				}
			}
		}
		is_module_call := resolved_module_call_name != ''
		if is_module_call {
			call_args := t.lower_missing_call_args(expr.lhs, expr.args)
			fn_info := t.generic_aware_call_fn_info(expr.lhs, resolved_module_call_name)
			mut args := []ast.Expr{cap: call_args.len}
			for i, arg in call_args {
				args << t.transform_call_arg_with_sumtype_check(arg, fn_info, i)
			}
			args = t.lower_variadic_args(expr.lhs, args)
			mut call_name := resolved_module_call_name
			if info := fn_info {
				if inferred := t.inferred_generic_call_name(call_name, info, call_args) {
					call_name = inferred
				}
			}
			return ast.CallExpr{
				lhs:  ast.Ident{
					name: call_name
				}
				args: args
				pos:  expr.pos
			}
		}
		if embedded_call := t.transform_promoted_embedded_method_call(sel, expr.args, expr.pos) {
			return embedded_call
		}
		if !is_module_call && !t.resolves_to_embedded_method(sel.lhs, sel.rhs.name) {
			if resolved := t.resolve_method_call_name(sel.lhs, sel.rhs.name) {
				// Guard against misresolution: if the receiver is known to be a string
				// (e.g., tos2() returns string), ensure string methods aren't resolved to
				// array methods. This can happen when the checker's type store is unreliable
				// (e.g., in ARM64-compiled binaries with chained-access issues).
				if resolved.starts_with('array__') && t.is_string_expr(sel.lhs) {
					call_args := t.lower_missing_call_args(expr.lhs, expr.args)
					mut args := []ast.Expr{cap: call_args.len + 1}
					args << t.transform_expr(sel.lhs)
					for arg in call_args {
						args << t.transform_expr(arg)
					}
					return ast.CallExpr{
						lhs:  ast.Ident{
							name: 'string__${sel.rhs.name}'
						}
						args: args
						pos:  expr.pos
					}
				}
				// For nested array .clone(), use clone_to_depth with the correct depth
				// so inner arrays are deeply cloned instead of shallow-copied.
				if resolved == 'array__clone' && expr.args.len == 0 {
					if recv_type := t.get_expr_type(sel.lhs) {
						depth := t.get_array_nesting_depth(recv_type)
						if depth > 1 {
							return ast.CallExpr{
								lhs:  ast.Ident{
									name: 'array__clone_to_depth'
								}
								args: [
									t.transform_expr(sel.lhs),
									ast.Expr(ast.BasicLiteral{
										kind:  .number
										value: '${depth - 1}'
									}),
								]
								pos:  expr.pos
							}
						}
					}
				}
				if sel.rhs.name == 'clone' && expr.args.len == 0 {
					if recv_type := t.get_expr_type(sel.lhs) {
						_ = t.auto_clone_fn_name_for_type(recv_type)
					}
				}
				// insert(i, arr) → insert_many(i, arr.data, arr.len)
				if resolved.ends_with('__insert') && expr.args.len == 2 {
					if arg_type := t.get_expr_type(expr.args[1]) {
						arg_base := t.unwrap_alias_and_pointer_type(arg_type)
						if arg_base is types.Array {
							arr_arg := t.transform_expr(expr.args[1])
							return ast.CallExpr{
								lhs:  ast.Ident{
									name: resolved.replace('__insert', '__insert_many')
								}
								args: [
									t.transform_expr(sel.lhs),
									t.transform_expr(expr.args[0]),
									ast.Expr(ast.SelectorExpr{
										lhs: arr_arg
										rhs: ast.Ident{
											name: 'data'
										}
									}),
									ast.Expr(ast.SelectorExpr{
										lhs: arr_arg
										rhs: ast.Ident{
											name: 'len'
										}
									}),
								]
								pos:  expr.pos
							}
						}
					}
				}
				// prepend(arr) → prepend_many(arr.data, arr.len)
				if resolved.ends_with('__prepend') && expr.args.len == 1 {
					if arg_type := t.get_expr_type(expr.args[0]) {
						arg_base := t.unwrap_alias_and_pointer_type(arg_type)
						if arg_base is types.Array {
							arr_arg := t.transform_expr(expr.args[0])
							return ast.CallExpr{
								lhs:  ast.Ident{
									name: resolved.replace('__prepend', '__prepend_many')
								}
								args: [
									t.transform_expr(sel.lhs),
									ast.Expr(ast.SelectorExpr{
										lhs: arr_arg
										rhs: ast.Ident{
											name: 'data'
										}
									}),
									ast.Expr(ast.SelectorExpr{
										lhs: arr_arg
										rhs: ast.Ident{
											name: 'len'
										}
									}),
								]
								pos:  expr.pos
							}
						}
					}
				}
				call_args := t.lower_missing_call_args(expr.lhs, expr.args)
				is_static := t.is_static_method_call(sel.lhs)
				fn_info := t.generic_aware_call_fn_info(expr.lhs, resolved)
				mut transformed_call_args := []ast.Expr{cap: call_args.len}
				for i, arg in call_args {
					transformed_call_args << t.transform_call_arg_with_sumtype_check(arg, fn_info, i)
				}
				transformed_call_args = t.lower_variadic_args(expr.lhs, transformed_call_args)
				mut args := []ast.Expr{cap: transformed_call_args.len + 1}
				if !is_static {
					args << t.transform_method_receiver_arg(sel.lhs, sel.rhs.name, resolved)
				}
				args << transformed_call_args
				mut call_name := resolved
				if info := fn_info {
					if receiver_inferred := t.receiver_generic_method_call_name(call_name, sel.lhs,
						info, call_args)
					{
						call_name = receiver_inferred
					} else if inferred := t.inferred_generic_call_name(call_name, info, call_args) {
						call_name = inferred
					}
				} else if receiver_inferred := t.receiver_generic_method_call_name(call_name,
					sel.lhs, CallFnInfo{}, call_args)
				{
					call_name = receiver_inferred
				}
				if call_name == resolved && !is_static {
					if full_info := t.generic_call_info_for_decl(resolved) {
						mut full_call_args := []ast.Expr{cap: call_args.len + 1}
						full_call_args << sel.lhs
						for arg in call_args {
							full_call_args << arg
						}
						if inferred := t.inferred_generic_call_name(resolved, full_info,
							full_call_args)
						{
							call_name = inferred
						}
						if full_bindings := t.generic_bindings_from_call_args(full_info,
							full_call_args)
						{
							t.register_generic_bindings(resolved, full_bindings)
						}
					}
				}
				return ast.CallExpr{
					lhs:  ast.Ident{
						name: call_name
					}
					args: args
					pos:  expr.pos
				}
			}
		}
	}
	// Generic method call: w.get[T](args) where LHS is GenericArgOrIndexExpr
	// wrapping a SelectorExpr. Resolve the method call and append the generic
	// specialization suffix so cleanc can later substitute concrete types.
	if expr.lhs is ast.GenericArgOrIndexExpr {
		gai := expr.lhs as ast.GenericArgOrIndexExpr
		if gai.lhs is ast.SelectorExpr {
			sel := gai.lhs as ast.SelectorExpr
			is_module_call := sel.lhs is ast.Ident && t.lookup_var_type(sel.lhs.name) == none
				&& (t.is_module_ident(sel.lhs.name) || t.get_module_scope(sel.lhs.name) != none)
			if !is_module_call {
				// Compute generic specialization suffix from the type arg
				suffix := '_T_' + t.generic_specialization_token(gai.expr)
				// When receiver matches the current method's receiver parameter
				// and get_expr_type would fail (generic body), use the known prefix
				recv_is_self := t.cur_fn_recv_param != '' && sel.lhs is ast.Ident
					&& (sel.lhs as ast.Ident).name == t.cur_fn_recv_param && t.get_expr_type(sel.lhs) == none
				if recv_is_self && t.cur_fn_recv_prefix != '' {
					call_args2 := t.lower_missing_call_args(expr.lhs, expr.args)
					fn_info2 := t.lookup_call_fn_info(expr.lhs)
					mut args2 := []ast.Expr{cap: call_args2.len + 1}
					args2 << t.transform_expr(sel.lhs)
					for i, arg in call_args2 {
						args2 << t.transform_call_arg_with_sumtype_check(arg, fn_info2, i)
					}
					return ast.CallExpr{
						lhs:  ast.Ident{
							name: '${t.cur_fn_recv_prefix}__${sel.rhs.name}${suffix}'
						}
						args: args2
						pos:  expr.pos
					}
				}
				method_name := sel.rhs.name + suffix
				if !t.resolves_to_embedded_method(sel.lhs, method_name) {
					if resolved := t.resolve_method_call_name(sel.lhs, method_name) {
						call_args2 := t.lower_missing_call_args(expr.lhs, expr.args)
						fn_info2 := t.lookup_call_fn_info(expr.lhs)
						mut args2 := []ast.Expr{cap: call_args2.len + 1}
						args2 << t.transform_expr(sel.lhs)
						for i, arg in call_args2 {
							args2 << t.transform_call_arg_with_sumtype_check(arg, fn_info2, i)
						}
						return ast.CallExpr{
							lhs:  ast.Ident{
								name: resolved
							}
							args: args2
							pos:  expr.pos
						}
					}
				}
				// Fallback: resolve without suffix and append suffix to the resolved name
				if !t.resolves_to_embedded_method(sel.lhs, sel.rhs.name) {
					if resolved := t.resolve_method_call_name(sel.lhs, sel.rhs.name) {
						call_args2 := t.lower_missing_call_args(expr.lhs, expr.args)
						fn_info2 := t.lookup_call_fn_info(expr.lhs)
						mut args2 := []ast.Expr{cap: call_args2.len + 1}
						args2 << t.transform_expr(sel.lhs)
						for i, arg in call_args2 {
							args2 << t.transform_call_arg_with_sumtype_check(arg, fn_info2, i)
						}
						return ast.CallExpr{
							lhs:  ast.Ident{
								name: resolved + suffix
							}
							args: args2
							pos:  expr.pos
						}
					}
				}
			}
		}
	}
	// Default: transform arguments.
	// This is important for smart cast propagation through method chains
	// e.g., stmt.name.replace() when stmt is smartcast
	call_args := t.lower_missing_call_args(expr.lhs, expr.args)
	// Look up function parameter types for sumtype re-wrapping
	fn_info := t.call_fn_info_for_lhs(expr.lhs)
	mut args := []ast.Expr{cap: call_args.len}
	for i, arg in call_args {
		// When an argument has an active smartcast but the function parameter
		// expects the original sumtype, temporarily disable the smartcast so the
		// original sumtype value is passed through without being unwrapped.
		args << t.transform_call_arg_with_sumtype_check(arg, fn_info, i)
	}
	args = t.lower_variadic_args(expr.lhs, args)
	mut call_lhs := t.transform_expr(expr.lhs)
	if expr.lhs is ast.Ident {
		if info := fn_info {
			if inferred := t.inferred_generic_call_name(expr.lhs.name, info, call_args) {
				t.register_generic_call_return_type(expr.lhs.name, info, call_args, expr.pos)
				call_lhs = ast.Expr(ast.Ident{
					name: inferred
					pos:  expr.lhs.pos
				})
			}
		}
	}
	return ast.CallExpr{
		// The fallback path keeps unresolved calls in call form, but the lhs can
		// still contain value expressions such as `buf[..n].bytestr`; lower those
		// receivers here so backend codegen never sees raw slice syntax.
		lhs:  call_lhs
		args: args
		pos:  expr.pos
	}
}

fn (mut t Transformer) transform_call_args_for_lhs(lhs ast.Expr, raw_args []ast.Expr) []ast.Expr {
	call_args := t.lower_missing_call_args(lhs, raw_args)
	fn_info := t.call_fn_info_for_lhs(lhs)
	mut args := []ast.Expr{cap: call_args.len}
	for i, arg in call_args {
		args << t.transform_call_arg_with_sumtype_check(arg, fn_info, i)
	}
	return t.lower_variadic_args(lhs, args)
}

struct CallFnInfo {
mut:
	param_types                    []types.Type
	param_names                    []string
	generic_param_names_by_param   []string
	generic_param_indexes_by_param []int
	generic_params                 []string
	is_variadic                    bool
}

fn (t &Transformer) call_fn_info_for_lhs(lhs ast.Expr) ?CallFnInfo {
	if lhs is ast.Ident && t.has_current_module_concrete_fn(lhs.name) {
		return t.lookup_call_fn_info(lhs)
	}
	if base_name := t.generic_call_base_name(lhs) {
		return t.generic_aware_call_fn_info(lhs, base_name)
	}
	return t.lookup_call_fn_info(lhs)
}

fn (t &Transformer) generic_aware_call_fn_info(lhs ast.Expr, base_name string) ?CallFnInfo {
	mut info := t.lookup_call_fn_info(lhs) or {
		decl_info := t.generic_call_info_for_decl(base_name) or { return none }
		return t.drop_method_receiver_from_call_info(lhs, decl_info)
	}
	if decl_info := t.generic_call_info_for_decl(base_name) {
		decl_call_info := t.drop_method_receiver_from_call_info(lhs, decl_info)
		needs_decl_generic_signature := info.generic_params.len == 0
			&& decl_call_info.generic_params.len > 0
		if info.generic_params.len == 0 {
			info.generic_params = decl_call_info.generic_params.clone()
		}
		if info.generic_param_names_by_param.len == 0 {
			info.generic_param_names_by_param = decl_call_info.generic_param_names_by_param.clone()
		}
		if info.generic_param_indexes_by_param.len == 0 {
			info.generic_param_indexes_by_param =
				decl_call_info.generic_param_indexes_by_param.clone()
		}
		if info.param_names.len == 0 {
			info.param_names = decl_call_info.param_names.clone()
		}
		if info.param_types.len == 0 || needs_decl_generic_signature {
			info.param_types = decl_call_info.param_types.clone()
		}
		if !info.is_variadic {
			info.is_variadic = decl_call_info.is_variadic
		}
	}
	return info
}

fn (t &Transformer) generic_call_info_for_decl(base_name string) ?CallFnInfo {
	decl := t.generic_fn_decl_for_call_info(base_name) or { return none }
	generic_params := decl_generic_param_names(decl)
	if generic_params.len == 0 {
		return none
	}
	mut param_types := []types.Type{}
	mut param_names := []string{}
	mut generic_param_names_by_param := []string{}
	mut generic_param_indexes_by_param := []int{}
	if decl.is_method {
		generic_param_names_by_param << direct_generic_param_name_from_expr(decl.receiver.typ,
			generic_params)
		generic_param_indexes_by_param << direct_generic_param_index_from_expr(decl.receiver.typ,
			generic_params)
		if recv_type := t.type_from_param_type_expr(decl.receiver.typ, generic_params) {
			param_types << recv_type
			param_names << decl.receiver.name
		}
	}
	for param in decl.typ.params {
		generic_param_names_by_param << direct_generic_param_name_from_expr(param.typ,
			generic_params)
		generic_param_indexes_by_param << direct_generic_param_index_from_expr(param.typ,
			generic_params)
		if typ := t.type_from_param_type_expr(param.typ, generic_params) {
			param_types << typ
			param_names << param.name
		}
	}
	return CallFnInfo{
		param_types:                    param_types
		param_names:                    param_names
		generic_param_names_by_param:   generic_param_names_by_param
		generic_param_indexes_by_param: generic_param_indexes_by_param
		generic_params:                 generic_params
		is_variadic:                    ast_params_are_variadic(decl.typ.params)
	}
}

fn (t &Transformer) generic_fn_decl_for_call_info(base_name string) ?ast.FnDecl {
	mut name := base_name
	bracket_pos := name.index_u8(`[`)
	if bracket_pos > 0 {
		name = name[..bracket_pos]
	}
	if name.contains('_T_') {
		name = name.all_before('_T_')
	} else if name.ends_with('_T') {
		name = name[..name.len - 2]
	}
	lookup_key := t.resolve_generic_decl_key_for_call(name, t.generic_fn_decl_index) or {
		return none
	}
	return t.generic_fn_decl_index[lookup_key] or { none }
}

fn ast_params_are_variadic(params []ast.Parameter) bool {
	if params.len == 0 {
		return false
	}
	last_param := params[params.len - 1]
	if last_param.typ is ast.PrefixExpr {
		return last_param.typ.op == .ellipsis
	}
	return false
}

fn direct_generic_param_name_from_expr(expr ast.Expr, generic_params []string) string {
	match expr {
		ast.Ident {
			if expr.name in generic_params {
				return expr.name
			}
		}
		ast.ModifierExpr {
			return direct_generic_param_name_from_expr(expr.expr, generic_params)
		}
		else {}
	}

	return ''
}

fn direct_generic_param_index_from_expr(expr ast.Expr, generic_params []string) int {
	name := direct_generic_param_name_from_expr(expr, generic_params)
	if name == '' {
		return -1
	}
	for i, generic_name in generic_params {
		if generic_name == name {
			return i
		}
	}
	return -1
}

fn call_fn_info_from_fn_type(fn_type types.FnType) CallFnInfo {
	return CallFnInfo{
		param_types:    fn_type.get_param_types()
		param_names:    fn_type.get_param_names()
		generic_params: fn_type.get_generic_params().filter(!it.starts_with('^'))
		is_variadic:    fn_type.is_variadic_fn()
	}
}

fn generic_infer_unwrap_alias(t &Transformer, typ types.Type) types.Type {
	if !types.type_has_valid_payload(typ) {
		return typ
	}
	match typ {
		types.Alias {
			base := t.live_alias_base_type(typ) or { return typ }
			return generic_infer_unwrap_alias(t, base)
		}
		else {
			return typ
		}
	}
}

fn (t &Transformer) infer_generic_type_from_call_arg(param_type types.Type, arg_type types.Type, generic_params []string, mut bindings map[string]types.Type) {
	t.infer_generic_type_from_call_arg_with_depth(param_type, arg_type, generic_params, mut
		bindings, 0)
}

fn (t &Transformer) infer_generic_type_from_call_arg_with_depth(param_type types.Type, arg_type types.Type, generic_params []string, mut bindings map[string]types.Type, depth int) {
	if !types.type_has_valid_payload(param_type) || !types.type_has_valid_payload(arg_type) {
		return
	}
	if depth > 32 {
		return
	}
	match param_type {
		types.Alias {
			t.infer_generic_type_from_call_arg_with_depth(param_type.base_type, arg_type,
				generic_params, mut bindings, depth + 1)
		}
		types.Array {
			arg_base := generic_infer_unwrap_alias(t, arg_type)
			if arg_base is types.Array {
				t.infer_generic_type_from_call_arg_with_depth(param_type.elem_type,
					arg_base.elem_type, generic_params, mut bindings, depth + 1)
			}
		}
		types.ArrayFixed {
			arg_base := generic_infer_unwrap_alias(t, arg_type)
			if arg_base is types.ArrayFixed {
				t.infer_generic_type_from_call_arg_with_depth(param_type.elem_type,
					arg_base.elem_type, generic_params, mut bindings, depth + 1)
			} else if arg_base is types.Array {
				t.infer_generic_type_from_call_arg_with_depth(param_type.elem_type,
					arg_base.elem_type, generic_params, mut bindings, depth + 1)
			}
		}
		types.FnType {
			arg_base := generic_infer_unwrap_alias(t, arg_type)
			if arg_base is types.FnType {
				param_types := param_type.get_param_types()
				arg_types := arg_base.get_param_types()
				max_params := if param_types.len < arg_types.len {
					param_types.len
				} else {
					arg_types.len
				}
				for i in 0 .. max_params {
					t.infer_generic_type_from_call_arg_with_depth(param_types[i], arg_types[i],
						generic_params, mut bindings, depth + 1)
				}
				if param_ret := param_type.get_return_type() {
					if arg_ret := arg_base.get_return_type() {
						t.infer_generic_type_from_call_arg_with_depth(param_ret, arg_ret,
							generic_params, mut bindings, depth + 1)
					}
				}
			}
		}
		types.Map {
			arg_base := generic_infer_unwrap_alias(t, arg_type)
			if arg_base is types.Map {
				t.infer_generic_type_from_call_arg_with_depth(param_type.key_type,
					arg_base.key_type, generic_params, mut bindings, depth + 1)
				t.infer_generic_type_from_call_arg_with_depth(param_type.value_type,
					arg_base.value_type, generic_params, mut bindings, depth + 1)
			}
		}
		types.NamedType {
			name := string(param_type)
			if name in generic_params && name !in bindings {
				bindings[name] = normalize_generic_concrete_type(arg_type)
			}
		}
		types.OptionType {
			arg_base := generic_infer_unwrap_alias(t, arg_type)
			if arg_base is types.OptionType {
				t.infer_generic_type_from_call_arg_with_depth(param_type.base_type,
					arg_base.base_type, generic_params, mut bindings, depth + 1)
			} else {
				t.infer_generic_type_from_call_arg_with_depth(param_type.base_type, arg_type,
					generic_params, mut bindings, depth + 1)
			}
		}
		types.Pointer {
			arg_base := generic_infer_unwrap_alias(t, arg_type)
			if arg_base is types.Pointer {
				t.infer_generic_type_from_call_arg_with_depth(param_type.base_type,
					arg_base.base_type, generic_params, mut bindings, depth + 1)
			} else {
				t.infer_generic_type_from_call_arg_with_depth(param_type.base_type, arg_type,
					generic_params, mut bindings, depth + 1)
			}
		}
		types.ResultType {
			arg_base := generic_infer_unwrap_alias(t, arg_type)
			if arg_base is types.ResultType {
				t.infer_generic_type_from_call_arg_with_depth(param_type.base_type,
					arg_base.base_type, generic_params, mut bindings, depth + 1)
			} else {
				t.infer_generic_type_from_call_arg_with_depth(param_type.base_type, arg_type,
					generic_params, mut bindings, depth + 1)
			}
		}
		types.Struct {
			arg_base := generic_infer_unwrap_alias(t, arg_type)
			if arg_base is types.Struct {
				for param_field in param_type.fields {
					arg_field := struct_field_by_name(arg_base, param_field.name) or { continue }
					t.infer_generic_type_from_call_arg_with_depth(param_field.typ, arg_field.typ,
						generic_params, mut bindings, depth + 1)
				}
			}
		}
		types.Tuple {
			arg_base := generic_infer_unwrap_alias(t, arg_type)
			if arg_base is types.Tuple {
				param_types := param_type.get_types()
				arg_types := arg_base.get_types()
				max_types := if param_types.len < arg_types.len {
					param_types.len
				} else {
					arg_types.len
				}
				for i in 0 .. max_types {
					t.infer_generic_type_from_call_arg_with_depth(param_types[i], arg_types[i],
						generic_params, mut bindings, depth + 1)
				}
			}
		}
		else {}
	}
}

fn (t &Transformer) call_arg_type_for_generic_infer(arg ast.Expr) ?types.Type {
	base_arg := if arg is ast.ModifierExpr {
		arg.expr
	} else {
		arg
	}
	if base_arg is ast.Ident {
		if ctx := t.find_smartcast_for_expr(base_arg.name) {
			variant_type_name := if ctx.variant_full != '' { ctx.variant_full } else { ctx.variant }
			if typ := t.c_name_to_type(variant_type_name) {
				return typ
			}
		}
		if typ := t.lookup_local_decl_type(base_arg.name) {
			return typ
		}
		if typ := t.lookup_var_type(base_arg.name) {
			return typ
		}
	}
	if base_arg is ast.BasicLiteral {
		match base_arg.kind {
			.number {
				if base_arg.value.contains('.') {
					return types.Type(types.f64_)
				}
				return types.Type(types.int_)
			}
			.string {
				return types.Type(types.string_)
			}
			.key_true, .key_false {
				return types.Type(types.bool_)
			}
			else {}
		}
	}
	return t.get_expr_type(base_arg)
}

fn (mut t Transformer) inline_generic_math_result_pos(pos token.Pos, arg ast.Expr) token.Pos {
	result_pos := if pos.id != 0 { pos } else { t.next_synth_pos() }
	if typ := t.call_arg_type_for_generic_infer(arg) {
		t.register_synth_type(result_pos, concrete_literal_array_elem_type(typ))
	}
	return result_pos
}

fn (mut t Transformer) make_inline_abs_expr(arg ast.Expr, pos token.Pos) ast.Expr {
	abs_cond := t.make_infix_expr(.lt, arg, t.make_number_expr('0'))
	return ast.Expr(ast.IfExpr{
		cond:      abs_cond
		stmts:     [
			ast.Stmt(ast.ExprStmt{
				expr: ast.PrefixExpr{
					op:   .minus
					expr: arg
				}
			}),
		]
		else_expr: ast.IfExpr{
			stmts: [
				ast.Stmt(ast.ExprStmt{
					expr: arg
				}),
			]
			pos:   pos
		}
		pos:       pos
	})
}

fn (t &Transformer) inferred_generic_call_name(base_name string, info CallFnInfo, call_args []ast.Expr) ?string {
	if base_name == '' || base_name.contains('_T_') || info.generic_params.len == 0 {
		return none
	}
	infer_info := t.generic_inference_info_for_call(base_name, info, call_args.len)
	bindings := t.generic_bindings_from_call_args(infer_info, call_args) or { return none }
	mut parts := []string{cap: infer_info.generic_params.len}
	for param_name in infer_info.generic_params {
		concrete := bindings[param_name] or { return none }
		parts << t.generic_specialization_token_from_type(concrete)
	}
	if parts.len == 0 {
		return none
	}
	base := if base_name.contains('_T_') {
		base_name.all_before('_T_')
	} else if base_name.ends_with('_T') {
		base_name[..base_name.len - 2]
	} else {
		base_name
	}
	return '${base}_T_${parts.join('_')}'
}

fn (t &Transformer) generic_inference_info_for_call(base_name string, info CallFnInfo, call_arg_count int) CallFnInfo {
	decl_info := t.generic_call_info_for_decl(base_name) or { return info }
	if decl_info.generic_params.len == 0 || decl_info.param_types.len != call_arg_count {
		return info
	}
	return decl_info
}

fn (mut t Transformer) register_generic_call_return_type(base_name string, info CallFnInfo, call_args []ast.Expr, pos token.Pos) {
	if !pos.is_valid() || base_name == '' {
		return
	}
	decl := t.generic_fn_decl_for_call_info(base_name) or { return }
	generic_params := decl_generic_param_names(decl)
	if generic_params.len == 0 || decl.typ.return_type is ast.EmptyExpr {
		return
	}
	infer_info := t.generic_inference_info_for_call(base_name, info, call_args.len)
	bindings := t.generic_bindings_from_call_args(infer_info, call_args) or { return }
	ret_type := t.type_from_param_type_expr(decl.typ.return_type, generic_params) or { return }
	t.register_synth_type(pos, substitute_type(ret_type, bindings))
}

fn (t &Transformer) generic_bindings_from_call_args(info CallFnInfo, call_args []ast.Expr) ?map[string]types.Type {
	if info.generic_params.len == 0 {
		return none
	}
	mut bindings := map[string]types.Type{}
	for i, arg in call_args {
		param_idx := i
		if param_idx >= info.generic_param_indexes_by_param.len {
			break
		}
		generic_idx := info.generic_param_indexes_by_param[param_idx]
		if generic_idx < 0 || generic_idx >= info.generic_params.len {
			continue
		}
		generic_name := info.generic_params[generic_idx]
		if generic_name in bindings {
			continue
		}
		if !t.call_info_param_is_direct_generic(info, param_idx, generic_name) {
			continue
		}
		arg_type := t.call_arg_type_for_generic_infer(arg) or { continue }
		bindings[generic_name] = arg_type
	}
	t.infer_generic_bindings_from_call_args(info.param_types, call_args, info.generic_params, 0, mut
		bindings)
	if bindings.len != info.generic_params.len && info.param_types.len > call_args.len {
		mut tail_bindings := map[string]types.Type{}
		param_offset := info.param_types.len - call_args.len
		for i, arg in call_args {
			param_idx := i + param_offset
			if param_idx >= info.generic_param_indexes_by_param.len {
				break
			}
			generic_idx := info.generic_param_indexes_by_param[param_idx]
			if generic_idx < 0 || generic_idx >= info.generic_params.len {
				continue
			}
			generic_name := info.generic_params[generic_idx]
			if generic_name in tail_bindings {
				continue
			}
			if !t.call_info_param_is_direct_generic(info, param_idx, generic_name) {
				continue
			}
			arg_type := t.call_arg_type_for_generic_infer(arg) or { continue }
			tail_bindings[generic_name] = arg_type
		}
		t.infer_generic_bindings_from_call_args(info.param_types, call_args, info.generic_params,
			param_offset, mut tail_bindings)
		if tail_bindings.len > bindings.len {
			bindings = tail_bindings.move()
		}
	}
	for param_name in info.generic_params {
		if param_name in bindings {
			continue
		}
		if concrete := t.cur_monomorphized_fn_bindings[param_name] {
			bindings[param_name] = concrete
		}
	}
	if bindings.len != info.generic_params.len {
		return none
	}
	return bindings
}

fn (t &Transformer) call_info_param_is_direct_generic(info CallFnInfo, param_idx int, generic_name string) bool {
	if param_idx < 0 || generic_name == '' {
		return false
	}
	if param_idx >= info.param_types.len {
		return true
	}
	param_type := generic_infer_unwrap_alias(t, info.param_types[param_idx])
	if param_type is types.Void {
		return true
	}
	if param_type is types.NamedType {
		return string(param_type) == generic_name
	}
	return false
}

fn (t &Transformer) infer_generic_bindings_from_call_args(param_types []types.Type, call_args []ast.Expr, generic_params []string, param_offset int, mut bindings map[string]types.Type) {
	if param_offset < 0 {
		return
	}
	for i, arg in call_args {
		param_idx := i + param_offset
		if param_idx >= param_types.len {
			break
		}
		arg_type := t.call_arg_type_for_generic_infer(arg) or { continue }
		t.infer_generic_type_from_call_arg(param_types[param_idx], arg_type, generic_params, mut
			bindings)
	}
}

fn (t &Transformer) drop_method_receiver_from_call_info(lhs ast.Expr, info CallFnInfo) CallFnInfo {
	mut trimmed := info
	if lhs !is ast.SelectorExpr {
		return trimmed
	}
	sel := lhs as ast.SelectorExpr
	if t.is_static_method_call(sel.lhs) {
		return trimmed
	}
	if t.resolve_method_call_name(sel.lhs, sel.rhs.name) == none {
		return trimmed
	}
	if trimmed.param_types.len == 0 {
		return trimmed
	}
	first_base := t.unwrap_alias_and_pointer_type(trimmed.param_types[0])
	if recv_type := t.get_expr_type(sel.lhs) {
		recv_base := t.unwrap_alias_and_pointer_type(recv_type)
		recv_name := recv_base.name()
		first_name := first_base.name()
		if !transformer_string_has_valid_data(recv_name)
			|| !transformer_string_has_valid_data(first_name) {
			return trimmed
		}
		if recv_name != first_name {
			resolved := t.resolve_method_call_name(sel.lhs, sel.rhs.name) or { return trimmed }
			recv_key := resolved.all_before_last('__')
			if !method_receiver_type_matches_resolved_name(t.type_to_c_name(first_base), recv_key) {
				if !method_receiver_type_matches_resolved_name(first_name, recv_key) {
					return trimmed
				}
			}
		}
	} else {
		resolved := t.resolve_method_call_name(sel.lhs, sel.rhs.name) or { return trimmed }
		recv_key := resolved.all_before_last('__')
		first_name := first_base.name()
		if !method_receiver_type_matches_resolved_name(t.type_to_c_name(first_base), recv_key) {
			if !transformer_string_has_valid_data(first_name)
				|| !method_receiver_type_matches_resolved_name(first_name, recv_key) {
				return trimmed
			}
		}
	}
	trimmed.param_types = trimmed.param_types[1..].clone()
	if trimmed.param_names.len > 0 {
		trimmed.param_names = trimmed.param_names[1..].clone()
	}
	if trimmed.generic_param_names_by_param.len > 0 {
		trimmed.generic_param_names_by_param = trimmed.generic_param_names_by_param[1..].clone()
	}
	if trimmed.generic_param_indexes_by_param.len > 0 {
		trimmed.generic_param_indexes_by_param = trimmed.generic_param_indexes_by_param[1..].clone()
	}
	return trimmed
}

fn call_info_without_first_param(info CallFnInfo) CallFnInfo {
	mut trimmed := info
	if trimmed.param_types.len > 0 {
		trimmed.param_types = trimmed.param_types[1..].clone()
	}
	if trimmed.param_names.len > 0 {
		trimmed.param_names = trimmed.param_names[1..].clone()
	}
	if trimmed.generic_param_names_by_param.len > 0 {
		trimmed.generic_param_names_by_param = trimmed.generic_param_names_by_param[1..].clone()
	}
	if trimmed.generic_param_indexes_by_param.len > 0 {
		trimmed.generic_param_indexes_by_param = trimmed.generic_param_indexes_by_param[1..].clone()
	}
	return trimmed
}

fn method_receiver_type_matches_resolved_name(type_name string, resolved_recv string) bool {
	mut lhs := type_name.trim_space()
	mut rhs := resolved_recv.trim_space()
	for lhs.ends_with('*') {
		lhs = lhs[..lhs.len - 1].trim_space()
	}
	for rhs.ends_with('*') {
		rhs = rhs[..rhs.len - 1].trim_space()
	}
	if lhs == '' || rhs == '' {
		return false
	}
	if lhs.contains('_T_') {
		lhs = lhs.all_before('_T_')
	}
	if rhs.contains('_T_') {
		rhs = rhs.all_before('_T_')
	}
	if lhs == rhs {
		return true
	}
	return lhs.all_after_last('__') == rhs.all_after_last('__')
}

fn (t &Transformer) lookup_call_fn_info(lhs ast.Expr) ?CallFnInfo {
	if lhs is ast.SelectorExpr {
		if resolved_static := t.resolve_static_type_method_call(lhs.lhs, lhs.rhs.name) {
			recv_key := resolved_static.all_before_last('__')
			mut lookup_names := []string{cap: 4}
			lookup_names << recv_key
			if recv_key.contains('__') {
				lookup_names << recv_key.all_after_last('__')
			}
			if lhs.lhs is ast.Ident {
				lookup_names << lhs.lhs.name
			} else if lhs.lhs is ast.SelectorExpr {
				lookup_names << lhs.lhs.rhs.name
			}
			for name in lookup_names {
				if fn_type := t.lookup_method_cached(name, lhs.rhs.name) {
					return call_fn_info_from_fn_type(fn_type)
				}
			}
		}
		if recv_type := t.get_expr_type(lhs.lhs) {
			base_type := t.unwrap_alias_and_pointer_type(recv_type)
			if base_type is types.Channel {
				if info := t.builtin_channel_call_fn_info(lhs.rhs.name) {
					return info
				}
				if fn_type := t.lookup_method_cached('chan', lhs.rhs.name) {
					return call_fn_info_from_fn_type(fn_type)
				}
			}
		}
	}
	// Prefer checker-resolved callable type for this exact call target.
	// This is the most reliable source for method parameter info (names + types).
	if lhs_type := t.get_expr_type(lhs) {
		callable := t.unwrap_alias_and_pointer_type(lhs_type)
		if callable is types.FnType {
			mut info := call_fn_info_from_fn_type(callable)
			return t.drop_method_receiver_from_call_info(lhs, info)
		}
	}
	if lhs is ast.Ident {
		if t.cur_module != '' {
			if fn_type := t.lookup_fn_cached(t.cur_module, lhs.name) {
				return call_fn_info_from_fn_type(fn_type)
			}
		}
		if fn_type := t.lookup_fn_cached('builtin', lhs.name) {
			return call_fn_info_from_fn_type(fn_type)
		}
		if lhs.name.contains('__') {
			module_name := lhs.name.all_before_last('__')
			fn_name := lhs.name.all_after_last('__')
			if fn_type := t.lookup_fn_cached(module_name, fn_name) {
				return call_fn_info_from_fn_type(fn_type)
			}
			short_module := if module_name.contains('.') {
				module_name.all_after_last('.')
			} else if module_name.contains('__') {
				module_name.all_after_last('__')
			} else {
				module_name
			}
			if short_module != module_name {
				if fn_type := t.lookup_fn_cached(short_module, fn_name) {
					return call_fn_info_from_fn_type(fn_type)
				}
			}
		}
		return none
	}
	if lhs is ast.SelectorExpr {
		if lhs.lhs is ast.Ident {
			mod_name := (lhs.lhs as ast.Ident).name
			mut module_names := []string{cap: 4}
			module_names << mod_name
			if resolved_mod := t.resolve_module_name(mod_name) {
				if resolved_mod !in module_names {
					module_names << resolved_mod
				}
				short_mod := if resolved_mod.contains('.') {
					resolved_mod.all_after_last('.')
				} else if resolved_mod.contains('__') {
					resolved_mod.all_after_last('__')
				} else {
					resolved_mod
				}
				if short_mod !in module_names {
					module_names << short_mod
				}
			}
			for module_name in module_names {
				if fn_type := t.lookup_fn_cached(module_name, lhs.rhs.name) {
					return call_fn_info_from_fn_type(fn_type)
				}
			}
		}
		if resolved_method := t.resolve_method_call_name(lhs.lhs, lhs.rhs.name) {
			recv_key := resolved_method.all_before_last('__')
			mut lookup_names := []string{cap: 2}
			lookup_names << recv_key
			if recv_key.contains('__') {
				lookup_names << recv_key.all_after_last('__')
			}
			for name in lookup_names {
				if fn_type := t.lookup_method_cached(name, lhs.rhs.name) {
					info := call_fn_info_from_fn_type(fn_type)
					return t.drop_method_receiver_from_call_info(lhs, info)
				}
			}
		}
		if recv_type := t.get_expr_type(lhs.lhs) {
			base_type := t.unwrap_alias_and_pointer_type(recv_type)
			type_name := base_type.name()
			mut lookup_names := []string{cap: 3}
			t.append_method_lookup_type_name(mut lookup_names, type_name)
			if lookup_names.len == 0 {
				return none
			}
			if !type_name.contains('__') && t.cur_module != '' && t.cur_module != 'main' {
				lookup_names << '${t.cur_module}__${type_name}'
			}
			for name in lookup_names {
				if fn_type := t.lookup_method_cached(name, lhs.rhs.name) {
					mut info := call_fn_info_from_fn_type(fn_type)
					return t.drop_method_receiver_from_call_info(lhs, info)
				}
			}
		}
	}
	return none
}

fn (t &Transformer) builtin_channel_call_fn_info(method_name string) ?CallFnInfo {
	if method_name == 'close' {
		ierror_type := t.lookup_type('builtin__IError') or {
			t.lookup_type('IError') or { types.Type(types.Interface{
				name: 'IError'
			}) }
		}
		return CallFnInfo{
			param_types: [types.Type(types.Array{
				elem_type: ierror_type
			})]
			param_names: ['err']
			is_variadic: true
		}
	}
	return none
}

fn (t &Transformer) lookup_call_param_types(lhs ast.Expr) []types.Type {
	if info := t.call_fn_info_for_lhs(lhs) {
		return info.param_types
	}
	return []types.Type{}
}

// receiver_type_to_c_prefix maps a receiver's unwrapped type to the C function
// name prefix used in method name mangling. For containers (array, map, string)
// it returns the generic prefix; for named types it returns the qualified C name.
fn (t &Transformer) receiver_type_to_c_prefix(typ types.Type) string {
	if !transformer_type_has_safe_payload(typ) {
		return ''
	}
	match typ {
		types.Array, types.ArrayFixed {
			return 'array'
		}
		types.Map {
			return 'map'
		}
		types.String {
			return 'string'
		}
		types.Alias {
			if !transformer_type_has_safe_payload(typ.base_type) {
				return ''
			}
			// Check if alias over container
			match typ.base_type {
				types.Array, types.ArrayFixed {
					return 'array'
				}
				types.Map {
					return 'map'
				}
				types.String {
					return 'string'
				}
				else {
					return t.type_to_c_name(typ)
				}
			}
		}
		types.Struct, types.Enum, types.Interface, types.SumType {
			return t.type_to_c_name(typ)
		}
		types.Channel {
			return 'chan'
		}
		types.Primitive, types.Char, types.Rune {
			return t.type_to_c_name(typ)
		}
		else {
			return ''
		}
	}
}

// is_static_method_call checks if the receiver of a method call is a type reference
// (not a variable/expression), indicating a static method call like EnumType.from_string(s).
// In such cases, the receiver should NOT be passed as the first argument.
fn (t &Transformer) is_static_method_call(receiver ast.Expr) bool {
	if receiver is ast.Ident {
		// Uppercase first letter indicates a type name, not a variable
		if receiver.name.len > 0 && receiver.name[0] >= `A` && receiver.name[0] <= `Z` {
			// Check if scope lookup resolves to a Type definition object (not a
			// Const/Fn/Global variable). Type objects represent type definitions
			// like enum/struct that are referenced, not value expressions.
			if t.scope != unsafe { nil } {
				if obj := unsafe { t.scope }.lookup_parent(receiver.name, 0) {
					if obj is types.Module {
						return false
					}
					if obj is types.Type || obj is types.TypeObject {
						return true
					}
					return false
				}
			}
			if mut scope := t.get_current_scope() {
				if obj := scope.lookup_parent(receiver.name, 0) {
					if obj is types.Module {
						return false
					}
					if obj is types.Type || obj is types.TypeObject {
						return true
					}
					return false
				}
			}
			if _ := t.lookup_type(receiver.name) {
				return true
			}
			if _ := t.lookup_var_type(receiver.name) {
				return false
			}
			// Not found in scope — assume type reference
			return true
		}
	}
	if receiver is ast.SelectorExpr {
		// Module-qualified type: module.TypeName
		if receiver.lhs is ast.Ident {
			mod_name := (receiver.lhs as ast.Ident).name
			type_name := receiver.rhs.name
			if t.get_module_scope(mod_name) != none && type_name.len > 0 && type_name[0] >= `A`
				&& type_name[0] <= `Z` {
				return true
			}
		}
	}
	return false
}

fn (t &Transformer) resolve_static_type_method_for_names(c_type_name string, lookup_names []string, method_name string) ?string {
	for lookup_name in lookup_names {
		if t.lookup_method_cached(lookup_name, method_name) != none {
			return '${c_type_name}__${method_name}'
		}
	}
	return none
}

fn (t &Transformer) resolve_static_type_method_call(receiver ast.Expr, method_name string) ?string {
	if receiver is ast.Ident {
		type_name := receiver.name
		if type_name.len == 0 || type_name[0] < `A` || type_name[0] > `Z` {
			return none
		}
		c_type_name := if t.cur_module != '' && t.cur_module != 'main' {
			'${t.cur_module.replace('.', '__')}__${type_name}'
		} else {
			type_name
		}
		mut lookup_names := []string{cap: 2}
		lookup_names << c_type_name
		if c_type_name != type_name {
			lookup_names << type_name
		}
		return t.resolve_static_type_method_for_names(c_type_name, lookup_names, method_name)
	}
	if receiver is ast.SelectorExpr {
		if receiver.lhs !is ast.Ident {
			return none
		}
		type_name := receiver.rhs.name
		if type_name.len == 0 || type_name[0] < `A` || type_name[0] > `Z` {
			return none
		}
		mod_ident := (receiver.lhs as ast.Ident).name
		mut module_names := []string{cap: 2}
		if resolved_mod := t.resolve_module_name(mod_ident) {
			module_names << resolved_mod
		}
		if t.get_module_scope(mod_ident) != none && mod_ident !in module_names {
			module_names << mod_ident
		}
		for mod_name in module_names {
			c_type_name := '${mod_name.replace('.', '__')}__${type_name}'
			mut lookup_names := []string{cap: 2}
			lookup_names << c_type_name
			lookup_names << type_name
			if resolved := t.resolve_static_type_method_for_names(c_type_name, lookup_names,
				method_name)
			{
				return resolved
			}
		}
	}
	return none
}

fn (t &Transformer) smartcast_selector_field_type(receiver ast.Expr) ?types.Type {
	if !t.has_active_smartcast() || receiver !is ast.SelectorExpr {
		return none
	}
	sel := receiver as ast.SelectorExpr
	full_str := t.expr_to_string(receiver)
	if ctx := t.find_smartcast_for_expr(full_str) {
		if typ := t.lookup_type(ctx.variant_full) {
			return typ
		}
		if typ := t.lookup_type(ctx.variant) {
			return typ
		}
	}
	lhs_str := t.expr_to_string(sel.lhs)
	if ctx := t.find_smartcast_for_expr(lhs_str) {
		if field_type := t.lookup_struct_field_type(ctx.variant_full, sel.rhs.name) {
			return field_type
		}
		if ctx.variant != ctx.variant_full {
			if field_type := t.lookup_struct_field_type(ctx.variant, sel.rhs.name) {
				return field_type
			}
		}
	}
	return none
}

fn (mut t Transformer) transform_method_receiver_expr(receiver ast.Expr) ast.Expr {
	if t.has_active_smartcast() && receiver is ast.SelectorExpr {
		sel := receiver as ast.SelectorExpr
		lhs_str := t.expr_to_string(sel.lhs)
		if ctx := t.find_smartcast_for_expr(lhs_str) {
			return t.apply_smartcast_field_access_ctx(sel.lhs, sel.rhs.name, ctx)
		}
	}
	return t.transform_expr(receiver)
}

fn (mut t Transformer) transform_call_lhs_for_smartcast(lhs ast.Expr) ast.Expr {
	if !t.has_active_smartcast() || lhs !is ast.SelectorExpr {
		return lhs
	}
	sel := lhs as ast.SelectorExpr
	return ast.SelectorExpr{
		lhs: t.transform_method_receiver_expr(sel.lhs)
		rhs: sel.rhs
		pos: sel.pos
	}
}

// resolve_method_call_name resolves a method call on a receiver to its mangled
// C function name. Returns e.g. "array__push" or "MyStruct__method". Returns
// none when the receiver type is unknown or the method is not registered
// (e.g. function pointer field calls), which prevents false lowering.
fn (t &Transformer) explicit_cast_receiver_type_name(receiver ast.Expr) ?string {
	match receiver {
		ast.CastExpr {
			return t.explicit_cast_type_expr_name(receiver.typ)
		}
		ast.CallOrCastExpr {
			if receiver.expr is ast.EmptyExpr || !t.call_or_cast_lhs_is_type(receiver.lhs) {
				return none
			}
			return t.explicit_cast_type_expr_name(receiver.lhs)
		}
		ast.ParenExpr {
			return t.explicit_cast_receiver_type_name(receiver.expr)
		}
		ast.ModifierExpr {
			return t.explicit_cast_receiver_type_name(receiver.expr)
		}
		else {}
	}

	return none
}

fn (t &Transformer) explicit_cast_type_expr_name(expr ast.Expr) ?string {
	type_name := t.expr_to_type_name(expr)
	if type_name == '' {
		return none
	}
	return t.v_type_name_to_c_name(type_name)
}

fn (t &Transformer) resolve_explicit_cast_method_name(receiver_type_name string, method_name string) ?string {
	if receiver_type_name == '' {
		return none
	}
	if t.lookup_method_cached(receiver_type_name, method_name) != none {
		return '${receiver_type_name}__${method_name}'
	}
	dunder := last_double_underscore(receiver_type_name)
	if dunder >= 0 {
		short_name := receiver_type_name[dunder + 2..]
		if t.lookup_method_cached(short_name, method_name) != none {
			return '${short_name}__${method_name}'
		}
	}
	if receiver_type_name.starts_with('Array_')
		&& t.lookup_method_cached('array', method_name) != none {
		return 'array__${method_name}'
	}
	if receiver_type_name.starts_with('Map_') && t.lookup_method_cached('map', method_name) != none {
		return 'map__${method_name}'
	}
	return none
}

fn (t &Transformer) resolve_alias_receiver_method_name(recv_type types.Type, method_name string) ?string {
	mut seen := []string{}
	return t.resolve_alias_receiver_method_name_inner(recv_type, method_name, mut seen)
}

fn (t &Transformer) exact_method_owner_name(raw_name string, method_name string) ?string {
	if raw_name == '' {
		return none
	}
	normalized := normalized_method_lookup_type_name(raw_name)
	if normalized == '' {
		return none
	}
	if t.lookup_method_cached(normalized, method_name) != none {
		return normalized
	}
	if last_double_underscore(normalized) < 0 && t.cur_module != '' && t.cur_module != 'main'
		&& t.cur_module != 'builtin' {
		qualified_mod := if t.cur_module.index_u8(`.`) >= 0 {
			t.cur_module.replace('.', '__')
		} else {
			t.cur_module
		}
		qualified := '${qualified_mod}__${normalized}'
		if t.lookup_method_cached(qualified, method_name) != none {
			return qualified
		}
	}
	return none
}

fn (t &Transformer) resolve_alias_receiver_method_name_inner(recv_type types.Type, method_name string, mut seen []string) ?string {
	match recv_type {
		types.Pointer {
			return t.resolve_alias_receiver_method_name_inner(recv_type.base_type, method_name, mut
				seen)
		}
		types.NamedType {
			key := 'named:${string(recv_type)}'
			if key in seen {
				return none
			}
			seen << key
			if resolved := t.lookup_type(string(recv_type)) {
				return t.resolve_alias_receiver_method_name_inner(resolved, method_name, mut seen)
			}
		}
		types.Alias {
			key := 'alias:${recv_type.name}'
			if key in seen {
				return none
			}
			seen << key
			alias_c_name := t.type_to_c_name(recv_type)
			for alias_name in [recv_type.name, alias_c_name] {
				if owner := t.exact_method_owner_name(alias_name, method_name) {
					return '${owner}__${method_name}'
				}
			}
		}
		else {}
	}

	return none
}

fn (t &Transformer) resolve_method_call_name(receiver ast.Expr, method_name string) ?string {
	if t.is_static_method_call(receiver) {
		return t.resolve_static_type_method_call(receiver, method_name)
	}
	if cast_type_name := t.explicit_cast_receiver_type_name(receiver) {
		if resolved := t.resolve_explicit_cast_method_name(cast_type_name, method_name) {
			return resolved
		}
	}
	mut recv_type := types.Type(types.void_)
	if smartcast_type := t.smartcast_selector_field_type(receiver) {
		receiver_str := t.expr_to_string(receiver)
		source_has_method := if ctx := t.find_smartcast_for_expr(receiver_str) {
			t.smartcast_source_has_cached_method(ctx, method_name)
		} else {
			false
		}
		if source_has_method {
			ctx := t.find_smartcast_for_expr(receiver_str) or { return none }
			recv_type = t.c_name_to_type(ctx.sumtype) or { smartcast_type }
		} else if declared_type := t.declared_expr_type_for_method_receiver(receiver) {
			recv_type = if t.type_has_cached_method(declared_type, method_name) {
				declared_type
			} else {
				smartcast_type
			}
		} else {
			recv_type = smartcast_type
		}
	} else if declared_type := t.declared_expr_type_for_method_receiver(receiver) {
		recv_type = if t.type_has_cached_method(declared_type, method_name) {
			declared_type
		} else {
			t.get_expr_type(receiver) or { declared_type }
		}
	} else {
		recv_type = t.get_expr_type(receiver) or {
			// When type info is unavailable (e.g. methods on typed arrays like []string
			// where the scope didn't load), try 'array' as fallback for methods that are
			// unique to arrays. Skip methods that also exist on string/map/other types
			// since we can't disambiguate without type info.
			if method_name !in ['str', 'hex', 'clone', 'free', 'trim', 'bytes', 'bytestr', 'replace',
				'contains', 'len', 'index', 'last_index', 'is_blank', 'join', 'to_upper', 'to_lower',
				'repeat', 'vbytes', 'plus_two', 'write_u8', 'write_string', 'write_rune'] {
				if t.lookup_method_cached('array', method_name) != none {
					return 'array__${method_name}'
				}
			}
			// When type lookup fails but receiver is a known string expression, resolve
			// as string method. This prevents falling through to SSA builder where
			// non-deterministic map iteration could resolve to the wrong overload.
			if t.is_string_expr(receiver) {
				if t.lookup_method_cached('string', method_name) != none {
					return 'string__${method_name}'
				}
			}
			return none
		}
	}
	if method_name == 'clone' {
		if _ := t.unwrap_map_type(recv_type) {
			return 'map__clone'
		}
	}
	if alias_method := t.resolve_alias_receiver_method_name(recv_type, method_name) {
		return alias_method
	}
	base_type := t.unwrap_alias_and_pointer_type(recv_type)
	mut c_prefix := t.receiver_type_to_c_prefix(base_type)
	// Guard against type misresolution: when the checker returns array type but
	// the receiver is actually a string expression (verified via structural analysis),
	// correct the prefix. This can happen in ARM64-compiled binaries where the
	// checker's type store may have incorrect entries due to chained-access issues.
	if c_prefix == 'array' && t.is_string_expr(receiver) {
		c_prefix = 'string'
	}
	// Self-hosted ARM64 builds can misresolve byte-oriented receivers as `i8`
	// even though the builtin helper methods are declared on `u8`.
	// Only fall back to u8 when i8 has no method of its own (e.g. bytestr, hex).
	if c_prefix == 'i8' && t.lookup_method_cached('i8', method_name) == none
		&& t.lookup_method_cached('u8', method_name) != none {
		c_prefix = 'u8'
	}
	if c_prefix == '' {
		return none
	}
	if base_type is types.Channel && method_name == 'close' {
		return 'chan__close'
	}
	// Build lookup names for method verification (same pattern as lookup_call_param_types)
	type_name := base_type.name()
	mut lookup_names := []string{cap: 5}
	t.append_method_lookup_type_name(mut lookup_names, type_name)
	if lookup_names.len == 0 {
		return none
	}
	if !type_name.contains('__') && t.cur_module != '' && t.cur_module != 'main' {
		lookup_names << '${t.cur_module}__${type_name}'
	}
	// For container types ([]T, map, etc.), also try 'array'/'map'/'string' since
	// methods on the generic container are registered under those names.
	if c_prefix == 'array' && type_name != 'array' && 'array' !in lookup_names {
		lookup_names << 'array'
	} else if c_prefix == 'map' && type_name != 'map' && 'map' !in lookup_names {
		lookup_names << 'map'
	} else if c_prefix == 'string' && type_name != 'string' && 'string' !in lookup_names {
		lookup_names << 'string'
	}
	base_method_name := generic_base_name_without_specialization(method_name)
	if base_method_name != method_name {
		for name in lookup_names {
			if t.lookup_method_cached(name, base_method_name) != none {
				return '${c_prefix}__${method_name}'
			}
		}
	}
	if specific_array_method := t.specific_array_method_c_name(receiver, method_name) {
		return specific_array_method
	}
	if resolved_declared_method := t.resolve_cached_method_fn_name_for_type(recv_type, method_name,
		c_prefix)
	{
		return resolved_declared_method
	}
	// Verify method exists via env.lookup_method
	for name in lookup_names {
		if t.lookup_method_cached(name, method_name) != none {
			// For array types: if the method is NOT on generic 'array' but on
			// a typed array (e.g., []rune.string()), use the specific C type name
			// (e.g., Array_rune) instead of generic 'array'.
			if c_prefix == 'array' && t.lookup_method_cached('array', method_name) == none {
				mut specific_name := t.type_to_c_name(base_type)
				if specific_name == 'Array_i8'
					&& method_name in ['bytestr', 'byterune', 'hex', 'utf8_to_utf32'] {
					specific_name = 'Array_u8'
				}
				return '${specific_name}__${method_name}'
			}
			return '${c_prefix}__${method_name}'
		}
	}
	// Fuzzy fallback: iterate method keys to find matching receiver types
	for key in t.candidate_method_keys(lookup_names) {
		mut matches_receiver := false
		for name in lookup_names {
			if t.method_key_matches_type_name(key, name) {
				matches_receiver = true
				break
			}
		}
		if !matches_receiver {
			continue
		}
		methods_for_type := t.cached_methods[key] or { continue }
		for method in methods_for_type {
			if method.get_name() == method_name {
				// When the method was found on a different module-qualified type
				// (e.g. key=mbedtls__SSLConn vs c_prefix=ssl__SSLConn), use the
				// actual method owner's name. This handles embedded struct method
				// promotion where the method body lives on the embedded type.
				if key != c_prefix && key.contains('__') && c_prefix.contains('__')
					&& key.all_after_last('__') == c_prefix.all_after_last('__') {
					return '${key}__${method_name}'
				}
				return '${c_prefix}__${method_name}'
			}
		}
	}
	// Embedded struct method fallback: if the type is a struct that embeds
	// other structs, check if the method exists on any embedded struct.
	// This handles cases like ssl.SSLConn embedding mbedtls.SSLConn.
	if base_type is types.Struct {
		for embedded in base_type.embedded {
			emb_name := embedded.name
			if emb_name == '' {
				continue
			}
			if t.lookup_method_cached(emb_name, method_name) != none {
				return '${emb_name}__${method_name}'
			}
			// Also try short name (without module prefix)
			short_name := emb_name.all_after_last('__')
			if short_name != emb_name && t.lookup_method_cached(short_name, method_name) != none {
				return '${emb_name}__${method_name}'
			}
		}
	}
	if method_name == 'clone' {
		if generated := t.clone_fn_name_for_type(recv_type) {
			return generated
		}
	}
	return none
}

fn (t &Transformer) resolve_cached_method_fn_name_for_type(recv_type types.Type, method_name string, c_prefix string) ?string {
	if method_name == '' {
		return none
	}
	base_type := t.unwrap_alias_and_pointer_type(recv_type)
	mut prefixes := []string{}
	for prefix in [c_prefix, t.type_to_c_name(base_type), base_type.name(),
		t.type_to_name(base_type)] {
		if prefix != '' && prefix !in prefixes {
			prefixes << prefix
		}
	}
	for prefix in prefixes {
		candidate := '${prefix}__${method_name}'
		if t.cached_function_name_exists(candidate) {
			return candidate
		}
	}
	return none
}

fn (t &Transformer) cached_function_name_exists(name string) bool {
	if name == '' {
		return false
	}
	if name in t.declared_method_fns {
		return true
	}
	if name in t.cached_fn_scopes {
		return true
	}
	return false
}

fn (t &Transformer) resolved_method_uses_declared_receiver(receiver ast.Expr, method_name string, resolved string) bool {
	declared_type := t.declared_expr_type_for_method_receiver(receiver) or { return false }
	if !t.type_has_cached_method(declared_type, method_name) {
		return false
	}
	base_type := t.unwrap_alias_and_pointer_type(declared_type)
	c_prefix := t.receiver_type_to_c_prefix(base_type)
	if c_prefix == '' {
		return false
	}
	return resolved == '${c_prefix}__${method_name}'
}

fn (mut t Transformer) transform_method_receiver_arg(receiver ast.Expr, method_name string, resolved string) ast.Expr {
	if t.resolved_method_uses_declared_receiver(receiver, method_name, resolved) {
		receiver_key := t.expr_to_string(receiver)
		if removed := t.remove_smartcast_for_expr_with_idx(receiver_key) {
			transformed := t.transform_expr(receiver)
			t.restore_smartcasts([removed])
			return transformed
		}
	}
	return t.transform_method_receiver_expr(receiver)
}

fn (t &Transformer) specific_array_method_c_name(receiver ast.Expr, method_name string) ?string {
	recv_type := t.get_expr_type(receiver) or { return none }
	base_type := t.unwrap_alias_and_pointer_type(recv_type)
	match base_type {
		types.Array, types.ArrayFixed {
			c_name := t.type_to_c_name(base_type)
			if c_name == '' {
				return none
			}
			base_name := base_type.name()
			if t.lookup_method_cached(base_name, method_name) != none {
				return '${c_name}__${method_name}'
			}
			if c_name != base_name && t.lookup_method_cached(c_name, method_name) != none {
				return '${c_name}__${method_name}'
			}
		}
		else {}
	}

	return none
}

fn (t &Transformer) resolves_to_embedded_method(receiver ast.Expr, method_name string) bool {
	recv_type := t.get_expr_type(receiver) or { return false }
	struct_type := t.live_struct_type_from_type(recv_type) or { return false }
	for embedded in struct_type.embedded {
		emb_name := embedded.name
		if emb_name == '' {
			continue
		}
		if t.lookup_method_cached(emb_name, method_name) != none {
			return true
		}
		short_name := emb_name.all_after_last('__')
		if short_name != emb_name && t.lookup_method_cached(short_name, method_name) != none {
			return true
		}
	}
	return false
}

fn (mut t Transformer) transform_promoted_embedded_method_call(sel ast.SelectorExpr, raw_args []ast.Expr, pos token.Pos) ?ast.Expr {
	recv_type := t.get_expr_type(sel.lhs) or { return none }
	owner_method_name := generic_base_name_without_specialization(sel.rhs.name)
	if t.type_has_cached_method(recv_type, owner_method_name) {
		return none
	}
	struct_type := t.live_struct_type_from_type(recv_type) or { return none }
	for embedded in struct_type.embedded {
		emb_name := embedded.name
		if emb_name == '' {
			continue
		}
		mut resolved := ''
		if t.lookup_method_cached(emb_name, sel.rhs.name) != none {
			resolved = '${emb_name}__${sel.rhs.name}'
		} else {
			short_name := emb_name.all_after_last('__')
			if short_name != emb_name && t.lookup_method_cached(short_name, sel.rhs.name) != none {
				resolved = '${emb_name}__${sel.rhs.name}'
			}
		}
		if resolved == '' {
			continue
		}
		base_field_name := embedded_field_name_from_type_name(emb_name)
		if base_field_name == '' {
			continue
		}
		parent_struct_name := t.type_to_c_name(types.Type(struct_type))
		mut field_name := base_field_name
		if concrete_owner := t.concrete_embedded_owner_name(parent_struct_name, base_field_name) {
			field_name = concrete_owner
		}
		embedded_type := t.field_type_from_receiver_type(recv_type, field_name) or {
			t.field_type_from_receiver_type(recv_type, base_field_name) or {
				types.Type(t.live_embedded_struct_type(embedded))
			}
		}
		embedded_receiver := t.synth_selector(t.transform_method_receiver_expr(sel.lhs),
			field_name, embedded_type)
		embedded_lhs := ast.Expr(ast.SelectorExpr{
			lhs: embedded_receiver
			rhs: sel.rhs
			pos: sel.pos
		})
		mut seeded_bindings := t.generic_bindings_for_struct_field(recv_type, field_name) or {
			t.generic_bindings_for_struct_field(recv_type, base_field_name) or {
				map[string]types.Type{}
			}
		}
		mut call_args := t.lower_missing_call_args(embedded_lhs, raw_args)
		fn_info := t.generic_aware_call_fn_info(embedded_lhs, resolved)
		mut promoted_info := CallFnInfo{}
		mut has_promoted_info := false
		if call_args_have_field_init(call_args) {
			if decl_info := t.generic_call_info_for_decl(resolved) {
				promoted_info = call_info_without_first_param(decl_info)
				if promoted_info.param_types.len > 0 {
					call_args = t.lower_field_init_call_args(call_args, promoted_info.param_names,
						promoted_info.param_types)
					has_promoted_info = true
				}
			}
		}
		if call_args_have_field_init(call_args) {
			if decl := t.generic_fn_decl_for_call_info(resolved) {
				if decl.typ.params.len > 0 {
					generic_params := decl_generic_param_names(decl)
					if param_type := t.type_from_param_type_expr(decl.typ.params[0].typ,
						generic_params)
					{
						concrete_param_type := substitute_type(param_type, seeded_bindings)
						promoted_info = CallFnInfo{
							param_types: [concrete_param_type]
							param_names: ['']
						}
						call_args = t.lower_field_init_call_args(call_args,
							promoted_info.param_names, promoted_info.param_types)
						has_promoted_info = true
					}
				}
			}
		}
		mut transformed_call_args := []ast.Expr{cap: call_args.len}
		for i, arg in call_args {
			if has_promoted_info {
				transformed_call_args << t.transform_call_arg_with_sumtype_check(arg,
					promoted_info, i)
			} else {
				transformed_call_args << t.transform_call_arg_with_sumtype_check(arg, fn_info, i)
			}
		}
		transformed_call_args = t.lower_variadic_args(embedded_lhs, transformed_call_args)
		mut args := []ast.Expr{cap: transformed_call_args.len + 1}
		args << embedded_receiver
		args << transformed_call_args
		mut call_name := resolved
		if has_promoted_info {
			if arg_bindings := t.generic_bindings_from_call_args(promoted_info, call_args) {
				for name, typ in arg_bindings {
					if name !in seeded_bindings {
						seeded_bindings[name] = typ
					}
				}
			}
			if seeded := t.generic_method_call_name_from_bindings(call_name, seeded_bindings) {
				call_name = seeded
			} else if receiver_inferred := t.receiver_generic_method_call_name(call_name,
				embedded_receiver, promoted_info, call_args)
			{
				call_name = receiver_inferred
			} else if inferred := t.inferred_generic_call_name(call_name, promoted_info, call_args) {
				if !generic_name_contains_placeholder_suffix(inferred) {
					call_name = inferred
				}
			}
		} else if info := fn_info {
			if arg_bindings := t.generic_bindings_from_call_args(info, call_args) {
				for name, typ in arg_bindings {
					if name !in seeded_bindings {
						seeded_bindings[name] = typ
					}
				}
			}
			if seeded := t.generic_method_call_name_from_bindings(call_name, seeded_bindings) {
				call_name = seeded
			} else if receiver_inferred := t.receiver_generic_method_call_name(call_name,
				embedded_receiver, info, call_args)
			{
				call_name = receiver_inferred
			} else if inferred := t.inferred_generic_call_name(call_name, info, call_args) {
				if !generic_name_contains_placeholder_suffix(inferred) {
					call_name = inferred
				}
			}
		} else if seeded := t.generic_method_call_name_from_bindings(call_name, seeded_bindings) {
			call_name = seeded
		} else if receiver_inferred := t.receiver_generic_method_call_name(call_name,
			embedded_receiver, CallFnInfo{}, call_args)
		{
			call_name = receiver_inferred
		}
		if call_name == resolved {
			if full_info := t.generic_call_info_for_decl(resolved) {
				mut full_call_args := []ast.Expr{cap: call_args.len + 1}
				full_call_args << embedded_receiver
				for arg in call_args {
					full_call_args << arg
				}
				if full_bindings := t.generic_bindings_from_call_args(full_info, full_call_args) {
					if generic_bindings_cover_params(full_bindings, decl_generic_param_names(t.generic_fn_decl_for_call(resolved) or {
						return none
					}))
					{
						t.register_generic_bindings(resolved, full_bindings)
						if seeded := t.generic_method_call_name_from_bindings(resolved,
							full_bindings)
						{
							call_name = seeded
						}
					}
				}
			}
		}
		return ast.Expr(ast.CallExpr{
			lhs:  ast.Ident{
				name: call_name
			}
			args: args
			pos:  pos
		})
	}
	return none
}

fn (t &Transformer) generic_bindings_for_struct_field(receiver_type types.Type, field_name string) ?map[string]types.Type {
	mut cur := receiver_type
	for {
		if cur is types.Pointer {
			cur = cur.base_type
			continue
		}
		if cur is types.Alias {
			if bindings := t.lookup_struct_field_generic_decl_bindings(cur.name, field_name) {
				return bindings
			}
			cur = cur.base_type
			continue
		}
		break
	}
	mut lookup_names := []string{}
	type_name := cur.name()
	if type_name != '' {
		lookup_names << type_name
	}
	c_name := t.type_to_c_name(cur)
	if c_name != '' && c_name !in lookup_names {
		lookup_names << c_name
	}
	short_name := t.type_to_name(cur)
	if short_name != '' && short_name !in lookup_names {
		lookup_names << short_name
	}
	for name in lookup_names {
		if bindings := t.lookup_struct_field_generic_decl_bindings(name, field_name) {
			return bindings
		}
	}
	return none
}

fn (t &Transformer) generic_method_call_name_from_bindings(base_name string, bindings map[string]types.Type) ?string {
	if base_name == '' || bindings.len == 0 {
		return none
	}
	decl := t.generic_fn_decl_for_call(base_name) or { return none }
	for param_name in decl_generic_param_names(decl) {
		concrete := bindings[param_name] or { return none }
		if clone_type_contains_generic_placeholder(concrete) {
			return none
		}
	}
	specialized := t.specialized_fn_name(decl, bindings)
	if specialized == '' || specialized == decl.name {
		return none
	}
	mut receiver_prefix := base_name.all_before_last('__')
	if receiver_prefix == '' {
		return none
	}
	if receiver_prefix.contains('_T_') {
		receiver_prefix = receiver_prefix.all_before('_T_')
	}
	return '${receiver_prefix}__${specialized}'
}

fn generic_name_contains_placeholder_suffix(name string) bool {
	if name.ends_with('_T') {
		return true
	}
	if !name.contains('_T_') {
		return false
	}
	for part in name.all_after('_T_').split('_') {
		if part.len == 1 && part[0] >= `A` && part[0] <= `Z` {
			return true
		}
	}
	return false
}

fn (mut t Transformer) lower_missing_call_args(lhs ast.Expr, args []ast.Expr) []ast.Expr {
	info := t.call_fn_info_for_lhs(lhs) or { return args }
	param_types := info.param_types
	if param_types.len == 0 {
		return args
	}
	param_names := info.param_names

	mut out := []ast.Expr{cap: args.len}
	for arg in args {
		out << arg
	}
	if call_args_have_field_init(out) {
		out = t.lower_field_init_call_args(out, param_names, param_types)
	}
	for i in 0 .. out.len {
		if i >= param_types.len {
			break
		}
		out[i] = t.resolve_expr_with_expected_type(out[i], param_types[i])
	}
	for i in out.len .. param_types.len {
		typ := t.unwrap_alias_and_pointer_type(param_types[i])
		match typ {
			types.Struct {
				out << t.empty_struct_arg_expr(param_types[i])
			}
			else {
				break
			}
		}
	}
	return out
}

// lower_variadic_args wraps already-transformed trailing args into an array
// literal when a variadic function receives more args than declared params.
// Must be called AFTER transform_expr has been applied to all args.
fn (t &Transformer) lower_variadic_args(lhs ast.Expr, args []ast.Expr) []ast.Expr {
	// Skip C function calls — C variadic functions use native C varargs, not V arrays.
	if lhs is ast.SelectorExpr {
		if lhs.lhs is ast.Ident && lhs.lhs.name == 'C' {
			return args
		}
	}
	info := t.call_fn_info_for_lhs(lhs) or { return args }
	if !info.is_variadic || info.param_types.len == 0 {
		return args
	}
	variadic_start := info.param_types.len - 1
	if args.len < variadic_start {
		return args
	}
	last_param_type := t.unwrap_alias_and_pointer_type(info.param_types[info.param_types.len - 1])
	// When args.len == param_types.len, check if the last arg is already an array
	// (e.g., passing []Signal to ...Signal, or using spread ...args). If so, don't wrap.
	// If we can't determine the arg type, be conservative and don't wrap.
	if args.len == info.param_types.len {
		if last_param_type is types.Array {
			last_arg := args[args.len - 1]
			// Spread operator (...args) passes an array directly, no wrapping needed
			if last_arg is ast.PrefixExpr && last_arg.op == .ellipsis {
				return args
			}
			if arg_type := t.get_expr_type(last_arg) {
				unwrapped := t.unwrap_alias_and_pointer_type(arg_type)
				if unwrapped is types.Array {
					return args
				}
			}
			// arg is not an array type, proceed to wrap it
		} else {
			return args
		}
	}
	if last_param_type is types.Array {
		elem_type_name := t.type_to_c_name(last_param_type.elem_type)
		if elem_type_name == '' {
			return args
		}
		variadic_count := args.len - variadic_start
		mut variadic_exprs := []ast.Expr{cap: variadic_count}
		for i in variadic_start .. args.len {
			variadic_exprs << args[i]
		}
		inner_array_typ := ast.Type(ast.ArrayType{
			elem_type: ast.Ident{
				name: elem_type_name
			}
		})
		array_arg := ast.Expr(ast.CallExpr{
			lhs:  ast.Ident{
				name: 'builtin__new_array_from_c_array_noscan'
			}
			args: [
				ast.Expr(ast.BasicLiteral{
					kind:  .number
					value: '${variadic_count}'
				}),
				ast.Expr(ast.BasicLiteral{
					kind:  .number
					value: '${variadic_count}'
				}),
				ast.Expr(ast.KeywordOperator{
					op:    .key_sizeof
					exprs: [ast.Expr(ast.Ident{
						name: elem_type_name
					})]
				}),
				ast.Expr(ast.ArrayInitExpr{
					typ:   ast.Expr(inner_array_typ)
					exprs: variadic_exprs
				}),
			]
		})
		mut new_out := []ast.Expr{cap: variadic_start + 1}
		for i in 0 .. variadic_start {
			new_out << args[i]
		}
		new_out << array_arg
		return new_out
	}
	return args
}

fn call_args_have_field_init(args []ast.Expr) bool {
	for arg in args {
		if arg is ast.FieldInit {
			return true
		}
	}
	return false
}

fn (mut t Transformer) empty_struct_arg_expr(param_type types.Type) ast.Expr {
	base := t.unwrap_alias_and_pointer_type(param_type)
	if base !is types.Struct {
		return ast.empty_expr
	}
	// For pointer parameters, use `&Type{}` which cleanc lowers to heap allocation.
	if t.is_pointer_type(param_type) {
		mut cur := param_type
		for cur is types.Alias {
			alias_t := cur as types.Alias
			cur = alias_t.base_type
		}
		inner := if cur is types.Pointer {
			ptr_t := cur as types.Pointer
			ptr_t.base_type
		} else {
			cur
		}
		if cur is types.Pointer {
			init_pos := t.next_synth_pos()
			ptr_pos := t.next_synth_pos()
			t.register_synth_type(init_pos, inner)
			t.register_synth_type(ptr_pos, param_type)
			init_expr := ast.Expr(ast.InitExpr{
				typ: t.type_to_ast_type_expr(inner)
				pos: init_pos
			})
			return ast.Expr(ast.PrefixExpr{
				op:   .amp
				expr: init_expr
				pos:  ptr_pos
			})
		}
		// Fallback: nil pointer.
		return ast.Expr(ast.Ident{
			name: 'nil'
		})
	}
	base_name := t.type_to_c_name(base)
	if base_name.ends_with('PRNGConfigStruct') {
		init_pos := t.next_synth_pos()
		t.register_synth_type(init_pos, param_type)
		return ast.Expr(ast.InitExpr{
			typ:    t.type_to_ast_type_expr(param_type)
			fields: [
				ast.FieldInit{
					name:  'seed_'
					value: ast.Expr(ast.CallExpr{
						lhs:  ast.Ident{
							name: 'seed__time_seed_array'
						}
						args: [
							ast.Expr(ast.BasicLiteral{
								kind:  .number
								value: '2'
							}),
						]
					})
				},
			]
			pos:    init_pos
		})
	}
	init_pos := t.next_synth_pos()
	t.register_synth_type(init_pos, param_type)
	return ast.Expr(ast.InitExpr{
		typ: t.type_to_ast_type_expr(param_type)
		pos: init_pos
	})
}

fn (mut t Transformer) lower_field_init_call_args(args []ast.Expr, param_names []string, param_types []types.Type) []ast.Expr {
	if args.len == 0 {
		return args
	}
	if param_names.len != param_types.len {
		// Keep call args intact when signature info is incomplete.
		return args
	}
	mut name_to_idx := map[string]int{}
	for i, name in param_names {
		if name.len > 0 {
			name_to_idx[name] = i
		}
	}
	// Decide whether this is a named-argument call (FieldInit names match param names),
	// or a struct-shorthand literal for the next struct parameter.
	mut is_named_args := false
	for arg in args {
		if arg is ast.FieldInit {
			if arg.name in name_to_idx {
				is_named_args = true
				break
			}
		}
	}
	if is_named_args {
		return t.lower_named_args_call(args, name_to_idx, param_types)
	}
	return t.lower_struct_shorthand_call(args, param_types)
}

fn (mut t Transformer) lower_named_args_call(args []ast.Expr, name_to_idx map[string]int, param_types []types.Type) []ast.Expr {
	mut positional := []ast.Expr{}
	mut named := map[int]ast.Expr{}
	mut seen_named := false
	for arg in args {
		if arg is ast.FieldInit {
			seen_named = true
			idx := name_to_idx[arg.name] or {
				panic('bug in v2 compiler: unknown named argument `${arg.name}`')
			}
			if idx < positional.len {
				panic('bug in v2 compiler: named argument `${arg.name}` overlaps positional args')
			}
			if idx in named {
				panic('bug in v2 compiler: named argument `${arg.name}` specified more than once')
			}
			named[idx] = arg.value
			continue
		}
		if seen_named {
			panic('bug in v2 compiler: positional arguments after named arguments are not supported')
		}
		positional << arg
	}
	if positional.len > param_types.len {
		panic('bug in v2 compiler: too many positional arguments (${positional.len})')
	}
	mut out := []ast.Expr{len: param_types.len, init: ast.empty_expr}
	for i, arg in positional {
		out[i] = arg
	}
	for idx, val in named {
		if idx >= out.len {
			panic('bug in v2 compiler: named argument index out of range (${idx})')
		}
		if out[idx] !is ast.EmptyExpr {
			panic('bug in v2 compiler: named argument overlaps positional argument at index ${idx}')
		}
		out[idx] = val
	}
	mut last := -1
	for i, val in out {
		if val !is ast.EmptyExpr {
			last = i
		}
	}
	if last < 0 {
		return []ast.Expr{}
	}
	// Fill gaps up to `last` for struct parameters (default empty init). Other gaps
	// indicate missing support for non-struct default arguments.
	for i in 0 .. last + 1 {
		if out[i] !is ast.EmptyExpr {
			continue
		}
		base := t.unwrap_alias_and_pointer_type(param_types[i])
		match base {
			types.Struct {
				out[i] = t.empty_struct_arg_expr(param_types[i])
			}
			else {
				panic('bug in v2 compiler: missing named argument for parameter ${i}')
			}
		}
	}
	return out[..last + 1]
}

fn (mut t Transformer) lower_struct_shorthand_call(args []ast.Expr, param_types []types.Type) []ast.Expr {
	mut positional := []ast.Expr{}
	mut fields := []ast.FieldInit{}
	mut seen_fields := false
	for arg in args {
		if arg is ast.FieldInit {
			seen_fields = true
			fields << ast.FieldInit{
				name:  arg.name
				value: arg.value
			}
			continue
		}
		if seen_fields {
			panic('bug in v2 compiler: positional arguments after struct-shorthand fields are not supported')
		}
		positional << arg
	}
	if fields.len == 0 {
		return args
	}
	param_idx := positional.len
	if param_idx >= param_types.len {
		panic('bug in v2 compiler: struct-shorthand call has more args than params (${t.cur_file_name}:${t.cur_fn_name_str} positional=${positional.len} params=${param_types.len})')
	}
	mut param_type := param_types[param_idx]
	mut base := t.unwrap_alias_and_pointer_type(param_type)
	if base !is types.Struct {
		match base {
			types.Array {
				array_type := base as types.Array
				elem_base := t.unwrap_alias_and_pointer_type(array_type.elem_type)
				if elem_base is types.Struct {
					param_type = array_type.elem_type
					base = types.Type(elem_base)
				}
			}
			types.ArrayFixed {
				array_fixed_type := base as types.ArrayFixed
				elem_base := t.unwrap_alias_and_pointer_type(array_fixed_type.elem_type)
				if elem_base is types.Struct {
					param_type = array_fixed_type.elem_type
					base = types.Type(elem_base)
				}
			}
			else {}
		}
	}
	if base !is types.Struct {
		// Some call sites still reach this heuristic with field-init args that are
		// not struct shorthands. Keep the original args so later lowering/codegen
		// can handle them instead of aborting the whole compilation.
		return args
	}
	mut init_typ := param_type
	if t.is_pointer_type(param_type) {
		mut cur := param_type
		for cur is types.Alias {
			alias_t := cur as types.Alias
			cur = alias_t.base_type
		}
		inner := if cur is types.Pointer {
			ptr_t := cur as types.Pointer
			ptr_t.base_type
		} else {
			cur
		}
		if cur is types.Pointer {
			init_typ = inner
		}
	}
	init_pos := t.next_synth_pos()
	t.register_synth_type(init_pos, init_typ)
	mut init_expr := ast.Expr(ast.InitExpr{
		typ:    t.type_to_ast_type_expr(init_typ)
		fields: fields
		pos:    init_pos
	})
	if t.is_pointer_type(param_type) {
		ptr_pos := t.next_synth_pos()
		t.register_synth_type(ptr_pos, param_type)
		init_expr = ast.Expr(ast.PrefixExpr{
			op:   .amp
			expr: init_expr
			pos:  ptr_pos
		})
	}
	mut out := []ast.Expr{cap: positional.len + 1}
	out << positional
	out << init_expr
	return out
}

fn (t &Transformer) expr_contains_ident_named(expr ast.Expr, name string) bool {
	match expr {
		ast.Ident {
			return expr.name == name
		}
		ast.SelectorExpr {
			return t.expr_contains_ident_named(expr.lhs, name)
		}
		ast.InfixExpr {
			return t.expr_contains_ident_named(expr.lhs, name)
				|| t.expr_contains_ident_named(expr.rhs, name)
		}
		ast.ParenExpr {
			return t.expr_contains_ident_named(expr.expr, name)
		}
		ast.PrefixExpr {
			return t.expr_contains_ident_named(expr.expr, name)
		}
		ast.ModifierExpr {
			return t.expr_contains_ident_named(expr.expr, name)
		}
		ast.CastExpr {
			return t.expr_contains_ident_named(expr.expr, name)
		}
		ast.CallExpr {
			if t.expr_contains_ident_named(expr.lhs, name) {
				return true
			}
			for arg in expr.args {
				if t.expr_contains_ident_named(arg, name) {
					return true
				}
			}
			return false
		}
		ast.CallOrCastExpr {
			return t.expr_contains_ident_named(expr.lhs, name)
				|| t.expr_contains_ident_named(expr.expr, name)
		}
		ast.IfExpr {
			if t.expr_contains_ident_named(expr.cond, name)
				|| t.expr_contains_ident_named(expr.else_expr, name) {
				return true
			}
			for stmt in expr.stmts {
				if t.stmt_uses_ident(stmt, name) {
					return true
				}
			}
			return false
		}
		ast.IndexExpr {
			return t.expr_contains_ident_named(expr.lhs, name)
				|| t.expr_contains_ident_named(expr.expr, name)
		}
		ast.ArrayInitExpr {
			for e in expr.exprs {
				if t.expr_contains_ident_named(e, name) {
					return true
				}
			}
			return false
		}
		ast.InitExpr {
			for field in expr.fields {
				if t.expr_contains_ident_named(field.value, name) {
					return true
				}
			}
			return false
		}
		else {
			return false
		}
	}
}

fn (t &Transformer) is_sort_compare_lambda_expr(expr ast.Expr) bool {
	if expr is ast.InfixExpr {
		return t.expr_contains_ident_named(expr, 'a') && t.expr_contains_ident_named(expr, 'b')
	}
	return false
}

fn (mut t Transformer) try_transform_native_interface_concrete_call(sel ast.SelectorExpr, args []ast.Expr, pos token.Pos, call_lhs ast.Expr) ?ast.Expr {
	if t.pref == unsafe { nil } || (t.pref.backend != .arm64 && t.pref.backend != .x64) {
		return none
	}
	concrete := t.get_interface_concrete_type_for_expr(sel.lhs) or { return none }
	call_args := t.lower_missing_call_args(call_lhs, args)
	fn_info := t.lookup_call_fn_info(call_lhs)
	mut transformed_args := []ast.Expr{cap: call_args.len}
	for i, arg in call_args {
		transformed_args << t.transform_call_arg_with_sumtype_check(arg, fn_info, i)
	}
	transformed_args = t.lower_variadic_args(call_lhs, transformed_args)
	resolved_method := '${concrete}__${sel.rhs.name}'
	mut native_args := []ast.Expr{cap: transformed_args.len + 1}
	native_receiver := t.native_interface_receiver_arg(sel.lhs, concrete)
	native_args << t.transform_expr(native_receiver)
	native_args << transformed_args
	return ast.Expr(ast.CallExpr{
		lhs:  ast.Ident{
			name: resolved_method
		}
		args: native_args
		pos:  pos
	})
}

fn (mut t Transformer) transform_call_or_cast_expr(expr ast.CallOrCastExpr) ast.Expr {
	// Inline generic math functions (abs[T], min[T], max[T]).
	if inlined := t.try_inline_generic_math_coce(expr) {
		return inlined
	}
	// Expand .filter() / .map() calls to hoisted statements + temp variable
	if expanded := t.try_expand_filter_or_map_expr(expr) {
		return expanded
	}
	if generic_lhs := t.generic_call_lhs_from_index_expr(expr.lhs) {
		return t.transform_call_or_cast_expr(ast.CallOrCastExpr{
			lhs:  generic_lhs
			expr: expr.expr
			pos:  expr.pos
		})
	}
	// Check if this is a flag enum method call: receiver.has(arg) or receiver.all(arg)
	if expr.lhs is ast.SelectorExpr {
		sel := expr.lhs as ast.SelectorExpr
		if t.is_native_be {
			if concrete := t.get_native_default_interface_concrete_type(sel.lhs, sel.rhs.name) {
				mut call_args := []ast.Expr{}
				if expr.expr !is ast.EmptyExpr {
					call_args << expr.expr
				}
				call_args = t.lower_missing_call_args(expr.lhs, call_args)
				mut native_args := []ast.Expr{cap: call_args.len + 1}
				native_args << t.transform_expr(sel.lhs)
				for arg in call_args {
					native_args << t.transform_expr(arg)
				}
				return ast.CallExpr{
					lhs:  ast.Ident{
						name: '${concrete}__${sel.rhs.name}'
					}
					args: native_args
					pos:  expr.pos
				}
			}
		}
		// Skip calls to conditionally compiled functions (e.g., @[if verbose ?])
		if sel.rhs.name in t.elided_fns {
			return ast.Expr(ast.BasicLiteral{
				kind:  .number
				value: '0'
			})
		}
		// arr.sort(a < b) may be parsed as CallOrCastExpr in single-arg form.
		if sel.rhs.name in ['sort', 'sorted'] && t.is_sort_compare_lambda_expr(expr.expr) {
			if result := t.transform_sort_call(sel.lhs, sel.rhs.name, [expr.expr], expr.pos) {
				return result
			}
		}
		method_name := sel.rhs.name
		if method_name == 'zero' && expr.expr is ast.EmptyExpr {
			receiver_type := t.get_enum_type(sel.lhs)
			if t.is_flag_enum(receiver_type) {
				return ast.CastExpr{
					typ:  ast.Ident{
						name: receiver_type
					}
					expr: ast.BasicLiteral{
						kind:  .number
						value: '0'
					}
					pos:  expr.pos
				}
			}
		}
		if method_name in ['has', 'all'] {
			arg0 := expr.expr
			is_string_arg := arg0 is ast.StringLiteral
				|| (arg0 is ast.BasicLiteral && arg0.kind == .string)
			if !is_string_arg {
				// Try to detect if receiver is a flag enum
				receiver_type := t.get_enum_type(sel.lhs)
				if t.is_flag_enum_receiver(sel.lhs, receiver_type) {
					// Transform the method call
					return t.transform_flag_enum_method(sel.lhs, method_name, [expr.expr],
						receiver_type)
				}
			}
		}
		if method_name in ['contains', 'index', 'last_index']
			&& t.specific_array_method_c_name(sel.lhs, method_name) == none {
			if info := t.get_array_method_info(sel.lhs) {
				fn_name := t.register_needed_array_method(info, method_name)
				return ast.CallExpr{
					lhs:  ast.Ident{
						name: fn_name
					}
					args: [
						t.transform_array_receiver_expr(sel.lhs),
						t.transform_expr(expr.expr),
					]
					pos:  expr.pos
				}
			}
		}
		// Check for smart-casted method call: se.lhs.method() when se.lhs is smartcast to Type
		if t.has_active_smartcast() {
			if smartcast_receiver := t.smartcast_method_receiver_context(sel.lhs) {
				variant_resolved_name := t.smartcast_variant_method_name(smartcast_receiver.ctx,
					sel.rhs.name) or { '' }
				mut args := []ast.Expr{cap: 1}
				if expr.expr !is ast.EmptyExpr {
					args << t.transform_expr(expr.expr)
				}
				// Resolve method name. When the variant type has the method, use the
				// variant's name to build the resolved call (e.g. Char__name instead
				// of Type__name) to avoid infinite recursion on sum type dispatch.
				// Do NOT route through transform_call_expr because it would
				// re-transform the already-casted receiver and args.
				mut resolved_name := ''
				if variant_resolved_name != '' {
					resolved_name = variant_resolved_name
				} else if rn := t.resolve_method_call_name(sel.lhs, sel.rhs.name) {
					resolved_name = rn
				}
				if resolved_name != '' {
					resolved := resolved_name
					is_static := t.is_static_method_call(sel.lhs)
					mut final_args := []ast.Expr{cap: args.len + 1}
					if !is_static {
						receiver_arg := if variant_resolved_name != '' {
							t.smartcast_method_receiver(smartcast_receiver.receiver,
								smartcast_receiver.ctx)
						} else {
							t.transform_method_receiver_arg(sel.lhs, sel.rhs.name, resolved)
						}
						final_args << receiver_arg
					}
					final_args << args
					return ast.CallExpr{
						lhs:  ast.Ident{
							name: resolved
						}
						args: final_args
						pos:  expr.pos
					}
				}
				// Fallback: keep as method call with casted receiver (let cleanc resolve)
				casted_receiver := t.smartcast_method_receiver(smartcast_receiver.receiver,
					smartcast_receiver.ctx)
				return ast.CallExpr{
					lhs:  ast.Expr(ast.SelectorExpr{
						lhs: casted_receiver
						rhs: sel.rhs
						pos: sel.pos
					})
					args: args
					pos:  expr.pos
				}
			}
		}
		// Check for interface method call: iface.method(arg)
		mut native_single_args := []ast.Expr{}
		if expr.expr !is ast.EmptyExpr {
			native_single_args << expr.expr
		}
		if native_call := t.try_transform_native_interface_concrete_call(sel, native_single_args,
			expr.pos, expr.lhs)
		{
			return native_call
		}
		if t.is_interface_receiver(sel.lhs) {
			mut call_args := []ast.Expr{}
			if expr.expr !is ast.EmptyExpr {
				call_args << expr.expr
			}
			call_args = t.lower_missing_call_args(expr.lhs, call_args)
			iface_method_fn_info := t.lookup_call_fn_info(expr.lhs)
			mut transformed_iface_args := []ast.Expr{cap: call_args.len}
			for i, arg in call_args {
				transformed_iface_args << t.transform_call_arg_with_sumtype_check(arg,
					iface_method_fn_info, i)
			}
			transformed_iface_args = t.lower_variadic_args(expr.lhs, transformed_iface_args)
			// Native backends (arm64/x64): resolve to direct concrete method call.
			if t.is_native_be {
				if concrete := t.get_interface_concrete_type_for_expr(sel.lhs) {
					resolved_iface_method := '${concrete}__${sel.rhs.name}'
					mut native_iface_args := []ast.Expr{cap: transformed_iface_args.len + 1}
					native_receiver := t.native_interface_receiver_arg(sel.lhs, concrete)
					native_iface_args << t.transform_expr(native_receiver)
					native_iface_args << transformed_iface_args
					return ast.CallExpr{
						lhs:  ast.Ident{
							name: resolved_iface_method
						}
						args: native_iface_args
						pos:  expr.pos
					}
				}
				if concrete := t.get_native_default_interface_concrete_type(sel.lhs, sel.rhs.name) {
					resolved_iface_method := '${concrete}__${sel.rhs.name}'
					mut native_iface_args := []ast.Expr{cap: transformed_iface_args.len + 1}
					native_iface_args << t.transform_expr(sel.lhs)
					native_iface_args << transformed_iface_args
					return ast.CallExpr{
						lhs:  ast.Ident{
							name: resolved_iface_method
						}
						args: native_iface_args
						pos:  expr.pos
					}
				}
			}
			// C/cleanc backends: Transform to vtable dispatch
			mut iface_args := []ast.Expr{cap: transformed_iface_args.len + 1}
			iface_args << t.synth_selector(sel.lhs, '_object', types.Type(types.voidptr_))
			iface_args << transformed_iface_args
			return ast.CallExpr{
				lhs:  ast.Expr(expr.lhs) // Keep the selector: iface.method
				args: iface_args
				pos:  expr.pos
			}
		}
	}
	// Check for println/eprintln with non-string argument
	// Transform: println(arr) -> println(Array_int_str(arr))
	if expr.lhs is ast.Ident {
		fn_name := expr.lhs.name
		if fn_name in ['println', 'eprintln', 'print', 'eprint'] {
			arg := expr.expr
			if arg is ast.StringInterLiteral {
				return ast.CallExpr{
					lhs:  ast.Expr(expr.lhs)
					args: [t.transform_expr(arg)]
					pos:  expr.pos
				}
			}
			if !t.is_string_expr(arg) {
				// Get the str function name and record it for generation
				str_fn_info := t.get_str_fn_info_for_expr(arg)
				if str_fn_info.str_fn_name != '' {
					// Record needed str function for later generation
					t.needed_str_fns[str_fn_info.str_fn_name] = str_fn_info.elem_type
					if typ := t.get_expr_type(arg) {
						if typ is types.Enum {
							t.needed_enum_str_fns[str_fn_info.str_fn_name] = typ
						}
					}
					mut str_call_args := []ast.Expr{cap: 1}
					str_call_args << t.transform_expr(arg)
					// Transform to println(Type_str(arg)) - use CallExpr for proper function call syntax
					return ast.CallExpr{
						lhs:  ast.Expr(expr.lhs)
						args: [
							ast.Expr(ast.CallExpr{
								lhs:  ast.Ident{
									name: str_fn_info.str_fn_name
								}
								args: str_call_args
								pos:  expr.pos
							}),
						]
						pos:  expr.pos
					}
				}
				// Fallback: try get_str_fn_name_for_expr for non-array types (int, bool, etc.)
				if str_fn_name := t.get_str_fn_name_for_expr(arg) {
					mut str_call_args := []ast.Expr{cap: 1}
					str_call_args << t.transform_expr(arg)
					return ast.CallExpr{
						lhs:  ast.Expr(expr.lhs)
						args: [
							ast.Expr(ast.CallExpr{
								lhs:  ast.Ident{
									name: str_fn_name
								}
								args: str_call_args
								pos:  expr.pos
							}),
						]
						pos:  expr.pos
					}
				}
			}
		}
	}
	// Check for explicit sum type cast: SumType(value) -> proper wrapping.
	// Important: do not transform `value` before variant inference, otherwise casts like
	// `Expr(EmptyExpr(0))` turn into `CastExpr` and variant inference can no longer
	// recover the variant.
	mut sumtype_name := ''
	mut sumtype_variants := []string{}
	if concrete_info := t.concrete_sumtype_wrap_info_from_lhs(expr.lhs) {
		sumtype_name = concrete_info.name
		sumtype_variants = concrete_info.variants.clone()
	}
	if sumtype_variants.len == 0 {
		if lhs_typ := t.lookup_type_from_expr(expr.lhs) {
			if lhs_typ is types.SumType {
				sumtype_name = t.type_to_c_name(lhs_typ)
			}
		}
	}
	if sumtype_name == '' {
		sumtype_name = t.type_expr_name_full(expr.lhs)
	}
	if sumtype_variants.len == 0 && (sumtype_name == '' || !t.is_sum_type(sumtype_name)) {
		if lhs_typ := t.get_expr_type(expr.lhs) {
			if lhs_typ is types.SumType {
				sumtype_name = t.type_to_c_name(lhs_typ)
			}
		}
	}
	if sumtype_variants.len == 0 && (sumtype_name == '' || !t.is_sum_type(sumtype_name))
		&& expr.pos.is_valid() {
		if expr_typ := t.get_expr_type(ast.Expr(expr)) {
			if expr_typ is types.SumType {
				sumtype_name = t.type_to_c_name(expr_typ)
			}
		}
	}
	if sumtype_variants.len == 0 && sumtype_name != '' && t.is_sum_type(sumtype_name) {
		sumtype_variants = t.get_sum_type_variants(sumtype_name)
	}
	mut lhs_is_type := t.call_or_cast_lhs_is_type(expr.lhs)
	if !lhs_is_type && expr.lhs is ast.Ident && t.cur_module != '' {
		qualified_lhs := '${t.cur_module}__${expr.lhs.name}'
		if _ := t.lookup_type(qualified_lhs) {
			lhs_is_type = true
		}
	}
	if sumtype_name != '' && lhs_is_type && sumtype_variants.len > 0 {
		if wrapped := t.wrap_sumtype_value_with_variants(expr.expr, sumtype_name, sumtype_variants) {
			return wrapped
		}
		// Fallback for cases where checker typing of `expr.expr` is already widened
		// to the target sum type and direct variant inference fails.
		transformed_sum_arg := t.transform_expr(expr.expr)
		if wrapped := t.wrap_sumtype_value_transformed_with_variants(transformed_sum_arg,
			sumtype_name, sumtype_variants)
		{
			return wrapped
		}
	}
	// If lhs is not a type name, this is a function call - convert to CallExpr
	// This ensures downstream code (cleanc) sees a function call, not a cast
	if expr.lhs is ast.Ident {
		if !t.call_or_cast_lhs_is_type(expr.lhs) {
			mut call_args := []ast.Expr{}
			if expr.expr !is ast.EmptyExpr {
				call_args << expr.expr
			}
			call_args = t.lower_missing_call_args(expr.lhs, call_args)
			coce_fn_info := t.generic_aware_call_fn_info(expr.lhs, expr.lhs.name)
			mut args := []ast.Expr{cap: call_args.len}
			for i, arg in call_args {
				args << t.transform_call_arg_with_sumtype_check(arg, coce_fn_info, i)
			}
			args = t.lower_variadic_args(expr.lhs, args)
			mut call_lhs := t.transform_call_lhs_for_smartcast(expr.lhs)
			if info := coce_fn_info {
				if inferred := t.inferred_generic_call_name(expr.lhs.name, info, call_args) {
					t.register_generic_call_return_type(expr.lhs.name, info, call_args, expr.pos)
					call_lhs = ast.Expr(ast.Ident{
						name: inferred
						pos:  expr.lhs.pos
					})
				}
			}
			return ast.CallExpr{
				lhs:  call_lhs
				args: args
				pos:  expr.pos
			}
		}
	}
	// Method call resolution: rewrite receiver.method(arg) -> Type__method(receiver, arg)
	if expr.lhs is ast.SelectorExpr {
		sel := expr.lhs as ast.SelectorExpr
		mut resolved_module_call_name := ''
		if sel.lhs is ast.Ident {
			lhs_name := (sel.lhs as ast.Ident).name
			if call_prefix := t.resolve_module_call_prefix(lhs_name) {
				if _ := t.lookup_fn_cached(call_prefix, sel.rhs.name) {
					resolved_module_call_name = '${call_prefix}__${sel.rhs.name}'
				}
			}
			if resolved_module_call_name == '' && t.lookup_var_type(lhs_name) == none {
				if resolved_mod := t.resolve_module_name(lhs_name) {
					mut module_names := []string{cap: 2}
					module_names << resolved_mod
					if lhs_name != resolved_mod {
						module_names << lhs_name
					}
					for mod_name in module_names {
						if _ := t.lookup_fn_cached(mod_name, sel.rhs.name) {
							call_mod := if mod_name.contains('.') {
								mod_name.all_after_last('.')
							} else if mod_name.contains('__') {
								mod_name.all_after_last('__')
							} else {
								mod_name
							}
							resolved_module_call_name = '${call_mod}__${sel.rhs.name}'
							break
						}
					}
				}
			}
		}
		if resolved_module_call_name != '' {
			mut call_args := []ast.Expr{}
			if expr.expr !is ast.EmptyExpr {
				call_args << expr.expr
			}
			call_args = t.lower_missing_call_args(expr.lhs, call_args)
			fn_info := t.generic_aware_call_fn_info(expr.lhs, resolved_module_call_name)
			mut args := []ast.Expr{cap: call_args.len}
			for i, arg in call_args {
				args << t.transform_call_arg_with_sumtype_check(arg, fn_info, i)
			}
			args = t.lower_variadic_args(expr.lhs, args)
			mut call_name := resolved_module_call_name
			if info := fn_info {
				if inferred := t.inferred_generic_call_name(call_name, info, call_args) {
					call_name = inferred
				}
			}
			return ast.CallExpr{
				lhs:  ast.Ident{
					name: call_name
				}
				args: args
				pos:  expr.pos
			}
		}
		if resolved_static := t.resolve_static_type_method_call(sel.lhs, sel.rhs.name) {
			mut call_args := []ast.Expr{}
			if expr.expr !is ast.EmptyExpr {
				call_args << expr.expr
			}
			call_args = t.lower_missing_call_args(expr.lhs, call_args)
			fn_info := t.generic_aware_call_fn_info(expr.lhs, resolved_static)
			mut args := []ast.Expr{cap: call_args.len}
			for i, arg in call_args {
				args << t.transform_call_arg_with_sumtype_check(arg, fn_info, i)
			}
			args = t.lower_variadic_args(expr.lhs, args)
			mut call_name := resolved_static
			if info := fn_info {
				if inferred := t.inferred_generic_call_name(call_name, info, call_args) {
					call_name = inferred
				}
			}
			return ast.CallExpr{
				lhs:  ast.Ident{
					name: call_name
				}
				args: args
				pos:  expr.pos
			}
		}
		is_module_call := sel.lhs is ast.Ident && t.lookup_var_type(sel.lhs.name) == none
			&& (t.is_module_ident(sel.lhs.name) || t.get_module_scope(sel.lhs.name) != none)
		mut promoted_args := []ast.Expr{}
		if expr.expr !is ast.EmptyExpr {
			promoted_args << expr.expr
		}
		if embedded_call := t.transform_promoted_embedded_method_call(sel, promoted_args, expr.pos) {
			return embedded_call
		}
		if !is_module_call && !t.resolves_to_embedded_method(sel.lhs, sel.rhs.name) {
			if resolved := t.resolve_method_call_name(sel.lhs, sel.rhs.name) {
				// prepend(arr) → prepend_many(arr.data, arr.len)
				if resolved.ends_with('__prepend') && expr.expr !is ast.EmptyExpr {
					if arg_type := t.get_expr_type(expr.expr) {
						arg_base := t.unwrap_alias_and_pointer_type(arg_type)
						if arg_base is types.Array {
							arr_arg := t.transform_expr(expr.expr)
							return ast.CallExpr{
								lhs:  ast.Ident{
									name: resolved.replace('__prepend', '__prepend_many')
								}
								args: [
									t.transform_expr(sel.lhs),
									ast.Expr(ast.SelectorExpr{
										lhs: arr_arg
										rhs: ast.Ident{
											name: 'data'
										}
									}),
									ast.Expr(ast.SelectorExpr{
										lhs: arr_arg
										rhs: ast.Ident{
											name: 'len'
										}
									}),
								]
								pos:  expr.pos
							}
						}
					}
				}
				// For nested array .repeat(n), use repeat_to_depth with the correct depth
				// so inner arrays are deeply cloned instead of shallow-copied.
				if resolved == 'array__repeat' {
					if recv_type := t.get_expr_type(sel.lhs) {
						depth := t.get_array_nesting_depth(recv_type)
						if depth > 1 {
							return ast.CallExpr{
								lhs:  ast.Ident{
									name: 'array__repeat_to_depth'
								}
								args: [
									t.transform_expr(sel.lhs),
									t.transform_expr(expr.expr),
									ast.Expr(ast.BasicLiteral{
										kind:  .number
										value: '${depth - 1}'
									}),
								]
								pos:  expr.pos
							}
						}
					}
				}
				// insert(i, arr) in single-arg form won't happen (needs 2 args)
				// but handle it for safety
				is_static := t.is_static_method_call(sel.lhs)
				mut call_args := []ast.Expr{}
				if expr.expr !is ast.EmptyExpr {
					call_args << expr.expr
				}
				call_args = t.lower_missing_call_args(expr.lhs, call_args)
				coce_method_fn_info := t.generic_aware_call_fn_info(expr.lhs, resolved)
				mut transformed_call_args := []ast.Expr{cap: call_args.len}
				for i, arg in call_args {
					transformed_call_args << t.transform_call_arg_with_sumtype_check(arg,
						coce_method_fn_info, i)
				}
				transformed_call_args = t.lower_variadic_args(expr.lhs, transformed_call_args)
				mut args := []ast.Expr{cap: transformed_call_args.len + 1}
				if !is_static {
					args << t.transform_method_receiver_arg(sel.lhs, sel.rhs.name, resolved)
				}
				args << transformed_call_args
				mut call_name := resolved
				if info := coce_method_fn_info {
					if receiver_inferred := t.receiver_generic_method_call_name(call_name, sel.lhs,
						info, call_args)
					{
						call_name = receiver_inferred
					} else if inferred := t.inferred_generic_call_name(call_name, info, call_args) {
						call_name = inferred
					}
				} else if receiver_inferred := t.receiver_generic_method_call_name(call_name,
					sel.lhs, CallFnInfo{}, call_args)
				{
					call_name = receiver_inferred
				}
				return ast.CallExpr{
					lhs:  ast.Ident{
						name: call_name
					}
					args: args
					pos:  expr.pos
				}
			}
		}
	}
	mut generic_module_args := []ast.Expr{}
	if expr.expr !is ast.EmptyExpr {
		generic_module_args << expr.expr
	}
	if generic_module_call := t.transform_generic_module_call_from_parts(expr.lhs,
		generic_module_args, expr.pos)
	{
		return generic_module_call
	}
	if expr.lhs is ast.GenericArgs {
		ga := expr.lhs as ast.GenericArgs
		if ga.lhs is ast.Ident {
			lhs := t.specialize_generic_callable_expr(ga.lhs, ga.args, ga.pos)
			args := if expr.expr is ast.EmptyExpr {
				[]ast.Expr{}
			} else {
				t.transform_call_args_for_lhs(lhs, [expr.expr])
			}
			return ast.CallExpr{
				lhs:  lhs
				args: args
				pos:  expr.pos
			}
		}
	}
	if expr.lhs is ast.GenericArgOrIndexExpr {
		gai := expr.lhs as ast.GenericArgOrIndexExpr
		if gai.lhs is ast.Ident {
			lhs := t.specialize_generic_callable_expr(gai.lhs, [gai.expr], gai.pos)
			args := if expr.expr is ast.EmptyExpr {
				[]ast.Expr{}
			} else {
				t.transform_call_args_for_lhs(lhs, [expr.expr])
			}
			return ast.CallExpr{
				lhs:  lhs
				args: args
				pos:  expr.pos
			}
		}
	}
	if expr.lhs is ast.GenericArgs {
		ga := expr.lhs as ast.GenericArgs
		if ga.lhs is ast.SelectorExpr {
			raw_args := if expr.expr is ast.EmptyExpr { []ast.Expr{} } else { [expr.expr] }
			if transformed := t.transform_generic_selector_method_call(ga.lhs as ast.SelectorExpr,
				ga.args, raw_args, expr.pos)
			{
				return transformed
			}
		}
	}
	if expr.lhs is ast.IndexExpr {
		idx := expr.lhs as ast.IndexExpr
		if idx.lhs is ast.SelectorExpr {
			raw_args := if expr.expr is ast.EmptyExpr { []ast.Expr{} } else { [expr.expr] }
			if transformed := t.transform_generic_selector_method_call(idx.lhs as ast.SelectorExpr, [
				idx.expr,
			], raw_args, expr.pos)
			{
				return transformed
			}
		}
	}
	// Generic method call: w.get[T](arg) where LHS is GenericArgOrIndexExpr
	// wrapping a SelectorExpr. Resolve the method call and append the generic
	// specialization suffix so cleanc can later substitute concrete types.
	if expr.lhs is ast.GenericArgOrIndexExpr {
		gai := expr.lhs as ast.GenericArgOrIndexExpr
		if gai.lhs is ast.SelectorExpr {
			sel := gai.lhs as ast.SelectorExpr
			is_module_call := sel.lhs is ast.Ident && t.lookup_var_type(sel.lhs.name) == none
				&& (t.is_module_ident(sel.lhs.name) || t.get_module_scope(sel.lhs.name) != none)
			if !is_module_call {
				suffix := '_T_' + t.generic_specialization_token(gai.expr)
				// When the receiver matches the current method's receiver parameter
				// and get_expr_type would fail (generic body), use the known prefix
				// directly to avoid wrong fallback to 'array__' prefix.
				recv_is_self := t.cur_fn_recv_param != '' && sel.lhs is ast.Ident
					&& (sel.lhs as ast.Ident).name == t.cur_fn_recv_param && t.get_expr_type(sel.lhs) == none
				if recv_is_self && t.cur_fn_recv_prefix != '' {
					mut args2 := []ast.Expr{cap: 2}
					args2 << t.transform_expr(sel.lhs)
					if expr.expr !is ast.EmptyExpr {
						args2 << t.transform_expr(expr.expr)
					}
					return ast.CallExpr{
						lhs:  ast.Ident{
							name: '${t.cur_fn_recv_prefix}__${sel.rhs.name}${suffix}'
						}
						args: args2
						pos:  expr.pos
					}
				}
				method_name := sel.rhs.name + suffix
				if !t.resolves_to_embedded_method(sel.lhs, method_name) {
					if resolved := t.resolve_method_call_name(sel.lhs, method_name) {
						mut args2 := []ast.Expr{cap: 2}
						args2 << t.transform_expr(sel.lhs)
						if expr.expr !is ast.EmptyExpr {
							args2 << t.transform_expr(expr.expr)
						}
						return ast.CallExpr{
							lhs:  ast.Ident{
								name: resolved
							}
							args: args2
							pos:  expr.pos
						}
					}
				}
				if !t.resolves_to_embedded_method(sel.lhs, sel.rhs.name) {
					if resolved := t.resolve_method_call_name(sel.lhs, sel.rhs.name) {
						mut args2 := []ast.Expr{cap: 2}
						args2 << t.transform_expr(sel.lhs)
						if expr.expr !is ast.EmptyExpr {
							args2 << t.transform_expr(expr.expr)
						}
						return ast.CallExpr{
							lhs:  ast.Ident{
								name: resolved + suffix
							}
							args: args2
							pos:  expr.pos
						}
					}
				}
			}
		}
	}
	// Unresolved CallOrCastExpr nodes are still ordinary calls when the lhs is not a
	// type reference. Route them through the normal call-arg lowering pipeline so
	// named args / struct-shorthand args are normalized before codegen.
	if !t.call_or_cast_lhs_is_type(expr.lhs) {
		mut call_args := []ast.Expr{}
		if expr.expr !is ast.EmptyExpr {
			call_args << expr.expr
		}
		call_args = t.lower_missing_call_args(expr.lhs, call_args)
		default_fn_info := t.call_fn_info_for_lhs(expr.lhs)
		mut args := []ast.Expr{cap: call_args.len}
		for i, arg in call_args {
			args << t.transform_call_arg_with_sumtype_check(arg, default_fn_info, i)
		}
		args = t.lower_variadic_args(expr.lhs, args)
		mut call_lhs := t.transform_expr(expr.lhs)
		if expr.lhs is ast.Ident {
			if info := default_fn_info {
				if inferred := t.inferred_generic_call_name(expr.lhs.name, info, call_args) {
					call_lhs = ast.Expr(ast.Ident{
						name: inferred
						pos:  expr.lhs.pos
					})
				}
			}
		}
		return ast.CallExpr{
			lhs:  call_lhs
			args: args
			pos:  expr.pos
		}
	}
	// Default: transform the value expression while preserving the original lhs.
	// For cast-like call-or-cast nodes, the lhs is a type expression rather than
	// a runtime value.
	transformed_arg := t.transform_expr(expr.expr)
	return t.lower_call_or_cast_expr(expr.lhs, transformed_arg, expr.pos)
}

// transform_call_arg_with_sumtype_check transforms a call argument, temporarily
// disabling any active smartcast when the function parameter is a sumtype.
// This prevents smartcast from unwrapping a sumtype value that should be passed as-is.
// It also wraps variant values in sum type init when the parameter expects a
// sum type but the argument is a variant (implicit conversion).
fn (mut t Transformer) transform_call_arg_with_sumtype_check(arg ast.Expr, fn_info ?CallFnInfo, idx int) ast.Expr {
	if info := fn_info {
		if idx < info.param_types.len {
			param_c_name := t.type_to_c_name(info.param_types[idx])
			if param_c_name != '' && t.is_sum_type(param_c_name) {
				if transformed := t.transform_declared_sumtype_value(arg, param_c_name) {
					return transformed
				}
				arg_str := t.expr_to_string(arg)
				if arg_str != '' {
					if ctx := t.find_smartcast_for_expr(arg_str) {
						// Only disable smartcast if parameter expects the SAME sumtype
						// as the arg's original sumtype. This avoids incorrectly removing
						// smartcasts when the parameter type is a DIFFERENT sumtype that
						// happens to be the smartcast variant (e.g., ast.Expr smartcast
						// to ast.Type, where ast.Type is itself a sumtype).
						if t.is_same_sumtype_name(ctx.sumtype, param_c_name) {
							if existing := t.remove_smartcast_for_expr(arg_str) {
								result := t.transform_expr(arg)
								t.push_smartcast_full(existing.expr, existing.variant,
									existing.variant_full, existing.sumtype)
								return result
							}
						}
					}
				}
				if wrapped := t.wrap_sumtype_value(arg, param_c_name) {
					return wrapped
				}
			}
		}
	}
	return t.transform_expr(arg)
}

// get_enum_type get enum type name from an expression
fn (t &Transformer) get_enum_type(expr ast.Expr) string {
	if recv_type := t.get_expr_type(expr) {
		base := t.unwrap_alias_and_pointer_type(recv_type)
		if base is types.Enum {
			return t.type_to_c_name(base)
		}
	}
	if expr is ast.Ident {
		if typ := t.lookup_var_type(expr.name) {
			base := t.unwrap_alias_and_pointer_type(typ)
			if base is types.Enum {
				return t.type_to_c_name(base)
			}
		}
		if typ := t.lookup_type(expr.name) {
			if typ is types.Enum {
				return t.type_to_c_name(typ)
			}
		}
		for _, scope in t.cached_scopes {
			if obj := scope.objects[expr.name] {
				if obj is types.Type {
					obj_type := obj as types.Type
					if obj_type is types.Enum {
						return t.type_to_c_name(obj_type)
					}
				}
			}
		}
	}
	// Fallback: for SelectorExpr (struct field access), resolve via struct field type
	if expr is ast.SelectorExpr {
		if field_type := t.get_struct_field_type(expr) {
			base := t.unwrap_alias_and_pointer_type(field_type)
			if base is types.Enum {
				return t.type_to_c_name(base)
			}
		}
	}
	return ''
}

// is_flag_enum_receiver verifies that a receiver expression is actually a flag
// enum value before rewriting `.has/.all` into bitwise operations.
fn (t &Transformer) is_flag_enum_receiver(receiver ast.Expr, inferred string) bool {
	mut inferred_flag_enum := false
	if inferred != '' {
		if !t.is_flag_enum(inferred) {
			return false
		}
		inferred_flag_enum = true
	}
	// Selector receivers (obj.field) are often more reliably typed through field
	// lookup than position-based env types.
	if receiver is ast.SelectorExpr {
		if field_type := t.get_struct_field_type(receiver) {
			base := t.unwrap_alias_and_pointer_type(field_type)
			if base is types.Enum {
				return base.is_flag
			}
			return false
		}
	}
	if recv_type := t.get_expr_type(receiver) {
		base := t.unwrap_alias_and_pointer_type(recv_type)
		if base is types.Enum {
			return base.is_flag
		}
		// Type information says this is not an enum (e.g. []Attribute.has()).
		return false
	}
	// If type lookup is missing, keep behavior only when enum inference succeeded.
	return inferred_flag_enum
}

// is_cast_type_name checks if a name is a type name that appears in casts (not a function)
fn is_builtin_primitive_cast_name(name string) bool {
	if name in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'f32', 'f64', 'bool',
		'byte', 'char', 'rune', 'usize', 'isize', 'string', 'byteptr', 'charptr', 'voidptr'] {
		return true
	}
	return false
}

// is_cast_type_name checks if a name is a type name that appears in casts (not a function)
fn (t &Transformer) is_cast_type_name(name string) bool {
	if is_builtin_primitive_cast_name(name) {
		return true
	}
	// Type names start with uppercase in V
	if name.len > 0 && name[0] >= `A` && name[0] <= `Z` {
		return true
	}
	return false
}

fn is_c_type_name_for_cast(name string) bool {
	// Keep in sync with cleanc `is_c_type_name`.
	// This list is only used to disambiguate `C.TYPE(x)` casts from `C.fn(x)` calls
	// in `CallOrCastExpr` lowering.
	if name in ['FILE', 'DIR', 'va_list', 'pthread_t', 'pthread_mutex_t', 'pthread_cond_t',
		'pthread_rwlock_t', 'pthread_attr_t', 'stat', 'tm', 'timespec', 'timeval', 'dirent',
		'termios', 'sockaddr', 'sockaddr_in', 'sockaddr_in6', 'sockaddr_un',
		'mach_timebase_info_data_t'] {
		return true
	}
	// Module-qualified C type names (e.g. C.log__Logger) are casts, not calls.
	if name.contains('__') {
		after := name.all_after_last('__')
		if after.len > 0 && after[0] >= `A` && after[0] <= `Z` {
			return true
		}
	}
	return false
}

fn (t &Transformer) call_or_cast_lhs_is_type(lhs ast.Expr) bool {
	match lhs {
		ast.Type {
			return true
		}
		ast.GenericArgs {
			return t.call_or_cast_lhs_is_type(lhs.lhs)
		}
		ast.GenericArgOrIndexExpr {
			return t.call_or_cast_lhs_is_type(lhs.lhs)
		}
		ast.Ident {
			// Built-in primitive types are always casts, never function calls.
			if is_builtin_primitive_cast_name(lhs.name) {
				return true
			}
			// Prefer functions when names clash (even if uppercase).
			if _ := t.get_fn_return_type(lhs.name) {
				return false
			}
			if _ := t.lookup_type(lhs.name) {
				return true
			}
			return t.is_cast_type_name(lhs.name)
		}
		ast.SelectorExpr {
			if lhs.lhs is ast.Ident {
				mod := (lhs.lhs as ast.Ident).name
				typ_name := lhs.rhs.name
				if mod == 'C' {
					return is_c_type_name_for_cast(typ_name)
				}
				qualified := '${mod}__${typ_name}'
				if _ := t.get_fn_return_type(qualified) {
					return false
				}
				if resolved_mod := t.resolve_module_name(mod) {
					resolved_qualified := '${resolved_mod.replace('.', '__')}__${typ_name}'
					if _ := t.get_fn_return_type(resolved_qualified) {
						return false
					}
				}
				return t.lookup_type(qualified) != none
			}
			return false
		}
		ast.ParenExpr {
			return t.call_or_cast_lhs_is_type(lhs.expr)
		}
		ast.ModifierExpr {
			return t.call_or_cast_lhs_is_type(lhs.expr)
		}
		ast.PrefixExpr {
			return t.call_or_cast_lhs_is_type(lhs.expr)
		}
		else {
			return false
		}
	}
}

fn (t &Transformer) call_or_cast_lhs_is_type_cursor(lhs ast.Cursor) bool {
	if !lhs.is_valid() {
		return false
	}
	match lhs.kind() {
		.typ_anon_struct, .typ_array_fixed, .typ_array, .typ_channel, .typ_fn, .typ_generic,
		.typ_map, .typ_nil, .typ_none, .typ_option, .typ_pointer, .typ_result, .typ_thread,
		.typ_tuple {
			return true
		}
		.expr_ident {
			name := lhs.name()
			if is_builtin_primitive_cast_name(name) {
				return true
			}
			if _ := t.get_fn_return_type(name) {
				return false
			}
			if _ := t.lookup_type(name) {
				return true
			}
			return t.is_cast_type_name(name)
		}
		.expr_selector {
			base := lhs.edge(0)
			if base.kind() != .expr_ident {
				return false
			}
			mod := base.name()
			typ_name := selector_rhs_name_cursor(lhs)
			if typ_name == '' {
				return false
			}
			if mod == 'C' {
				return is_c_type_name_for_cast(typ_name)
			}
			qualified := '${mod}__${typ_name}'
			if _ := t.get_fn_return_type(qualified) {
				return false
			}
			if resolved_mod := t.resolve_module_name(mod) {
				resolved_qualified := '${resolved_mod.replace('.', '__')}__${typ_name}'
				if _ := t.get_fn_return_type(resolved_qualified) {
					return false
				}
			}
			return t.lookup_type(qualified) != none
		}
		.expr_paren, .expr_modifier, .expr_prefix {
			return t.call_or_cast_lhs_is_type_cursor(lhs.edge(0))
		}
		else {
			return false
		}
	}
}

fn (t &Transformer) lower_call_or_cast_expr(lhs ast.Expr, arg ast.Expr, pos token.Pos) ast.Expr {
	// `CallOrCastExpr` with no argument is always a call (`foo()`), never a cast.
	if arg is ast.EmptyExpr {
		return ast.CallExpr{
			lhs:  lhs
			args: []ast.Expr{}
			pos:  pos
		}
	}
	if t.call_or_cast_lhs_is_type(lhs) {
		return ast.CastExpr{
			typ:  lhs
			expr: arg
			pos:  pos
		}
	}
	return ast.CallExpr{
		lhs:  lhs
		args: [arg]
		pos:  pos
	}
}

// get_call_return_type returns the return type of a function call
fn (t &Transformer) get_call_return_type(expr ast.Expr) string {
	// Prefer checker-computed expression type when available.
	// This is the most reliable source for local/private function returns.
	if expr_type := t.resolve_expr_type(expr) {
		c_name := t.type_to_c_name(expr_type)
		if c_name != '' && c_name != 'void' {
			return c_name
		}
		name := expr_type.name()
		if name != '' {
			return name
		}
	}
	if ret_type := t.fn_pointer_call_return_type(expr) {
		c_name := t.type_to_c_name(ret_type)
		if c_name != '' && c_name != 'void' {
			return c_name
		}
		name := ret_type.name()
		if name != '' {
			return name
		}
	}

	mut fn_name := ''
	mut is_method := false
	mut is_module_fn := false
	mut mod_name := ''
	mut selector_expr := ast.SelectorExpr{}
	if expr is ast.CallExpr {
		if expr.lhs is ast.Ident {
			fn_name = expr.lhs.name
		} else if expr.lhs is ast.SelectorExpr {
			// Method call or module.function call
			selector_expr = expr.lhs as ast.SelectorExpr
			fn_name = selector_expr.rhs.name
			// Check if LHS is a module name (starts with lowercase and no field access)
			if selector_expr.lhs is ast.Ident {
				lhs_name := (selector_expr.lhs as ast.Ident).name
				// Check if it's a module by looking it up
				if t.get_module_scope(lhs_name) != none {
					is_module_fn = true
					mod_name = lhs_name
				} else {
					is_method = true
				}
			} else {
				is_method = true
			}
		}
	} else if expr is ast.CallOrCastExpr {
		if expr.lhs is ast.Ident {
			fn_name = expr.lhs.name
		} else if expr.lhs is ast.SelectorExpr {
			selector_expr = expr.lhs as ast.SelectorExpr
			fn_name = selector_expr.rhs.name
			// Check if LHS is a module name
			if selector_expr.lhs is ast.Ident {
				lhs_name := (selector_expr.lhs as ast.Ident).name
				if t.get_module_scope(lhs_name) != none {
					is_module_fn = true
					mod_name = lhs_name
				} else {
					is_method = true
				}
			} else {
				is_method = true
			}
		}
	}
	if fn_name != '' {
		// Check function return type using scope lookup
		if is_module_fn && mod_name != '' {
			// Look up function in the specific module's scope
			if mut mod_scope := t.get_module_scope(mod_name) {
				if obj := mod_scope.lookup_parent(fn_name, 0) {
					if obj is types.Fn {
						fn_typ := obj.get_typ()
						if fn_typ is types.FnType {
							if ret := fn_typ.get_return_type() {
								return ret.name()
							}
						}
					}
				}
			}
		} else {
			// Look up in current module
			if ret_type := t.get_fn_return_type(fn_name) {
				return ret_type.name()
			}
		}
		// For method calls, try to look up the method's return type from the receiver
		if is_method && selector_expr.lhs !is ast.EmptyExpr {
			// Get receiver type and look up method
			if recv_type := t.get_expr_type(selector_expr.lhs) {
				// Get the base type name for method lookup
				base_type := if recv_type is types.Pointer { recv_type.base_type } else { recv_type }
				type_name := base_type.name()
				// Look up method using environment
				if fn_typ := t.lookup_method_cached(type_name, fn_name) {
					if ret := fn_typ.get_return_type() {
						return ret.name()
					}
				}
			}
		}
	}
	return ''
}

// --- Generic math function inlining for native backends ---

// try_inline_generic_math_call handles CallExpr (multi-arg calls):
// - min(a, b) → if a < b { a } else { b }
// - max(a, b) → if a > b { a } else { b }
// - maxof[T]() / minof[T]() → numeric constant
fn (mut t Transformer) try_inline_generic_math_call(expr ast.CallExpr) ?ast.Expr {
	// Handle maxof[T]() and minof[T]() - these have GenericArgs lhs
	if expr.lhs is ast.GenericArgs && expr.args.len == 0 {
		ga := expr.lhs as ast.GenericArgs
		if ga.lhs is ast.Ident && ga.args.len == 1 {
			fn_name := (ga.lhs as ast.Ident).name
			type_name := ga.args[0].name()
			if fn_name == 'maxof' {
				if val := t.maxof_constant(type_name) {
					return val
				}
			} else if fn_name == 'minof' {
				if val := t.minof_constant(type_name) {
					return val
				}
			}
		}
	}
	// Handle maxof[T]() and minof[T]() - parser may produce GenericArgOrIndexExpr for single arg
	if expr.lhs is ast.GenericArgOrIndexExpr && expr.args.len == 0 {
		gaoi := expr.lhs as ast.GenericArgOrIndexExpr
		if gaoi.lhs is ast.Ident {
			fn_name := (gaoi.lhs as ast.Ident).name
			type_name := gaoi.expr.name()
			if fn_name == 'maxof' {
				if val := t.maxof_constant(type_name) {
					return val
				}
			} else if fn_name == 'minof' {
				if val := t.minof_constant(type_name) {
					return val
				}
			}
		}
	}
	// Handle abs(x) - single-arg call (may be CallExpr within math module)
	if expr.lhs is ast.Ident && expr.args.len == 1 {
		name := (expr.lhs as ast.Ident).name
		if name == 'abs' {
			arg := t.transform_expr(expr.args[0])
			pos := t.inline_generic_math_result_pos(expr.pos, expr.args[0])
			return t.make_inline_abs_expr(arg, pos)
		}
	}
	// Handle math.abs(x) - module-qualified single-arg call
	if expr.lhs is ast.SelectorExpr && expr.args.len == 1 {
		sel := expr.lhs as ast.SelectorExpr
		if sel.lhs is ast.Ident && (sel.lhs as ast.Ident).name == 'math' && sel.rhs.name == 'abs' {
			arg := t.transform_expr(expr.args[0])
			pos := t.inline_generic_math_result_pos(expr.pos, expr.args[0])
			return t.make_inline_abs_expr(arg, pos)
		}
	}
	// Handle min(a, b) and max(a, b) - bare ident calls within math module
	if expr.lhs is ast.Ident && expr.args.len == 2 {
		name := (expr.lhs as ast.Ident).name
		if name == 'min' || name == 'max' {
			a := t.transform_expr(expr.args[0])
			b := t.transform_expr(expr.args[1])
			op := if name == 'min' { token.Token.lt } else { token.Token.gt }
			return t.make_inline_if_expr(a, b, op)
		}
	}
	// Handle math.min(a, b) and math.max(a, b) - module-qualified calls
	if expr.lhs is ast.SelectorExpr && expr.args.len == 2 {
		sel := expr.lhs as ast.SelectorExpr
		if sel.lhs is ast.Ident && (sel.lhs as ast.Ident).name == 'math' {
			name := sel.rhs.name
			if name == 'min' || name == 'max' {
				a := t.transform_expr(expr.args[0])
				b := t.transform_expr(expr.args[1])
				op := if name == 'min' { token.Token.lt } else { token.Token.gt }
				return t.make_inline_if_expr(a, b, op)
			}
		}
	}
	// Handle already-resolved maxof_T() and minof_T() names (0-arg calls)
	if expr.lhs is ast.Ident && expr.args.len == 0 {
		name := (expr.lhs as ast.Ident).name
		if name.starts_with('maxof_') {
			type_name := name[6..]
			if val := t.maxof_constant(type_name) {
				return val
			}
		} else if name.starts_with('minof_') {
			type_name := name[6..]
			if val := t.minof_constant(type_name) {
				return val
			}
		}
	}
	return none
}

// try_inline_generic_math_coce handles CallOrCastExpr (single-arg calls):
// - abs(x) → if x < 0 { -x } else { x }
fn (mut t Transformer) try_inline_generic_math_coce(expr ast.CallOrCastExpr) ?ast.Expr {
	// Handle maxof[T]() / minof[T]() when parsed as CallOrCastExpr
	if expr.lhs is ast.GenericArgOrIndexExpr && expr.expr is ast.EmptyExpr {
		gaoi := expr.lhs as ast.GenericArgOrIndexExpr
		if gaoi.lhs is ast.Ident {
			fn_name := (gaoi.lhs as ast.Ident).name
			type_name := gaoi.expr.name()
			if fn_name == 'maxof' {
				if val := t.maxof_constant(type_name) {
					return val
				}
			} else if fn_name == 'minof' {
				if val := t.minof_constant(type_name) {
					return val
				}
			}
		}
	}
	if expr.lhs is ast.GenericArgs && expr.expr is ast.EmptyExpr {
		ga := expr.lhs as ast.GenericArgs
		if ga.lhs is ast.Ident && ga.args.len == 1 {
			fn_name := (ga.lhs as ast.Ident).name
			type_name := ga.args[0].name()
			if fn_name == 'maxof' {
				if val := t.maxof_constant(type_name) {
					return val
				}
			} else if fn_name == 'minof' {
				if val := t.minof_constant(type_name) {
					return val
				}
			}
		}
	}
	if expr.lhs is ast.Ident {
		name := (expr.lhs as ast.Ident).name
		if name == 'abs' && expr.expr !is ast.EmptyExpr {
			arg := t.transform_expr(expr.expr)
			pos := t.inline_generic_math_result_pos(expr.pos, expr.expr)
			return t.make_inline_abs_expr(arg, pos)
		}
	}
	// Handle math.abs(x)
	if expr.lhs is ast.SelectorExpr {
		sel := expr.lhs as ast.SelectorExpr
		if sel.lhs is ast.Ident
			&& (sel.lhs as ast.Ident).name == 'math' && sel.rhs.name == 'abs' && expr.expr !is ast.EmptyExpr {
			arg := t.transform_expr(expr.expr)
			pos := t.inline_generic_math_result_pos(expr.pos, expr.expr)
			return t.make_inline_abs_expr(arg, pos)
		}
	}
	return none
}

// make_inline_if_expr creates: if a OP b { a } else { b }
fn (mut t Transformer) make_inline_if_expr(a ast.Expr, b ast.Expr, op token.Token) ast.Expr {
	return ast.IfExpr{
		cond:      t.make_infix_expr(op, a, b)
		stmts:     [
			ast.Stmt(ast.ExprStmt{
				expr: a
			}),
		]
		else_expr: ast.IfExpr{
			stmts: [
				ast.Stmt(ast.ExprStmt{
					expr: b
				}),
			]
		}
	}
}

fn (t &Transformer) maxof_constant(type_name string) ?ast.Expr {
	val := match type_name {
		'i8' { '127' }
		'i16' { '32767' }
		'i32' { '2147483647' }
		'int' { '2147483647' }
		'i64' { '9223372036854775807' }
		'u8' { '255' }
		'byte' { '255' }
		'u16' { '65535' }
		'u32' { '4294967295' }
		'u64' { '18446744073709551615' }
		'f32' { '3.40282346638528859811704183484516925440e+38' }
		'f64' { '1.797693134862315708145274237317043567981e+308' }
		else { return none }
	}

	return ast.Expr(ast.BasicLiteral{
		kind:  .number
		value: val
	})
}

fn (mut t Transformer) minof_constant(type_name string) ?ast.Expr {
	// For i64 min, use subtraction expression to avoid C literal overflow:
	// -9223372036854775807 - 1 (same pattern as C's LLONG_MIN definition)
	if type_name == 'i64' {
		return t.make_infix_expr(.minus, ast.Expr(ast.PrefixExpr{
			op:   .minus
			expr: ast.BasicLiteral{
				kind:  .number
				value: '9223372036854775807'
			}
		}), t.make_number_expr('1'))
	}
	val := match type_name {
		'i8' { '-128' }
		'i16' { '-32768' }
		'i32' { '-2147483648' }
		'int' { '-2147483648' }
		'u8' { '0' }
		'byte' { '0' }
		'u16' { '0' }
		'u32' { '0' }
		'u64' { '0' }
		'f32' { '-3.40282346638528859811704183484516925440e+38' }
		'f64' { '-1.797693134862315708145274237317043567981e+308' }
		else { return none }
	}

	return ast.Expr(ast.BasicLiteral{
		kind:  .number
		value: val
	})
}
