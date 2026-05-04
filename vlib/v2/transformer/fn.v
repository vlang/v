// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module transformer

import v2.ast
import v2.token
import v2.types

// get_fn_type gets the signature for a function.
fn (t &Transformer) get_fn_type(fn_name string) ?types.FnType {
	// First try the current module scope.
	if mut scope := t.get_module_scope(t.cur_module) {
		if obj := scope.lookup_parent(fn_name, 0) {
			if obj is types.Fn {
				fn_typ := obj.get_typ()
				if fn_typ is types.FnType {
					return fn_typ
				}
			}
		}
	}
	// Try builtin scope directly (many common functions are here).
	if t.cur_module != 'builtin' {
		if mut scope := t.get_module_scope('builtin') {
			if obj := scope.lookup_parent(fn_name, 0) {
				if obj is types.Fn {
					fn_typ := obj.get_typ()
					if fn_typ is types.FnType {
						return fn_typ
					}
				}
			}
		}
	}
	// Fallback: scan all module scopes for local/private functions.
	scope_keys := t.cached_scopes.keys()
	for sk in scope_keys {
		scope_ptr := t.cached_scopes[sk] or { continue }
		mut scope := unsafe { scope_ptr }
		if obj := scope.lookup_parent(fn_name, 0) {
			if obj is types.Fn {
				fn_typ := obj.get_typ()
				if fn_typ is types.FnType {
					return fn_typ
				}
			}
		}
	}
	return none
}

// get_fn_return_type gets the return type for a function.
fn (t &Transformer) get_fn_return_type(fn_name string) ?types.Type {
	fn_type := t.get_fn_type(fn_name) or { return none }
	return fn_type.get_return_type()
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

fn (t &Transformer) enum_type_name_for_return_value(typ types.Type) string {
	match typ {
		types.OptionType {
			return t.enum_type_name_for_return_value(typ.base_type)
		}
		types.ResultType {
			return t.enum_type_name_for_return_value(typ.base_type)
		}
		types.Enum {
			return t.type_to_c_name(typ)
		}
		else {
			return ''
		}
	}
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
	if wrapper_type := t.channel_receive_wrapper_type(expr) {
		return wrapper_type
	}
	if ret_type := t.resolve_call_return_type(expr) {
		if ret_type is types.OptionType || ret_type is types.ResultType {
			return ret_type
		}
	}
	if ret_type := t.get_method_return_type(expr) {
		if ret_type is types.OptionType || ret_type is types.ResultType {
			return ret_type
		}
	}
	ret_type_name := t.get_call_return_type(expr)
	if ret_type_name.starts_with('_option_') {
		base_name := ret_type_name['_option_'.len..]
		if base_type := t.c_return_base_type_to_type(base_name) {
			return types.Type(types.OptionType{
				base_type: base_type
			})
		}
	} else if ret_type_name.starts_with('_result_') {
		base_name := ret_type_name['_result_'.len..]
		if base_type := t.c_return_base_type_to_type(base_name) {
			return types.Type(types.ResultType{
				base_type: base_type
			})
		}
	}
	if typ := t.get_expr_type(expr) {
		if typ is types.OptionType || typ is types.ResultType {
			return typ
		}
	}
	return none
}

fn (t &Transformer) c_return_base_type_to_type(type_name string) ?types.Type {
	mut name := type_name
	mut is_ptr := false
	if name.ends_with('*') {
		name = name.trim_right('*')
		is_ptr = true
	} else if name.ends_with('ptr') && name !in ['voidptr', 'charptr', 'byteptr'] {
		name = name[..name.len - 3]
		is_ptr = true
	}
	base_type := t.c_name_to_type(name) or { return none }
	if is_ptr {
		return types.Type(types.Pointer{
			base_type: base_type
		})
	}
	return base_type
}

// extract_return_sumtype_name extracts the base sumtype name from a return type AST node.
// For ?SumType (OptionType) or !SumType (ResultType), returns the base type name.
fn (t &Transformer) extract_return_sumtype_name(return_type ast.Expr) string {
	if return_type is ast.Type {
		return t.extract_base_type_name_from_type(return_type)
	}
	return ''
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
		return expr.name
	}
	if expr is ast.SelectorExpr {
		if expr.lhs is ast.Ident {
			lhs_ident := expr.lhs as ast.Ident
			qualified := '${lhs_ident.name}__${expr.rhs.name}'
			if t.is_sum_type(qualified) {
				return qualified
			}
		}
		return expr.rhs.name
	}
	return ''
}

// get_method_return_type tries to get the return type for a method call.
// Returns the return type if found, none otherwise.
fn (t &Transformer) get_method_return_type(expr ast.Expr) ?types.Type {
	fn_type := t.get_method_fn_type(expr) or { return none }
	return fn_type.get_return_type()
}

fn (t &Transformer) get_method_selector(expr ast.Expr) ?ast.SelectorExpr {
	// Check if this is a method call (CallExpr or CallOrCastExpr with SelectorExpr lhs)
	if expr is ast.CallExpr {
		call_lhs := t.unwrap_call_target_lhs(expr.lhs)
		if call_lhs is ast.SelectorExpr {
			return call_lhs as ast.SelectorExpr
		}
	} else if expr is ast.CallOrCastExpr {
		call_lhs := t.unwrap_call_target_lhs(expr.lhs)
		if call_lhs is ast.SelectorExpr {
			return call_lhs as ast.SelectorExpr
		}
	}
	return none
}

fn (t &Transformer) get_method_fn_type(expr ast.Expr) ?types.FnType {
	sel_expr := t.get_method_selector(expr) or { return none }
	method_name := sel_expr.rhs.name
	mut lookup_type_names := []string{}
	// Get the receiver type from the checker's stored types.
	if receiver_type := t.resolve_expr_type(sel_expr.lhs) {
		t.append_method_lookup_type_name(mut lookup_type_names, receiver_type.name())
		base_type := t.unwrap_alias_and_pointer_type(receiver_type)
		t.append_method_lookup_type_name(mut lookup_type_names, base_type.name())
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
	return t.lookup_method_fn_type(lookup_type_names, method_name)
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

fn (t &Transformer) resolve_call_return_type(expr ast.Expr) ?types.Type {
	mut call_lhs := ast.empty_expr
	if expr is ast.CallExpr {
		call_lhs = t.unwrap_call_target_lhs(expr.lhs)
	} else if expr is ast.CallOrCastExpr {
		call_lhs = t.unwrap_call_target_lhs(expr.lhs)
	} else {
		return none
	}
	if lhs_type := t.get_expr_type(call_lhs) {
		if ret_type := callable_return_type(lhs_type) {
			return ret_type
		}
	}
	match call_lhs {
		ast.Ident {
			ident := call_lhs as ast.Ident
			if var_type := t.lookup_var_type(ident.name) {
				if ret_type := callable_return_type(var_type) {
					return ret_type
				}
			}
			return t.get_fn_return_type(ident.name)
		}
		ast.SelectorExpr {
			sel := call_lhs as ast.SelectorExpr
			if sel.lhs is ast.Ident {
				mod_name := (sel.lhs as ast.Ident).name
				if mut mod_scope := t.get_module_scope(mod_name) {
					if obj := mod_scope.lookup_parent(sel.rhs.name, 0) {
						if obj is types.Fn {
							fn_typ := obj.get_typ()
							if fn_typ is types.FnType {
								if ret_type := fn_typ.get_return_type() {
									return ret_type
								}
							}
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

fn callable_return_type(typ types.Type) ?types.Type {
	mut cur := typ
	for cur is types.Pointer {
		cur = (cur as types.Pointer).base_type
	}
	if cur is types.Alias {
		base := cur.base_type
		if base is types.FnType {
			if ret_type := base.get_return_type() {
				return ret_type
			}
		}
	}
	if cur is types.FnType {
		if ret_type := cur.get_return_type() {
			return ret_type
		}
	}
	return none
}

fn (t &Transformer) append_method_lookup_type_name(mut names []string, raw_name string) {
	if raw_name == '' {
		return
	}
	mut normalized := raw_name.replace('.', '__')
	if normalized.starts_with('&') {
		normalized = normalized[1..]
	}
	if normalized.ends_with('*') {
		normalized = normalized[..normalized.len - 1]
	}
	if normalized == '' {
		return
	}
	names << normalized
	if normalized.contains('__') {
		names << normalized.all_after_last('__')
	}
}

fn (t &Transformer) method_key_matches_type_name(method_key string, type_name string) bool {
	if method_key == '' || type_name == '' {
		return false
	}
	normalized_key := method_key.replace('.', '__')
	normalized_type := type_name.replace('.', '__')
	if normalized_key == normalized_type {
		return true
	}
	short_type := if normalized_type.contains('__') {
		normalized_type.all_after_last('__')
	} else {
		normalized_type
	}
	short_key := if normalized_key.contains('__') {
		normalized_key.all_after_last('__')
	} else {
		normalized_key
	}
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

fn (t &Transformer) lookup_method_fn_type(type_names []string, method_name string) ?types.FnType {
	if method_name == '' {
		return none
	}
	mut seen := map[string]bool{}
	for raw_name in type_names {
		if raw_name == '' {
			continue
		}
		if raw_name in seen {
			continue
		}
		seen[raw_name] = true
		if fn_type := t.lookup_method_cached(raw_name, method_name) {
			return fn_type
		}
	}
	mkeys := t.cached_methods.keys()
	for key in mkeys {
		mut matches_receiver := false
		for type_name in seen.keys() {
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
				return method_typ
			}
		}
	}
	return none
}

fn (t &Transformer) lookup_method_return_type(type_names []string, method_name string) ?types.Type {
	fn_type := t.lookup_method_fn_type(type_names, method_name) or { return none }
	return fn_type.get_return_type()
}

fn (t &Transformer) lookup_mangled_method_fn_type(fn_name string) ?types.FnType {
	if !fn_name.contains('__') {
		return none
	}
	method_name := fn_name.all_after_last('__')
	receiver_name := fn_name.all_before_last('__')
	mut lookup_type_names := []string{}
	t.append_method_lookup_type_name(mut lookup_type_names, receiver_name)
	return t.lookup_method_fn_type(lookup_type_names, method_name)
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

fn (t &Transformer) smartcast_variant_method_name(ctx SmartcastContext, method_name string) ?string {
	if method_name == '' {
		return none
	}
	if t.lookup_method_cached(ctx.variant, method_name) == none
		&& t.lookup_method_cached(ctx.variant_full, method_name) == none {
		return none
	}
	variant_prefix := if ctx.variant_full.contains('__') {
		ctx.variant_full
	} else if t.cur_module != '' && t.cur_module != 'main' && t.cur_module != 'builtin'
		&& ctx.variant_full !in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'byte', 'rune', 'f32', 'f64', 'usize', 'isize', 'bool', 'string', 'voidptr', 'charptr', 'byteptr'] {
		'${t.cur_module}__${ctx.variant_full}'
	} else {
		ctx.variant_full
	}
	if variant_prefix == '' {
		return none
	}
	return '${variant_prefix}__${method_name}'
}

fn (t &Transformer) smartcast_context_for_method_receiver(receiver ast.Expr) ?SmartcastContext {
	receiver_str := t.expr_to_string(receiver)
	if receiver_str != '' {
		if ctx := t.find_smartcast_for_expr(receiver_str) {
			return ctx
		}
	}
	if receiver is ast.SelectorExpr {
		cast_base_expr := t.as_cast_base_expr(receiver.lhs) or { return none }
		cast_base := t.expr_to_string(cast_base_expr)
		if cast_base != '' {
			normalized_receiver := '${cast_base}.${receiver.rhs.name}'
			if ctx := t.find_smartcast_for_expr(normalized_receiver) {
				return ctx
			}
		}
	}
	return none
}

fn (t &Transformer) as_cast_base_expr(expr ast.Expr) ?ast.Expr {
	if expr is ast.AsCastExpr {
		return expr.expr
	}
	if expr is ast.ParenExpr {
		return t.as_cast_base_expr(expr.expr)
	}
	if expr is ast.ModifierExpr {
		return t.as_cast_base_expr(expr.expr)
	}
	return none
}

// resolve_expr_type resolves the type of an expression, falling back to scope
// lookup when the checker didn't store a type at the expression's position.
fn (t &Transformer) resolve_expr_type(expr ast.Expr) ?types.Type {
	if !expr_has_valid_data(expr) {
		return none
	}
	if expr is ast.ParenExpr {
		return t.resolve_expr_type(expr.expr)
	}
	if expr is ast.ModifierExpr {
		return t.resolve_expr_type(expr.expr)
	}
	if expr is ast.AsCastExpr {
		return t.infer_decl_type_from_type_expr(expr.typ)
	}
	if expr is ast.CastExpr {
		return t.infer_decl_type_from_type_expr(expr.typ)
	}
	if expr is ast.CallExpr || expr is ast.CallOrCastExpr {
		if ret_type := t.get_method_return_type(expr) {
			return ret_type
		}
		fn_name := t.get_call_fn_name(expr)
		if fn_name != '' {
			if ret_type := t.get_fn_return_type(fn_name) {
				return ret_type
			}
		}
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
	if expr is ast.SelectorExpr {
		if field_type := t.get_struct_field_type(expr) {
			return field_type
		}
		return none
	}
	if expr is ast.IndexExpr {
		// For slices (a[x..y]), the result type is the same as the container
		if expr.expr is ast.RangeExpr {
			if resolved_type := t.resolve_expr_type(expr.lhs) {
				return resolved_type
			}
		} else if resolved_type := t.resolve_expr_type(expr.lhs) {
			base := t.unwrap_alias_and_pointer_type(resolved_type)
			match base {
				types.Array {
					return base.elem_type
				}
				types.ArrayFixed {
					return base.elem_type
				}
				types.Map {
					return base.value_type
				}
				types.String {
					if u8_type := t.lookup_type('u8') {
						return u8_type
					}
					return types.Type(types.int_)
				}
				else {}
			}
		}
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
	if wrapper_type := t.expr_wrapper_type_for_or(expr) {
		return wrapper_type is types.ResultType
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
	return match expr {
		ast.CallExpr {
			true
		}
		ast.CastExpr {
			t.contains_call_expr(expr.expr)
		}
		ast.ParenExpr {
			t.contains_call_expr(expr.expr)
		}
		ast.CallOrCastExpr {
			t.contains_call_expr(expr.expr)
		}
		ast.PrefixExpr {
			t.contains_call_expr(expr.expr)
		}
		ast.PostfixExpr {
			t.contains_call_expr(expr.expr)
		}
		ast.InfixExpr {
			t.contains_call_expr(expr.lhs) || t.contains_call_expr(expr.rhs)
		}
		ast.OrExpr {
			t.contains_call_expr(expr.expr)
		}
		ast.ArrayInitExpr {
			mut has_call := false
			for e in expr.exprs {
				if t.contains_call_expr(e) {
					has_call = true
					break
				}
			}
			has_call = has_call || (expr.init !is ast.EmptyExpr && t.contains_call_expr(expr.init))
			has_call = has_call || (expr.len !is ast.EmptyExpr && t.contains_call_expr(expr.len))
			has_call = has_call || (expr.cap !is ast.EmptyExpr && t.contains_call_expr(expr.cap))
			has_call
		}
		ast.InitExpr {
			mut has_call := false
			for field in expr.fields {
				if t.contains_call_expr(field.value) {
					has_call = true
					break
				}
			}
			has_call
		}
		ast.MapInitExpr {
			mut has_call := false
			for key in expr.keys {
				if t.contains_call_expr(key) {
					has_call = true
					break
				}
			}
			if !has_call {
				for val in expr.vals {
					if t.contains_call_expr(val) {
						has_call = true
						break
					}
				}
			}
			has_call
		}
		ast.SelectorExpr {
			t.contains_call_expr(expr.lhs)
		}
		ast.IndexExpr {
			t.contains_call_expr(expr.lhs) || t.contains_call_expr(expr.expr)
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

fn is_void_type(typ types.Type) bool {
	ret_name := typ.name()
	return ret_name == '' || ret_name == 'void' || ret_name == 'Void'
}

// is_void_call_expr checks if an expression is a function call that returns void.
// Used to detect or-blocks that end with a void call (e.g. error_with_pos()).
fn (mut t Transformer) is_void_call_expr(expr ast.Expr) bool {
	fn_name := t.get_call_fn_name(expr)
	if fn_name == '' {
		return false
	}
	if fn_type := t.get_method_fn_type(expr) {
		if ret := fn_type.get_return_type() {
			return is_void_type(ret)
		}
		return true
	}
	if _ := t.get_method_selector(expr) {
		return false
	}
	if fn_type := t.get_fn_type(fn_name) {
		if ret := fn_type.get_return_type() {
			return is_void_type(ret)
		}
		return true
	}
	if fn_type := t.lookup_mangled_method_fn_type(fn_name) {
		if ret := fn_type.get_return_type() {
			return is_void_type(ret)
		}
		return true
	}
	// No signature found. Keep unknown calls as value expressions; otherwise
	// `x = opt() or { fallback_call() }` discards the fallback and reads the
	// failed option payload.
	return false
}

fn unique_c_param_name(raw_name string, index int, mut seen map[string]bool) string {
	mut name := raw_name
	if name == '_' {
		name = '_v_blank_param_${index}'
	}
	if name == '' {
		return name
	}
	if name !in seen {
		seen[name] = true
		return name
	}
	mut suffix := index
	for {
		candidate := '${name}_${suffix}'
		if candidate !in seen {
			seen[candidate] = true
			return candidate
		}
		suffix++
	}
	return name
}

fn normalize_blank_fn_parameters(decl ast.FnDecl) ast.FnDecl {
	mut seen := map[string]bool{}
	mut changed := false
	mut receiver := decl.receiver
	if receiver.name != '' {
		new_name := unique_c_param_name(receiver.name, 0, mut seen)
		if new_name != receiver.name {
			changed = true
			receiver = ast.Parameter{
				name:   new_name
				typ:    receiver.typ
				is_mut: receiver.is_mut
				pos:    receiver.pos
			}
		}
	}
	mut params := []ast.Parameter{cap: decl.typ.params.len}
	for i, param in decl.typ.params {
		new_name := unique_c_param_name(param.name, i + 1, mut seen)
		if new_name != param.name {
			changed = true
			params << ast.Parameter{
				name:   new_name
				typ:    param.typ
				is_mut: param.is_mut
				pos:    param.pos
			}
		} else {
			params << param
		}
	}
	if !changed {
		return decl
	}
	return ast.FnDecl{
		attributes: decl.attributes
		is_public:  decl.is_public
		is_method:  decl.is_method
		is_static:  decl.is_static
		receiver:   receiver
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

fn (mut t Transformer) transform_fn_decl(input_decl ast.FnDecl) ast.FnDecl {
	decl := normalize_blank_fn_parameters(input_decl)
	// Skip uninstantiated generic functions - their bodies were never type-checked
	// and they will never be called, so emit an empty body.
	if decl.typ.generic_params.len > 0 {
		mut has_generic_types := decl.name in t.env.generic_types
		if !has_generic_types {
			for key, _ in t.env.generic_types {
				if key.starts_with('${decl.name}[') || key.contains('.${decl.name}[')
					|| key.ends_with('.${decl.name}') {
					has_generic_types = true
					break
				}
			}
		}
		if !has_generic_types {
			return ast.FnDecl{
				attributes: decl.attributes
				is_public:  decl.is_public
				is_method:  decl.is_method
				is_static:  decl.is_static
				receiver:   decl.receiver
				language:   decl.language
				name:       decl.name
				typ:        decl.typ
				stmts:      []
				pos:        decl.pos
			}
		}
	}
	// Check for conditional compilation attributes (e.g., @[if verbose ?])
	// Skip functions whose conditions evaluate to false, and mark them for call elision
	for attr in decl.attributes {
		if attr.comptime_cond !is ast.EmptyExpr {
			if !t.eval_comptime_cond(attr.comptime_cond) {
				t.elided_fns[decl.name] = true
				return ast.FnDecl{
					attributes: decl.attributes
					is_public:  decl.is_public
					is_method:  decl.is_method
					is_static:  decl.is_static
					receiver:   decl.receiver
					language:   decl.language
					name:       decl.name
					typ:        decl.typ
					stmts:      []
					pos:        decl.pos
				}
			}
		}
	}

	// Detect @[live] functions for hot code reloading
	mut live_fn_detected := false
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
			live_fn_detected = true
		}
	}

	// Save current scope and fn_root_scope
	old_scope := t.scope
	old_fn_root_scope := t.fn_root_scope

	// Get the function's scope from the environment (populated by checker)
	// This contains parameter types, receiver type, and local variables
	// For methods, include receiver type in the key (e.g., "SortedMap__set")
	// The key must match how the checker generates it (using resolved/base type)
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
	if fn_scope := t.cached_fn_scopes[fn_scope_key] {
		t.scope = types.new_scope(fn_scope)
		t.fn_root_scope = t.scope
	} else {
		// Fallback: create a new scope if function scope not found
		t.open_scope()
		t.fn_root_scope = t.scope
	}

	// Set current function return type for sum type wrapping in returns
	// and enum shorthand resolution
	old_fn_ret_type_name := t.cur_fn_ret_type_name
	old_fn_return_enum_type_name := t.cur_fn_return_enum_type_name
	old_fn_return_type_override := t.cur_fn_return_type_override
	old_fn_has_return_type_override := t.cur_fn_has_return_type_override
	old_fn_returns_option := t.cur_fn_returns_option
	old_fn_returns_result := t.cur_fn_returns_result
	t.cur_fn_return_enum_type_name = ''
	t.cur_fn_return_type_override = types.Type(types.void_)
	t.cur_fn_has_return_type_override = false
	t.cur_fn_returns_option = false
	t.cur_fn_returns_result = false
	if decl.typ.return_type is ast.Type {
		return_type := decl.typ.return_type as ast.Type
		t.cur_fn_returns_option = return_type is ast.OptionType
		t.cur_fn_returns_result = return_type is ast.ResultType
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
	if ret_type := t.get_fn_return_type(scope_fn_name) {
		t.cur_fn_return_enum_type_name = t.enum_type_name_for_return_value(ret_type)
	} else if ret_type := t.get_fn_return_type(fn_scope_key) {
		t.cur_fn_return_enum_type_name = t.enum_type_name_for_return_value(ret_type)
	}
	if t.cur_fn_return_enum_type_name == '' && t.cur_fn_ret_type_name != '' {
		if ret_type := t.lookup_type(t.cur_fn_ret_type_name) {
			t.cur_fn_return_enum_type_name = t.enum_type_name_for_return_value(ret_type)
		}
	}
	if return_type := t.infer_decl_type_from_type_expr(decl.typ.return_type) {
		t.cur_fn_return_type_override = return_type
		t.cur_fn_has_return_type_override = true
	}

	// Transform function body
	// Clear per-function state: array_elem_type_overrides tracks .map() result types
	// and must not leak across function boundaries (e.g., variable 'a' in one function
	// must not affect variable 'a' in another function).
	t.array_elem_type_overrides = map[string]string{}
	old_fn_name_str := t.cur_fn_name_str
	old_fn_recv_prefix := t.cur_fn_recv_prefix
	old_fn_recv_param := t.cur_fn_recv_param
	old_cur_fn_mut_params := t.cur_fn_mut_params.clone()
	old_decl_type_overrides := t.decl_type_overrides.clone()
	old_static_local_renames := t.static_local_renames.clone()
	t.static_local_renames = map[string]string{}
	t.cur_fn_mut_params = []string{}
	t.decl_type_overrides = map[string]types.Type{}
	for param in decl.typ.params {
		if param.name != '' {
			if param_type := t.infer_decl_type_from_type_expr(param.typ) {
				t.decl_type_overrides[param.name] = param_type
			}
		}
		if (param.is_mut || param.typ is ast.ModifierExpr) && param.name != '' {
			t.cur_fn_mut_params << param.name
		}
	}
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
		if decl.receiver.name != '' {
			if receiver_type := t.infer_decl_type_from_type_expr(decl.receiver.typ) {
				t.decl_type_overrides[decl.receiver.name] = receiver_type
			}
		}
		if (decl.receiver.is_mut || decl.receiver.typ is ast.ModifierExpr)
			&& decl.receiver.name != '' {
			t.cur_fn_mut_params << decl.receiver.name
		}
	} else {
		t.cur_fn_recv_prefix = ''
		t.cur_fn_recv_param = ''
	}
	transformed_stmts := t.transform_stmts(decl.stmts)
	t.cur_fn_name_str = old_fn_name_str
	t.cur_fn_recv_prefix = old_fn_recv_prefix
	t.cur_fn_recv_param = old_fn_recv_param
	t.cur_fn_mut_params = old_cur_fn_mut_params.clone()
	t.decl_type_overrides = old_decl_type_overrides.clone()
	t.static_local_renames = old_static_local_renames.clone()
	t.cur_fn_ret_type_name = old_fn_ret_type_name
	t.cur_fn_return_enum_type_name = old_fn_return_enum_type_name
	t.cur_fn_return_type_override = old_fn_return_type_override
	t.cur_fn_has_return_type_override = old_fn_has_return_type_override
	t.cur_fn_returns_option = old_fn_returns_option
	t.cur_fn_returns_result = old_fn_returns_result

	// Lower defer statements: collect defers, remove them from body,
	// inject defer body before every return and at end of function
	has_return_type := decl.typ.return_type !is ast.EmptyExpr
	fn_return_type := t.get_fn_return_type(scope_fn_name) or {
		t.get_fn_return_type(fn_scope_key) or { types.Type(types.void_) }
	}
	final_stmts := t.lower_defer_stmts(transformed_stmts, has_return_type, fn_return_type)

	if t.fn_root_scope != unsafe { nil } {
		t.cached_fn_scopes[fn_scope_key] = t.fn_root_scope
	}

	// Restore previous scope and fn_root_scope
	t.scope = old_scope
	t.fn_root_scope = old_fn_root_scope

	// For @[live] functions, force @[noinline] so the function gets its own
	// symbol in the binary (required for -hot-fn extraction).
	if live_fn_detected && !decl.attributes.has('noinline') {
		mut new_attrs := []ast.Attribute{cap: decl.attributes.len + 1}
		new_attrs << ast.Attribute{
			value: ast.Expr(ast.Ident{
				name: 'noinline'
			})
		}
		for a in decl.attributes {
			new_attrs << a
		}
		return ast.FnDecl{
			attributes: new_attrs
			is_public:  decl.is_public
			is_method:  decl.is_method
			is_static:  decl.is_static
			receiver:   decl.receiver
			language:   decl.language
			name:       decl.name
			typ:        decl.typ
			stmts:      final_stmts
			pos:        decl.pos
		}
	}

	return ast.FnDecl{
		attributes: decl.attributes
		is_public:  decl.is_public
		is_method:  decl.is_method
		is_static:  decl.is_static
		receiver:   decl.receiver
		language:   decl.language
		name:       decl.name
		typ:        decl.typ
		stmts:      final_stmts
		pos:        decl.pos
	}
}

fn (mut t Transformer) transform_call_expr(expr ast.CallExpr) ast.Expr {
	// Resolve $d('key', default) comptime define calls to their default value.
	// $d reads from compile-time environment; we just use the default.
	if expr.lhs is ast.Ident && expr.lhs.name == 'd' && expr.args.len == 2 {
		return t.transform_expr(expr.args[1])
	}
	if expr.lhs is ast.Ident && expr.args.len == 1 && t.call_or_cast_lhs_is_type(expr.lhs) {
		arg_key := t.expr_to_string(expr.args[0])
		if arg_key != '' {
			if ctx := t.find_smartcast_for_expr(arg_key) {
				return ast.CallExpr{
					lhs:  expr.lhs
					args: [t.apply_smartcast_direct_ctx(expr.args[0], ctx)]
					pos:  expr.pos
				}
			}
		}
	}
	// Inline generic math functions (abs[T], min[T], max[T], maxof[T], minof[T]).
	// Generic function declarations are not instantiated by the compiler, so these
	// become unresolved symbols unless inlined here.
	if inlined := t.try_inline_generic_math_call(expr) {
		return inlined
	}
	if expr.lhs is ast.SelectorExpr {
		if fnptr_call := t.transform_fn_pointer_field_call(expr.lhs, expr.args, expr.pos) {
			return fnptr_call
		}
	}
	// Expand .filter() / .map() calls to hoisted statements + temp variable
	if expanded := t.try_expand_filter_or_map_expr(expr) {
		return expanded
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
		if sel.lhs is ast.Ident {
			lhs_name := sel.lhs.name
			if obj := t.module_selector_object(lhs_name, sel.rhs.name) {
				if obj is types.Fn {
					resolved_mod := t.selector_module_name(lhs_name) or { lhs_name }
					call_mod := if resolved_mod.contains('.') {
						resolved_mod.all_after_last('.')
					} else if resolved_mod.contains('__') {
						resolved_mod.all_after_last('__')
					} else {
						resolved_mod
					}
					args := t.transform_call_args_for_lhs(expr.lhs, expr.args)
					return ast.CallExpr{
						lhs:  ast.Ident{
							name: '${call_mod}__${sel.rhs.name}'
							pos:  sel.rhs.pos
						}
						args: args
						pos:  expr.pos
					}
				}
			}
		}
		if sel.rhs.name == 'work_on_items' {
			if lowered := t.transform_pool_work_on_items(sel.lhs, expr.args, expr.pos) {
				return lowered
			}
		}
		if t.pref != unsafe { nil } && (t.pref.backend == .arm64 || t.pref.backend == .x64) {
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
		if sel.rhs.name == 'free' && t.resolve_method_call_name(sel.lhs, 'free') == none {
			return ast.Expr(ast.BasicLiteral{
				kind:  .number
				value: '0'
			})
		}
		if sel.rhs.name == 'pos' && expr.args.len == 0 {
			if lhs_type := t.resolve_expr_type(sel.lhs) {
				base := t.unwrap_alias_and_pointer_type(lhs_type)
				if base is types.SumType {
					sumtype_name := t.type_to_c_name(base)
					return ast.Expr(ast.CallExpr{
						lhs:  ast.Ident{
							name: '${sumtype_name}__pos'
							pos:  sel.rhs.pos
						}
						args: [t.transform_expr(sel.lhs)]
						pos:  expr.pos
					})
				}
			}
			if t.resolve_method_call_name(sel.lhs, 'pos') == none {
				return ast.Expr(ast.SelectorExpr{
					lhs: t.transform_expr(sel.lhs)
					rhs: sel.rhs
					pos: expr.pos
				})
			}
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
		if method_name in ['from_string', 'str', 'values', 'zero'] {
			if lowered := t.transform_static_enum_method_call(sel.lhs, method_name, expr.args,
				expr.pos)
			{
				return lowered
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
		if method_name in ['contains', 'index', 'last_index'] && expr.args.len == 1 {
			arg_matches_elem := t.array_method_arg_matches_elem(sel.lhs, expr.args[0])
			if !arg_matches_elem {
				if info := t.get_array_method_info(sel.lhs) {
					return ast.CallExpr{
						lhs:  ast.Ident{
							name: '${info.array_type}__${method_name}'
						}
						args: [
							t.transform_array_receiver_expr(sel.lhs),
							t.transform_expr(expr.args[0]),
						]
						pos:  expr.pos
					}
				}
			}
			if !arg_matches_elem {
				if concrete_method := t.resolve_array_concrete_method_name(sel.lhs, method_name) {
					return ast.CallExpr{
						lhs:  ast.Ident{
							name: concrete_method
						}
						args: [
							t.transform_array_receiver_expr(sel.lhs),
							t.transform_expr(expr.args[0]),
						]
						pos:  expr.pos
					}
				}
			}
			mut has_concrete_method := false
			if !arg_matches_elem {
				if resolved := t.resolve_method_call_name(sel.lhs, method_name) {
					has_concrete_method = !resolved.starts_with('array__')
				}
			}
			if !has_concrete_method {
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
		}
		// Transform direct .str() calls on arrays/maps to specialized function calls
		// e.g., a.str() where a is []int -> Array_int_str(a)
		if method_name == 'str' && expr.args.len == 0 {
			// Keep explicit user-defined/declared str() methods (e.g. strings.Builder).
			// Only lower to helper calls when there is no real method on the receiver type.
			str_fn_info := t.get_str_fn_info_for_expr(sel.lhs)
			if str_fn_info.str_fn_name != '' && str_fn_info.str_fn_name != 'Array_u8_str' {
				is_array_or_map_str := str_fn_info.str_fn_name.starts_with('Array_')
					|| str_fn_info.str_fn_name.starts_with('Array_fixed_')
					|| str_fn_info.str_fn_name.starts_with('Map_')
				if is_array_or_map_str || t.get_method_return_type(ast.Expr(expr)) == none {
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
			if ctx := t.smartcast_context_for_method_receiver(sel.lhs) {
				if variant_method := t.smartcast_variant_method_name(ctx, sel.rhs.name) {
					// Smartcasts narrow the receiver inside this branch. Prefer the
					// concrete variant method when it exists to avoid recursively
					// dispatching back through the enclosing sumtype method.
					casted_receiver := t.smartcast_method_receiver(sel.lhs, ctx)
					mut args := []ast.Expr{cap: expr.args.len + 1}
					args << casted_receiver
					for arg in expr.args {
						args << t.transform_expr(arg)
					}
					return ast.CallExpr{
						lhs:  ast.Ident{
							name: variant_method
						}
						args: args
						pos:  expr.pos
					}
				}
				if !ctx.sumtype.starts_with('__iface__')
					&& t.lookup_method_cached(ctx.sumtype, sel.rhs.name) != none {
					removed_ctx := t.remove_smartcast_for_expr(ctx.expr)
					mut args := []ast.Expr{cap: expr.args.len + 1}
					args << t.transform_expr(sel.lhs)
					if removed := removed_ctx {
						t.push_smartcast_full(removed.expr, removed.variant, removed.variant_full,
							removed.sumtype)
					}
					for arg in expr.args {
						args << t.transform_expr(arg)
					}
					return ast.CallExpr{
						lhs:  ast.Ident{
							name: '${ctx.sumtype}__${sel.rhs.name}'
						}
						args: args
						pos:  expr.pos
					}
				}
			}
		}
		// Check for interface method call: iface.method(args...)
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
			if t.pref != unsafe { nil } && (t.pref.backend == .arm64 || t.pref.backend == .x64) {
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
	// Method call resolution: rewrite receiver.method(args) -> Type__method(receiver, args)
	if expr.lhs is ast.SelectorExpr {
		sel := expr.lhs as ast.SelectorExpr
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
			resolved_mod := t.resolve_module_name(lhs_name) or { lhs_name }
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
		is_module_call := resolved_module_call_name != ''
		if is_module_call {
			args := t.transform_call_args_for_lhs(expr.lhs, expr.args)
			return ast.CallExpr{
				lhs:  ast.Ident{
					name: resolved_module_call_name
				}
				args: args
				pos:  expr.pos
			}
		}
		if !is_module_call {
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
				if resolved.starts_with('Array_')
					&& sel.rhs.name in ['contains', 'index', 'last_index'] && expr.args.len == 1
					&& t.array_method_arg_matches_elem(sel.lhs, expr.args[0]) {
					if info := t.get_array_method_info(sel.lhs) {
						fn_name := t.register_needed_array_method(info, sel.rhs.name)
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
				is_static := t.is_static_method_call(sel.lhs)
				fn_info := t.call_arg_info_for_resolved_method(expr.lhs, resolved, is_static)
				call_args := t.lower_missing_call_args_with_info(expr.args, fn_info)
				mut transformed_call_args := []ast.Expr{cap: call_args.len}
				for i, arg in call_args {
					transformed_call_args << t.transform_call_arg_with_sumtype_check(arg, fn_info, i)
				}
				transformed_call_args = t.lower_variadic_args(expr.lhs, transformed_call_args)
				mut args := []ast.Expr{cap: transformed_call_args.len + 1}
				if !is_static {
					args << t.transform_expr(sel.lhs)
				}
				args << transformed_call_args
				return ast.CallExpr{
					lhs:  ast.Ident{
						name: resolved
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
			if resolved := t.transform_generic_method_call(sel, [gai.expr], expr.lhs, expr.args,
				expr.pos)
			{
				return resolved
			}
		}
	}
	if expr.lhs is ast.IndexExpr {
		idx := expr.lhs as ast.IndexExpr
		if idx.lhs is ast.SelectorExpr {
			sel := idx.lhs as ast.SelectorExpr
			if resolved := t.transform_generic_method_call(sel, [idx.expr], expr.lhs, expr.args,
				expr.pos)
			{
				return resolved
			}
		}
	}
	if expr.lhs is ast.GenericArgs {
		ga := expr.lhs as ast.GenericArgs
		if ga.lhs is ast.SelectorExpr {
			sel := ga.lhs as ast.SelectorExpr
			if resolved := t.transform_generic_method_call(sel, ga.args, expr.lhs, expr.args,
				expr.pos)
			{
				return resolved
			}
		}
	}
	// Default: transform arguments and lhs recursively
	// This is important for smart cast propagation through method chains
	// e.g., stmt.name.replace() when stmt is smartcast
	call_args := t.lower_missing_call_args(expr.lhs, expr.args)
	// Look up function parameter types for sumtype re-wrapping
	fn_info := t.lookup_call_fn_info(expr.lhs)
	mut args := []ast.Expr{cap: call_args.len}
	for i, arg in call_args {
		// When an argument has an active smartcast but the function parameter
		// expects the original sumtype, temporarily disable the smartcast so the
		// original sumtype value is passed through without being unwrapped.
		args << t.transform_call_arg_with_sumtype_check(arg, fn_info, i)
	}
	args = t.lower_variadic_args(expr.lhs, args)
	return ast.CallExpr{
		lhs:  t.transform_expr(expr.lhs)
		args: args
		pos:  expr.pos
	}
}

fn (mut t Transformer) transform_call_args_for_lhs(lhs ast.Expr, raw_args []ast.Expr) []ast.Expr {
	call_args := t.lower_missing_call_args(lhs, raw_args)
	fn_info := t.lookup_call_fn_info(lhs)
	mut args := []ast.Expr{cap: call_args.len}
	for i, arg in call_args {
		args << t.transform_call_arg_with_sumtype_check(arg, fn_info, i)
	}
	return t.lower_variadic_args(lhs, args)
}

struct CallFnInfo {
mut:
	param_types []types.Type
	param_names []string
	is_variadic bool
	is_noreturn bool
}

fn (t &Transformer) call_info_from_callable_type(typ types.Type) ?CallFnInfo {
	match typ {
		types.FnType {
			return CallFnInfo{
				param_types: typ.get_param_types()
				param_names: typ.get_param_names()
				is_variadic: typ.is_variadic_fn()
				is_noreturn: typ.is_noreturn()
			}
		}
		types.Alias {
			return t.call_info_from_callable_type(typ.base_type)
		}
		types.Pointer {
			return t.call_info_from_callable_type(typ.base_type)
		}
		else {}
	}

	return none
}

fn (mut t Transformer) transform_fn_pointer_field_call(sel ast.SelectorExpr, raw_args []ast.Expr, pos token.Pos) ?ast.Expr {
	field_type := t.get_struct_field_type(sel) or { return none }
	fn_info := t.call_info_from_callable_type(field_type) or { return none }
	typed_lhs := t.synth_selector(t.transform_expr(sel.lhs), sel.rhs.name, field_type)
	mut args := []ast.Expr{cap: raw_args.len}
	for i, arg in raw_args {
		args << t.transform_call_arg_with_sumtype_check(arg, fn_info, i)
	}
	args = t.lower_variadic_args(typed_lhs, args)
	return ast.Expr(ast.CallExpr{
		lhs:  typed_lhs
		args: args
		pos:  pos
	})
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
	if resolved := t.resolve_method_call_name(sel.lhs, sel.rhs.name) {
		recv_key := resolved.all_before_last('__')
		first_base := t.unwrap_alias_and_pointer_type(trimmed.param_types[0])
		first_c_name := t.type_to_c_name(first_base)
		if first_c_name == recv_key
			|| first_c_name.all_after_last('__') == recv_key.all_after_last('__') {
			trimmed.param_types = trimmed.param_types[1..].clone()
			if trimmed.param_names.len > 0 {
				trimmed.param_names = trimmed.param_names[1..].clone()
			}
			return trimmed
		}
	}
	recv_type := t.get_expr_type(sel.lhs) or { return trimmed }
	recv_base := t.unwrap_alias_and_pointer_type(recv_type)
	first_base := t.unwrap_alias_and_pointer_type(trimmed.param_types[0])
	if recv_base.name() != first_base.name() {
		return trimmed
	}
	trimmed.param_types = trimmed.param_types[1..].clone()
	if trimmed.param_names.len > 0 {
		trimmed.param_names = trimmed.param_names[1..].clone()
	}
	return trimmed
}

fn (t &Transformer) call_arg_info_for_resolved_method(lhs ast.Expr, resolved string, is_static bool) ?CallFnInfo {
	if resolved == '' {
		return t.lookup_call_fn_info(lhs)
	}
	method_name := resolved.all_after_last('__')
	recv_key := resolved.all_before_last('__')
	mut lookup_names := []string{cap: 2}
	if recv_key != '' && recv_key != resolved {
		lookup_names << recv_key
		if recv_key.contains('__') {
			lookup_names << recv_key.all_after_last('__')
		}
	}
	for name in lookup_names {
		if fn_type := t.lookup_method_cached(name, method_name) {
			mut info := CallFnInfo{
				param_types: fn_type.get_param_types()
				param_names: fn_type.get_param_names()
				is_variadic: fn_type.is_variadic_fn()
				is_noreturn: fn_type.is_noreturn()
			}
			if !is_static {
				info = t.drop_method_receiver_from_call_info(lhs, info)
			}
			return info
		}
	}
	if info := t.lookup_call_fn_info(lhs) {
		return info
	}
	resolved_info := t.lookup_call_fn_info(ast.Expr(ast.Ident{
		name: resolved
	})) or { return none }
	mut trimmed := resolved_info
	if !is_static {
		trimmed = t.drop_method_receiver_from_call_info(lhs, trimmed)
	}
	return trimmed
}

fn (t &Transformer) lookup_call_fn_info(lhs ast.Expr) ?CallFnInfo {
	// Prefer checker-resolved callable type for this exact call target.
	// This is the most reliable source for method parameter info (names + types).
	if lhs_type := t.get_expr_type(lhs) {
		callable := t.unwrap_alias_and_pointer_type(lhs_type)
		if callable is types.FnType {
			mut info := CallFnInfo{
				param_types: callable.get_param_types()
				param_names: callable.get_param_names()
				is_variadic: callable.is_variadic_fn()
				is_noreturn: callable.is_noreturn()
			}
			return t.drop_method_receiver_from_call_info(lhs, info)
		}
	}
	if lhs is ast.Ident {
		if t.cur_module != '' {
			if fn_type := t.lookup_fn_cached(t.cur_module, lhs.name) {
				return CallFnInfo{
					param_types: fn_type.get_param_types()
					param_names: fn_type.get_param_names()
					is_variadic: fn_type.is_variadic_fn()
					is_noreturn: fn_type.is_noreturn()
				}
			}
		}
		if fn_type := t.lookup_fn_cached('builtin', lhs.name) {
			return CallFnInfo{
				param_types: fn_type.get_param_types()
				param_names: fn_type.get_param_names()
				is_variadic: fn_type.is_variadic_fn()
				is_noreturn: fn_type.is_noreturn()
			}
		}
		if lhs.name.contains('__') {
			module_name := lhs.name.all_before_last('__')
			fn_name := lhs.name.all_after_last('__')
			if fn_type := t.lookup_fn_cached(module_name, fn_name) {
				return CallFnInfo{
					param_types: fn_type.get_param_types()
					param_names: fn_type.get_param_names()
					is_variadic: fn_type.is_variadic_fn()
					is_noreturn: fn_type.is_noreturn()
				}
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
					return CallFnInfo{
						param_types: fn_type.get_param_types()
						param_names: fn_type.get_param_names()
						is_variadic: fn_type.is_variadic_fn()
						is_noreturn: fn_type.is_noreturn()
					}
				}
			}
		}
		return none
	}
	if lhs is ast.SelectorExpr {
		if lhs.lhs is ast.Ident {
			lhs_name := (lhs.lhs as ast.Ident).name
			mut module_names := []string{}
			if mod_name := t.selector_module_name(lhs_name) {
				module_names << mod_name
				if mod_name.contains('.') {
					module_names << mod_name.all_after_last('.')
				} else if mod_name.contains('__') {
					module_names << mod_name.all_after_last('__')
				}
			}
			module_names << lhs_name
			mut seen := map[string]bool{}
			for mod_name in module_names {
				if mod_name == '' || mod_name in seen {
					continue
				}
				seen[mod_name] = true
				if fn_type := t.lookup_fn_cached(mod_name, lhs.rhs.name) {
					return CallFnInfo{
						param_types: fn_type.get_param_types()
						param_names: fn_type.get_param_names()
						is_variadic: fn_type.is_variadic_fn()
						is_noreturn: fn_type.is_noreturn()
					}
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
					info := CallFnInfo{
						param_types: fn_type.get_param_types()
						param_names: fn_type.get_param_names()
						is_variadic: fn_type.is_variadic_fn()
						is_noreturn: fn_type.is_noreturn()
					}
					return t.drop_method_receiver_from_call_info(lhs, info)
				}
			}
		}
		if recv_type := t.get_expr_type(lhs.lhs) {
			base_type := t.unwrap_alias_and_pointer_type(recv_type)
			type_name := base_type.name()
			mut lookup_names := []string{cap: 3}
			lookup_names << type_name
			if type_name.contains('__') {
				lookup_names << type_name.all_after_last('__')
			} else if t.cur_module != '' && t.cur_module != 'main' {
				lookup_names << '${t.cur_module}__${type_name}'
			}
			for name in lookup_names {
				if fn_type := t.lookup_method_cached(name, lhs.rhs.name) {
					mut info := CallFnInfo{
						param_types: fn_type.get_param_types()
						param_names: fn_type.get_param_names()
						is_variadic: fn_type.is_variadic_fn()
						is_noreturn: fn_type.is_noreturn()
					}
					return t.drop_method_receiver_from_call_info(lhs, info)
				}
			}
		}
	}
	return none
}

fn (t &Transformer) lookup_call_param_types(lhs ast.Expr) []types.Type {
	if info := t.lookup_call_fn_info(lhs) {
		return info.param_types
	}
	return []types.Type{}
}

// receiver_type_to_c_prefix maps a receiver's unwrapped type to the C function
// name prefix used in method name mangling. For containers (array, map, string)
// it returns the generic prefix; for named types it returns the qualified C name.
fn (t &Transformer) receiver_type_to_c_prefix(typ types.Type) string {
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
		types.Struct, types.Enum, types.SumType {
			return t.type_to_c_name(typ)
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
			if mut scope := t.get_current_scope() {
				if obj := scope.lookup_parent(receiver.name, 0) {
					return obj is types.Type
				}
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

// resolve_method_call_name resolves a method call on a receiver to its mangled
// C function name. Returns e.g. "array__push" or "MyStruct__method". Returns
// none when the receiver type is unknown or the method is not registered
// (e.g. function pointer field calls), which prevents false lowering.
fn (t &Transformer) resolve_method_call_name(receiver ast.Expr, method_name string) ?string {
	mut recv_type_opt := t.get_expr_type(receiver)
	if recv_type_opt == none && receiver is ast.SelectorExpr {
		recv_type_opt = t.get_struct_field_type(receiver)
	}
	recv_type := recv_type_opt or {
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
	// Build lookup names for method verification (same pattern as lookup_call_param_types)
	type_name := base_type.name()
	mut lookup_names := []string{cap: 5}
	lookup_names << type_name
	if type_name.contains('__') {
		lookup_names << type_name.all_after_last('__')
	} else if t.cur_module != '' && t.cur_module != 'main' {
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
	// For alias types over containers (e.g. strings.Builder = []u8), check if the
	// method is defined on the alias itself. If so, use the alias C name as prefix.
	if base_type is types.Alias {
		alias_c_name := t.type_to_c_name(base_type)
		// type_name is already base_type.name() (the alias name like 'strings__Builder')
		if t.lookup_method_cached(type_name, method_name) != none {
			return '${alias_c_name}__${method_name}'
		}
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
	method_keys := t.cached_methods.keys()
	for key in method_keys {
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
				if c_prefix == 'array' && t.lookup_method_cached('array', method_name) == none {
					specific_name := t.type_to_c_name(base_type)
					if specific_name != '' {
						return '${specific_name}__${method_name}'
					}
				}
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

fn (t &Transformer) resolve_array_concrete_method_name(receiver ast.Expr, method_name string) ?string {
	mut recv_type_opt := t.get_expr_type(receiver)
	if recv_type_opt == none && receiver is ast.SelectorExpr {
		recv_type_opt = t.get_struct_field_type(receiver)
	}
	recv_type := recv_type_opt or { return none }
	base_type := t.unwrap_alias_and_pointer_type(recv_type)
	if base_type !is types.Array && base_type !is types.ArrayFixed {
		return none
	}
	if t.lookup_method_cached('array', method_name) != none {
		return none
	}
	specific_name := t.type_to_c_name(base_type)
	if specific_name == '' {
		return none
	}
	mut lookup_names := []string{cap: 4}
	t.append_method_lookup_type_name(mut lookup_names, base_type.name())
	t.append_method_lookup_type_name(mut lookup_names, specific_name)
	if elem := t.get_array_elem_type_str(receiver) {
		t.append_method_lookup_type_name(mut lookup_names, '[]${elem}')
		if elem.contains('__') {
			t.append_method_lookup_type_name(mut lookup_names, '[]${elem.all_after_last('__')}')
		}
	}
	if _ := t.lookup_method_return_type(lookup_names, method_name) {
		return '${specific_name}__${method_name}'
	}
	return none
}

fn (t &Transformer) array_method_arg_matches_elem(receiver ast.Expr, arg ast.Expr) bool {
	elem := t.get_array_elem_type_str(receiver) or { return true }
	if elem == '' || elem == 'voidptr' {
		return true
	}
	if t.is_string_expr(arg) {
		return elem == 'string'
	}
	if arg_type := t.get_expr_type(arg) {
		arg_c := t.type_to_c_name(t.unwrap_alias_and_pointer_type(arg_type))
		if arg_c == '' {
			return true
		}
		if arg_c == elem {
			return true
		}
		return arg_c.all_after_last('__') == elem.all_after_last('__')
	}
	return true
}

fn (mut t Transformer) lower_missing_call_args(lhs ast.Expr, args []ast.Expr) []ast.Expr {
	info := t.lookup_call_fn_info(lhs) or { return args }
	return t.lower_missing_call_args_for_info(args, info)
}

fn (mut t Transformer) lower_missing_call_args_with_info(args []ast.Expr, fn_info ?CallFnInfo) []ast.Expr {
	if info := fn_info {
		return t.lower_missing_call_args_for_info(args, info)
	}
	return args
}

fn (mut t Transformer) lower_missing_call_args_for_info(args []ast.Expr, info CallFnInfo) []ast.Expr {
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
			types.OptionType {
				out << t.none_value_expr_for_option_type(typ)
			}
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

fn (t &Transformer) generic_method_suffix(generic_args []ast.Expr) string {
	return t.generic_specialization_suffix(generic_args)
}

fn (mut t Transformer) register_generic_call_specialization(fn_name string, generic_args []ast.Expr) {
	if fn_name == '' || generic_args.len == 0 {
		return
	}
	param_names := ['T', 'U', 'V', 'K', 'W']
	mut spec := map[string]types.Type{}
	for i, arg in generic_args {
		if i >= param_names.len {
			break
		}
		concrete := t.infer_decl_type_from_type_expr(arg) or { continue }
		spec[param_names[i]] = concrete
	}
	if spec.len == 0 {
		return
	}
	mut specs := t.env.generic_types[fn_name]
	for existing in specs {
		mut same := true
		for name, typ in spec {
			existing_typ := existing[name] or {
				same = false
				break
			}
			if t.type_to_c_name(existing_typ) != t.type_to_c_name(typ) {
				same = false
				break
			}
		}
		if same {
			return
		}
	}
	specs << spec
	t.env.generic_types[fn_name] = specs
}

fn (mut t Transformer) transform_generic_method_call(sel ast.SelectorExpr, generic_args []ast.Expr, call_lhs ast.Expr, raw_args []ast.Expr, pos token.Pos) ?ast.Expr {
	is_module_call := sel.lhs is ast.Ident && t.selector_module_name(sel.lhs.name) != none
	if is_module_call {
		return none
	}
	suffix := t.generic_method_suffix(generic_args)
	if suffix == '' {
		return none
	}
	t.register_generic_call_specialization(sel.rhs.name, generic_args)
	call_args := t.lower_missing_call_args(call_lhs, raw_args)
	fn_info := t.lookup_call_fn_info(call_lhs)
	recv_is_self := t.cur_fn_recv_param != '' && sel.lhs is ast.Ident
		&& (sel.lhs as ast.Ident).name == t.cur_fn_recv_param && t.get_expr_type(sel.lhs) == none
	if recv_is_self && t.cur_fn_recv_prefix != '' {
		mut transformed_call_args := []ast.Expr{cap: call_args.len}
		for i, arg in call_args {
			transformed_call_args << t.transform_call_arg_with_sumtype_check(arg, fn_info, i)
		}
		transformed_call_args = t.lower_variadic_args(call_lhs, transformed_call_args)
		mut args := []ast.Expr{cap: transformed_call_args.len + 1}
		args << t.transform_expr(sel.lhs)
		args << transformed_call_args
		return ast.CallExpr{
			lhs:  ast.Ident{
				name: '${t.cur_fn_recv_prefix}__${sel.rhs.name}${suffix}'
			}
			args: args
			pos:  pos
		}
	}
	method_name := sel.rhs.name + suffix
	if resolved := t.resolve_method_call_name(sel.lhs, method_name) {
		mut transformed_call_args := []ast.Expr{cap: call_args.len}
		for i, arg in call_args {
			transformed_call_args << t.transform_call_arg_with_sumtype_check(arg, fn_info, i)
		}
		transformed_call_args = t.lower_variadic_args(call_lhs, transformed_call_args)
		mut args := []ast.Expr{cap: transformed_call_args.len + 1}
		args << t.transform_expr(sel.lhs)
		args << transformed_call_args
		return ast.CallExpr{
			lhs:  ast.Ident{
				name: resolved
			}
			args: args
			pos:  pos
		}
	}
	if resolved := t.resolve_method_call_name(sel.lhs, sel.rhs.name) {
		mut transformed_call_args := []ast.Expr{cap: call_args.len}
		for i, arg in call_args {
			transformed_call_args << t.transform_call_arg_with_sumtype_check(arg, fn_info, i)
		}
		transformed_call_args = t.lower_variadic_args(call_lhs, transformed_call_args)
		mut args := []ast.Expr{cap: transformed_call_args.len + 1}
		args << t.transform_expr(sel.lhs)
		args << transformed_call_args
		return ast.CallExpr{
			lhs:  ast.Ident{
				name: resolved + suffix
			}
			args: args
			pos:  pos
		}
	}
	return none
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
	info := t.lookup_call_fn_info(lhs) or { return args }
	if !info.is_variadic || info.param_types.len == 0 || args.len < info.param_types.len {
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
		variadic_start := info.param_types.len - 1
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
		return t.lower_struct_shorthand_call(args, param_types)
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
			types.OptionType {
				out[i] = t.none_value_expr_for_option_type(base)
			}
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

fn (mut t Transformer) transform_call_or_cast_expr(expr ast.CallOrCastExpr) ast.Expr {
	// Inline generic math functions (abs[T], min[T], max[T]).
	if inlined := t.try_inline_generic_math_coce(expr) {
		return inlined
	}
	if expr.lhs is ast.SelectorExpr && expr.expr !is ast.EmptyExpr {
		if fnptr_call := t.transform_fn_pointer_field_call(expr.lhs, [expr.expr], expr.pos) {
			return fnptr_call
		}
	}
	// Expand .filter() / .map() calls to hoisted statements + temp variable
	if expanded := t.try_expand_filter_or_map_expr(expr) {
		return expanded
	}
	// Check if this is a flag enum method call: receiver.has(arg) or receiver.all(arg)
	if expr.lhs is ast.SelectorExpr {
		sel := expr.lhs as ast.SelectorExpr
		if sel.lhs is ast.Ident {
			lhs_name := sel.lhs.name
			if obj := t.module_selector_object(lhs_name, sel.rhs.name) {
				if obj is types.Fn {
					resolved_mod := t.selector_module_name(lhs_name) or { lhs_name }
					call_mod := if resolved_mod.contains('.') {
						resolved_mod.all_after_last('.')
					} else if resolved_mod.contains('__') {
						resolved_mod.all_after_last('__')
					} else {
						resolved_mod
					}
					raw_args := if expr.expr is ast.EmptyExpr { []ast.Expr{} } else { [
							expr.expr,
						] }
					args := t.transform_call_args_for_lhs(expr.lhs, raw_args)
					return ast.CallExpr{
						lhs:  ast.Ident{
							name: '${call_mod}__${sel.rhs.name}'
							pos:  sel.rhs.pos
						}
						args: args
						pos:  expr.pos
					}
				}
			}
		}
		if sel.rhs.name == 'work_on_items' && expr.expr !is ast.EmptyExpr {
			if lowered := t.transform_pool_work_on_items(sel.lhs, [expr.expr], expr.pos) {
				return lowered
			}
		}
		if t.pref != unsafe { nil } && (t.pref.backend == .arm64 || t.pref.backend == .x64) {
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
		if sel.rhs.name == 'free' && t.resolve_method_call_name(sel.lhs, 'free') == none {
			return ast.Expr(ast.BasicLiteral{
				kind:  .number
				value: '0'
			})
		}
		if sel.rhs.name == 'pos' && expr.expr is ast.EmptyExpr {
			if lhs_type := t.resolve_expr_type(sel.lhs) {
				base := t.unwrap_alias_and_pointer_type(lhs_type)
				if base is types.SumType {
					sumtype_name := t.type_to_c_name(base)
					return ast.Expr(ast.CallExpr{
						lhs:  ast.Ident{
							name: '${sumtype_name}__pos'
							pos:  sel.rhs.pos
						}
						args: [t.transform_expr(sel.lhs)]
						pos:  expr.pos
					})
				}
			}
			if t.resolve_method_call_name(sel.lhs, 'pos') == none {
				return ast.Expr(ast.SelectorExpr{
					lhs: t.transform_expr(sel.lhs)
					rhs: sel.rhs
					pos: expr.pos
				})
			}
		}
		// arr.sort(a < b) may be parsed as CallOrCastExpr in single-arg form.
		if sel.rhs.name in ['sort', 'sorted'] && t.is_sort_compare_lambda_expr(expr.expr) {
			if result := t.transform_sort_call(sel.lhs, sel.rhs.name, [expr.expr], expr.pos) {
				return result
			}
		}
		method_name := sel.rhs.name
		if method_name in ['from_string', 'str', 'values', 'zero'] {
			mut static_args := []ast.Expr{}
			if expr.expr !is ast.EmptyExpr {
				static_args << expr.expr
			}
			if lowered := t.transform_static_enum_method_call(sel.lhs, method_name, static_args,
				expr.pos)
			{
				return lowered
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
		if method_name in ['contains', 'index', 'last_index'] {
			arg_matches_elem := t.array_method_arg_matches_elem(sel.lhs, expr.expr)
			if !arg_matches_elem {
				if info := t.get_array_method_info(sel.lhs) {
					return ast.CallExpr{
						lhs:  ast.Ident{
							name: '${info.array_type}__${method_name}'
						}
						args: [
							t.transform_array_receiver_expr(sel.lhs),
							t.transform_expr(expr.expr),
						]
						pos:  expr.pos
					}
				}
			}
			if !arg_matches_elem {
				if concrete_method := t.resolve_array_concrete_method_name(sel.lhs, method_name) {
					return ast.CallExpr{
						lhs:  ast.Ident{
							name: concrete_method
						}
						args: [
							t.transform_array_receiver_expr(sel.lhs),
							t.transform_expr(expr.expr),
						]
						pos:  expr.pos
					}
				}
			}
			mut has_concrete_method := false
			if !arg_matches_elem {
				if resolved := t.resolve_method_call_name(sel.lhs, method_name) {
					has_concrete_method = !resolved.starts_with('array__')
				}
			}
			if !has_concrete_method {
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
		}
		// Check for smart-casted method call: se.lhs.method() when se.lhs is smartcast to Type
		if t.has_active_smartcast() {
			if ctx := t.smartcast_context_for_method_receiver(sel.lhs) {
				// Check if the method exists on the variant type. If not, the method
				// is defined on the sum type and we should NOT apply the smartcast
				// to the receiver.
				variant_method_name := t.smartcast_variant_method_name(ctx, sel.rhs.name) or { '' }
				variant_has_method := variant_method_name != ''
				casted_receiver := if variant_has_method {
					t.smartcast_method_receiver(sel.lhs, ctx)
				} else {
					t.transform_expr(sel.lhs)
				}
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
				if variant_has_method {
					resolved_name = variant_method_name
				} else if rn := t.resolve_method_call_name(sel.lhs, sel.rhs.name) {
					resolved_name = rn
				}
				if resolved_name != '' {
					resolved := resolved_name
					is_static := t.is_static_method_call(sel.lhs)
					mut final_args := []ast.Expr{cap: args.len + 1}
					if !is_static {
						final_args << casted_receiver
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
			if t.pref != unsafe { nil } && (t.pref.backend == .arm64 || t.pref.backend == .x64) {
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
	mut sumtype_name := t.type_expr_name_full(expr.lhs)
	if sumtype_name == '' || !t.is_sum_type(sumtype_name) {
		if lhs_typ := t.get_expr_type(expr.lhs) {
			if lhs_typ is types.SumType {
				sumtype_name = lhs_typ.get_name()
			}
		}
	}
	if (sumtype_name == '' || !t.is_sum_type(sumtype_name)) && expr.pos.is_valid() {
		if expr_typ := t.get_expr_type(ast.Expr(expr)) {
			if expr_typ is types.SumType {
				sumtype_name = expr_typ.get_name()
			}
		}
	}
	mut lhs_is_type := t.call_or_cast_lhs_is_type(expr.lhs)
	if !lhs_is_type && expr.lhs is ast.Ident && t.cur_module != '' {
		qualified_lhs := '${t.cur_module}__${expr.lhs.name}'
		if _ := t.lookup_type(qualified_lhs) {
			lhs_is_type = true
		}
	}
	if sumtype_name != '' && lhs_is_type && t.is_sum_type(sumtype_name) {
		arg_key := t.expr_to_string(expr.expr)
		if arg_key != '' {
			if ctx := t.find_smartcast_for_expr(arg_key) {
				if t.sumtype_contains_variant_recursive(sumtype_name, ctx.variant_full, 0)
					|| t.sumtype_contains_variant_recursive(sumtype_name, ctx.variant, 0) {
					direct_arg := t.apply_smartcast_direct_ctx(expr.expr, ctx)
					if wrapped := t.build_sumtype_init(direct_arg, ctx.variant_full, sumtype_name) {
						return wrapped
					}
					if wrapped := t.build_sumtype_init(direct_arg, ctx.variant, sumtype_name) {
						return wrapped
					}
				}
			}
		}
		if wrapped := t.wrap_sumtype_value(expr.expr, sumtype_name) {
			return wrapped
		}
		// Fallback for cases where checker typing of `expr.expr` is already widened
		// to the target sum type and direct variant inference fails.
		transformed_sum_arg := t.transform_expr(expr.expr)
		if wrapped := t.wrap_sumtype_value_transformed(transformed_sum_arg, sumtype_name) {
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
			coce_fn_info := t.lookup_call_fn_info(expr.lhs)
			mut args := []ast.Expr{cap: call_args.len}
			for i, arg in call_args {
				args << t.transform_call_arg_with_sumtype_check(arg, coce_fn_info, i)
			}
			args = t.lower_variadic_args(expr.lhs, args)
			return ast.CallExpr{
				lhs:  ast.Expr(expr.lhs)
				args: args
				pos:  expr.pos
			}
		}
	}
	// Method call resolution: rewrite receiver.method(arg) -> Type__method(receiver, arg)
	if expr.lhs is ast.SelectorExpr {
		sel := expr.lhs as ast.SelectorExpr
		is_module_call := sel.lhs is ast.Ident && t.selector_module_name(sel.lhs.name) != none
		if !is_module_call {
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
				coce_method_fn_info := t.call_arg_info_for_resolved_method(expr.lhs, resolved,
					is_static)
				call_args = t.lower_missing_call_args_with_info(call_args, coce_method_fn_info)
				mut transformed_call_args := []ast.Expr{cap: call_args.len}
				for i, arg in call_args {
					transformed_call_args << t.transform_call_arg_with_sumtype_check(arg,
						coce_method_fn_info, i)
				}
				transformed_call_args = t.lower_variadic_args(expr.lhs, transformed_call_args)
				mut args := []ast.Expr{cap: transformed_call_args.len + 1}
				if !is_static {
					args << t.transform_expr(sel.lhs)
				}
				args << transformed_call_args
				return ast.CallExpr{
					lhs:  ast.Ident{
						name: resolved
					}
					args: args
					pos:  expr.pos
				}
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
			mut raw_args := []ast.Expr{}
			if expr.expr !is ast.EmptyExpr {
				raw_args << expr.expr
			}
			if resolved := t.transform_generic_method_call(sel, [gai.expr], expr.lhs, raw_args,
				expr.pos)
			{
				return resolved
			}
		}
	}
	if expr.lhs is ast.IndexExpr {
		idx := expr.lhs as ast.IndexExpr
		if idx.lhs is ast.SelectorExpr {
			sel := idx.lhs as ast.SelectorExpr
			mut raw_args := []ast.Expr{}
			if expr.expr !is ast.EmptyExpr {
				raw_args << expr.expr
			}
			if resolved := t.transform_generic_method_call(sel, [idx.expr], expr.lhs, raw_args,
				expr.pos)
			{
				return resolved
			}
		}
	}
	if expr.lhs is ast.GenericArgs {
		ga := expr.lhs as ast.GenericArgs
		if ga.lhs is ast.SelectorExpr {
			sel := ga.lhs as ast.SelectorExpr
			mut raw_args := []ast.Expr{}
			if expr.expr !is ast.EmptyExpr {
				raw_args << expr.expr
			}
			if resolved := t.transform_generic_method_call(sel, ga.args, expr.lhs, raw_args,
				expr.pos)
			{
				return resolved
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
		default_fn_info := t.lookup_call_fn_info(expr.lhs)
		mut args := []ast.Expr{cap: call_args.len}
		for i, arg in call_args {
			args << t.transform_call_arg_with_sumtype_check(arg, default_fn_info, i)
		}
		args = t.lower_variadic_args(expr.lhs, args)
		return ast.CallExpr{
			lhs:  t.transform_expr(expr.lhs)
			args: args
			pos:  expr.pos
		}
	}
	// Default: transform lhs and expression recursively
	// This is important for smart cast propagation through method chains
	transformed_lhs := if t.call_or_cast_lhs_is_type(expr.lhs) {
		expr.lhs
	} else {
		t.transform_expr(expr.lhs)
	}
	mut transformed_arg := t.transform_expr(expr.expr)
	if t.call_or_cast_lhs_is_type(expr.lhs) {
		arg_key := t.expr_to_string(expr.expr)
		if arg_key != '' {
			if ctx := t.find_smartcast_for_expr(arg_key) {
				transformed_arg = t.apply_smartcast_direct_ctx(expr.expr, ctx)
			}
		}
	}
	return t.lower_call_or_cast_expr(transformed_lhs, transformed_arg, expr.pos)
}

// transform_call_arg_with_sumtype_check transforms a call argument, temporarily
// disabling any active smartcast when the function parameter is a sumtype.
// This prevents smartcast from unwrapping a sumtype value that should be passed as-is.
// For native backends (arm64/x64), also wraps variant values in sum type init when
// the parameter expects a sum type but the argument is a variant (implicit conversion).
fn (mut t Transformer) call_arg_selector_should_keep_sumtype(arg ast.Expr, ctx SmartcastContext, param_c_name string) bool {
	if arg !is ast.SelectorExpr && arg !is ast.IndexExpr {
		return false
	}
	mut arg_type_name := t.declared_sumtype_name_for_expr(arg)
	if arg_type_name == '' {
		arg_type := t.resolve_expr_type(arg) or { return false }
		arg_base := t.unwrap_alias_and_pointer_type(arg_type)
		arg_type_name = t.type_to_c_name(arg_base)
	}
	arg_type_name = t.normalize_sumtype_name(arg_type_name)
	param_sumtype_name := t.normalize_sumtype_name(param_c_name)
	if arg_type_name == '' || !t.is_sum_type(arg_type_name) {
		return false
	}
	if !type_names_match_for_smartcast(ctx.variant_full, param_sumtype_name)
		&& !type_names_match_for_smartcast(ctx.variant, param_sumtype_name) {
		return false
	}
	return t.sumtype_contains_variant_recursive(arg_type_name, ctx.variant_full, 0)
		|| t.sumtype_contains_variant_recursive(arg_type_name, ctx.variant, 0)
}

fn (mut t Transformer) transform_call_arg_with_sumtype_check(arg ast.Expr, fn_info ?CallFnInfo, idx int) ast.Expr {
	if info := fn_info {
		if idx < info.param_types.len {
			param_type := info.param_types[idx]
			param_c_name := t.normalize_sumtype_name(t.type_to_c_name(param_type))
			if param_c_name != '' && !t.is_sum_type(param_c_name) {
				arg_str := t.expr_to_string(arg)
				if arg_str != '' {
					if ctx := t.find_smartcast_for_expr(arg_str) {
						if type_names_match_for_smartcast(ctx.variant_full, param_c_name)
							|| type_names_match_for_smartcast(ctx.variant, param_c_name) {
							mut smartcast_arg := arg
							mut needs_address := false
							mut address_pos := token.Pos{}
							if arg is ast.ModifierExpr {
								smartcast_arg = arg.expr
								needs_address = true
								address_pos = arg.pos
							}
							direct_arg := t.apply_smartcast_direct_ctx(smartcast_arg, ctx)
							if needs_address {
								return ast.PrefixExpr{
									op:   token.Token.amp
									expr: direct_arg
									pos:  address_pos
								}
							}
							return direct_arg
						}
						param_base := t.unwrap_alias_and_pointer_type(param_type)
						param_base_c_name := t.normalize_sumtype_name(t.type_to_c_name(param_base))
						if arg !is ast.ModifierExpr && t.is_pointer_type(param_type)
							&& param_base_c_name != ''
							&& (type_names_match_for_smartcast(ctx.variant_full, param_base_c_name)
							|| type_names_match_for_smartcast(ctx.variant, param_base_c_name)) {
							direct_arg := t.apply_smartcast_direct_ctx(arg, ctx)
							return ast.PrefixExpr{
								op:   token.Token.amp
								expr: direct_arg
							}
						}
						if arg is ast.ModifierExpr {
							if param_base_c_name != ''&& (type_names_match_for_smartcast(ctx.variant_full, param_base_c_name)
								|| type_names_match_for_smartcast(ctx.variant, param_base_c_name)) {
								direct_arg := t.apply_smartcast_direct_ctx(arg.expr, ctx)
								return ast.PrefixExpr{
									op:   token.Token.amp
									expr: direct_arg
									pos:  arg.pos
								}
							}
						}
					}
				}
			}
			if param_c_name != '' && t.is_sum_type(param_c_name) {
				if arg is ast.IfExpr {
					old_sumtype_return_wrap := t.sumtype_return_wrap
					t.sumtype_return_wrap = param_c_name
					result := t.transform_expr(arg)
					t.sumtype_return_wrap = old_sumtype_return_wrap
					return result
				}
				arg_str := t.expr_to_string(arg)
				declared_sumtype := t.declared_sumtype_name_for_expr(arg)
				if declared_sumtype != '' && t.is_same_sumtype_name(declared_sumtype, param_c_name) {
					return t.transform_expr_without_smartcast_as_type(arg, arg_str,
						info.param_types[idx])
				}
				if (param_c_name == 'Expr' || param_c_name.ends_with('__Expr'))
					&& arg is ast.SelectorExpr && arg.rhs.name == 'expr' {
					return t.transform_expr_without_smartcast_as_type(arg, arg_str,
						info.param_types[idx])
				}
				if arg_str != '' {
					if ctx := t.find_smartcast_for_expr(arg_str) {
						// Only disable smartcast if parameter expects the SAME sumtype
						// as the arg's original sumtype. This avoids incorrectly removing
						// smartcasts when the parameter type is a DIFFERENT sumtype that
						// happens to be the smartcast variant (e.g., ast.Expr smartcast
						// to ast.Type, where ast.Type is itself a sumtype).
						if ctx.sumtype == param_c_name
							|| t.is_same_sumtype_name(ctx.sumtype, param_c_name)
							|| t.sumtype_contains_variant_recursive(param_c_name, ctx.variant_full, 0)
							|| t.sumtype_contains_variant_recursive(param_c_name, ctx.variant, 0) {
							return t.transform_expr_without_smartcast_as_type(arg, arg_str,
								info.param_types[idx])
						}
					}
				}
				if wrapped := t.wrap_sumtype_value(arg, param_c_name) {
					return wrapped
				}
			}
			if param_c_name != '' {
				arg_str := t.expr_to_string(arg)
				if arg_str != '' {
					if existing := t.remove_smartcast_for_expr_with_idx(arg_str) {
						if existing.ctx.sumtype != '' && t.is_sum_type(existing.ctx.sumtype)
							&& !type_names_match_for_smartcast(existing.ctx.variant_full, param_c_name)
							&& !type_names_match_for_smartcast(existing.ctx.variant, param_c_name) {
							result := t.transform_expr(arg)
							t.insert_smartcast_at(existing.idx, existing.ctx)
							return result
						}
						t.insert_smartcast_at(existing.idx, existing.ctx)
					}
				}
			}
		}
	} else {
		arg_str := t.expr_to_string(arg)
		if arg_str != '' {
			if existing := t.remove_smartcast_for_expr_with_idx(arg_str) {
				if existing.ctx.sumtype != '' && t.is_sum_type(existing.ctx.sumtype) {
					declared_sumtype := t.declared_sumtype_name_for_expr(arg)
					if declared_sumtype != '' && t.is_sum_type(declared_sumtype) {
						mut result := t.transform_expr(arg)
						if declared_type := t.lookup_type(declared_sumtype) {
							result = t.with_synth_type(result, declared_type)
						}
						t.insert_smartcast_at(existing.idx, existing.ctx)
						return result
					}
					if arg is ast.SelectorExpr && arg.rhs.name == 'expr' {
						mut result := t.transform_expr(arg)
						if declared_type := t.lookup_type('ast__Expr') {
							result = t.with_synth_type(result, declared_type)
						} else if declared_type := t.lookup_type('Expr') {
							result = t.with_synth_type(result, declared_type)
						}
						t.insert_smartcast_at(existing.idx, existing.ctx)
						return result
					}
					result := t.transform_expr(arg)
					t.insert_smartcast_at(existing.idx, existing.ctx)
					return result
				}
				t.insert_smartcast_at(existing.idx, existing.ctx)
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

fn (t &Transformer) static_enum_receiver_type(receiver ast.Expr) ?types.Type {
	match receiver {
		ast.Ident {
			if typ := t.lookup_type(receiver.name) {
				if typ is types.Enum {
					return typ
				}
			}
			if t.cur_module != '' && t.cur_module != 'main' && t.cur_module != 'builtin'
				&& !receiver.name.contains('__') {
				if typ := t.lookup_type('${t.cur_module}__${receiver.name}') {
					if typ is types.Enum {
						return typ
					}
				}
			}
		}
		ast.SelectorExpr {
			if receiver.lhs is ast.Ident {
				module_name := (receiver.lhs as ast.Ident).name
				if typ := t.lookup_type('${module_name}__${receiver.rhs.name}') {
					if typ is types.Enum {
						return typ
					}
				}
			}
		}
		else {}
	}

	return none
}

fn (mut t Transformer) transform_static_enum_method_call(receiver ast.Expr, method_name string, args []ast.Expr, pos token.Pos) ?ast.Expr {
	enum_type := t.static_enum_receiver_type(receiver) or { return none }
	base := t.unwrap_alias_and_pointer_type(enum_type)
	if base is types.Enum && base.is_flag {
		if method_name == 'zero' && args.len == 0 {
			return t.zero_value_expr_for_type(types.Type(base))
		}
	}
	if base is types.Enum {
		enum_c_name := t.type_to_c_name(types.Type(base))
		if method_name in ['from_string', 'str'] {
			mut transformed_args := []ast.Expr{cap: args.len}
			for arg in args {
				transformed_args << t.transform_expr(arg)
			}
			return ast.Expr(ast.CallExpr{
				lhs:  ast.Ident{
					name: '${enum_c_name}__${method_name}'
				}
				args: transformed_args
				pos:  pos
			})
		}
		if method_name == 'values' && args.len == 0 {
			return ast.Expr(ast.Ident{
				name: '__enum_values_${enum_c_name}'
				pos:  pos
			})
		}
	}
	return none
}

fn (mut t Transformer) transform_pool_work_on_items(receiver ast.Expr, args []ast.Expr, pos token.Pos) ?ast.Expr {
	if args.len != 1 {
		return none
	}
	items_expr := t.transform_expr(args[0])
	items_pointers := ast.Expr(ast.CallExpr{
		lhs:  ast.Ident{
			name: 'array__pointers'
			pos:  pos
		}
		args: [items_expr]
		pos:  pos
	})
	return ast.Expr(ast.CallExpr{
		lhs:  ast.Expr(ast.SelectorExpr{
			lhs: t.transform_expr(receiver)
			rhs: ast.Ident{
				name: 'work_on_pointers'
				pos:  pos
			}
			pos: pos
		})
		args: [items_pointers]
		pos:  pos
	})
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
		field_type := t.infer_expr_type(receiver)
		if field_type != '' && !t.is_flag_enum(field_type) {
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
fn (t &Transformer) is_cast_type_name(name string) bool {
	// Built-in primitive types
	if name in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'f32', 'f64', 'bool',
		'byte', 'char', 'rune', 'usize', 'isize', 'string', 'byteptr', 'charptr', 'voidptr'] {
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
		ast.Ident {
			// Built-in primitive types are always casts, never function calls.
			if lhs.name in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'f32',
				'f64', 'bool', 'byte', 'char', 'rune', 'usize', 'isize', 'string', 'byteptr',
				'charptr', 'voidptr'] {
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
				}
			})
		}
	}
	// Handle math.abs(x) - module-qualified single-arg call
	if expr.lhs is ast.SelectorExpr && expr.args.len == 1 {
		sel := expr.lhs as ast.SelectorExpr
		if sel.lhs is ast.Ident && (sel.lhs as ast.Ident).name == 'math' && sel.rhs.name == 'abs' {
			arg := t.transform_expr(expr.args[0])
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
				}
			})
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
				}
			})
		}
	}
	// Handle math.abs(x)
	if expr.lhs is ast.SelectorExpr {
		sel := expr.lhs as ast.SelectorExpr
		if sel.lhs is ast.Ident
			&& (sel.lhs as ast.Ident).name == 'math' && sel.rhs.name == 'abs' && expr.expr !is ast.EmptyExpr {
			arg := t.transform_expr(expr.expr)
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
				}
			})
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
