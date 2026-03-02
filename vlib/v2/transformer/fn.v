// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module transformer

import v2.ast
import v2.token
import v2.types

// get_fn_return_type gets the return type for a function
fn (t &Transformer) get_fn_return_type(fn_name string) ?types.Type {
	// First try the current module scope.
	if mut scope := t.get_module_scope(t.cur_module) {
		if obj := scope.lookup_parent(fn_name, 0) {
			if obj is types.Fn {
				fn_typ := obj.get_typ()
				if fn_typ is types.FnType {
					return fn_typ.get_return_type()
				}
			}
		}
	}
	// If current module is mangled, also try its short module name.
	if t.cur_module.contains('__') {
		short_module := t.cur_module.all_after_last('__')
		if mut scope := t.get_module_scope(short_module) {
			if obj := scope.lookup_parent(fn_name, 0) {
				if obj is types.Fn {
					fn_typ := obj.get_typ()
					if fn_typ is types.FnType {
						return fn_typ.get_return_type()
					}
				}
			}
		}
	}
	// Fallback: scan all module scopes for local/private functions.
	lock t.env.scopes {
		scope_names := t.env.scopes.keys()
		for module_name in scope_names {
			scope_ptr := t.env.scopes[module_name] or { continue }
			mut scope := unsafe { scope_ptr }
			if obj := scope.lookup_parent(fn_name, 0) {
				if obj is types.Fn {
					fn_typ := obj.get_typ()
					if fn_typ is types.FnType {
						return fn_typ.get_return_type()
					}
				}
			}
		}
	}
	return none
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
	// Check if this is a method call (CallExpr or CallOrCastExpr with SelectorExpr lhs)
	mut sel_expr := ast.SelectorExpr{}
	mut has_sel := false
	if expr is ast.CallExpr {
		if expr.lhs is ast.SelectorExpr {
			sel_expr = expr.lhs as ast.SelectorExpr
			has_sel = true
		}
	} else if expr is ast.CallOrCastExpr {
		if expr.lhs is ast.SelectorExpr {
			sel_expr = expr.lhs as ast.SelectorExpr
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
		if sel_expr.lhs is ast.SelectorExpr {
			selector_type_name := t.get_selector_type_name(sel_expr.lhs as ast.SelectorExpr)
			if selector_type_name != '' {
				t.append_method_lookup_type_name(mut lookup_type_names, selector_type_name)
			}
		} else if sel_expr.lhs is ast.Ident {
			var_type_name := t.get_var_type_name(sel_expr.lhs.name)
			t.append_method_lookup_type_name(mut lookup_type_names, var_type_name)
		}
		if ret_type := t.lookup_method_return_type(lookup_type_names, method_name) {
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
	if method_key == type_name {
		return true
	}
	short_type := if type_name.contains('__') {
		type_name.all_after_last('__')
	} else {
		type_name
	}
	short_key := if method_key.contains('__') {
		method_key.all_after_last('__')
	} else {
		method_key
	}
	if short_key == short_type {
		return true
	}
	if method_key.ends_with('__${short_type}') {
		return true
	}
	if type_name.ends_with('__${short_key}') {
		return true
	}
	return false
}

fn (t &Transformer) lookup_method_return_type(type_names []string, method_name string) ?types.Type {
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
		if fn_type := t.env.lookup_method(raw_name, method_name) {
			return fn_type.get_return_type()
		}
	}
	lock t.env.methods {
		method_keys := t.env.methods.keys()
		for key in method_keys {
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
			methods_for_type := t.env.methods[key] or { continue }
			for method in methods_for_type {
				if method.get_name() != method_name {
					continue
				}
				method_typ := method.get_typ()
				if method_typ is types.FnType {
					return method_typ.get_return_type()
				}
			}
		}
	}
	return none
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
		return t.lookup_var_type(expr.name)
	}
	if expr is ast.IndexExpr {
		// For slices (a[x..y]), the result type is the same as the container
		if expr.expr is ast.RangeExpr {
			return t.resolve_expr_type(expr.lhs)
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
	pos := expr.pos()
	if pos.is_valid() {
		if typ := t.env.get_expr_type(pos.id) {
			if typ is types.OptionType {
				return true
			}
		}
	}
	// Fallback: try method return type lookup
	if ret_type := t.get_method_return_type(expr) {
		return ret_type is types.OptionType
	}
	return false
}

// expr_returns_result checks if an expression returns a Result type by looking up
// its type from the checker's environment. Works for both function and method calls.
fn (t &Transformer) expr_returns_result(expr ast.Expr) bool {
	if !expr_has_valid_data(expr) {
		return false
	}
	pos := expr.pos()
	if pos.is_valid() {
		if typ := t.env.get_expr_type(pos.id) {
			if typ is types.ResultType {
				return true
			}
		}
	}
	// Fallback: try method return type lookup
	if ret_type := t.get_method_return_type(expr) {
		return ret_type is types.ResultType
	}
	return false
}

// get_expr_base_type gets the base type name for an expression returning Result/Option
fn (t &Transformer) get_expr_base_type(expr ast.Expr) string {
	if !expr_has_valid_data(expr) {
		return ''
	}
	pos := expr.pos()
	if pos.is_valid() {
		if typ := t.env.get_expr_type(pos.id) {
			match typ {
				types.ResultType {
					return typ.base_type.name()
				}
				types.OptionType {
					return typ.base_type.name()
				}
				else {}
			}
		}
	}
	// Fallback: try method return type lookup
	if ret_type := t.get_method_return_type(expr) {
		match ret_type {
			types.ResultType {
				return ret_type.base_type.name()
			}
			types.OptionType {
				return ret_type.base_type.name()
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
		if expr.lhs is ast.Ident {
			return expr.lhs.name
		}
		// Handle module-qualified calls: strconv.common_parse_int(...)
		if expr.lhs is ast.SelectorExpr {
			return expr.lhs.rhs.name
		}
	}
	if expr is ast.CallOrCastExpr {
		if expr.lhs is ast.Ident {
			return expr.lhs.name
		}
		// Handle module-qualified calls: strconv.common_parse_int(...)
		if expr.lhs is ast.SelectorExpr {
			return expr.lhs.rhs.name
		}
	}
	return ''
}

// is_void_call_expr checks if an expression is a function call that returns void.
// Used to detect or-blocks that end with a void call (e.g. error_with_pos()).
fn (mut t Transformer) is_void_call_expr(expr ast.Expr) bool {
	fn_name := t.get_call_fn_name(expr)
	if fn_name == '' {
		return false
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
	// Check using fn return type lookup
	if ret := t.get_fn_return_type(fn_name) {
		ret_name := ret.name()
		if ret_name != '' && ret_name != 'void' && ret_name != 'Void' {
			return false
		}
		return true
	}
	// No return type found — likely a void function
	return true
}

fn (mut t Transformer) transform_fn_decl(decl ast.FnDecl) ast.FnDecl {
	// Skip uninstantiated generic functions - their bodies were never type-checked
	// and they will never be called, so emit an empty body.
	if decl.typ.generic_params.len > 0 {
		has_generic_types := decl.name in t.env.generic_types
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
	if fn_scope := t.env.get_fn_scope(t.cur_module, scope_fn_name) {
		t.scope = fn_scope
		// Set fn_root_scope so temp variables can be registered here
		t.fn_root_scope = fn_scope
	} else {
		// Fallback: create a new scope if function scope not found
		t.open_scope()
		t.fn_root_scope = t.scope
	}

	// Set current function return type for sum type wrapping in returns
	// and enum shorthand resolution
	old_fn_ret_type_name := t.cur_fn_ret_type_name
	if decl.typ.return_type is ast.Ident {
		ret_name := decl.typ.return_type.name
		// Qualify with module prefix for enum shorthand resolution
		// (e.g., Token → token__Token so resolve_enum_shorthand produces token__Token__member)
		if t.cur_module != '' && t.cur_module != 'main' && t.cur_module != 'builtin'
			&& !ret_name.contains('__') {
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

	// Transform function body
	// Clear per-function state: array_elem_type_overrides tracks .map() result types
	// and must not leak across function boundaries (e.g., variable 'a' in one function
	// must not affect variable 'a' in another function).
	t.array_elem_type_overrides = map[string]string{}
	old_fn_name_str := t.cur_fn_name_str
	t.cur_fn_name_str = decl.name
	transformed_stmts := t.transform_stmts(decl.stmts)
	t.cur_fn_name_str = old_fn_name_str
	t.cur_fn_ret_type_name = old_fn_ret_type_name

	// Lower defer statements: collect defers, remove them from body,
	// inject defer body before every return and at end of function
	has_return_type := decl.typ.return_type !is ast.EmptyExpr
	final_stmts := t.lower_defer_stmts(transformed_stmts, has_return_type)

	// Restore previous scope and fn_root_scope
	t.scope = old_scope
	t.fn_root_scope = old_fn_root_scope

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
	// Inline generic math functions for native backends (arm64/x64).
	// The SSA builder skips generic function declarations, so abs[T], min[T], max[T]
	// become unresolved symbols. Inline them as simple if-expressions.
	if t.pref != unsafe { nil } && (t.pref.backend == .arm64 || t.pref.backend == .x64) {
		if inlined := t.try_inline_generic_math_call(expr) {
			return inlined
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
		// Skip calls to conditionally compiled functions (e.g., @[if verbose ?])
		if sel.rhs.name in t.elided_fns {
			return ast.Expr(ast.BasicLiteral{
				kind:  .number
				value: '0'
			})
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
				if result := t.transform_sort_call(sel.lhs, sel.rhs.name, [expr.args[0]],
					expr.pos)
				{
					return result
				}
			}
		}
		method_name := sel.rhs.name
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
			if t.get_method_return_type(ast.Expr(expr)) == none {
				str_fn_info := t.get_str_fn_info_for_expr(sel.lhs)
				if str_fn_info.str_fn_name != '' {
					t.needed_str_fns[str_fn_info.str_fn_name] = str_fn_info.elem_type
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
			receiver_str := t.expr_to_string(sel.lhs)
			if ctx := t.find_smartcast_for_expr(receiver_str) {
				// Check if the method exists on the variant type. If not, the method
				// is defined on the sum type and we should NOT apply the smartcast
				// to the receiver. E.g. `for cur is types.Alias { cur.base_type() }`
				// where base_type() is defined on types.Type (the sum type), not on Alias.
				variant_has_method := t.env.lookup_method(ctx.variant, sel.rhs.name) != none
					|| t.env.lookup_method(ctx.variant_full, sel.rhs.name) != none
				if variant_has_method {
					// Transform receiver with smart cast and keep the method call structure
					casted_receiver := t.apply_smartcast_receiver_ctx(sel.lhs, ctx)
					mut args := []ast.Expr{cap: expr.args.len}
					for arg in expr.args {
						args << t.transform_expr(arg)
					}
					return ast.CallExpr{
						lhs:  ast.SelectorExpr{
							lhs: casted_receiver
							rhs: sel.rhs
							pos: sel.pos
						}
						args: args
						pos:  expr.pos
					}
				}
			}
		}
		// Check for interface method call: iface.method(args...)
		if t.is_interface_receiver(sel.lhs) {
			// Native backends (arm64/x64): resolve to direct concrete method call.
			// `iface.method(args...)` → `ConcreteType__method(iface, args...)`
			if t.pref != unsafe { nil } && (t.pref.backend == .arm64 || t.pref.backend == .x64) {
				if sel.lhs is ast.Ident {
					if concrete := t.get_interface_concrete_type(sel.lhs.name) {
						resolved_method := '${concrete}__${sel.rhs.name}'
						mut native_args := []ast.Expr{cap: expr.args.len + 1}
						native_args << t.transform_expr(sel.lhs)
						for arg in expr.args {
							native_args << t.transform_expr(arg)
						}
						return ast.CallExpr{
							lhs:  ast.Ident{
								name: resolved_method
							}
							args: native_args
							pos:  expr.pos
						}
					}
				}
			}
			// C/cleanc backends: Transform to vtable dispatch
			// Prepend iface._object to the args list
			mut new_args := []ast.Expr{cap: expr.args.len + 1}
			new_args << t.synth_selector(sel.lhs, '_object', types.Type(types.voidptr_))
			for arg in expr.args {
				new_args << t.transform_expr(arg)
			}
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
				// Get the str function name for the argument type
				if str_fn_name := t.get_str_fn_name_for_expr(arg) {
					mut str_call_args := []ast.Expr{cap: 1}
					str_call_args << t.transform_expr(arg)
					// Transform to println(Type_str(arg))
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
					mut args := []ast.Expr{cap: expr.args.len}
					for arg in expr.args {
						args << t.transform_expr(arg)
					}
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
		is_module_call := sel.lhs is ast.Ident && t.get_module_scope(sel.lhs.name) != none
			&& t.lookup_var_type(sel.lhs.name) == none
		if !is_module_call {
			if resolved := t.resolve_method_call_name(sel.lhs, sel.rhs.name) {
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
				mut transformed_call_args := []ast.Expr{cap: call_args.len}
				for arg in call_args {
					transformed_call_args << t.transform_expr(arg)
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

struct CallFnInfo {
	param_types []types.Type
	param_names []string
	is_variadic bool
}

fn (t &Transformer) lookup_call_fn_info(lhs ast.Expr) ?CallFnInfo {
	// Prefer checker-resolved callable type for this exact call target.
	// This is the most reliable source for method parameter info (names + types).
	if lhs_type := t.get_expr_type(lhs) {
		callable := t.unwrap_alias_and_pointer_type(lhs_type)
		if callable is types.FnType {
			return CallFnInfo{
				param_types: callable.get_param_types()
				param_names: callable.get_param_names()
				is_variadic: callable.is_variadic_fn()
			}
		}
	}
	if lhs is ast.Ident {
		if t.cur_module != '' {
			if fn_type := t.env.lookup_fn(t.cur_module, lhs.name) {
				return CallFnInfo{
					param_types: fn_type.get_param_types()
					param_names: fn_type.get_param_names()
					is_variadic: fn_type.is_variadic_fn()
				}
			}
		}
		if fn_type := t.env.lookup_fn('builtin', lhs.name) {
			return CallFnInfo{
				param_types: fn_type.get_param_types()
				param_names: fn_type.get_param_names()
				is_variadic: fn_type.is_variadic_fn()
			}
		}
		return none
	}
	if lhs is ast.SelectorExpr {
		if lhs.lhs is ast.Ident {
			mod_name := (lhs.lhs as ast.Ident).name
			if fn_type := t.env.lookup_fn(mod_name, lhs.rhs.name) {
				return CallFnInfo{
					param_types: fn_type.get_param_types()
					param_names: fn_type.get_param_names()
					is_variadic: fn_type.is_variadic_fn()
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
				if fn_type := t.env.lookup_method(name, lhs.rhs.name) {
					return CallFnInfo{
						param_types: fn_type.get_param_types()
						param_names: fn_type.get_param_names()
						is_variadic: fn_type.is_variadic_fn()
					}
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
	recv_type := t.get_expr_type(receiver) or {
		// When type info is unavailable (e.g. methods on typed arrays like []string
		// where the scope didn't load), try 'array' as fallback for methods that are
		// unique to arrays. Skip methods that also exist on string/map/other types
		// since we can't disambiguate without type info.
		if method_name !in ['str', 'hex', 'clone', 'free', 'trim', 'bytes', 'bytestr', 'replace',
			'contains', 'len', 'index', 'last_index', 'is_blank', 'join', 'to_upper', 'to_lower',
			'repeat', 'vbytes', 'plus_two', 'write_u8', 'write_string', 'write_rune'] {
			if t.env.lookup_method('array', method_name) != none {
				return 'array__${method_name}'
			}
		}
		return none
	}
	base_type := t.unwrap_alias_and_pointer_type(recv_type)
	c_prefix := t.receiver_type_to_c_prefix(base_type)
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
		if t.env.lookup_method(type_name, method_name) != none {
			return '${alias_c_name}__${method_name}'
		}
	}
	// Verify method exists via env.lookup_method
	for name in lookup_names {
		if t.env.lookup_method(name, method_name) != none {
			// For array types: if the method is NOT on generic 'array' but on
			// a typed array (e.g., []rune.string()), use the specific C type name
			// (e.g., Array_rune) instead of generic 'array'.
			if c_prefix == 'array' && t.env.lookup_method('array', method_name) == none {
				specific_name := t.type_to_c_name(base_type)
				return '${specific_name}__${method_name}'
			}
			return '${c_prefix}__${method_name}'
		}
	}
	// Fuzzy fallback: iterate method keys to find matching receiver types
	lock t.env.methods {
		method_keys := t.env.methods.keys()
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
			methods_for_type := t.env.methods[key] or { continue }
			for method in methods_for_type {
				if method.get_name() == method_name {
					return '${c_prefix}__${method_name}'
				}
			}
		}
	}
	return none
}

fn (mut t Transformer) lower_missing_call_args(lhs ast.Expr, args []ast.Expr) []ast.Expr {
	info := t.lookup_call_fn_info(lhs) or { return args }
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
			arg_type := t.get_expr_type(last_arg) or { return args }
			unwrapped := t.unwrap_alias_and_pointer_type(arg_type)
			if unwrapped is types.Array {
				return args
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
			cur = cur.base_type()
		}
		inner := cur.base_type()
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
		panic('bug in v2 compiler: struct-shorthand call has more args than params')
	}
	param_type := param_types[param_idx]
	base := t.unwrap_alias_and_pointer_type(param_type)
	if base !is types.Struct {
		panic('bug in v2 compiler: FieldInit call args must be lowered in v2.transformer (param ${param_idx} is not a struct)')
	}
	mut init_typ := param_type
	if t.is_pointer_type(param_type) {
		mut cur := param_type
		for cur is types.Alias {
			cur = cur.base_type()
		}
		inner := cur.base_type()
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
	// Inline generic math functions for native backends (arm64/x64).
	if t.pref != unsafe { nil } && (t.pref.backend == .arm64 || t.pref.backend == .x64) {
		if inlined := t.try_inline_generic_math_coce(expr) {
			return inlined
		}
	}
	// Expand .filter() / .map() calls to hoisted statements + temp variable
	if expanded := t.try_expand_filter_or_map_expr(expr) {
		return expanded
	}
	// Check if this is a flag enum method call: receiver.has(arg) or receiver.all(arg)
	if expr.lhs is ast.SelectorExpr {
		sel := expr.lhs as ast.SelectorExpr
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
			receiver_str := t.expr_to_string(sel.lhs)
			if ctx := t.find_smartcast_for_expr(receiver_str) {
				// Check if the method exists on the variant type. If not, the method
				// is defined on the sum type and we should NOT apply the smartcast
				// to the receiver.
				variant_has_method := t.env.lookup_method(ctx.variant, sel.rhs.name) != none
					|| t.env.lookup_method(ctx.variant_full, sel.rhs.name) != none
				casted_receiver := if variant_has_method {
					t.apply_smartcast_receiver_ctx(sel.lhs, ctx)
				} else {
					t.transform_expr(sel.lhs)
				}
				mut args := []ast.Expr{cap: 1}
				if expr.expr !is ast.EmptyExpr {
					args << t.transform_expr(expr.expr)
				}
				// Resolve method name using the ORIGINAL (pre-smartcast) receiver,
				// then build the result directly. Do NOT route through transform_call_expr
				// because it would re-transform the already-casted receiver and args,
				// causing double smartcast dereferences.
				if resolved := t.resolve_method_call_name(sel.lhs, sel.rhs.name) {
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
			// Native backends (arm64/x64): resolve to direct concrete method call.
			if t.pref != unsafe { nil } && (t.pref.backend == .arm64 || t.pref.backend == .x64) {
				if sel.lhs is ast.Ident {
					if concrete := t.get_interface_concrete_type(sel.lhs.name) {
						resolved_iface_method := '${concrete}__${sel.rhs.name}'
						mut native_iface_args := []ast.Expr{cap: 2}
						native_iface_args << t.transform_expr(sel.lhs)
						if expr.expr !is ast.EmptyExpr {
							native_iface_args << t.transform_expr(expr.expr)
						}
						return ast.CallExpr{
							lhs:  ast.Ident{
								name: resolved_iface_method
							}
							args: native_iface_args
							pos:  expr.pos
						}
					}
				}
			}
			// C/cleanc backends: Transform to vtable dispatch
			return ast.CallExpr{
				lhs:  ast.Expr(expr.lhs) // Keep the selector: iface.method
				args: [
					t.synth_selector(sel.lhs, '_object', types.Type(types.voidptr_)),
					t.transform_expr(expr.expr),
				]
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
	sumtype_name := t.type_expr_name_full(expr.lhs)
	if sumtype_name != '' && t.call_or_cast_lhs_is_type(expr.lhs) && t.is_sum_type(sumtype_name) {
		if wrapped := t.wrap_sumtype_value(expr.expr, sumtype_name) {
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
		is_module_call := sel.lhs is ast.Ident && t.get_module_scope(sel.lhs.name) != none
			&& t.lookup_var_type(sel.lhs.name) == none
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
				call_args = t.lower_missing_call_args(expr.lhs, call_args)
				mut transformed_call_args := []ast.Expr{cap: call_args.len}
				for arg in call_args {
					transformed_call_args << t.transform_expr(arg)
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
	// Default: transform lhs and expression recursively
	// This is important for smart cast propagation through method chains
	transformed_lhs := t.transform_expr(expr.lhs)
	transformed_arg := t.transform_expr(expr.expr)
	return t.lower_call_or_cast_expr(transformed_lhs, transformed_arg, expr.pos)
}

// transform_call_arg_with_sumtype_check transforms a call argument, temporarily
// disabling any active smartcast when the function parameter is a sumtype.
// This prevents smartcast from unwrapping a sumtype value that should be passed as-is.
fn (mut t Transformer) transform_call_arg_with_sumtype_check(arg ast.Expr, fn_info ?CallFnInfo, idx int) ast.Expr {
	if info := fn_info {
		if idx < info.param_types.len {
			param_c_name := t.type_to_c_name(info.param_types[idx])
			if param_c_name != '' && t.is_sum_type(param_c_name) {
				arg_str := t.expr_to_string(arg)
				if arg_str != '' {
					if ctx := t.find_smartcast_for_expr(arg_str) {
						// Only disable smartcast if parameter expects the SAME sumtype
						// as the arg's original sumtype. This avoids incorrectly removing
						// smartcasts when the parameter type is a DIFFERENT sumtype that
						// happens to be the smartcast variant (e.g., ast.Expr smartcast
						// to ast.Type, where ast.Type is itself a sumtype).
						if ctx.sumtype == param_c_name {
							if existing := t.remove_smartcast_for_expr(arg_str) {
								result := t.transform_expr(arg)
								t.push_smartcast_full(existing.expr, existing.variant,
									existing.variant_full, existing.sumtype)
								return result
							}
						}
					}
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
	return name in ['FILE', 'DIR', 'va_list', 'pthread_t', 'pthread_mutex_t', 'pthread_cond_t',
		'pthread_rwlock_t', 'pthread_attr_t', 'stat', 'tm', 'timespec', 'timeval', 'dirent',
		'termios', 'sockaddr', 'sockaddr_in', 'sockaddr_in6', 'sockaddr_un',
		'mach_timebase_info_data_t']
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
				if fn_typ := t.env.lookup_method(type_name, fn_name) {
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
			return ast.Expr(ast.IfExpr{
				cond:      ast.InfixExpr{
					op:  .lt
					lhs: arg
					rhs: ast.BasicLiteral{
						kind:  .number
						value: '0'
					}
				}
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
			return ast.Expr(ast.IfExpr{
				cond:      ast.InfixExpr{
					op:  .lt
					lhs: arg
					rhs: ast.BasicLiteral{
						kind:  .number
						value: '0'
					}
				}
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
			return ast.Expr(ast.IfExpr{
				cond:      ast.InfixExpr{
					op:  .lt
					lhs: arg
					rhs: ast.BasicLiteral{
						kind:  .number
						value: '0'
					}
				}
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
			return ast.Expr(ast.IfExpr{
				cond:      ast.InfixExpr{
					op:  .lt
					lhs: arg
					rhs: ast.BasicLiteral{
						kind:  .number
						value: '0'
					}
				}
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
fn (t &Transformer) make_inline_if_expr(a ast.Expr, b ast.Expr, op token.Token) ast.Expr {
	return ast.IfExpr{
		cond:      ast.InfixExpr{
			op:  op
			lhs: a
			rhs: b
		}
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

fn (t &Transformer) minof_constant(type_name string) ?ast.Expr {
	// For i64 min, use subtraction expression to avoid overflow in literal parsing:
	// -9223372036854775807 - 1
	if type_name == 'i64' {
		return ast.Expr(ast.InfixExpr{
			op:  .minus
			lhs: ast.PrefixExpr{
				op:   .minus
				expr: ast.BasicLiteral{
					kind:  .number
					value: '9223372036854775807'
				}
			}
			rhs: ast.BasicLiteral{
				kind:  .number
				value: '1'
			}
		})
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
