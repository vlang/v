// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module cleanc

import v2.ast
import v2.types
import strings

fn (mut g Gen) is_error_call_expr(expr ast.Expr) bool {
	match expr {
		ast.CallExpr {
			if expr.lhs is ast.Ident {
				name := sanitize_fn_ident(expr.lhs.name)
				if name == 'error' {
					return true
				}
			}
			// Check if the call returns IError
			ret := g.get_call_return_type(expr.lhs, expr.args.len) or { '' }
			if ret == 'IError' {
				return true
			}
		}
		else {}
	}
	// Also check environment type
	expr_type := g.get_expr_type(expr)
	if expr_type == 'IError' {
		return true
	}
	return false
}

pub fn (mut g Gen) set_cached_init_calls(calls []string) {
	g.cached_init_calls = calls.clone()
}

pub fn (mut g Gen) set_used_fn_keys(used map[string]bool) {
	g.used_fn_keys = used.clone()
}

fn (g &Gen) should_emit_fn_decl(module_name string, decl ast.FnDecl) bool {
	_ = g
	_ = module_name
	_ = decl
	// Until markused keying is complete for all v2 self-host call edges,
	// emit all parsed function declarations in cleanc mode.
	return true
}

fn (mut g Gen) emit_cached_init_call_decls() {
	if g.cached_init_calls.len == 0 {
		return
	}
	for fn_name in g.cached_init_calls {
		key := 'cached_init_decl_${fn_name}'
		if key in g.emitted_types {
			continue
		}
		g.emitted_types[key] = true
		g.sb.writeln('void ${fn_name}(void);')
	}
	g.sb.writeln('')
}

fn (mut g Gen) ensure_tuple_alias_for_fn_return(node ast.FnDecl, ret_type string) {
	tuple_name := tuple_payload_type_name(ret_type)
	if tuple_name == '' || tuple_name in g.tuple_aliases {
		return
	}
	// Resolve tuple element types from the Environment's function type metadata.
	if g.env != unsafe { nil } {
		fn_name := if node.is_method {
			v_type_name := g.receiver_type_to_scope_name(node.receiver.typ)
			if v_type_name != '' {
				'${v_type_name}__${node.name}'
			} else {
				node.name
			}
		} else {
			node.name
		}
		if fields := g.tuple_fields_from_env(fn_name) {
			if fields.len > 0 {
				g.tuple_aliases[tuple_name] = fields.clone()
			}
			return
		}
	}
}

// tuple_fields_from_env looks up a function in the module scope and extracts
// tuple element types from its return type.
fn (mut g Gen) tuple_fields_from_env(fn_name string) ?[]string {
	if g.env == unsafe { nil } {
		return none
	}
	// Look up the function object in the module scope.
	if mut mod_scope := g.env_scope(g.cur_module) {
		if obj := mod_scope.lookup_parent(fn_name, 0) {
			if obj is types.Fn {
				fn_obj := obj as types.Fn
				fn_typ := fn_obj.get_typ()
				if fn_typ is types.FnType {
					return g.extract_tuple_fields_from_return_type(fn_typ)
				}
			}
		}
	}
	// Also check builtin scope for builtin functions.
	if g.cur_module != 'builtin' {
		if mut builtin_scope := g.env_scope('builtin') {
			if obj := builtin_scope.lookup_parent(fn_name, 0) {
				if obj is types.Fn {
					fn_obj := obj as types.Fn
					fn_typ := fn_obj.get_typ()
					if fn_typ is types.FnType {
						return g.extract_tuple_fields_from_return_type(fn_typ)
					}
				}
			}
		}
	}
	return none
}

fn (mut g Gen) extract_tuple_fields_from_return_type(fn_typ types.FnType) ?[]string {
	ret_type := fn_typ.get_return_type() or { return none }
	// Unwrap Option/Result wrappers to get the payload type.
	inner := match ret_type {
		types.OptionType { ret_type.base_type }
		types.ResultType { ret_type.base_type }
		else { ret_type }
	}
	if inner is types.Tuple {
		tuple_types := inner.get_types()
		mut fields := []string{cap: tuple_types.len}
		for elem_type in tuple_types {
			c := g.types_type_to_c(elem_type)
			if c == '' {
				return none
			}
			fields << c
		}
		return fields
	}
	return none
}

fn (mut g Gen) collect_fn_signatures() {
	for file in g.files {
		g.set_file_module(file)
		for stmt in file.stmts {
			if !stmt_has_valid_data(stmt) {
				continue
			}
			match stmt {
				ast.FnDecl {
					if stmt.language == .js {
						continue
					}
					mut fn_name := ''
					if stmt.stmts.len == 0 && stmt.language == .c {
						// Keep raw symbol names for C / system declaration-only functions.
						fn_name = sanitize_fn_ident(stmt.name)
					} else {
						fn_name = g.get_fn_name(stmt)
					}
					if fn_name == '' {
						continue
					}
					mut ret_type := if stmt.name == 'main' {
						'int'
					} else if stmt.typ.return_type !is ast.EmptyExpr {
						g.expr_type_to_c(stmt.typ.return_type)
					} else {
						'void'
					}
					ret_type = normalize_signature_type_name(ret_type, 'void')
					if ret_type.starts_with('_option_') || ret_type.starts_with('_result_') {
						g.register_alias_type(ret_type)
					}
					g.ensure_tuple_alias_for_fn_return(stmt, ret_type)
					mut params := []bool{}
					mut param_types := []string{}
					if stmt.is_method && stmt.receiver.name != '' {
						recv_type := normalize_signature_type_name(g.expr_type_to_c(stmt.receiver.typ),
							'void*')
						params << (stmt.receiver.is_mut || recv_type.ends_with('*'))
						param_types << if stmt.receiver.is_mut && !recv_type.ends_with('*') {
							recv_type + '*'
						} else {
							recv_type
						}
					}
					for param in stmt.typ.params {
						param_type := normalize_signature_type_name(g.expr_type_to_c(param.typ),
							'int')
						params << (param.is_mut || param_type.ends_with('*'))
						param_types << if param.is_mut && !param_type.ends_with('*') {
							param_type + '*'
						} else {
							param_type
						}
					}
					g.fn_param_is_ptr[fn_name] = params
					g.fn_param_types[fn_name] = param_types
					g.fn_return_types[fn_name] = ret_type
				}
				else {}
			}
		}
	}
}

fn (mut g Gen) gen_fn_decl(node ast.FnDecl) {
	if !g.should_emit_fn_decl(g.cur_module, node) {
		return
	}
	// Generate V and .c.v function bodies, but skip JS and C extern declarations.
	if node.language == .js {
		return
	}
	// Header files carry declaration-only function nodes.
	if g.cur_file_name.ends_with('.vh') {
		return
	}
	if node.language == .c && node.stmts.len == 0 {
		return
	}
	// Generic functions: handled via emit_generic_fn_macro in the forward declaration pass
	if node.typ.generic_params.len > 0 {
		return
	}

	fn_name := g.get_fn_name(node)
	if fn_name == '' {
		return
	}
	fn_key := 'fn_${fn_name}'
	if fn_key in g.emitted_types {
		return
	}
	g.emitted_types[fn_key] = true

	// Set function scope for type lookups
	g.cur_fn_name = node.name
	g.runtime_local_types = map[string]string{}
	g.is_module_ident_cache = map[string]bool{}
	g.not_local_var_cache = map[string]bool{}
	g.resolved_module_names = map[string]string{}
	g.cur_fn_ret_type = if node.name == 'main' {
		'int'
	} else if fn_ret := g.fn_return_types[fn_name] {
		fn_ret
	} else if node.typ.return_type !is ast.EmptyExpr {
		g.expr_type_to_c(node.typ.return_type)
	} else {
		'void'
	}
	g.cur_fn_ret_type = normalize_signature_type_name(g.cur_fn_ret_type, 'void')
	if g.env != unsafe { nil } {
		// For methods, the checker stores scopes using V-style receiver type names
		// (e.g. "[]string__free"), while get_fn_name returns C-style names
		// (e.g. "Array_string__free"). Use V-style name for scope lookup.
		scope_fn_name := if node.is_method {
			// The checker stores scopes using V-style receiver type names
			// (e.g. "[]string__free", "Builder__go_back"). Convert the AST
			// receiver type expression to match.
			v_type_name := g.receiver_type_to_scope_name(node.receiver.typ)
			if v_type_name != '' {
				'${v_type_name}__${node.name}'
			} else {
				fn_name
			}
		} else {
			node.name
		}
		if fn_scope := g.env.get_fn_scope(g.cur_module, scope_fn_name) {
			g.cur_fn_scope = fn_scope
		} else if node.is_method {
			// Fallback: for type aliases (e.g. Builder = []u8), the checker resolves
			// the alias and uses the underlying type name. Try env-based resolution.
			mut found := false
			if expr_has_valid_data(node.receiver.typ) {
				receiver_pos := node.receiver.typ.pos()
				if receiver_pos.is_valid() {
					if recv_type := g.env.get_expr_type(receiver_pos.id) {
						base_type := recv_type.base_type()
						alt_scope_name := '${base_type.name()}__${node.name}'
						if fn_scope2 := g.env.get_fn_scope(g.cur_module, alt_scope_name) {
							g.cur_fn_scope = fn_scope2
							found = true
						}
					}
				}
			}
			if !found {
				g.cur_fn_scope = unsafe { nil }
			}
		} else {
			g.cur_fn_scope = unsafe { nil }
		}
	}

	// Track mut parameter names for pointer detection
	g.cur_fn_mut_params = map[string]bool{}
	if node.is_method && node.receiver.name != '' && node.receiver.is_mut {
		g.cur_fn_mut_params[node.receiver.name] = true
	}
	for param in node.typ.params {
		if param.is_mut {
			g.cur_fn_mut_params[param.name] = true
		}
		// Register parameter types from the function declaration.
		// The declaration type is authoritative and ensures correct types
		// even when the env position lookup returns wrong results.
		param_type := g.expr_type_to_c(param.typ)
		if param_type != '' && param.name != '' {
			ptype := if param.is_mut && !param_type.ends_with('*') {
				param_type + '*'
			} else {
				param_type
			}
			g.runtime_local_types[param.name] = ptype
		}
	}

	// Generate function header
	g.gen_fn_head(node)
	g.sb.writeln(' {')
	g.indent++

	// Main function: initialize argc/argv
	if node.name == 'main' {
		g.write_indent()
		g.sb.writeln('g_main_argc = ___argc;')
		g.write_indent()
		g.sb.writeln('g_main_argv = (void*)___argv;')
		for init_call in g.cached_init_calls {
			g.write_indent()
			g.sb.writeln('${init_call}();')
		}
	}
	g.gen_stmts(node.stmts)

	// Implicit return 0 for main
	if node.name == 'main' {
		g.write_indent()
		g.sb.writeln('return 0;')
	}

	g.indent--
	g.sb.writeln('}')
	g.sb.writeln('')
}

fn (mut g Gen) gen_fn_head(node ast.FnDecl) {
	fn_name := g.get_fn_name(node)
	mut ret := if fn_ret := g.fn_return_types[fn_name] {
		fn_ret
	} else if node.typ.return_type !is ast.EmptyExpr {
		g.expr_type_to_c(node.typ.return_type)
	} else {
		'void'
	}
	ret = normalize_signature_type_name(ret, 'void')
	if node.name == 'main' {
		ret = 'int'
	}
	sig_param_types := g.fn_param_types[fn_name] or { []string{} }

	// main takes argc/argv
	if node.name == 'main' {
		g.sb.write_string(ret)
		g.sb.write_string(' ')
		g.sb.write_string(fn_name)
		g.sb.write_string('(int ___argc, char** ___argv)')
		return
	}

	g.sb.write_string(ret)
	g.sb.write_string(' ')
	g.sb.write_string(fn_name)
	g.sb.write_string('(')

	mut first := true
	mut sig_idx := 0
	// Receiver as first param for methods
	if node.is_method && node.receiver.name != '' {
		receiver_type := if sig_idx < sig_param_types.len {
			normalize_signature_type_name(sig_param_types[sig_idx], 'void*')
		} else if node.receiver.is_mut {
			rt := normalize_signature_type_name(g.expr_type_to_c(node.receiver.typ), 'void*')
			if rt.ends_with('*') {
				rt
			} else {
				rt + '*'
			}
		} else {
			normalize_signature_type_name(g.expr_type_to_c(node.receiver.typ), 'void*')
		}
		g.sb.write_string(receiver_type)
		g.sb.write_string(' ')
		g.sb.write_string(node.receiver.name)
		sig_idx++
		first = false
	}

	for param in node.typ.params {
		if !first {
			g.sb.write_string(', ')
		}
		first = false
		t := if sig_idx < sig_param_types.len {
			normalize_signature_type_name(sig_param_types[sig_idx], 'int')
		} else if param.is_mut {
			pt := normalize_signature_type_name(g.expr_type_to_c(param.typ), 'int')
			if pt.ends_with('*') {
				pt
			} else {
				pt + '*'
			}
		} else {
			normalize_signature_type_name(g.expr_type_to_c(param.typ), 'int')
		}
		g.sb.write_string(t)
		g.sb.write_string(' ')
		g.sb.write_string(param.name)
		sig_idx++
	}
	g.sb.write_string(')')
}

fn (mut g Gen) get_fn_name(node ast.FnDecl) string {
	if node.name == 'main' {
		return 'main'
	}
	// Prevent collisions with libc symbols from builtin wrappers.
	if !node.is_method && (node.name in c_stdlib_fns || is_c_runtime_function(node.name))
		&& (g.cur_module == '' || g.cur_module == 'main' || g.cur_module == 'builtin') {
		return ''
	}
	name := sanitize_fn_ident(node.name)
	// Methods: ReceiverType__method_name
	if node.is_method && node.receiver.name != '' {
		// For pointer alias types (byteptr, charptr), use the V name directly
		// to avoid collisions. e.g., byteptr.str() -> byteptr__str, not u8__str
		// which would conflict with the actual u8.str() method.
		if node.receiver.typ is ast.Ident && node.receiver.typ.name in ['byteptr', 'charptr'] {
			return node.receiver.typ.name + '__' + name
		}
		receiver_type := g.expr_type_to_c(node.receiver.typ)
		// Strip pointer suffix for method naming
		base_type := if receiver_type.ends_with('*') {
			receiver_type[..receiver_type.len - 1]
		} else {
			receiver_type
		}
		return base_type + '__' + name
	}
	// Static methods: Type.method() -> Type__method
	if node.is_static {
		receiver_type := g.expr_type_to_c(node.receiver.typ)
		base_type := if receiver_type.ends_with('*') {
			receiver_type[..receiver_type.len - 1]
		} else {
			receiver_type
		}
		return base_type + '__' + name
	}
	if g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin' {
		return g.cur_module + '__' + name
	}
	return name
}

fn (g &Gen) contains_call_expr(e ast.Expr) bool {
	if e is ast.CallExpr {
		return true
	}
	if e is ast.CallOrCastExpr {
		return true
	}
	if e is ast.CastExpr {
		return g.contains_call_expr(e.expr)
	}
	if e is ast.ParenExpr {
		return g.contains_call_expr(e.expr)
	}
	if e is ast.ArrayInitExpr {
		for elem in e.exprs {
			if g.contains_call_expr(elem) {
				return true
			}
		}
	}
	return false
}

fn (g Gen) get_fn_type_from_expr(e ast.Expr) ?ast.FnType {
	if e is ast.Type {
		if e is ast.FnType {
			return e
		}
	}
	return none
}

// Needed for self
fn (mut g Gen) expr_is_pointer(arg ast.Expr) bool {
	base_arg := if arg is ast.ModifierExpr { arg.expr } else { arg }
	if base_arg is ast.Ident {
		if base_arg.name == 'nil' {
			return true
		}
		// Prefer function-scope type information for identifiers.
		// Environment lookups can over-approximate loop variables as pointers.
		if local_type := g.get_local_var_c_type(base_arg.name) {
			if base_arg.name in g.cur_fn_mut_params {
				return true
			}
			return local_type.ends_with('*') || local_type in ['voidptr', 'charptr', 'byteptr']
		}
		if base_arg.name in g.cur_fn_mut_params {
			return true
		}
	}
	if raw_type := g.get_raw_type(base_arg) {
		if raw_type is types.Pointer || raw_type is types.Nil {
			return true
		}
		// Handle type aliases that resolve to pointers (e.g., voidptr, charptr, byteptr)
		if raw_type is types.Alias {
			if raw_type.base_type is types.Pointer {
				return true
			}
		}
	}
	match base_arg {
		ast.PrefixExpr {
			return base_arg.op == .amp
		}
		ast.SelectorExpr {
			if base_arg.rhs.name == 'data' {
				lhs_type := g.get_expr_type(base_arg.lhs)
				if lhs_type == 'array' || lhs_type.starts_with('Array_') || lhs_type == 'map'
					|| lhs_type.starts_with('Map_') || lhs_type == 'string'
					|| lhs_type.starts_with('strings__Builder') {
					return true
				}
			}
			field_type := g.selector_field_type(base_arg)
			if field_type.ends_with('*')
				|| field_type in ['void*', 'char*', 'u8*', 'byteptr', 'charptr', 'voidptr'] {
				return true
			}
		}
		ast.Ident {
			// handled above
		}
		ast.UnsafeExpr {
			if base_arg.stmts.len > 0 {
				last := base_arg.stmts[base_arg.stmts.len - 1]
				if last is ast.ExprStmt {
					return g.expr_produces_pointer(last.expr)
				}
			}
		}
		else {}
	}
	return g.get_expr_type(base_arg).ends_with('*')
}

fn (mut g Gen) expr_produces_pointer(arg ast.Expr) bool {
	base_arg := if arg is ast.ModifierExpr { arg.expr } else { arg }
	if g.expr_is_pointer(base_arg) {
		return true
	}
	match base_arg {
		ast.CastExpr {
			return g.is_pointer_type(base_arg.typ)
		}
		ast.InfixExpr {
			if base_arg.op == .plus || base_arg.op == .minus {
				return g.expr_produces_pointer(base_arg.lhs)
					|| g.expr_produces_pointer(base_arg.rhs)
			}
		}
		ast.ParenExpr {
			return g.expr_produces_pointer(base_arg.expr)
		}
		ast.UnsafeExpr {
			if base_arg.stmts.len > 0 {
				last := base_arg.stmts[base_arg.stmts.len - 1]
				if last is ast.ExprStmt {
					return g.expr_produces_pointer(last.expr)
				}
			}
		}
		else {}
	}
	return false
}

fn (mut g Gen) can_take_address(arg ast.Expr) bool {
	base_arg := if arg is ast.ModifierExpr { arg.expr } else { arg }
	match base_arg {
		ast.Ident, ast.IndexExpr, ast.ParenExpr {
			return true
		}
		ast.SelectorExpr {
			// Enum values / module symbols / type-qualified selectors are rvalues.
			if base_arg.lhs is ast.EmptyExpr {
				return false
			}
			if base_arg.lhs is ast.Ident {
				if g.is_module_ident(base_arg.lhs.name) || g.is_type_name(base_arg.lhs.name) {
					return false
				}
			}
			if raw_type := g.get_raw_type(base_arg) {
				if raw_type is types.Enum {
					return false
				}
			}
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g Gen) gen_addr_of_expr(arg ast.Expr, typ string) {
	base_arg := if arg is ast.ModifierExpr { arg.expr } else { arg }
	if g.can_take_address(base_arg) {
		g.sb.write_string('&')
		g.expr(base_arg)
		return
	}
	// StringLiteral already generates a compound literal string with `.is_lit = 1`
	// so just take its address directly to avoid double-wrapping
	if base_arg is ast.StringLiteral {
		g.sb.write_string('&')
		g.expr(base_arg)
		return
	}
	// InitExpr: check if it generates a compound literal or a function call
	if base_arg is ast.InitExpr {
		init_type := g.expr_type_to_c(base_arg.typ)
		if init_type.starts_with('Map_') || init_type == 'map' {
			// Map init lowers to a function call; wrap it in an array compound literal
			// so the produced pointer stays valid for the whole surrounding block.
			g.sb.write_string('&((${init_type}[1]){')
			g.expr(base_arg)
			g.sb.write_string('}[0])')
			return
		}
		// Non-map InitExpr generates a compound literal - just take address
		g.sb.write_string('&')
		g.expr(base_arg)
		return
	}
	// ArrayInitExpr already generates a compound literal
	if base_arg is ast.ArrayInitExpr {
		g.sb.write_string('&')
		g.expr(base_arg)
		return
	}
	raw_addr := if typ == '' { 'int' } else { typ }
	addr_type := if raw_addr.ends_with('*')
		&& (raw_addr.starts_with('Array_') || raw_addr.starts_with('Map_')) {
		mangle_alias_component(raw_addr)
	} else {
		unmangle_c_ptr_type(raw_addr)
	}
	// For non-primitive aggregate values (array/map/string/struct/option/result),
	// use a 1-element array compound literal and take the first element's address.
	// This avoids statement-expression temporaries whose address can escape scope.
	is_nonprimitive_value := addr_type !in primitive_types && !addr_type.ends_with('*')
	if addr_type in ['array', 'map', 'string'] || addr_type.starts_with('Array_')
		|| addr_type.starts_with('Map_') || addr_type.starts_with('_option_')
		|| addr_type.starts_with('_result_') || is_nonprimitive_value {
		g.sb.write_string('&((${addr_type}[1]){')
		g.expr(base_arg)
		g.sb.write_string('}[0])')
		return
	}
	g.sb.write_string('&(${addr_type}){')
	g.expr(base_arg)
	g.sb.write_string('}')
}

fn (mut g Gen) fn_pointer_return_type(expr ast.Expr) string {
	if raw_type := g.get_raw_type(expr) {
		match raw_type {
			types.FnType {
				if rt := raw_type.get_return_type() {
					return g.types_type_to_c(rt)
				}
				return 'void'
			}
			types.Alias {
				if raw_type.base_type is types.FnType {
					if rt := raw_type.base_type.get_return_type() {
						return g.types_type_to_c(rt)
					}
					return 'void'
				}
			}
			types.Pointer {
				if raw_type.base_type is types.FnType {
					if rt := raw_type.base_type.get_return_type() {
						return g.types_type_to_c(rt)
					}
					return 'void'
				}
				if raw_type.base_type is types.Alias && raw_type.base_type.base_type is types.FnType {
					if rt := raw_type.base_type.base_type.get_return_type() {
						return g.types_type_to_c(rt)
					}
					return 'void'
				}
			}
			else {}
		}
	}
	return ''
}

fn (mut g Gen) is_fn_pointer_expr(expr ast.Expr) bool {
	return g.fn_pointer_return_type(expr) != ''
}

fn (mut g Gen) should_auto_deref(arg ast.Expr) bool {
	if raw_type := g.get_raw_type(arg) {
		if raw_type is types.Pointer {
			match raw_type.base_type {
				types.Array, types.Map, types.Struct, types.Interface, types.Alias, types.String {
					return true
				}
				else {}
			}
		}
	}
	t := g.get_expr_type(arg)
	return t.ends_with('*') && t !in ['void*', 'char*', 'byteptr', 'charptr']
}

fn (mut g Gen) gen_call_arg(fn_name string, idx int, arg ast.Expr) {
	base_arg := if arg is ast.ModifierExpr { arg.expr } else { arg }
	if fn_name == 'new_map_init_noscan_value' && idx in [7, 8] {
		// new_map_init_noscan_value expects raw key/value element pointers.
		// Lowered dynamic arrays arrive as `array` values; pass their `.data`.
		// Raw fixed array literals should be emitted directly and decay to pointers.
		if base_arg is ast.ArrayInitExpr {
			g.expr(base_arg)
		} else {
			g.expr(base_arg)
			g.sb.write_string('.data')
		}
		return
	}
	if ptr_params := g.fn_param_is_ptr[fn_name] {
		if idx < ptr_params.len {
			want_ptr := ptr_params[idx]
			if want_ptr && base_arg is ast.PrefixExpr && base_arg.op == .amp {
				inner := base_arg.expr
				if g.expr_is_pointer(inner) || g.expr_produces_pointer(inner) {
					g.expr(inner)
					return
				}
			}
			got_ptr := g.expr_is_pointer(base_arg)
			// Also check for pointer-producing expressions (casts, pointer arithmetic)
			// that expr_is_pointer misses but that produce pointer values in C.
			if want_ptr && !got_ptr && g.expr_produces_pointer(base_arg) {
				g.expr(base_arg)
				return
			}
			if want_ptr && !got_ptr && g.can_take_address(base_arg) {
				// Don't take address of function pointer variables
				if base_arg is ast.Ident {
					if local_type := g.get_local_var_c_type(base_arg.name) {
						if local_type == 'fn_ptr' || local_type.ends_with('*') {
							g.expr(base_arg)
							return
						}
					}
				}
				g.sb.write_string('&')
				g.expr(base_arg)
				return
			}
			if want_ptr && !got_ptr && !g.can_take_address(base_arg) {
				// Can't take address of expression (e.g., function call return value).
				// Use compound literal to make it addressable with proper type.
				if raw := g.get_raw_type(base_arg) {
					if raw is types.Pointer {
						// Already a pointer at the type level, just emit
						g.expr(base_arg)
						return
					}
					c_type := g.types_type_to_c(raw)
					if c_type != '' {
						g.gen_addr_of_expr(base_arg, c_type)
						return
					}
				}
				// Fallback: use expression type or default to array
				wrap_type := g.get_expr_type(base_arg)
				if wrap_type != '' {
					g.gen_addr_of_expr(base_arg, wrap_type)
					return
				}
				if base_arg is ast.StringLiteral || base_arg is ast.StringInterLiteral {
					g.gen_addr_of_expr(base_arg, 'string')
					return
				}
				if base_arg is ast.BasicLiteral {
					lit_type := if base_arg.kind == .string {
						'string'
					} else {
						'int'
					}
					g.gen_addr_of_expr(base_arg, lit_type)
					return
				}
				g.gen_addr_of_expr(base_arg, 'int')
				return
			}
			if !want_ptr && got_ptr && g.should_auto_deref(base_arg) {
				// Don't auto-deref when the param type is a pointer alias (e.g. Coordptr = Coord*)
				if param_types := g.fn_param_types[fn_name] {
					if idx < param_types.len && param_types[idx].ends_with('ptr') {
						g.expr(base_arg)
						return
					}
				}
				g.sb.write_string('(*')
				g.expr(base_arg)
				g.sb.write_string(')')
				return
			}
		}
	}
	// Check if argument needs sum type wrapping (variant passed where sum type expected)
	if param_types := g.fn_param_types[fn_name] {
		if idx < param_types.len {
			param_type := param_types[idx]
			if variants := g.sum_type_variants[param_type] {
				// Selector/index expressions can be smartcast-narrowed in the checker env,
				// while still evaluating to the original sum value representation.
				// If the raw (declared) type already matches the sum param type, pass it through.
				if raw := g.get_raw_type(base_arg) {
					raw_c := g.types_type_to_c(raw)
					if raw_c == param_type {
						g.expr(base_arg)
						return
					}
				}
				mut arg_type := g.get_expr_type(base_arg)
				// For smartcast dereference patterns (*(Type*)(expr._data._Type)),
				// extract the actual type from the cast
				if arg_type == 'int' || arg_type == '' {
					// Unwrap ParenExpr if present
					inner_arg := if base_arg is ast.ParenExpr {
						base_arg.expr
					} else {
						base_arg
					}
					if inner_arg is ast.PrefixExpr && inner_arg.op == .mul {
						if inner_arg.expr is ast.CastExpr {
							cast_type := g.expr_type_to_c(inner_arg.expr.typ)
							if cast_type.ends_with('*') {
								arg_type = cast_type[..cast_type.len - 1]
							}
						}
					}
				}
				if arg_type != param_type && arg_type != '' {
					mut tag := -1
					mut field_name := ''
					for i, v in variants {
						if v == arg_type || arg_type.ends_with('__${v}')
							|| v.ends_with('__${arg_type}') {
							tag = i
							field_name = v
							break
						}
					}
					if tag >= 0 {
						is_primitive :=
							arg_type in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'f32', 'f64', 'bool', 'rune', 'byte', 'usize', 'isize']
							|| arg_type in g.primitive_type_aliases
						g.gen_sum_type_wrap(param_type, field_name, tag, is_primitive,
							base_arg, arg_type)
						return
					}
				}
			}
		}
	}
	g.expr(base_arg)
}

fn (mut g Gen) resolve_container_method_name(receiver ast.Expr, method_name string, expected_params int) string {
	mut candidates := []string{}
	if raw_type := g.get_raw_type(receiver) {
		match raw_type {
			types.Pointer {
				match raw_type.base_type {
					types.Array, types.ArrayFixed {
						candidates << 'array'
					}
					types.Map {
						candidates << 'map'
					}
					types.String {
						candidates << 'string'
					}
					types.Alias {
						match raw_type.base_type.base_type {
							types.Array, types.ArrayFixed {
								candidates << 'array'
							}
							types.Map {
								candidates << 'map'
							}
							types.String {
								candidates << 'string'
							}
							else {}
						}
					}
					else {}
				}
			}
			types.Array, types.ArrayFixed {
				candidates << 'array'
			}
			types.Map {
				candidates << 'map'
			}
			types.String {
				candidates << 'string'
			}
			types.Alias {
				match raw_type.base_type {
					types.Array, types.ArrayFixed {
						candidates << 'array'
					}
					types.Map {
						candidates << 'map'
					}
					types.String {
						candidates << 'string'
					}
					else {}
				}
			}
			else {}
		}
	}
	recv_name := g.method_receiver_base_type(receiver)
	if recv_name == 'array' || recv_name.starts_with('Array_') {
		candidates << 'array'
	}
	if recv_name == 'map' || recv_name.starts_with('Map_') {
		candidates << 'map'
	}
	if recv_name == 'string' {
		candidates << 'string'
	}
	for base in candidates {
		candidate := '${base}__${method_name}'
		if params := g.fn_param_is_ptr[candidate] {
			if params.len == expected_params {
				return candidate
			}
		} else if candidate in g.fn_return_types {
			return candidate
		}
	}
	return ''
}

fn (mut g Gen) resolve_call_name(lhs ast.Expr, arg_count int) string {
	mut name := ''
	if lhs is ast.Ident {
		name = sanitize_fn_ident(lhs.name)
	} else if lhs is ast.SelectorExpr {
		if lhs.lhs is ast.Ident && lhs.lhs.name == 'C' {
			return lhs.rhs.name
		}
		// Static type method call: Type.method(...)
		if lhs.lhs is ast.Ident && g.is_type_name(lhs.lhs.name) {
			return '${g.get_qualified_name(lhs.lhs.name)}__${sanitize_fn_ident(lhs.rhs.name)}'
		}
		if lhs.lhs is ast.Ident && g.is_module_ident(lhs.lhs.name) {
			mod_name := g.resolve_module_name(lhs.lhs.name)
			name = '${mod_name}__${sanitize_fn_ident(lhs.rhs.name)}'
		} else {
			method_name := sanitize_fn_ident(lhs.rhs.name)
			base_type := g.method_receiver_base_type(lhs.lhs)
			name = '${base_type}__${method_name}'
		}
	}
	if name == 'builtin__new_array_from_c_array_noscan' {
		name = 'new_array_from_c_array'
	}
	if name == 'builtin__array_push_noscan' {
		name = 'array__push'
	}
	if name == 'voidptr__vbytes' {
		name = 'void__vbytes'
	}
	if name.ends_with('__bytes') && name !in g.fn_return_types && name !in g.fn_param_is_ptr {
		if 'string__bytes' in g.fn_return_types || 'string__bytes' in g.fn_param_is_ptr {
			name = 'string__bytes'
		}
	}
	if is_c_runtime_function(name) {
		return name
	}
	if name != '' && g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin'
		&& !name.contains('__') {
		qualified := '${g.cur_module}__${name}'
		if qualified in g.fn_return_types || qualified in g.fn_param_is_ptr {
			return qualified
		}
		if name in g.fn_return_types || name in g.fn_param_is_ptr {
			return name
		}
		return qualified
	}
	return name
}

fn (mut g Gen) get_call_return_type(lhs ast.Expr, arg_count int) ?string {
	if lhs is ast.SelectorExpr {
		if lhs.lhs is ast.Ident && lhs.lhs.name == 'C' {
			if ret := g.fn_return_types[lhs.rhs.name] {
				return ret
			}
			// Fallbacks for common C symbols used by vlib/os where declaration
			// metadata can be missing in transformed environments.
			match lhs.rhs.name {
				'open', 'chdir', 'proc_pidpath' { return 'int' }
				'signal' { return 'void*' }
				else {}
			}
		}
	}
	if lhs is ast.SelectorExpr {
		if lhs.rhs.name in ['hash_fn', 'key_eq_fn', 'clone_fn', 'free_fn'] {
			base_type := g.method_receiver_base_type(lhs.lhs)
			if base_type == 'map' || base_type.starts_with('Map_') {
				return match lhs.rhs.name {
					'hash_fn' { 'u64' }
					'key_eq_fn' { 'bool' }
					else { 'void' }
				}
			}
		}
	}
	// Prefer explicit fn_return_types registration (includes result/option wrappers)
	// over fn_pointer_return_type (which may return the unwrapped type from the checker).
	c_name := g.resolve_call_name(lhs, arg_count)
	if c_name != '' {
		if ret := g.fn_return_types[c_name] {
			return ret
		}
	}
	if lhs !is ast.Ident {
		mut allow_fn_ptr_lookup := true
		if lhs is ast.SelectorExpr {
			if lhs.lhs is ast.Ident
				&& (g.is_module_ident(lhs.lhs.name) || g.is_type_name(lhs.lhs.name)) {
				// Module/type selectors represent direct function/method calls, not
				// function-pointer values.
				allow_fn_ptr_lookup = false
			}
		}
		if allow_fn_ptr_lookup {
			fn_ptr_ret := g.fn_pointer_return_type(lhs)
			if fn_ptr_ret != '' {
				return fn_ptr_ret
			}
		}
	}
	if c_name == '' {
		if lhs is ast.Ident {
			match lhs.name {
				'open', 'chdir', 'proc_pidpath' { return 'int' }
				'signal' { return 'void*' }
				else {}
			}
		}
		return none
	}
	match c_name {
		'open', 'chdir', 'proc_pidpath' { return 'int' }
		'signal' { return 'void*' }
		else {}
	}
	return none
}

fn (mut g Gen) gen_array_contains_call(name string, call_args []ast.Expr) bool {
	if !name.starts_with('array__contains_') || call_args.len != 2 {
		return false
	}
	elem_type := g.infer_array_contains_elem_type(name, call_args)
	g.sb.write_string('array__contains(')
	g.expr(call_args[0])
	g.sb.write_string(', ')
	g.gen_addr_of_expr(call_args[1], elem_type)
	g.sb.write_string(')')
	return true
}

fn (mut g Gen) call_expr(lhs ast.Expr, args []ast.Expr) {
	if lhs is ast.SelectorExpr {
		if lhs.lhs is ast.Ident {
			lhs_ident := lhs.lhs as ast.Ident
			if lhs_ident.name == 'C' && lhs.rhs.name in ['FD_ZERO', 'FD_SET', 'FD_ISSET'] {
				g.sb.write_string('${lhs.rhs.name}(')
				for i, arg in args {
					if i > 0 {
						g.sb.write_string(', ')
					}
					g.expr(arg)
				}
				g.sb.write_string(')')
				return
			}
		}
	}
	if lhs is ast.SelectorExpr && g.is_fn_pointer_expr(lhs) {
		mut should_emit_fnptr_call := true
		resolved := g.resolve_call_name(lhs, args.len)
		if resolved != '' && (resolved in g.fn_param_is_ptr || resolved in g.fn_return_types) {
			should_emit_fnptr_call = false
		}
		// When there is a naming collision (e.g. `seed` is both a function and a
		// sub-module in the `rand` module), `is_module_ident` returns false and
		// `resolve_call_name` resolves to the function type.  Try treating lhs.lhs
		// as a module name by checking if `lhs_name__rhs_name` exists as a known
		// function.
		if should_emit_fnptr_call && lhs.lhs is ast.Ident {
			alt := '${lhs.lhs.name}__${sanitize_fn_ident(lhs.rhs.name)}'
			if alt in g.fn_param_is_ptr || alt in g.fn_return_types {
				g.sb.write_string('${alt}(')
				for i, arg in args {
					if i > 0 {
						g.sb.write_string(', ')
					}
					g.expr(arg)
				}
				g.sb.write_string(')')
				return
			}
		}
		if should_emit_fnptr_call {
			// Function pointer fields are stored as void* in C.
			// We need to cast to the correct function pointer type before calling.
			ret_type := g.fn_pointer_return_type(lhs)
			c_ret := if ret_type == '' { 'void' } else { ret_type }
			// For pointer-to-interface types, the transformer doesn't add _object
			// (it only handles non-pointer interface vars like IError err).
			// Check if this is a vtable method call on an interface pointer
			// and if so, pass _object as the first argument.
			receiver_type := g.get_expr_type(lhs.lhs)
			base_receiver := receiver_type.trim_right('*')
			if receiver_type.ends_with('*') && g.is_interface_type(base_receiver) {
				method_name := lhs.rhs.name
				if g.is_interface_vtable_method(base_receiver, method_name) {
					mut cast_sig := '${c_ret} (*)(void*)'
					if methods := g.interface_methods[base_receiver] {
						for method in methods {
							if method.name == method_name {
								cast_sig = method.cast_signature
								break
							}
						}
					}
					g.sb.write_string('((${cast_sig})')
					g.expr(lhs)
					g.sb.write_string(')(')
					g.expr(lhs.lhs)
					g.sb.write_string('->_object')
					for _, arg in args {
						g.sb.write_string(', ')
						g.expr(arg)
					}
					g.sb.write_string(')')
					return
				}
			}
			g.sb.write_string('((${c_ret}(*)())')
			g.expr(lhs)
			g.sb.write_string(')(')
			for i, arg in args {
				if i > 0 {
					g.sb.write_string(', ')
				}
				g.expr(arg)
			}
			g.sb.write_string(')')
			return
		}
	}
	mut name := ''
	mut call_args := []ast.Expr{}
	call_args << args
	if lhs is ast.Ident {
		name = sanitize_fn_ident(lhs.name)
	} else if lhs is ast.SelectorExpr {
		if lhs.rhs.name in ['hash_fn', 'key_eq_fn', 'clone_fn', 'free_fn'] {
			base_type := g.method_receiver_base_type(lhs.lhs)
			if base_type == 'map' || base_type.starts_with('Map_') {
				g.expr(lhs)
				g.sb.write_string('(')
				for i, arg in args {
					if i > 0 {
						g.sb.write_string(', ')
					}
					g.expr(arg)
				}
				g.sb.write_string(')')
				return
			}
		}
		// Handle C.puts, C.putchar etc.
		if lhs.lhs is ast.Ident && lhs.lhs.name == 'C' {
			name = lhs.rhs.name
			g.sb.write_string('${name}(')
			for i, arg in args {
				if i > 0 {
					g.sb.write_string(', ')
				}
				g.expr(arg)
			}
			g.sb.write_string(')')
			return
		}
		// module.fn(...) => module__fn(...)
		if lhs.lhs is ast.Ident && g.is_type_name(lhs.lhs.name) {
			// Static type method call: Type.method(...)
			name = '${g.get_qualified_name(lhs.lhs.name)}__${sanitize_fn_ident(lhs.rhs.name)}'
		} else if lhs.lhs is ast.Ident && g.is_module_ident(lhs.lhs.name) {
			mod_name := g.resolve_module_name(lhs.lhs.name)
			name = '${mod_name}__${sanitize_fn_ident(lhs.rhs.name)}'
		} else {
			// value.method(args...) => ReceiverType__method(value, args...)
			name = g.resolve_call_name(lhs, args.len)
			if name == '' {
				method_name := sanitize_fn_ident(lhs.rhs.name)
				mut base_type := g.method_receiver_base_type(lhs.lhs)
				if base_type == '' {
					// Fall back to checker/inferred receiver type when selector base
					// typing is incomplete (common with interface values).
					base_type = g.get_expr_type(lhs.lhs).trim_space().trim_right('*')
				}
				name = '${base_type}__${method_name}'
			}
			call_args = []ast.Expr{cap: args.len + 1}
			call_args << lhs.lhs
			call_args << args
		}
	}

	// Transformer helper maps directly to builtin implementation name in vlib.
	if name == 'builtin__new_array_from_c_array_noscan' {
		name = 'new_array_from_c_array'
	}
	if name == 'builtin__array_push_noscan' {
		name = 'array__push'
	}
	if name == 'array__bytestr'
		&& ('Array_u8__bytestr' in g.fn_param_is_ptr || 'Array_u8__bytestr' in g.fn_return_types) {
		name = 'Array_u8__bytestr'
	}
	if name == 'os__exit' {
		name = 'exit'
	}
	if name.starts_with('strings__Builder__') && name !in g.fn_param_is_ptr
		&& name !in g.fn_return_types {
		method_name := name.all_after_last('__')
		array_name := 'array__${method_name}'
		if array_name in g.fn_param_is_ptr || array_name in g.fn_return_types {
			name = array_name
		}
	}
	if name.starts_with('array__') && call_args.len > 0 {
		method_name := name['array__'.len..]
		elem_type := g.infer_array_elem_type_from_expr(call_args[0]).trim_right('*')
		if elem_type != '' {
			specialized := 'Array_' + mangle_alias_component(elem_type) + '__' + method_name
			if method_name == 'clone' {
				// eprintln('[cleanc spec] name=${name} elem_type=${elem_type} specialized=${specialized} in_params=${specialized in g.fn_param_is_ptr} in_rets=${specialized in g.fn_return_types}')
			}
			if specialized in g.fn_param_is_ptr || specialized in g.fn_return_types {
				name = specialized
			}
		}
	}
	for arg in call_args {
		if arg is ast.FieldInit {
			panic('bug in v2 compiler: FieldInit call args should have been lowered in v2.transformer')
		}
	}
	if name != '' {
		g.called_fn_names[name] = true
	}
	if name.contains('__new') && call_args.len > 0 {
		expected_ctor_type := name.all_before_last('__new')
		mut arg_type_name := ''
		first_arg := call_args[0]
		match first_arg {
			ast.Ident {
				arg_type_name = first_arg.name
			}
			ast.SelectorExpr {
				if first_arg.lhs is ast.Ident {
					lhs_ident := first_arg.lhs as ast.Ident
					arg_type_name = '${lhs_ident.name}__${first_arg.rhs.name}'
				} else {
					arg_type_name = first_arg.rhs.name
				}
			}
			ast.Type {
				arg_type_name = g.expr_type_to_c(first_arg)
			}
			else {}
		}
		if arg_type_name == '' {
			arg_type_name = g.get_expr_type(first_arg)
		}
		arg_type_name = arg_type_name.trim_space().trim_left('&').trim_left('*')
		if arg_type_name != '' && (arg_type_name == expected_ctor_type
			|| arg_type_name == expected_ctor_type.all_after_last('__')) {
			call_args.delete(0)
		}
	}
	if call_args.len == 1 && g.is_type_name(name) && !name.ends_with('__new') {
		g.gen_type_cast_expr(name, call_args[0])
		return
	}
	if lhs is ast.Ident && g.is_type_name(lhs.name) && call_args.len == 1 {
		type_name := g.expr_type_to_c(lhs)
		g.gen_type_cast_expr(type_name, call_args[0])
		return
	}
	if lhs is ast.SelectorExpr && call_args.len == 1 && lhs.lhs is ast.Ident {
		qualified_type := '${lhs.lhs.name}__${lhs.rhs.name}'
		if g.is_type_name(qualified_type) {
			type_name := g.expr_type_to_c(lhs)
			g.gen_type_cast_expr(type_name, call_args[0])
			return
		}
	}
	if name == 'panic' && call_args.len == 1 {
		arg := call_args[0]
		arg_type := g.get_expr_type(arg)
		if arg_type == 'IError' || (arg is ast.Ident && arg.name == 'err') {
			g.sb.write_string('panic(IError__str(')
			g.expr(arg)
			g.sb.write_string('))')
			return
		}
	}
	mut local_call_type := ''
	if lhs is ast.Ident {
		local_call_type = g.get_local_var_c_type(lhs.name) or { '' }
	}
	if local_call_type in ['void*', 'voidptr'] {
		if name in ['FD_ZERO', 'FD_SET', 'FD_ISSET'] {
			g.sb.write_string('${name}(')
			for i, arg in call_args {
				if i > 0 {
					g.sb.write_string(', ')
				}
				g.expr(arg)
			}
			g.sb.write_string(')')
			return
		}
		mut arg_types := []string{cap: call_args.len}
		for arg in call_args {
			mut at := g.get_expr_type(arg)
			if at == '' || at == 'int_literal' || at == 'float_literal' {
				at = 'int'
			}
			if at == 'voidptr' {
				at = 'void*'
			}
			arg_types << at
		}
		ret_type := g.fn_pointer_return_type(lhs)
		c_ret := if ret_type == '' { 'void' } else { ret_type }
		g.sb.write_string('((${c_ret} (*)(')
		for i, at in arg_types {
			if i > 0 {
				g.sb.write_string(', ')
			}
			g.sb.write_string(at)
		}
		g.sb.write_string('))')
		g.expr(lhs)
		g.sb.write_string(')(')
		for i, arg in call_args {
			if i > 0 {
				g.sb.write_string(', ')
			}
			g.expr(arg)
		}
		g.sb.write_string(')')
		return
	}
	if g.gen_array_contains_call(name, call_args) {
		return
	}
	// Handle type-specific array equality: Array_T__eq(arr1, arr2)
	// Generated by transformer for arrays of structs/maps where memcmp won't work
	if name.starts_with('Array_') && name.ends_with('__eq') && !name.starts_with('Array_fixed_')
		&& call_args.len == 2 {
		elem_type_name := name['Array_'.len..name.len - '__eq'.len]
		g.gen_typed_array_eq(elem_type_name, call_args)
		return
	}
	if name == 'array__eq' && call_args.len == 2 {
		// Check if either argument is a fixed array type
		arg0_type := g.get_expr_type(call_args[0])
		arg1_type := g.get_expr_type(call_args[1])
		if arg0_type.starts_with('Array_fixed_') || arg1_type.starts_with('Array_fixed_') {
			fixed_type := if arg0_type.starts_with('Array_fixed_') {
				arg0_type
			} else {
				arg1_type
			}
			// Parse element type from Array_fixed_T_N
			elem_type, arr_len := parse_fixed_array_elem_type(fixed_type)
			if arr_len > 0 && fixed_array_elem_needs_deep_eq(elem_type) {
				// Element-wise comparison for strings, arrays, maps, structs
				g.gen_fixed_array_deep_eq(fixed_type, elem_type, arr_len, call_args)
				return
			}
			g.sb.write_string('(memcmp(')
			g.gen_fixed_array_cmp_operand(call_args[0], fixed_type)
			g.sb.write_string(', ')
			g.gen_fixed_array_cmp_operand(call_args[1], fixed_type)
			g.sb.write_string(', sizeof(${fixed_type})) == 0)')
			return
		}
		g.sb.write_string('__v2_array_eq(')
		if g.expr_is_pointer(call_args[0]) {
			g.sb.write_string('*')
		}
		g.expr(call_args[0])
		g.sb.write_string(', ')
		if g.expr_is_pointer(call_args[1]) {
			g.sb.write_string('*')
		}
		g.expr(call_args[1])
		g.sb.write_string(')')
		return
	}
	if name.starts_with('Array_') && name.ends_with('__sort') && call_args.len == 1 {
		g.sb.write_string('array__sort((array*)')
		if g.expr_is_pointer(call_args[0]) {
			g.expr(call_args[0])
		} else {
			g.sb.write_string('&')
			g.expr(call_args[0])
		}
		g.sb.write_string(', NULL)')
		return
	}
	if name == 'os__join_path' && call_args.len == 2 && g.get_expr_type(call_args[1]) == 'string' {
		g.sb.write_string('os__join_path_single(')
		g.gen_call_arg('os__join_path_single', 0, call_args[0])
		g.sb.write_string(', ')
		g.gen_call_arg('os__join_path_single', 1, call_args[1])
		g.sb.write_string(')')
		return
	}
	// array__repeat → array__repeat_to_depth with automatic depth for deep clone
	if name == 'array__repeat' && call_args.len == 2 {
		g.sb.write_string('array__repeat_to_depth(')
		g.gen_call_arg(name, 0, call_args[0])
		g.sb.write_string(', ')
		g.gen_call_arg(name, 1, call_args[1])
		g.sb.write_string(', 3)')
		return
	}
	// array__clone → array__clone_to_depth with depth 0 (shallow memcpy clone).
	// Depth > 0 uses element_size heuristics that misidentify non-string types
	// of the same size (e.g. tagged unions like ast.Expr are 16 bytes == sizeof(string)).
	if name == 'array__clone' && call_args.len == 1 {
		g.sb.write_string('array__clone_to_depth(')
		g.gen_call_arg(name, 0, call_args[0])
		g.sb.write_string(', 0)')
		return
	}
	// array__insert with array arg → array__insert_many
	// Only when arg's elem type matches receiver's elem type (not for nested arrays like [][]T.insert([]T))
	if name == 'array__insert' && call_args.len == 3 && g.expr_is_array_value(call_args[2])
		&& g.infer_array_elem_type_from_expr(call_args[2]) == g.infer_array_elem_type_from_expr(call_args[0]) {
		rhs_tmp := '_ins_many_tmp_${g.tmp_counter}'
		g.tmp_counter++
		arr_rhs_type := g.expr_array_runtime_type(call_args[2])
		g.sb.write_string('({ ${arr_rhs_type} ${rhs_tmp} = ')
		g.expr(call_args[2])
		g.sb.write_string('; array__insert_many(')
		g.gen_call_arg(name, 0, call_args[0])
		g.sb.write_string(', ')
		g.gen_call_arg(name, 1, call_args[1])
		g.sb.write_string(', ${rhs_tmp}.data, ${rhs_tmp}.len); })')
		return
	}
	// array__prepend with array arg → array__prepend_many
	// Only when arg's elem type matches receiver's elem type (not for nested arrays like [][]T.prepend([]T))
	if name == 'array__prepend' && call_args.len == 2 && g.expr_is_array_value(call_args[1])
		&& g.infer_array_elem_type_from_expr(call_args[1]) == g.infer_array_elem_type_from_expr(call_args[0]) {
		rhs_tmp := '_prep_many_tmp_${g.tmp_counter}'
		g.tmp_counter++
		arr_rhs_type := g.expr_array_runtime_type(call_args[1])
		g.sb.write_string('({ ${arr_rhs_type} ${rhs_tmp} = ')
		g.expr(call_args[1])
		g.sb.write_string('; array__prepend_many(')
		g.gen_call_arg(name, 0, call_args[0])
		g.sb.write_string(', ${rhs_tmp}.data, ${rhs_tmp}.len); })')
		return
	}
	if name in ['array__pop', 'array__pop_left'] && call_args.len == 1 {
		elem_type := g.infer_array_elem_type_from_expr(call_args[0])
		if elem_type != '' {
			g.sb.write_string('(*(${elem_type}*)${name}(')
			g.gen_call_arg(name, 0, call_args[0])
			g.sb.write_string('))')
			return
		}
	}
	if name in ['array__first', 'array__last'] && call_args.len == 1 {
		elem_type := g.infer_array_elem_type_from_expr(call_args[0])
		if elem_type != '' {
			g.sb.write_string('(*(${elem_type}*)${name}(')
			g.gen_call_arg(name, 0, call_args[0])
			g.sb.write_string('))')
			return
		}
	}
	if name == 'voidptr__vbytes' {
		name = 'void__vbytes'
	}
	if name == 'signal' && call_args.len == 2 {
		g.sb.write_string('signal(')
		g.expr(call_args[0])
		g.sb.write_string(', ((void (*)(int))')
		g.expr(call_args[1])
		g.sb.write_string('))')
		return
	}

	// Handle builtin print functions with type-aware argument conversion
	if name in ['println', 'eprintln', 'print', 'eprint'] {
		if call_args.len == 1 {
			arg := call_args[0]
			mut arg_type := g.get_expr_type(arg)

			mut c_name := name
			builtin_name := 'builtin__${name}'
			if builtin_name in g.fn_param_is_ptr || builtin_name in g.fn_return_types {
				c_name = builtin_name
			} else if name in g.fn_param_is_ptr || name in g.fn_return_types {
				c_name = name
			}

			// String literals and interpolation strings are always strings,
			// regardless of what position-based type lookup returns.
			if arg is ast.StringLiteral || arg is ast.StringInterLiteral
				|| (arg is ast.BasicLiteral && arg.kind == .string) {
				arg_type = 'string'
			}

			// If argument is already a str-function call, treat as string to avoid double wrapping
			if arg_type != 'string' {
				if arg is ast.CallExpr && arg.lhs is ast.Ident {
					called_fn := (arg.lhs as ast.Ident).name
					if called_fn.ends_with('_str') || called_fn.ends_with('__str') {
						arg_type = 'string'
					}
				}
			}

			if arg_type == 'string' {
				g.sb.write_string('${c_name}(')
				g.expr(arg)
				g.sb.write_string(')')
			} else if arg is ast.Ident && arg.name == 'err' {
				g.sb.write_string('${c_name}(IError__str(')
				g.expr(arg)
				g.sb.write_string('))')
			} else if arg_type == 'IError' {
				g.sb.write_string('${c_name}(IError__str(')
				g.expr(arg)
				g.sb.write_string('))')
			} else if arg_type in ['int', 'i8', 'i16', 'i32'] {
				g.sb.write_string('${c_name}(int__str(')
				g.expr(arg)
				g.sb.write_string('))')
			} else if arg_type == 'i64' {
				g.sb.write_string('${c_name}(i64__str(')
				g.expr(arg)
				g.sb.write_string('))')
			} else if arg_type == 'u64' {
				g.sb.write_string('${c_name}(u64__str(')
				g.expr(arg)
				g.sb.write_string('))')
			} else if arg_type == 'bool' {
				g.sb.write_string('${c_name}(bool__str(')
				g.expr(arg)
				g.sb.write_string('))')
			} else {
				// Fallback
				g.sb.write_string('${c_name}(/* ${arg_type} */ int__str(')
				g.expr(arg)
				g.sb.write_string('))')
			}
			return
		}
	}

	// Regular function call - mangle name based on module
	mut c_name := name
	is_local_call := local_call_type != ''
	if is_c_runtime_function(name) {
		c_name = name
	} else if name != '' && g.cur_module != '' && g.cur_module != 'main'
		&& g.cur_module != 'builtin' && !name.contains('__') && !is_local_call {
		qualified := '${g.cur_module}__${name}'
		if qualified in g.fn_param_is_ptr || qualified in g.fn_return_types {
			c_name = qualified
		} else if name in g.fn_param_is_ptr || name in g.fn_return_types {
			c_name = name
		} else {
			c_name = qualified
		}
	}
	if c_name in ['os__exit', 'builder__exit'] {
		c_name = 'exit'
	}
	g.sb.write_string('${c_name}(')
	mut total_args := call_args.len
	if param_types := g.fn_param_types[c_name] {
		if param_types.len > total_args {
			total_args = param_types.len
		}
	} else if params := g.fn_param_is_ptr[c_name] {
		if params.len > total_args {
			total_args = params.len
		}
	}
	for i in 0 .. total_args {
		if i > 0 {
			g.sb.write_string(', ')
		}
		if i < call_args.len {
			if c_name == 'signal' && i == 1 {
				g.sb.write_string('((void (*)(int))')
				g.expr(call_args[i])
				g.sb.write_string(')')
				continue
			}
			g.gen_call_arg(c_name, i, call_args[i])
		} else {
			// Default arguments should be lowered by transformer; keep C generation moving.
			if param_types := g.fn_param_types[c_name] {
				if i < param_types.len {
					g.sb.write_string(zero_value_for_type(param_types[i]))
				} else {
					g.sb.write_string('0')
				}
			} else {
				g.sb.write_string('0')
			}
		}
	}
	g.sb.write_string(')')
}

fn (mut g Gen) ensure_cur_fn_scope() ?&types.Scope {
	if g.cur_fn_scope != unsafe { nil } {
		return g.cur_fn_scope
	}
	if g.env == unsafe { nil } || g.cur_fn_name == '' {
		return none
	}
	if fn_scope := g.env.get_fn_scope(g.cur_module, g.cur_fn_name) {
		g.cur_fn_scope = fn_scope
		return fn_scope
	}
	suffix := '__${g.cur_fn_name}'
	mut matched_key := ''
	fn_scope_keys := lock g.env.fn_scopes {
		g.env.fn_scopes.keys()
	}
	for key in fn_scope_keys {
		if key.ends_with(suffix) {
			matched_key = key
			break
		}
	}
	if matched_key != '' {
		if fn_scope := g.env_fn_scope_by_key(matched_key) {
			g.cur_fn_scope = fn_scope
			return fn_scope
		}
	}
	return none
}

fn (g &Gen) env_fn_scope_by_key(key string) ?&types.Scope {
	if g.env == unsafe { nil } {
		return none
	}
	return g.env.get_fn_scope_by_key(key)
}

fn (mut g Gen) gen_fn_literal(node ast.FnLiteral) {
	anon_name := '_anon_fn_${g.tmp_counter}'
	g.tmp_counter++

	// Determine return type
	ret_type := if node.typ.return_type !is ast.EmptyExpr {
		g.expr_type_to_c(node.typ.return_type)
	} else {
		'void'
	}

	// Save current state
	saved_sb := g.sb
	saved_indent := g.indent
	saved_fn_name := g.cur_fn_name
	saved_fn_ret := g.cur_fn_ret_type
	saved_fn_scope := g.cur_fn_scope
	saved_fn_mut_params := g.cur_fn_mut_params.clone()
	saved_local_types := g.runtime_local_types.clone()

	// Use new builder for anon fn
	g.sb = strings.new_builder(1024)
	g.indent = 0
	g.cur_fn_name = anon_name
	g.cur_fn_ret_type = ret_type
	g.cur_fn_mut_params = map[string]bool{}
	g.runtime_local_types = map[string]string{}

	// Generate function definition
	g.sb.write_string('static ${ret_type} ${anon_name}(')
	for i, param in node.typ.params {
		if i > 0 {
			g.sb.write_string(', ')
		}
		param_type := g.expr_type_to_c(param.typ)
		g.sb.write_string('${param_type} ${param.name}')
		if param.is_mut {
			g.cur_fn_mut_params[param.name] = true
		}
		// Register param type so expr_is_pointer and auto-deref work correctly
		ptype := if param.is_mut && !param_type.ends_with('*') {
			param_type + '*'
		} else {
			param_type
		}
		g.runtime_local_types[param.name] = ptype
	}
	if node.typ.params.len == 0 {
		g.sb.write_string('void')
	}
	g.sb.writeln(') {')
	g.indent = 1
	g.gen_stmts(node.stmts)
	g.sb.writeln('}')
	g.sb.writeln('')

	// Capture anon fn content
	anon_def := g.sb.str()
	g.anon_fn_defs << anon_def

	// Restore state
	g.sb = saved_sb
	g.indent = saved_indent
	g.cur_fn_name = saved_fn_name
	g.cur_fn_ret_type = saved_fn_ret
	g.cur_fn_scope = saved_fn_scope
	g.cur_fn_mut_params = saved_fn_mut_params.clone()
	g.runtime_local_types = saved_local_types.clone()

	// Emit function name at use site
	g.sb.write_string(anon_name)
}

fn (g &Gen) get_str_fn_for_type(expr_type string) ?string {
	if expr_type == ''
		|| expr_type in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'f32', 'f64', 'bool', 'string', 'char', 'rune', 'voidptr', 'byteptr', 'charptr'] {
		return none
	}
	// Check for int__str, Array_int_str, etc.
	str_fn := '${expr_type}_str'
	if str_fn in g.fn_return_types || str_fn in g.fn_param_is_ptr {
		return str_fn
	}
	// Also check the __str variant
	str_fn2 := '${expr_type}__str'
	if str_fn2 in g.fn_return_types || str_fn2 in g.fn_param_is_ptr {
		return str_fn2
	}
	// For array types, try Array_ELEM_str / Array_fixed_ELEM_N_str
	if expr_type == 'array' || expr_type.starts_with('Array_') {
		if expr_type == 'array' {
			return 'Array_int_str'
		}
		return '${expr_type}_str'
	}
	// For Map types, try Map_K_V_str
	if expr_type.starts_with('Map_') {
		return '${expr_type}_str'
	}
	return none
}

// emit_generic_fn_macro emits a C macro definition for known simple generic functions
// (abs, min, max, clamp). For other generic functions, emits a stub.
// Also emits a typedef for the generic type parameter T.
fn (mut g Gen) emit_generic_fn_macro(fn_name string, node ast.FnDecl) {
	macro_key := 'generic_macro_${fn_name}'
	if macro_key in g.emitted_types {
		return
	}
	g.emitted_types[macro_key] = true
	if node.typ.params.len == 1 && node.name == 'abs' {
		pname := node.typ.params[0].name
		g.sb.writeln('#define ${fn_name}(${pname}) ((${pname}) < 0 ? -(${pname}) : (${pname}))')
	} else if node.typ.params.len == 2 && node.name == 'min' {
		p0 := node.typ.params[0].name
		p1 := node.typ.params[1].name
		g.sb.writeln('#define ${fn_name}(${p0}, ${p1}) ((${p0}) < (${p1}) ? (${p0}) : (${p1}))')
	} else if node.typ.params.len == 2 && node.name == 'max' {
		p0 := node.typ.params[0].name
		p1 := node.typ.params[1].name
		g.sb.writeln('#define ${fn_name}(${p0}, ${p1}) ((${p0}) > (${p1}) ? (${p0}) : (${p1}))')
	} else if node.typ.params.len == 3 && node.name == 'clamp' {
		p0 := node.typ.params[0].name
		p1 := node.typ.params[1].name
		p2 := node.typ.params[2].name
		g.sb.writeln('#define ${fn_name}(${p0}, ${p1}, ${p2}) ((${p0}) < (${p1}) ? (${p1}) : ((${p0}) > (${p2}) ? (${p2}) : (${p0})))')
	} else if node.typ.params.len == 0 && node.name in ['maxof', 'minof'] {
		// Emit specialized macros for maxof[T]()/minof[T]() - comptime functions
		// that return the min/max value for each numeric type.
		prefix := if fn_name.contains('__') { fn_name.all_before_last('__') + '__' } else { '' }
		is_max := node.name == 'maxof'
		// max_f32/max_f64 are in the math module and emitted as math__max_f32/math__max_f64
		type_const_pairs := if is_max {
			[['i8', 'max_i8'], ['i16', 'max_i16'], ['i32', 'max_i32'],
				['i64', 'max_i64'], ['int', 'max_int'], ['u8', 'max_u8'],
				['u16', 'max_u16'], ['u32', 'max_u32'], ['u64', 'max_u64'],
				['f32', 'math__max_f32'], ['f64', 'math__max_f64']]
		} else {
			[['i8', 'min_i8'], ['i16', 'min_i16'], ['i32', 'min_i32'],
				['i64', 'min_i64'], ['int', 'min_int'], ['u8', '((u8)(0))'],
				['u16', '((u16)(0))'], ['u32', '((u32)(0))'],
				['u64', '((u64)(0))'], ['f32', '(-math__max_f32)'],
				['f64', '(-math__max_f64)']]
		}
		for pair in type_const_pairs {
			g.sb.writeln('#define ${prefix}${node.name}_${pair[0]}() (${pair[1]})')
		}
	} else {
		// Fallback: emit a stub with array types.
		// Only emit stub bodies for modules that should be emitted in the current
		// cache bundle. Otherwise the same stub appears in both builtin.o and vlib.o.
		if !g.should_emit_module(g.cur_module) {
			return
		}
		g.sb.write_string('/* generic stub */ array ${fn_name}(')
		for i, param in node.typ.params {
			if i > 0 {
				g.sb.write_string(', ')
			}
			g.sb.write_string('array')
			if param.is_mut {
				g.sb.write_string('*')
			}
			pname := if param.name == 'array' { '_v_array' } else { param.name }
			g.sb.write_string(' ${pname}')
		}
		if node.typ.params.len == 0 {
			g.sb.write_string('void')
		}
		g.sb.writeln(') {')
		// For generic functions with a single mut array param (e.g., fn f[T](mut arr []T) []T),
		// return a clone of the input instead of a zero array. This handles the common
		// pattern of returning arr.clone() or similar transformations.
		// Only do this when the parameter type is actually an array type (ArrayType in AST),
		// not for mut struct pointers like mut t &T.
		mut param_is_array := false
		mut clone_param := ''
		if node.typ.params.len == 1 && node.typ.params[0].is_mut {
			ptyp := node.typ.params[0].typ
			if ptyp is ast.Type && ptyp is ast.ArrayType {
				param_is_array = true
				clone_param = if node.typ.params[0].name == 'array' {
					'_v_array'
				} else {
					node.typ.params[0].name
				}
			}
		}
		if param_is_array {
			g.sb.writeln('\treturn array__clone_to_depth(${clone_param}, 0);')
		} else {
			g.sb.writeln('\treturn (array){0};')
		}
		g.sb.writeln('}')
	}
	// Emit typedef for generic type parameter T as f64 (default numeric type)
	for gp in node.typ.generic_params {
		if gp is ast.Ident {
			qualified := if fn_name.contains('__') {
				'${fn_name.all_before_last('__')}__${gp.name}'
			} else {
				gp.name
			}
			tdef_key := 'typedef_${qualified}'
			if tdef_key !in g.emitted_types {
				g.emitted_types[tdef_key] = true
				g.sb.writeln('typedef f64 ${qualified};')
			}
		}
	}
}
