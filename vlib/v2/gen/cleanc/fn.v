// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module cleanc

import v2.ast
import v2.markused
import v2.types
import strings
import os

fn (mut g Gen) is_error_call_expr(expr ast.Expr) bool {
	match expr {
		ast.CallOrCastExpr {
			if expr.lhs is ast.Ident {
				name := sanitize_fn_ident(expr.lhs.name)
				if name in ['error', 'error_posix', 'error_with_code', 'error_win32'] {
					return true
				}
			}
		}
		ast.CallExpr {
			if expr.lhs is ast.Ident {
				name := sanitize_fn_ident(expr.lhs.name)
				if name in ['error', 'error_posix', 'error_with_code', 'error_win32'] {
					return true
				}
			}
			// Check if the call returns IError
			ret := g.get_call_return_type(expr.lhs, expr.args) or { '' }
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
	if g.cached_init_calls.len == 0 {
		return true
	}
	if g.cached_init_calls.len > 0 && !g.should_emit_module(module_name) {
		return true
	}
	if g.used_fn_keys.len == 0 || g.env == unsafe { nil } {
		return true
	}
	if should_always_emit_for_markused(g.cur_file_name) {
		return true
	}
	if is_builtin_map_file(g.cur_file_name) && should_keep_builtin_map_decl(decl) {
		return true
	}
	if is_builtin_string_file(g.cur_file_name) && should_keep_builtin_string_decl(decl) {
		return true
	}
	if is_builtin_array_file(g.cur_file_name) && should_keep_builtin_array_decl(decl) {
		return true
	}
	if decl.name.starts_with('__v_init_consts_') {
		return true
	}
	// Module init/deinit functions are called from the synthesized test main,
	// which runs after markused, so they won't be in used_fn_keys.
	if decl.name == 'init' || decl.name == 'deinit' {
		return true
	}
	// Methods on array types ([]T) and other types with unresolvable receivers
	// may produce 'unknown' receiver in the markused key, causing them to be
	// incorrectly pruned. Always emit methods whose receiver can't be resolved.
	// Also always emit methods on array receivers ([]T), since the markused
	// key for these can differ between the walker and the gen lookup.
	if decl.is_method {
		key2 := markused.decl_key(module_name, decl, g.env)
		if key2.contains('|unknown|') {
			return true
		}
		if decl.receiver.typ is ast.Type && decl.receiver.typ is ast.ArrayType {
			return true
		}
	}
	if g.force_emit_fn_names.len > 0 && !decl.is_method {
		c_name := if module_name.len > 0 && module_name != 'builtin' && module_name != 'main' {
			'${module_name}__${sanitize_fn_ident(decl.name)}'
		} else {
			sanitize_fn_ident(decl.name)
		}
		if c_name in g.force_emit_fn_names {
			return true
		}
	}
	// Check if this function was force-requested by generated code (e.g. map str functions).
	if g.force_emit_fn_names.len > 0 && decl.name == 'str' && decl.is_method {
		// Build the C function name for method str: ReceiverType__str
		recv_type_name := if decl.receiver.typ is ast.Ident {
			decl.receiver.typ.name
		} else {
			''
		}
		if recv_type_name.len > 0 {
			c_name := if module_name.len > 0 && module_name != 'builtin' && module_name != 'main' {
				'${module_name}__${recv_type_name}__str'
			} else {
				'${recv_type_name}__str'
			}
			if c_name in g.force_emit_fn_names {
				return true
			}
		}
	}
	key := markused.decl_key(module_name, decl, g.env)
	return key in g.used_fn_keys
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
	mut tuple_types := []ast.Expr{}
	if node.typ.return_type is ast.Type && node.typ.return_type is ast.TupleType {
		tuple_types = (node.typ.return_type as ast.TupleType).types.clone()
	} else if node.typ.return_type is ast.Type && node.typ.return_type is ast.OptionType {
		opt_type := node.typ.return_type as ast.OptionType
		if opt_type.base_type is ast.Type && opt_type.base_type is ast.TupleType {
			tuple_types = (opt_type.base_type as ast.TupleType).types.clone()
		}
	} else if node.typ.return_type is ast.Type && node.typ.return_type is ast.ResultType {
		res_type := node.typ.return_type as ast.ResultType
		if res_type.base_type is ast.Type && res_type.base_type is ast.TupleType {
			tuple_types = (res_type.base_type as ast.TupleType).types.clone()
		}
	}
	if tuple_types.len == 0 {
		return
	}
	mut fields := []string{cap: tuple_types.len}
	for tuple_type in tuple_types {
		c_type := g.expr_type_to_c(tuple_type)
		if c_type == '' {
			return
		}
		fields << c_type
	}
	if fields.len > 0 {
		g.tuple_aliases[tuple_name] = fields.clone()
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

fn fixed_array_return_wrapper_name(ret_type string) string {
	return '_v_${ret_type}'
}

fn (g &Gen) c_fn_return_type_from_v(ret_type string) string {
	if ret_type.starts_with('Array_fixed_') {
		return g.fixed_array_ret_wrappers[ret_type] or { fixed_array_return_wrapper_name(ret_type) }
	}
	return ret_type
}

fn (mut g Gen) infer_vector_return_type_from_stmts(stmts []ast.Stmt) string {
	for stmt in stmts {
		match stmt {
			ast.ReturnStmt {
				if stmt.exprs.len == 0 {
					continue
				}
				ret_expr := stmt.exprs[0]
				if ret_expr is ast.InitExpr {
					init_type := g.expr_type_to_c(ret_expr.typ)
					if vector_elem_type_for_name(init_type) != '' {
						return init_type
					}
				}
				mut expr_type := g.get_expr_type(ret_expr)
				if expr_type == '' || expr_type == 'int' {
					if raw := g.get_raw_type(ret_expr) {
						expr_type = g.types_type_to_c(raw)
					}
				}
				expr_type = expr_type.trim_right('*')
				if vector_elem_type_for_name(expr_type) != '' {
					return expr_type
				}
			}
			ast.BlockStmt {
				inferred := g.infer_vector_return_type_from_stmts(stmt.stmts)
				if inferred != '' {
					return inferred
				}
			}
			ast.ForStmt {
				inferred := g.infer_vector_return_type_from_stmts(stmt.stmts)
				if inferred != '' {
					return inferred
				}
			}
			ast.ExprStmt {
				if stmt.expr is ast.IfExpr {
					inferred := g.infer_vector_return_type_from_stmts(stmt.expr.stmts)
					if inferred != '' {
						return inferred
					}
					if stmt.expr.else_expr is ast.IfExpr {
						else_inferred := g.infer_vector_return_type_from_stmts(stmt.expr.else_expr.stmts)
						if else_inferred != '' {
							return else_inferred
						}
					}
				}
			}
			else {}
		}
	}
	return ''
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
					if g.cur_module == 'eventbus' && stmt.is_method
						&& receiver_generic_param_names(stmt).len > 0 {
						prev_generic_types := g.active_generic_types.clone()
						string_types := {
							'T': types.Type(types.string_)
						}
						g.active_generic_types = string_types.clone()
						if stmt.name in ['subscribe_method', 'unsubscribe_method'] {
							spec_name := g.specialized_fn_name(stmt, string_types)
							g.active_generic_types = prev_generic_types.clone()
							if spec_name != '' {
								prev_generic_types2 := g.active_generic_types.clone()
								g.active_generic_types = string_types.clone()
								g.register_fn_signature(stmt, spec_name)
								g.active_generic_types = prev_generic_types2.clone()
								continue
							}
						} else {
							eb_fn_name := g.get_fn_name(stmt)
							if eb_fn_name != '' {
								g.register_fn_signature(stmt, eb_fn_name)
								g.active_generic_types = prev_generic_types.clone()
								continue
							}
						}
						g.active_generic_types = prev_generic_types.clone()
					}
					if g.generic_fn_param_names(stmt).len > 0 {
						prev_generic_types := g.active_generic_types.clone()
						for spec in g.generic_fn_specializations(stmt) {
							g.active_generic_types = spec.generic_types.clone()
							g.register_fn_signature(stmt, spec.name)
						}
						g.active_generic_types = prev_generic_types.clone()
						continue
					}
					// For methods on generic structs, resolve receiver generic
					// params so the signature has correct concrete types.
					recv_gp := receiver_generic_param_names(stmt)
					if recv_gp.len > 0 {
						if bindings := g.get_receiver_generic_bindings(stmt) {
							prev_gt := g.active_generic_types.clone()
							g.active_generic_types = bindings.clone()
							gfn := g.get_fn_name(stmt)
							if gfn != '' {
								g.register_fn_signature(stmt, gfn)
							}
							g.active_generic_types = prev_gt.clone()
							continue
						}
					}
					mut fn_name := ''
					if stmt.stmts.len == 0 && stmt.language == .c {
						// Keep raw symbol names for C / system declaration-only functions.
						fn_name = sanitize_fn_ident(stmt.name)
					} else {
						fn_name = g.get_fn_name(stmt)
					}
					g.register_fn_signature(stmt, fn_name)
				}
				else {}
			}
		}
	}
}

fn (mut g Gen) register_fn_signature(node ast.FnDecl, fn_name string) {
	if fn_name == '' {
		return
	}
	// If this function has an @[export] attribute, map the V-qualified
	// name to the export name so call sites resolve correctly.
	for attr in node.attributes {
		if attr.name == 'export' {
			if attr.value is ast.StringLiteral {
				export_name := attr.value.value.trim('\'"')
				if export_name.len > 0 {
					v_name := sanitize_fn_ident(node.name)
					v_qualified := if g.cur_module != '' && g.cur_module != 'main'
						&& g.cur_module != 'builtin' {
						'${g.cur_module}__${v_name}'
					} else {
						v_name
					}
					if v_qualified != export_name {
						g.export_fn_names[v_qualified] = export_name
					}
				}
			}
		}
	}
	mut ret_type := if node.name == 'main' {
		'int'
	} else if node.typ.return_type !is ast.EmptyExpr {
		g.expr_type_to_c(node.typ.return_type)
	} else {
		'void'
	}
	ret_type = normalize_signature_type_name(ret_type, 'void')
	if ret_type == 'int' {
		inferred := g.infer_vector_return_type_from_stmts(node.stmts)
		if inferred != '' {
			ret_type = inferred
		}
	}
	if ret_type.starts_with('_option_') || ret_type.starts_with('_result_') {
		g.register_alias_type(ret_type)
	}
	if ret_type.starts_with('Array_fixed_') {
		g.fixed_array_ret_wrappers[ret_type] = fixed_array_return_wrapper_name(ret_type)
	}
	g.ensure_tuple_alias_for_fn_return(node, ret_type)
	mut params := []bool{}
	mut param_types := []string{}
	if node.is_method && node.receiver.name != '' {
		recv_type := normalize_signature_type_name(g.expr_type_to_c(node.receiver.typ),
			'void*')
		params << (node.receiver.is_mut || recv_type.ends_with('*'))
		param_types << if node.receiver.is_mut && !recv_type.ends_with('*') {
			recv_type + '*'
		} else {
			recv_type
		}
	}
	for param in node.typ.params {
		param_type := normalize_signature_type_name(g.expr_type_to_c(param.typ), 'int')
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

fn collect_generic_placeholder_names_from_expr(expr ast.Expr, mut seen map[string]bool, mut out []string) {
	match expr {
		ast.Ident {
			if is_generic_placeholder_type_name(expr.name) && expr.name !in seen {
				seen[expr.name] = true
				out << expr.name
			}
		}
		ast.PrefixExpr {
			collect_generic_placeholder_names_from_expr(expr.expr, mut seen, mut out)
		}
		ast.ModifierExpr {
			collect_generic_placeholder_names_from_expr(expr.expr, mut seen, mut out)
		}
		ast.ParenExpr {
			collect_generic_placeholder_names_from_expr(expr.expr, mut seen, mut out)
		}
		ast.SelectorExpr {
			collect_generic_placeholder_names_from_expr(expr.lhs, mut seen, mut out)
		}
		ast.GenericArgOrIndexExpr {
			collect_generic_placeholder_names_from_expr(expr.lhs, mut seen, mut out)
			collect_generic_placeholder_names_from_expr(expr.expr, mut seen, mut out)
		}
		ast.GenericArgs {
			collect_generic_placeholder_names_from_expr(expr.lhs, mut seen, mut out)
			for arg in expr.args {
				collect_generic_placeholder_names_from_expr(arg, mut seen, mut out)
			}
		}
		ast.Type {
			match expr {
				ast.ArrayType {
					collect_generic_placeholder_names_from_expr(expr.elem_type, mut seen, mut
						out)
				}
				ast.ArrayFixedType {
					collect_generic_placeholder_names_from_expr(expr.elem_type, mut seen, mut
						out)
					collect_generic_placeholder_names_from_expr(expr.len, mut seen, mut
						out)
				}
				ast.MapType {
					collect_generic_placeholder_names_from_expr(expr.key_type, mut seen, mut
						out)
					collect_generic_placeholder_names_from_expr(expr.value_type, mut seen, mut
						out)
				}
				ast.OptionType {
					collect_generic_placeholder_names_from_expr(expr.base_type, mut seen, mut
						out)
				}
				ast.ResultType {
					collect_generic_placeholder_names_from_expr(expr.base_type, mut seen, mut
						out)
				}
				ast.TupleType {
					for typ in expr.types {
						collect_generic_placeholder_names_from_expr(typ, mut seen, mut
							out)
					}
				}
				ast.FnType {
					for param in expr.params {
						collect_generic_placeholder_names_from_expr(param.typ, mut seen, mut
							out)
					}
					if expr.return_type !is ast.EmptyExpr {
						collect_generic_placeholder_names_from_expr(expr.return_type, mut
							seen, mut out)
					}
				}
				ast.GenericType {
					// Recurse into params (e.g. LinkedList[T] → extract T)
					for param in expr.params {
						collect_generic_placeholder_names_from_expr(param, mut seen, mut
							out)
					}
				}
				else {}
			}
		}
		else {}
	}
}

fn (g &Gen) generic_fn_param_names(node ast.FnDecl) []string {
	mut names := []string{}
	for gp in node.typ.generic_params {
		if gp is ast.Ident && gp.name != '' {
			names << gp.name
		}
	}
	if names.len == 0 && node.is_method && g.cur_module == 'eventbus'
		&& node.name in ['subscribe_method', 'unsubscribe_method'] {
		mut seen := map[string]bool{}
		if node.receiver.typ !is ast.EmptyExpr {
			collect_generic_placeholder_names_from_expr(node.receiver.typ, mut seen, mut
				names)
		}
		for param in node.typ.params {
			collect_generic_placeholder_names_from_expr(param.typ, mut seen, mut names)
		}
	}
	return names
}

// receiver_generic_param_names collects generic placeholder names from a
// method's receiver type (e.g. LinkedList[T] → ['T']). These params are
// inherited from the struct definition, not the function's own generic_params.
fn receiver_generic_param_names(node ast.FnDecl) []string {
	if !node.is_method {
		return []string{}
	}
	mut seen := map[string]bool{}
	mut names := []string{}
	if node.receiver.typ !is ast.EmptyExpr {
		collect_generic_placeholder_names_from_expr(node.receiver.typ, mut seen, mut names)
	}
	return names
}

// get_receiver_generic_bindings returns the concrete type bindings for a method
// on a generic struct by looking up the struct's recorded bindings from
// GenericType instantiations (e.g. LinkedList[ValueInfo] → {T: ValueInfo}).
fn (mut g Gen) get_receiver_generic_bindings(node ast.FnDecl) ?map[string]types.Type {
	if !node.is_method {
		return none
	}
	// Get the receiver's C type name (e.g. json2__LinkedList)
	recv_c_name := g.expr_type_to_c(node.receiver.typ)
	if recv_c_name in g.generic_struct_bindings {
		return g.generic_struct_bindings[recv_c_name]
	}
	// Also try with current module prefix
	if !recv_c_name.contains('__') && g.cur_module != '' && g.cur_module != 'main'
		&& g.cur_module != 'builtin' {
		qualified := '${g.cur_module}__${recv_c_name}'
		if qualified in g.generic_struct_bindings {
			return g.generic_struct_bindings[qualified]
		}
	}
	return none
}

fn (mut g Gen) specialized_fn_name(node ast.FnDecl, generic_types map[string]types.Type) string {
	generic_params := g.generic_fn_param_names(node)
	if generic_params.len == 0 {
		return ''
	}
	prev_generic_types := g.active_generic_types.clone()
	g.active_generic_types = generic_types.clone()
	base_name := g.get_fn_name(node)
	g.active_generic_types = prev_generic_types.clone()
	if base_name == '' {
		return ''
	}
	mut suffixes := []string{}
	for param_name in generic_params {
		if concrete := generic_types[param_name] {
			suffixes << g.generic_specialization_token_from_type(concrete)
		}
	}
	if suffixes.len == 0 {
		return base_name
	}
	return '${base_name}_${suffixes.join('_')}'
}

fn (g &Gen) generic_key_matches_decl(node ast.FnDecl, key string) bool {
	return key == node.name || key.starts_with('${node.name}[') || key.contains('.${node.name}[')
}

// build_generic_spec_index precomputes a reverse index from function names
// to matching keys in env.generic_types. This avoids O(n*m) iteration
// in generic_fn_specializations (called per generic fn per file).
fn (mut g Gen) build_generic_spec_index() {
	if g.env == unsafe { nil } {
		return
	}
	for key, _ in g.env.generic_types {
		// Extract the base function name from the key.
		// Keys can be: "fn_name", "fn_name[T]", "module.fn_name[T]"
		mut fn_name := key
		bracket_idx := key.index_u8(`[`)
		if bracket_idx > 0 {
			fn_name = key[..bracket_idx]
		}
		// If qualified (contains '.'), also index by the short name
		dot_idx := fn_name.last_index_u8(`.`)
		if dot_idx > 0 && dot_idx < fn_name.len - 1 {
			short_name := fn_name[dot_idx + 1..]
			if short_name.len > 0 {
				g.generic_spec_index[short_name] << key
			}
		}
		g.generic_spec_index[fn_name] << key
	}
}

fn generic_param_names(params []ast.Expr) []string {
	mut seen := map[string]bool{}
	mut names := []string{}
	for param in params {
		collect_generic_placeholder_names_from_expr(param, mut seen, mut names)
	}
	return names
}

fn (g &Gen) fallback_generic_bindings_for_names(param_names []string) ?map[string]types.Type {
	if param_names.len == 0 || g.env == unsafe { nil } {
		return none
	}
	mut bindings := map[string]types.Type{}
	for param_name in param_names {
		mut found := false
		for _, generic_maps in g.env.generic_types {
			for generic_types in generic_maps {
				concrete := generic_types[param_name] or { continue }
				if concrete.name() == param_name || type_contains_generic_placeholder(concrete) {
					continue
				}
				bindings[param_name] = concrete
				found = true
				break
			}
			if found {
				break
			}
		}
		if !found {
			return none
		}
	}
	return bindings
}

fn (mut g Gen) generic_fn_specializations(node ast.FnDecl) []GenericFnSpecialization {
	generic_params := g.generic_fn_param_names(node)
	if generic_params.len == 0 || g.env == unsafe { nil } {
		return []GenericFnSpecialization{}
	}
	mut specs := []GenericFnSpecialization{}
	mut seen := map[string]bool{}
	// Use precomputed index for O(1) lookup instead of iterating all generic types.
	matching_keys := g.generic_spec_index[node.name]
	for key in matching_keys {
		if !g.generic_key_matches_decl(node, key) {
			continue
		}
		generic_maps := g.env.generic_types[key]
		for generic_types in generic_maps {
			mut skip_spec := false
			for param_name in generic_params {
				concrete := generic_types[param_name] or {
					skip_spec = true
					break
				}
				if concrete.name() == param_name || type_contains_generic_placeholder(concrete) {
					skip_spec = true
					break
				}
			}
			if skip_spec {
				continue
			}
			spec_name := g.specialized_fn_name(node, generic_types)
			if spec_name == '' || spec_name in seen {
				continue
			}
			seen[spec_name] = true
			specs << GenericFnSpecialization{
				name:          spec_name
				generic_types: generic_types.clone()
			}
		}
	}
	if specs.len == 0 && g.cur_module == 'eventbus' && node.is_method
		&& node.name in ['subscribe_method', 'unsubscribe_method'] && generic_params == ['T'] {
		generic_types := {
			'T': types.Type(types.string_)
		}
		spec_name := g.specialized_fn_name(node, generic_types)
		if spec_name != '' && spec_name !in seen {
			seen[spec_name] = true
			specs << GenericFnSpecialization{
				name:          spec_name
				generic_types: generic_types.clone()
			}
		}
	}
	if specs.len > 0 || generic_params.len == 0 {
		return specs
	}
	// Fallback: nested generic helpers inside another specialized generic function
	// may only record placeholder bindings like `T -> T` in env.generic_types.
	// Reuse concrete bindings seen for the same generic parameter names elsewhere.
	if generic_params.len == 1 {
		param_name := generic_params[0]
		mut fallback_types := map[string]types.Type{}
		for _, generic_maps in g.env.generic_types {
			for generic_types in generic_maps {
				concrete := generic_types[param_name] or { continue }
				if concrete.name() == param_name || type_contains_generic_placeholder(concrete) {
					continue
				}
				fallback_types[concrete.name()] = concrete
			}
		}
		for _, concrete in fallback_types {
			generic_types := {
				param_name: concrete
			}
			spec_name := g.specialized_fn_name(node, generic_types)
			if spec_name == '' || spec_name in seen {
				continue
			}
			seen[spec_name] = true
			specs << GenericFnSpecialization{
				name:          spec_name
				generic_types: generic_types.clone()
			}
		}
	}
	if specs.len == 0 {
		if bindings := g.fallback_generic_bindings_for_names(generic_params) {
			prev_generic_types := g.active_generic_types.clone()
			g.active_generic_types = bindings.clone()
			spec_name := g.get_fn_name(node)
			g.active_generic_types = prev_generic_types.clone()
			if spec_name != '' && spec_name !in seen {
				seen[spec_name] = true
				specs << GenericFnSpecialization{
					name:          spec_name
					generic_types: bindings.clone()
				}
			}
		}
	}
	return specs
}

fn infer_generic_type_bindings_from_param(param ast.Expr, concrete types.Type, generic_params []string, mut bindings map[string]types.Type) {
	match param {
		ast.Ident {
			if param.name in generic_params {
				bindings[param.name] = concrete
			}
		}
		ast.PrefixExpr {
			if concrete is types.Pointer {
				infer_generic_type_bindings_from_param(param.expr, concrete.base_type,
					generic_params, mut bindings)
			}
		}
		ast.ModifierExpr {
			infer_generic_type_bindings_from_param(param.expr, concrete, generic_params, mut
				bindings)
		}
		ast.Type {
			match param {
				ast.ArrayType {
					if concrete is types.Array {
						infer_generic_type_bindings_from_param(param.elem_type, concrete.elem_type,
							generic_params, mut bindings)
					}
				}
				ast.ArrayFixedType {
					if concrete is types.ArrayFixed {
						infer_generic_type_bindings_from_param(param.elem_type, concrete.elem_type,
							generic_params, mut bindings)
					}
				}
				ast.MapType {
					if concrete is types.Map {
						infer_generic_type_bindings_from_param(param.key_type, concrete.key_type,
							generic_params, mut bindings)
						infer_generic_type_bindings_from_param(param.value_type, concrete.value_type,
							generic_params, mut bindings)
					}
				}
				ast.OptionType {
					if concrete is types.OptionType {
						infer_generic_type_bindings_from_param(param.base_type, concrete.base_type,
							generic_params, mut bindings)
					}
				}
				ast.ResultType {
					if concrete is types.ResultType {
						infer_generic_type_bindings_from_param(param.base_type, concrete.base_type,
							generic_params, mut bindings)
					}
				}
				else {}
			}
		}
		else {}
	}
}

fn direct_generic_placeholder_name(e ast.Expr) string {
	match e {
		ast.Ident {
			if is_generic_placeholder_type_name(e.name) {
				return e.name
			}
		}
		ast.PrefixExpr {
			return direct_generic_placeholder_name(e.expr)
		}
		ast.ModifierExpr {
			return direct_generic_placeholder_name(e.expr)
		}
		else {}
	}
	return ''
}

fn (g &Gen) qualify_local_call_name(name string) string {
	if name != '' && g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin'
		&& !name.contains('__') {
		return '${g.cur_module}__${name}'
	}
	return name
}

fn (mut g Gen) find_generic_fn_decl_by_base_name(name string) ?ast.FnDecl {
	prev_module := g.cur_module
	prev_file_name := g.cur_file_name
	prev_active_generic_types := g.active_generic_types.clone()
	g.active_generic_types = map[string]types.Type{}
	defer {
		g.cur_module = prev_module
		g.cur_file_name = prev_file_name
		g.active_generic_types = prev_active_generic_types.clone()
	}
	for file in g.files {
		g.set_file_module(file)
		for stmt in file.stmts {
			if stmt is ast.FnDecl {
				if g.generic_fn_param_names(stmt).len == 0 {
					continue
				}
				qualified := g.get_fn_name(stmt)
				if qualified == name {
					return stmt
				}
				// Also match unqualified names: 'encode' matches 'json2__encode'
				if !name.contains('__') && !stmt.is_method && stmt.name == name {
					return stmt
				}
			}
		}
	}
	return none
}

fn (g &Gen) find_specialized_call_name(name string, token string) ?string {
	if name == '' || token == '' {
		return none
	}
	candidate := '${name}_${token}'
	if candidate in g.fn_param_is_ptr || candidate in g.fn_return_types {
		return candidate
	}
	if !name.contains('__') {
		qualified := g.qualify_local_call_name(candidate)
		if qualified != candidate
			&& (qualified in g.fn_param_is_ptr || qualified in g.fn_return_types) {
			return qualified
		}
	}
	return none
}

fn (mut g Gen) try_specialize_generic_call_name(name string, call_args []ast.Expr) ?string {
	if name == '' {
		return none
	}
	if name in ['eventbus__Subscriber__subscribe_method', 'eventbus__Subscriber__unsubscribe_method']
		&& call_args.len > 1 {
		base_arg := if call_args[1] is ast.ModifierExpr {
			(call_args[1] as ast.ModifierExpr).expr
		} else {
			call_args[1]
		}
		mut arg_type_name := g.get_expr_type(base_arg).trim_space().trim_right('*')
		if (arg_type_name == '' || arg_type_name == 'int') && base_arg is ast.Ident {
			arg_type_name = (g.get_local_var_c_type(base_arg.name) or { '' }).trim_space().trim_right('*')
		}
		if arg_type_name == 'string' {
			candidate := '${name}_string'
			if candidate in g.fn_param_is_ptr || candidate in g.fn_return_types {
				return candidate
			}
		}
	}
	if name in ['decode_value_T', 'json2__decode_value_T'] && call_args.len > 1 {
		base_arg := if call_args[1] is ast.ModifierExpr {
			(call_args[1] as ast.ModifierExpr).expr
		} else {
			call_args[1]
		}
		arg_type_name := g.get_expr_type(base_arg).trim_space().trim_right('*')
		if arg_type_name != '' && arg_type_name != 'int' {
			if candidate := g.find_specialized_call_name(g.qualify_local_call_name('decode_value'),
				sanitize_generic_token_part(arg_type_name))
			{
				return candidate
			}
		}
	}
	if name in ['Encoder__encode_value_T', 'json2__Encoder__encode_value_T', 'Encoder__encode_value', 'json2__Encoder__encode_value']
		&& call_args.len > 1 {
		base_arg := if call_args[1] is ast.ModifierExpr {
			(call_args[1] as ast.ModifierExpr).expr
		} else {
			call_args[1]
		}
		mut arg_type_name := g.get_expr_type(base_arg).trim_space().trim_right('*')
		if (arg_type_name == '' || arg_type_name == 'int') && base_arg is ast.Ident {
			arg_type_name = (g.get_local_var_c_type(base_arg.name) or { '' }).trim_space().trim_right('*')
		}
		if arg_type_name != '' && arg_type_name != 'int' {
			if candidate := g.find_specialized_call_name('json2__Encoder__encode_value',
				sanitize_generic_token_part(arg_type_name))
			{
				return candidate
			}
		}
	}
	if g.active_generic_types.len > 0 {
		mut parts := name.split('_')
		mut changed := false
		for i, part in parts {
			if concrete := g.active_generic_types[part] {
				parts[i] = g.generic_specialization_token_from_type(concrete)
				changed = true
			}
		}
		if changed {
			candidate := parts.join('_')
			if candidate in g.fn_param_is_ptr || candidate in g.fn_return_types {
				return candidate
			}
		}
	}
	if call_args.len > 0 {
		base_arg := if call_args[0] is ast.ModifierExpr {
			(call_args[0] as ast.ModifierExpr).expr
		} else {
			call_args[0]
		}
		mut arg_type_name := g.get_expr_type(base_arg).trim_space()
		if (arg_type_name == '' || arg_type_name == 'int') && base_arg is ast.Ident {
			arg_type_name = (g.get_local_var_c_type(base_arg.name) or { '' }).trim_space()
		}
		if arg_type_name == '' || arg_type_name == 'int' {
			if raw := g.get_raw_type(base_arg) {
				arg_type_name = g.types_type_to_c(raw).trim_space()
			}
		}
		arg_type_name = arg_type_name.trim_right('*')
		if arg_type_name != '' && arg_type_name != 'int' {
			mut candidate_types := [arg_type_name]
			if !arg_type_name.contains('__') && g.cur_module != '' && g.cur_module != 'main'
				&& g.cur_module != 'builtin' {
				candidate_types << '${g.cur_module}__${arg_type_name}'
			}
			for concrete_type_name in candidate_types {
				if candidate := g.find_specialized_call_name(name, sanitize_generic_token_part(concrete_type_name)) {
					return candidate
				}
			}
		}
	}
	if name in ['decode_value', 'json2__decode_value'] && g.cur_fn_ret_type.starts_with('_result_') {
		result_base := g.cur_fn_ret_type['_result_'.len..]
		if candidate := g.find_specialized_call_name(g.qualify_local_call_name('decode_value'),
			sanitize_generic_token_part(result_base))
		{
			return candidate
		}
	}
	decl := g.find_generic_fn_decl_by_base_name(name) or { return none }
	generic_params := g.generic_fn_param_names(decl)
	if generic_params.len == 0 || g.env == unsafe { nil } {
		return none
	}
	if generic_params.len == 1 {
		param_name := generic_params[0]
		arg_offset := if decl.is_method && call_args.len == decl.typ.params.len + 1 { 1 } else { 0 }
		for i, param in decl.typ.params {
			arg_idx := i + arg_offset
			if arg_idx >= call_args.len || !expr_has_generic_placeholder(param.typ) {
				continue
			}
			direct_placeholder := direct_generic_placeholder_name(param.typ)
			if direct_placeholder != param_name {
				continue
			}
			base_arg := if call_args[arg_idx] is ast.ModifierExpr {
				(call_args[arg_idx] as ast.ModifierExpr).expr
			} else {
				call_args[arg_idx]
			}
			mut arg_type_name := g.get_expr_type(base_arg).trim_space()
			if (arg_type_name == '' || arg_type_name == 'int') && base_arg is ast.Ident {
				arg_type_name = (g.get_local_var_c_type(base_arg.name) or { '' }).trim_space()
			}
			if arg_type_name == '' || arg_type_name == 'int' {
				if raw := g.get_raw_type(base_arg) {
					arg_type_name = g.types_type_to_c(raw).trim_space()
				}
			}
			arg_type_name = arg_type_name.trim_right('*')
			if arg_type_name == '' || arg_type_name == 'int' {
				continue
			}
			mut candidate_types := [arg_type_name]
			if !arg_type_name.contains('__') && g.cur_module != '' && g.cur_module != 'main'
				&& g.cur_module != 'builtin' {
				candidate_types << '${g.cur_module}__${arg_type_name}'
			}
			for concrete_type_name in candidate_types {
				if candidate := g.find_specialized_call_name(name, sanitize_generic_token_part(concrete_type_name)) {
					return candidate
				}
			}
		}
	}
	mut bindings := map[string]types.Type{}
	arg_offset := if decl.is_method && call_args.len == decl.typ.params.len + 1 { 1 } else { 0 }
	for i, param in decl.typ.params {
		arg_idx := i + arg_offset
		if arg_idx >= call_args.len || !expr_has_generic_placeholder(param.typ) {
			continue
		}
		arg := if call_args[arg_idx] is ast.ModifierExpr {
			(call_args[arg_idx] as ast.ModifierExpr).expr
		} else {
			call_args[arg_idx]
		}
		if !expr_has_valid_data(arg) {
			continue
		}
		pos := arg.pos()
		if pos.id == 0 {
			continue
		}
		concrete := g.env.get_expr_type(pos.id) or {
			if raw := g.get_raw_type(arg) {
				raw
			} else {
				continue
			}
		}
		infer_generic_type_bindings_from_param(param.typ, concrete, generic_params, mut
			bindings)
	}
	if bindings.len != generic_params.len {
		return none
	}
	candidate := g.specialized_fn_name(decl, bindings)
	if candidate in g.fn_param_is_ptr || candidate in g.fn_return_types {
		return candidate
	}
	// specialized_fn_name uses g.cur_module (caller's module) which may differ
	// from the generic function's module. Fall back to constructing the name
	// from the original `name` (which already has the correct module prefix).
	mut suffixes := []string{}
	for param_name in generic_params {
		if concrete := bindings[param_name] {
			suffixes << g.generic_specialization_token_from_type(concrete)
		}
	}
	if suffixes.len > 0 {
		alt_candidate := '${name}_${suffixes.join('_')}'
		if alt_candidate in g.fn_param_is_ptr || alt_candidate in g.fn_return_types {
			return alt_candidate
		}
	}
	return none
}

// register_builder_methods ensures strings__Builder methods are known to
// fn_param_is_ptr so call sites emit &sb for the receiver.  The Builder
// is a typedef for array and its methods take a mutable (pointer) receiver.
fn (mut g Gen) register_builder_methods() {
	builder_methods := [
		// name, has_extra_param, return_type
		['strings__Builder__write_rune', 'true', 'void'],
		['strings__Builder__write_u8', 'true', 'void'],
		['strings__Builder__write_string', 'true', 'void'],
		['strings__Builder__write_ptr', 'true', 'void'],
		['strings__Builder__write_repeated_rune', 'true', 'void'],
		['strings__Builder__spart', 'false', 'string'],
		['strings__Builder__cut_last', 'true', 'void'],
		['strings__Builder__str', 'false', 'string'],
	]
	for entry in builder_methods {
		name := entry[0]
		has_extra := entry[1] == 'true'
		ret := entry[2]
		if name !in g.fn_param_is_ptr {
			if has_extra {
				g.fn_param_is_ptr[name] = [true, false]
				g.fn_param_types[name] = ['strings__Builder*', 'int']
			} else {
				g.fn_param_is_ptr[name] = [true]
				g.fn_param_types[name] = ['strings__Builder*']
			}
			g.fn_return_types[name] = ret
		}
	}
}

fn (mut g Gen) emit_fixed_array_return_wrappers() {
	mut fixed_types := g.fixed_array_ret_wrappers.keys()
	fixed_types.sort()
	for fixed_type in fixed_types {
		wrapper_name := g.fixed_array_ret_wrappers[fixed_type] or {
			fixed_array_return_wrapper_name(fixed_type)
		}
		key := 'fixed_ret_wrapper_${wrapper_name}'
		if key in g.emitted_types {
			continue
		}
		g.emitted_types[key] = true
		g.sb.writeln('typedef struct ${wrapper_name} {')
		g.sb.writeln('\t${fixed_type} ret_arr;')
		g.sb.writeln('} ${wrapper_name};')
		g.sb.writeln('')
	}
}

fn (mut g Gen) gen_fn_decl(node ast.FnDecl) {
	g.gen_fn_decl_ptr(&node)
}

fn (mut g Gen) gen_fn_decl_ptr(node &ast.FnDecl) {
	if !g.should_emit_fn_decl(g.cur_module, *node) {
		return
	}
	// All other functions are resolved from the host executable.
	if g.pref != unsafe { nil } && g.pref.is_shared_lib {
		if !node.attributes.has('live') {
			return
		}
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
	// eventbus module: all generic struct methods use T = string.
	// For subscribe_method/unsubscribe_method, the specialized name (_string suffix) is used.
	// For other methods (publish, has_subscriber, etc.), keep the base name but resolve T = string.
	if g.cur_module == 'eventbus' && node.is_method && receiver_generic_param_names(*node).len > 0 {
		prev_generic_types := g.active_generic_types.clone()
		string_types := {
			'T': types.Type(types.string_)
		}
		g.active_generic_types = string_types.clone()
		if node.name in ['subscribe_method', 'unsubscribe_method'] {
			spec_name := g.specialized_fn_name(*node, string_types)
			if spec_name != '' {
				g.gen_fn_decl_with_name_ptr(node, spec_name)
				g.active_generic_types = prev_generic_types.clone()
				return
			}
		} else {
			fn_name := g.get_fn_name(*node)
			if fn_name != '' {
				g.gen_fn_decl_with_name_ptr(node, fn_name)
				g.active_generic_types = prev_generic_types.clone()
				return
			}
		}
		g.active_generic_types = prev_generic_types.clone()
	}
	if g.generic_fn_param_names(*node).len > 0 {
		prev_generic_types := g.active_generic_types.clone()
		for spec in g.generic_fn_specializations(*node) {
			g.active_generic_types = spec.generic_types.clone()
			g.gen_fn_decl_with_name_ptr(node, spec.name)
		}
		g.active_generic_types = prev_generic_types.clone()
		return
	}

	// For methods on generic structs (e.g. LinkedList[T].push), resolve the
	// generic params from the receiver type using recorded struct bindings.
	// This ensures T resolves to the concrete type (e.g. ValueInfo) instead
	// of the default fallback (f64).
	recv_generic_params := receiver_generic_param_names(*node)
	if recv_generic_params.len > 0 {
		if bindings := g.get_receiver_generic_bindings(*node) {
			prev_generic_types := g.active_generic_types.clone()
			g.active_generic_types = bindings.clone()
			fn_name := g.get_fn_name(*node)
			if fn_name != '' {
				g.gen_fn_decl_with_name_ptr(node, fn_name)
			}
			g.active_generic_types = prev_generic_types.clone()
			return
		}
	}

	fn_name := g.get_fn_name(*node)
	if fn_name == '' {
		return
	}
	g.gen_fn_decl_with_name_ptr(node, fn_name)
}

fn (mut g Gen) gen_fn_decl_with_name(node ast.FnDecl, fn_name string) {
	g.gen_fn_decl_with_name_ptr(&node, fn_name)
}

// Keep pass 5 on pointer-based FnDecl access; large by-value copies here
// corrupt the self-hosted cleanc caller after the first emitted body.
fn (mut g Gen) gen_fn_decl_with_name_ptr(node &ast.FnDecl, fn_name string) {
	fn_key := 'fn_${fn_name}'
	if fn_key in g.blocked_fn_keys {
		return
	}
	if g.should_skip_plain_v_fallback_fn(fn_key) {
		return
	}
	if fn_key in g.emitted_types {
		return
	}
	g.emitted_types[fn_key] = true

	// Set function scope for type lookups
	g.cur_fn_name = node.name
	if os.getenv('V2_TRACE_FN') != '' {
		println('   - C Gen/fn ${g.cur_file_name}:${node.name}')
	}
	g.runtime_local_types = map[string]string{}
	g.cur_fn_returned_idents = g.collect_returned_idents(node.stmts)
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
	g.cur_fn_c_ret_type = g.c_fn_return_type_from_v(g.cur_fn_ret_type)
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
	if node.is_method && node.receiver.name != '' {
		mut receiver_type := ''
		if sig_param_types := g.fn_param_types[fn_name] {
			if sig_param_types.len > 0 {
				receiver_type = normalize_signature_type_name(sig_param_types[0], '')
			}
		}
		if receiver_type == '' {
			receiver_type = g.expr_type_to_c(node.receiver.typ)
		}
		if node.receiver.is_mut || receiver_type.ends_with('*') {
			g.cur_fn_mut_params[node.receiver.name] = true
		}
	}
	// Register receiver type in local runtime type cache so chained selector
	// resolution (e.g. a.x.y) can infer field types reliably.
	if node.is_method && node.receiver.name != '' {
		mut receiver_type := ''
		if sig_param_types := g.fn_param_types[fn_name] {
			if sig_param_types.len > 0 {
				receiver_type = normalize_signature_type_name(sig_param_types[0], '')
			}
		}
		if receiver_type == '' {
			receiver_type = g.expr_type_to_c(node.receiver.typ)
		}
		if receiver_type != '' {
			g.runtime_local_types[node.receiver.name] = receiver_type
		}
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
			// Also register under the C-renamed name (e.g. 'array' → '_v_array')
			// so that body references using the renamed identifier can find the type.
			if param.name == 'array' {
				g.runtime_local_types['_v_array'] = ptype
			}
		}
	}

	// Check for @[live] attribute
	is_live_fn := node.name != 'main' && node.attributes.has('live')

	// Generate function header (with impl_live_ prefix for @[live] functions)
	if is_live_fn {
		g.gen_fn_head_live_ptr(node, fn_name)
	} else {
		g.gen_fn_head_with_name_ptr(node, fn_name)
	}
	g.sb.writeln(' {')
	g.indent++

	// Main function: initialize argc/argv
	if node.name == 'main' {
		g.write_indent()
		g.sb.writeln('g_main_argc = ___argc;')
		g.write_indent()
		g.sb.writeln('g_main_argv = (void*)___argv;')
		// GC initialization (translated from Go's runtime GC init)
		if g.pref != unsafe { nil } && g.pref.gc_mode == .vgc {
			g.write_indent()
			g.sb.writeln('// VGC initialization (concurrent tri-color mark-and-sweep)')
			g.write_indent()
			g.sb.writeln('builtin__vgc_init();')
		}
		for init_call in g.cached_init_calls {
			g.write_indent()
			g.sb.writeln('${init_call}();')
		}
		// Call module init() functions (e.g., rand__init) that the transformer
		// doesn't inject. These must run before user code.
		for init_fn, _ in g.fn_return_types {
			if init_fn.ends_with('__init') && init_fn.count('__') == 1 {
				first_char := init_fn[0]
				if first_char >= `a` && first_char <= `z` {
					if params := g.fn_param_is_ptr[init_fn] {
						if params.len == 0 {
							g.write_indent()
							g.sb.writeln('${init_fn}();')
						}
					} else {
						g.write_indent()
						g.sb.writeln('${init_fn}();')
					}
				}
			}
		}
	}
	// Live reload: emit init call placeholder in main (will be defined by emit_live_reload_infrastructure)
	if node.name == 'main' {
		g.write_indent()
		g.sb.writeln('__v_live_init();')
	}
	g.gen_stmts(node.stmts)

	// Implicit return 0 for main
	if node.name == 'main' {
		g.write_indent()
		g.sb.writeln('return 0;')
	} else if g.cur_fn_ret_type.starts_with('_result_') {
		g.write_indent()
		g.sb.writeln('return (${g.cur_fn_ret_type}){0};')
	} else if g.cur_fn_ret_type.starts_with('_option_') {
		g.write_indent()
		g.sb.writeln('return (${g.cur_fn_ret_type}){0};')
	}

	g.indent--
	g.sb.writeln('}')
	g.sb.writeln('')
}

fn (mut g Gen) collect_returned_idents(stmts []ast.Stmt) map[string]bool {
	mut out := map[string]bool{}
	g.collect_returned_idents_from_stmts(stmts, mut out)
	return out
}

fn (mut g Gen) collect_returned_idents_from_stmts(stmts []ast.Stmt, mut out map[string]bool) {
	for stmt in stmts {
		match stmt {
			ast.ReturnStmt {
				if stmt.exprs.len == 1 {
					if name := returned_ident_name(stmt.exprs[0]) {
						out[name] = true
					}
				}
			}
			ast.BlockStmt {
				g.collect_returned_idents_from_stmts(stmt.stmts, mut out)
			}
			ast.ComptimeStmt {
				if stmt.stmt !is ast.EmptyStmt {
					g.collect_returned_idents_from_stmts([stmt.stmt], mut out)
				}
			}
			ast.DeferStmt {
				g.collect_returned_idents_from_stmts(stmt.stmts, mut out)
			}
			ast.ForStmt {
				if stmt.init !is ast.EmptyStmt {
					g.collect_returned_idents_from_stmts([stmt.init], mut out)
				}
				if stmt.post !is ast.EmptyStmt {
					g.collect_returned_idents_from_stmts([stmt.post], mut out)
				}
				g.collect_returned_idents_from_stmts(stmt.stmts, mut out)
			}
			ast.LabelStmt {
				if stmt.stmt !is ast.EmptyStmt {
					g.collect_returned_idents_from_stmts([stmt.stmt], mut out)
				}
			}
			ast.ExprStmt {
				g.collect_returned_idents_from_expr(stmt.expr, mut out)
			}
			else {}
		}
	}
}

fn (mut g Gen) collect_returned_idents_from_expr(expr ast.Expr, mut out map[string]bool) {
	match expr {
		ast.IfExpr {
			g.collect_returned_idents_from_stmts(expr.stmts, mut out)
			if expr.else_expr !is ast.EmptyExpr {
				g.collect_returned_idents_from_expr(expr.else_expr, mut out)
			}
		}
		ast.MatchExpr {
			for branch in expr.branches {
				g.collect_returned_idents_from_stmts(branch.stmts, mut out)
			}
		}
		ast.SelectExpr {
			g.collect_returned_idents_from_stmts(expr.stmts, mut out)
			if expr.next !is ast.EmptyExpr {
				g.collect_returned_idents_from_expr(expr.next, mut out)
			}
		}
		ast.UnsafeExpr {
			g.collect_returned_idents_from_stmts(expr.stmts, mut out)
		}
		ast.ParenExpr {
			g.collect_returned_idents_from_expr(expr.expr, mut out)
		}
		ast.ModifierExpr {
			g.collect_returned_idents_from_expr(expr.expr, mut out)
		}
		ast.CastExpr {
			g.collect_returned_idents_from_expr(expr.expr, mut out)
		}
		else {}
	}
}

fn returned_ident_name(expr ast.Expr) ?string {
	match expr {
		ast.Ident {
			return expr.name
		}
		ast.ParenExpr {
			return returned_ident_name(expr.expr)
		}
		ast.ModifierExpr {
			return returned_ident_name(expr.expr)
		}
		ast.CastExpr {
			return returned_ident_name(expr.expr)
		}
		else {}
	}
	return none
}

fn (mut g Gen) gen_fn_head(node ast.FnDecl) {
	fn_name := g.get_fn_name(node)
	if fn_name == '' {
		return
	}
	g.gen_fn_head_with_name_ptr(&node, fn_name)
}

fn (mut g Gen) gen_fn_head_with_name(node ast.FnDecl, fn_name string) {
	g.gen_fn_head_with_name_ptr(&node, fn_name)
}

fn (mut g Gen) gen_fn_head_with_name_ptr(node &ast.FnDecl, fn_name string) {
	mut ret := if fn_ret := g.fn_return_types[fn_name] {
		fn_ret
	} else if node.typ.return_type !is ast.EmptyExpr {
		g.expr_type_to_c(node.typ.return_type)
	} else {
		'void'
	}
	ret = normalize_signature_type_name(ret, 'void')
	mut c_ret := g.c_fn_return_type_from_v(ret)
	if node.name == 'main' {
		ret = 'int'
		c_ret = 'int'
	}
	sig_param_types := g.fn_param_types[fn_name] or { []string{} }

	// main takes argc/argv
	if node.name == 'main' {
		g.sb.write_string(c_ret)
		g.sb.write_string(' ')
		g.sb.write_string(fn_name)
		g.sb.write_string('(int ___argc, char** ___argv)')
		return
	}

	g.sb.write_string(c_ret)
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
		// Rename V variables that clash with C type names (matches expr.v Ident handler)
		pname := if param.name == 'array' { '_v_array' } else { param.name }
		g.sb.write_string(pname)
		sig_idx++
	}
	g.sb.write_string(')')
}

// gen_c_extern_forward_decl emits a forward declaration for a C extern function.
// e.g., `fn C.macos_objc_msg_void1(obj Id, selector Sel, a0 voidptr)` emits:
//   `void macos_objc_msg_void1(void* obj, void* selector, void* a0);`
fn (mut g Gen) gen_c_extern_forward_decl(node ast.FnDecl) {
	// Extract C function name: strip 'C.' prefix if present
	mut c_name := node.name
	if c_name.starts_with('C.') {
		c_name = c_name[2..]
	}
	if c_name == '' {
		return
	}
	// Skip C stdlib functions — they're already declared by system headers.
	if c_name in c_stdlib_fns || is_c_runtime_function(c_name) {
		return
	}
	// Only emit C extern forward declarations for user/third-party modules
	// (from .vmodules/ or the project directory). Skip all vlib modules since
	// they wrap system functions already declared by system headers.
	if !g.cur_file_name.contains('.vmodules/') && g.cur_module != 'main' {
		return
	}
	// Skip already-emitted declarations
	decl_key := 'c_extern_${c_name}'
	if decl_key in g.emitted_types {
		return
	}
	g.emitted_types[decl_key] = true

	// Return type
	mut ret := if node.typ.return_type !is ast.EmptyExpr {
		g.expr_type_to_c(node.typ.return_type)
	} else {
		'void'
	}
	ret = normalize_signature_type_name(ret, 'void')
	c_ret := g.c_fn_return_type_from_v(ret)

	g.sb.write_string(c_ret)
	g.sb.write_string(' ')
	g.sb.write_string(c_name)
	g.sb.write_string('(')

	mut first := true
	for param in node.typ.params {
		if !first {
			g.sb.write_string(', ')
		}
		first = false
		mut t := normalize_signature_type_name(g.expr_type_to_c(param.typ), 'int')
		if param.is_mut && !t.ends_with('*') {
			t = t + '*'
		}
		g.sb.write_string(t)
		if param.name != '' {
			pname := if param.name == 'array' { '_v_array' } else { param.name }
			g.sb.write_string(' ')
			g.sb.write_string(pname)
		}
	}
	if first {
		g.sb.write_string('void')
	}
	g.sb.writeln(');')
}

// gen_fn_head_live generates the function signature with `impl_live_` prefix for @[live]
// functions, and records the function info for generating wrappers and dlsym binding.
fn (mut g Gen) gen_fn_head_live(node ast.FnDecl, fn_name string) {
	g.gen_fn_head_live_ptr(&node, fn_name)
}

// gen_fn_head_live_ptr mirrors gen_fn_head_live without copying ast.FnDecl in pass 5.
fn (mut g Gen) gen_fn_head_live_ptr(node &ast.FnDecl, fn_name string) {
	sig_param_types := g.fn_param_types[fn_name] or { []string{} }

	mut c_ret := g.cur_fn_c_ret_type
	is_void := c_ret == 'void'

	// Build parameter list string and arg forwarding string
	mut params_parts := []string{}
	mut args_parts := []string{}
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
		params_parts << '${receiver_type} ${node.receiver.name}'
		args_parts << node.receiver.name
		sig_idx++
	}

	for param in node.typ.params {
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
		params_parts << '${t} ${param.name}'
		args_parts << param.name
		sig_idx++
	}

	params_str := params_parts.join(', ')
	args_str := args_parts.join(', ')

	// Record live function info for later wrapper generation
	g.live_fns << LiveFnInfo{
		c_name:   fn_name
		ret_type: c_ret
		params:   params_str
		args:     args_str
		is_void:  is_void
	}
	if g.cur_file_name.len > 0 {
		g.live_source_file = os.real_path(g.cur_file_name)
	}

	// Write function signature: ret_type impl_live_FUNC(params)
	// Mark with visibility("default") so it's exported from shared libraries
	// even when -fvisibility=hidden is used.
	g.sb.write_string('__attribute__((visibility("default"))) ')
	g.sb.write_string(c_ret)
	g.sb.write_string(' impl_live_')
	g.sb.write_string(fn_name)
	g.sb.write_string('(')
	g.sb.write_string(params_str)
	g.sb.write_string(')')
}

fn (mut g Gen) get_fn_name(node ast.FnDecl) string {
	// Check for @[export: 'name'] attribute — use the export name as the C symbol.
	for attr in node.attributes {
		if attr.name == 'export' {
			if attr.value is ast.StringLiteral {
				export_name := attr.value.value.trim('\'"')
				if export_name.len > 0 {
					return export_name
				}
			}
		}
	}
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
		// Strip pointer suffix and 'struct ' prefix for method naming.
		// C struct types like 'struct sg_pipeline' must become 'sg_pipeline'
		// in function names to produce valid C identifiers.
		mut base_type := if receiver_type.ends_with('*') {
			receiver_type[..receiver_type.len - 1]
		} else {
			receiver_type
		}
		if base_type.starts_with('struct ') {
			base_type = base_type[7..]
		}
		if g.cur_module != 'builtin' && base_type in primitive_types {
			// Non-builtin primitive receiver methods are typically accidental generic
			// instantiations (e.g. int__multiply) that produce invalid scalar field access.
			return ''
		}
		return base_type + '__' + name
	}
	// Static methods: Type.method() -> Type__method
	if node.is_static {
		receiver_type := g.expr_type_to_c(node.receiver.typ)
		mut base_type := if receiver_type.ends_with('*') {
			receiver_type[..receiver_type.len - 1]
		} else {
			receiver_type
		}
		if base_type.starts_with('struct ') {
			base_type = base_type[7..]
		}
		return base_type + '__' + name
	}
	if g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin' {
		// Don't add module prefix if name already starts with it (e.g., generated
		// enum str functions placed back in their source module).
		if name.starts_with(g.cur_module + '__') {
			return name
		}
		return g.cur_module + '__' + name
	}
	// Rename builtin `panic` to avoid conflict with macOS mach/mach.h's
	// `extern void panic(const char *, ...)`.
	if name == 'panic' {
		return 'v_panic'
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
	match arg {
		ast.ModifierExpr {
			return g.expr_is_pointer(arg.expr)
		}
		ast.ParenExpr {
			return g.expr_is_pointer(arg.expr)
		}
		ast.CastExpr {
			// The result type of a cast is determined by the target type, not the source.
			// e.g. u8(ptr) produces u8, not a pointer, even though ptr is a pointer.
			target_type := g.expr_type_to_c(arg.typ)
			if target_type.ends_with('*') {
				return true
			}
			// For C typedef struct casts like C.log__Logger(ptr), if the source is a pointer,
			// the result is also a pointer (type pun cast). The cast doesn't unwrap the pointer.
			if target_type.contains('__') && g.expr_is_pointer(arg.expr) {
				return true
			}
			return false
		}
		ast.CallOrCastExpr {
			// When the LHS is a type (not a function), this is a type cast.
			// In V, C.Type(ptr) where ptr is a pointer means "treat as Type*",
			// so the result is still a pointer.
			if g.call_or_cast_lhs_is_type(arg.lhs) {
				return g.expr_is_pointer(arg.expr)
			}
		}
		ast.Ident {
			if arg.name == 'nil' {
				return true
			}
			// Prefer function-scope type information for identifiers.
			// Environment lookups can over-approximate loop variables as pointers.
			if local_type := g.get_local_var_c_type(arg.name) {
				if arg.name in g.cur_fn_mut_params {
					return true
				}
				return local_type.ends_with('*')
					|| local_type in ['voidptr', 'charptr', 'byteptr', 'chan']
			}
			if arg.name in g.cur_fn_mut_params {
				return true
			}
		}
		ast.PrefixExpr {
			return arg.op == .amp
		}
		ast.SelectorExpr {
			if arg.rhs.name == 'data' {
				lhs_type := g.get_expr_type(arg.lhs)
				if lhs_type == 'array' || lhs_type.starts_with('Array_') || lhs_type == 'map'
					|| lhs_type.starts_with('Map_') || lhs_type == 'string'
					|| lhs_type.starts_with('strings__Builder') {
					return true
				}
			}
			field_type := g.selector_field_type(arg)
			if field_type.ends_with('*')
				|| field_type in ['void*', 'char*', 'u8*', 'byteptr', 'charptr', 'voidptr'] {
				return true
			}
		}
		ast.UnsafeExpr {
			if arg.stmts.len > 0 {
				last := arg.stmts[arg.stmts.len - 1]
				if last is ast.ExprStmt {
					return g.expr_produces_pointer(last.expr)
				}
			}
		}
		else {}
	}
	if raw_type := g.get_raw_type(arg) {
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
	return g.get_expr_type(arg).ends_with('*')
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

// is_simple_addressable returns true if the expression is cheap to re-evaluate
// (e.g., an identifier or field access), unlike a function call which has side effects
// and generates significant code.
fn (g &Gen) is_simple_addressable(expr ast.Expr) bool {
	if expr is ast.Ident {
		return true
	}
	if expr is ast.SelectorExpr {
		return g.is_simple_addressable(expr.lhs)
	}
	if expr is ast.ParenExpr {
		return g.is_simple_addressable(expr.expr)
	}
	return false
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
					return g.fn_return_type_to_c(rt)
				}
				return 'void'
			}
			types.Alias {
				if raw_type.base_type is types.FnType {
					if rt := raw_type.base_type.get_return_type() {
						return g.fn_return_type_to_c(rt)
					}
					return 'void'
				}
			}
			types.Pointer {
				if raw_type.base_type is types.FnType {
					if rt := raw_type.base_type.get_return_type() {
						return g.fn_return_type_to_c(rt)
					}
					return 'void'
				}
				if raw_type.base_type is types.Alias && raw_type.base_type.base_type is types.FnType {
					if rt := raw_type.base_type.base_type.get_return_type() {
						return g.fn_return_type_to_c(rt)
					}
					return 'void'
				}
			}
			else {}
		}
	}
	return ''
}

// fn_return_type_to_c converts a function return type to its C representation,
// handling Tuple types by registering the appropriate tuple alias.
fn (mut g Gen) fn_return_type_to_c(t types.Type) string {
	if t is types.Tuple {
		elem_types := t.get_types()
		mut c_types := []string{cap: elem_types.len}
		for et in elem_types {
			c_types << g.types_type_to_c(et)
		}
		return g.register_tuple_alias(c_types)
	}
	return g.types_type_to_c(t)
}

fn (mut g Gen) is_fn_pointer_expr(expr ast.Expr) bool {
	return g.fn_pointer_return_type(expr) != ''
}

fn extract_fn_type(raw_type types.Type) ?types.FnType {
	match raw_type {
		types.FnType {
			return raw_type
		}
		types.Alias {
			if raw_type.base_type is types.FnType {
				return raw_type.base_type as types.FnType
			}
		}
		types.Pointer {
			if raw_type.base_type is types.FnType {
				return raw_type.base_type as types.FnType
			}
			if raw_type.base_type is types.Alias && raw_type.base_type.base_type is types.FnType {
				return raw_type.base_type.base_type as types.FnType
			}
		}
		else {}
	}
	return none
}

// fn_pointer_param_is_ptr extracts parameter pointer-ness from a fn-pointer type.
// Returns an array of bools (true = param expects pointer, false = by value).
fn (mut g Gen) fn_pointer_param_is_ptr(expr ast.Expr) []bool {
	raw_type := g.get_raw_type(expr) or { return []bool{} }
	fn_type := extract_fn_type(raw_type) or { return []bool{} }
	param_types := fn_type.get_param_types()
	mut result := []bool{cap: param_types.len}
	for pt in param_types {
		c_name := g.types_type_to_c(pt)
		result << (c_name.ends_with('*') || pt is types.Pointer)
	}
	return result
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
	if fn_name in ['ui__EventMngr__add_receiver', 'ui__EventMngr__rm_receiver'] && idx == 1 {
		if g.gen_interface_cast('ui__Widget', base_arg) {
			return
		}
	}
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
			should_deref := g.should_auto_deref(base_arg)
				|| (base_arg is ast.Ident && base_arg.name in g.cur_fn_mut_params)
			if !want_ptr && got_ptr && should_deref {
				// Don't auto-deref when the param type is a pointer alias (e.g. Coordptr = Coord*)
				if param_types := g.fn_param_types[fn_name] {
					if idx < param_types.len {
						if param_types[idx].ends_with('ptr') {
							g.expr(base_arg)
							return
						}
						param_base := param_types[idx].trim_right('*')
						if g.is_interface_type(param_base) {
							mut arg_type := g.get_expr_type(base_arg).trim_space()
							if (arg_type == '' || arg_type == 'int') && base_arg is ast.Ident {
								arg_type = (g.get_local_var_c_type(base_arg.name) or { '' }).trim_space()
							}
							if arg_type == '' || arg_type == 'int' {
								if raw := g.get_raw_type(base_arg) {
									arg_type = g.types_type_to_c(raw).trim_space()
								}
							}
							if arg_type.trim_right('*') != param_base {
								if g.gen_interface_cast(param_base, base_arg) {
									return
								}
								g.expr(base_arg)
								return
							}
						}
					}
				}
				g.sb.write_string('(*')
				g.expr(base_arg)
				g.sb.write_string(')')
				return
			}
		}
	}
	if param_types := g.fn_param_types[fn_name] {
		if idx < param_types.len {
			param_type := param_types[idx]
			param_base := param_type.trim_right('*')
			if g.is_interface_type(param_base) {
				mut arg_type := g.get_expr_type(base_arg).trim_space()
				if (arg_type == '' || arg_type == 'int') && base_arg is ast.Ident {
					arg_type = (g.get_local_var_c_type(base_arg.name) or { '' }).trim_space()
				}
				mut raw_arg_type := ''
				if arg_type == '' || arg_type == 'int' {
					if raw := g.get_raw_type(base_arg) {
						raw_arg_type = g.types_type_to_c(raw).trim_space()
						arg_type = raw_arg_type
					}
				} else if raw := g.get_raw_type(base_arg) {
					raw_arg_type = g.types_type_to_c(raw).trim_space()
				}
				arg_base := arg_type.trim_right('*')
				raw_arg_base := raw_arg_type.trim_right('*')
				if arg_base == param_base {
					if raw_arg_base != '' && raw_arg_base != 'int' && raw_arg_base != param_base {
						if param_type.ends_with('*') {
							if g.gen_heap_interface_cast(param_base, base_arg) {
								return
							}
						} else {
							if g.gen_interface_cast(param_base, base_arg) {
								return
							}
						}
					}
					if param_type.ends_with('*') {
						if arg_type.ends_with('*') {
							g.expr(base_arg)
							return
						}
						if g.can_take_address(base_arg) {
							g.sb.write_string('&')
							g.expr(base_arg)
							return
						}
					} else if arg_type.ends_with('*') && g.should_auto_deref(base_arg) {
						g.sb.write_string('(*')
						g.expr(base_arg)
						g.sb.write_string(')')
						return
					}
				} else if arg_base != '' && arg_base != 'int' {
					if param_type.ends_with('*') {
						if g.gen_heap_interface_cast(param_base, base_arg) {
							return
						}
					} else {
						if g.gen_interface_cast(param_base, base_arg) {
							return
						}
					}
				}
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
				// But verify with get_expr_type: if it returns a known variant, the raw type
				// lookup hit the wrong scope entry (e.g. struct field vs. local variable).
				if raw := g.get_raw_type(base_arg) {
					raw_c := g.types_type_to_c(raw)
					if raw_c == param_type {
						expr_type := g.get_expr_type(base_arg)
						if expr_type == param_type || expr_type == '' || expr_type == 'int'
							|| expr_type !in variants {
							g.expr(base_arg)
							return
						}
						// expr_type is a known variant but raw_type says sum type:
						// raw_type lookup was wrong, fall through to wrapping
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
	if param_types := g.fn_param_types[fn_name] {
		if idx < param_types.len {
			param_type := param_types[idx]
			if param_type.starts_with('Array_fixed_') && base_arg is ast.ArrayInitExpr {
				g.sb.write_string('((${param_type})')
				g.expr(base_arg)
				g.sb.write_string(')')
				return
			}
			if param_type == 'Array_string' && g.get_expr_type(base_arg) == 'string' {
				g.sb.write_string('((array){ .data = &(string[1]){')
				g.expr(base_arg)
				g.sb.write_string('}, .offset = 0, .len = 1, .cap = 1, .flags = 0, .element_size = sizeof(string) })')
				return
			}
		}
	}
	// Detect bound method references: b.method_name passed as a fn pointer arg.
	// Generate a static trampoline function + __thread capture variable.
	// Only applies when the expected parameter is a function pointer type.
	if base_arg is ast.SelectorExpr {
		receiver_type := g.get_expr_type(base_arg.lhs).trim_right('*')
		if receiver_type != '' && base_arg.rhs.name != '' {
			// Check if the expected parameter type is a function pointer.
			// If not, this is a struct field access (e.g., s.str, w.id), not a method value.
			mut param_is_fn_ptr := false
			if param_types := g.fn_param_types[fn_name] {
				if idx < param_types.len {
					pt := param_types[idx]
					param_is_fn_ptr = pt.contains('(*)') || g.is_fn_pointer_alias_type(pt)
				}
			}
			method_c_name := '${receiver_type}__${base_arg.rhs.name}'
			if param_is_fn_ptr
				&& (method_c_name in g.fn_param_is_ptr || method_c_name in g.fn_return_types) {
				tramp_name := '__bound_method_${receiver_type}__${base_arg.rhs.name}'
				if tramp_name !in g.emitted_trampolines {
					g.emitted_trampolines[tramp_name] = true
					// Build trampoline function as a string and add to deferred defs
					ret_type := g.fn_return_types[method_c_name] or { 'void' }
					param_is_ptr := g.fn_param_is_ptr[method_c_name] or { []bool{} }
					param_types := g.fn_param_types[method_c_name] or { []string{} }
					recv_is_ptr := if param_is_ptr.len > 0 { param_is_ptr[0] } else { false }
					recv_var := '__bound_recv_${receiver_type}__${base_arg.rhs.name}'
					mut def := 'static __thread ${receiver_type}* ${recv_var};\n'
					def += 'static ${ret_type} ${tramp_name}('
					for pi in 1 .. param_types.len {
						if pi > 1 {
							def += ', '
						}
						def += '${param_types[pi]} _p${pi}'
					}
					def += ') {\n\t'
					if ret_type != 'void' {
						def += 'return '
					}
					if recv_is_ptr {
						def += '${method_c_name}(${recv_var}'
					} else {
						def += '${method_c_name}(*${recv_var}'
					}
					for pi in 1 .. param_types.len {
						def += ', _p${pi}'
					}
					def += ');\n}\n'
					g.trampoline_defs << def
				}
				// Set capture variable and pass trampoline
				recv_var := '__bound_recv_${receiver_type}__${base_arg.rhs.name}'
				g.sb.write_string('(${recv_var} = ')
				if g.expr_is_pointer(base_arg.lhs) {
					g.expr(base_arg.lhs)
				} else {
					g.sb.write_string('&')
					g.expr(base_arg.lhs)
				}
				g.sb.write_string(', ${tramp_name})')
				return
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
			if name !in g.fn_return_types && name !in g.fn_param_is_ptr {
				if alias_base := g.alias_base_c_type(base_type) {
					alias_name := '${alias_base}__${method_name}'
					if alias_name in g.fn_return_types || alias_name in g.fn_param_is_ptr {
						name = alias_name
					}
				}
			}
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

fn (mut g Gen) get_call_return_type(lhs ast.Expr, call_args []ast.Expr) ?string {
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
	mut c_name := g.resolve_call_name(lhs, call_args.len)
	if c_name != '' {
		if specialized_name := g.try_specialize_generic_call_name(c_name, call_args) {
			c_name = specialized_name
		}
		if ret := g.fn_return_types[c_name] {
			return ret
		}
	}
	if lhs is ast.SelectorExpr {
		mut is_static_selector := false
		if lhs.lhs is ast.Ident {
			is_static_selector = g.is_module_ident(lhs.lhs.name) || g.is_type_name(lhs.lhs.name)
		}
		if !is_static_selector {
			base_type := g.method_receiver_base_type(lhs.lhs)
			if embedded := g.resolve_method_on_embedded_receiver(base_type, lhs.rhs.name) {
				if ret := g.fn_return_types[embedded.method_c_name] {
					return ret
				}
			}
			// For interface vtable method calls (e.g., w.size() where w is Widget),
			// look up the method's return type from interface_methods.
			if g.is_interface_type(base_type) {
				if method := g.interface_method_by_name(base_type, lhs.rhs.name) {
					if method.ret_type != '' {
						return method.ret_type
					}
				}
			}
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

fn (g &Gen) resolve_method_on_concrete_type(receiver_type string, method_name string) ?string {
	clean_type := strip_pointer_type_name(unmangle_c_ptr_type(receiver_type))
	if clean_type == '' || method_name == '' {
		return none
	}
	sanitized := sanitize_fn_ident(method_name)
	mut candidates := []string{}
	candidates << '${clean_type}__${sanitized}'
	short_name := short_type_name(clean_type)
	if short_name != '' && short_name != clean_type {
		candidates << '${short_name}__${sanitized}'
	}
	if g.cur_module != '' && !clean_type.contains('__') {
		candidates << '${g.cur_module}__${clean_type}__${sanitized}'
	}
	for candidate in candidates {
		if candidate in g.fn_return_types || candidate in g.fn_param_is_ptr {
			return candidate
		}
	}
	return none
}

struct EmbeddedMethodResolution {
	owner         string
	method_c_name string
}

fn (mut g Gen) resolve_method_on_embedded_decl(info StructDeclInfo, method_name string) ?EmbeddedMethodResolution {
	saved_module := g.cur_module
	defer {
		g.cur_module = saved_module
	}
	g.cur_module = info.mod
	for emb in info.decl.embedded {
		embedded_c_type := g.expr_type_to_c(emb)
		owner := embedded_owner_field_name(embedded_c_type)
		if method_c_name := g.resolve_method_on_concrete_type(embedded_c_type, method_name) {
			return EmbeddedMethodResolution{
				owner:         owner
				method_c_name: method_c_name
			}
		}
		if embedded_info := g.find_struct_decl_info_by_c_name(embedded_c_type) {
			if nested := g.resolve_method_on_embedded_decl(embedded_info, method_name) {
				return EmbeddedMethodResolution{
					owner:         owner
					method_c_name: nested.method_c_name
				}
			}
			g.cur_module = info.mod
		}
	}
	return none
}

fn (mut g Gen) resolve_method_on_embedded_receiver(receiver_type string, method_name string) ?EmbeddedMethodResolution {
	clean_type := strip_pointer_type_name(unmangle_c_ptr_type(receiver_type))
	if clean_type == '' || method_name == '' {
		return none
	}
	mut lookup_name := clean_type
	mut struct_type := g.lookup_struct_type_by_c_name(lookup_name)
	if struct_type.fields.len == 0 && struct_type.embedded.len == 0 {
		if alias_base := g.alias_base_c_type(clean_type) {
			lookup_name = alias_base
			struct_type = g.lookup_struct_type_by_c_name(alias_base)
		}
	}
	if struct_type.fields.len == 0 && struct_type.embedded.len == 0 {
		if decl_info := g.find_struct_decl_info_by_c_name(lookup_name) {
			return g.resolve_method_on_embedded_decl(decl_info, method_name)
		}
		return none
	}
	for embedded in struct_type.embedded {
		embedded_c_type := g.types_type_to_c(embedded)
		owner := embedded_owner_field_name(embedded_c_type)
		if method_c_name := g.resolve_method_on_concrete_type(embedded_c_type, method_name) {
			return EmbeddedMethodResolution{
				owner:         owner
				method_c_name: method_c_name
			}
		}
		if nested := g.resolve_method_on_embedded_receiver(embedded_c_type, method_name) {
			return EmbeddedMethodResolution{
				owner:         owner
				method_c_name: nested.method_c_name
			}
		}
	}
	if decl_info := g.find_struct_decl_info_by_c_name(lookup_name) {
		return g.resolve_method_on_embedded_decl(decl_info, method_name)
	}
	return none
}

fn (mut g Gen) gen_embedded_receiver_arg(receiver ast.Expr, owner string, callee_name string) {
	mut expect_ptr := false
	if ptr_params := g.fn_param_is_ptr[callee_name] {
		if ptr_params.len > 0 && ptr_params[0] {
			expect_ptr = true
		}
	}
	if expect_ptr {
		g.sb.write_string('&')
	}
	mut use_ptr := g.expr_is_pointer(receiver)
	if !use_ptr {
		receiver_type := g.get_expr_type(receiver)
		use_ptr = receiver_type.ends_with('*')
	}
	g.expr(receiver)
	sep := if use_ptr { '->' } else { '.' }
	g.sb.write_string('${sep}${escape_c_keyword(owner)}')
}

fn (mut g Gen) is_iface_object_arg_for_receiver(arg ast.Expr, receiver ast.Expr) bool {
	if arg !is ast.SelectorExpr {
		return false
	}
	sel := arg as ast.SelectorExpr
	if sel.rhs.name != '_object' {
		return false
	}
	receiver_src := g.expr_to_string(receiver)
	sel_lhs_src := g.expr_to_string(sel.lhs)
	return receiver_src != '' && receiver_src == sel_lhs_src
}

fn (mut g Gen) interface_call_args_without_object(receiver ast.Expr, args []ast.Expr) []ast.Expr {
	if args.len > 0 && g.is_iface_object_arg_for_receiver(args[0], receiver) {
		mut out := []ast.Expr{cap: args.len - 1}
		for i in 1 .. args.len {
			out << args[i]
		}
		return out
	}
	return args
}

fn (mut g Gen) gen_interface_call_arg(expected_type string, arg ast.Expr) {
	base_arg := if arg is ast.ModifierExpr { arg.expr } else { arg }
	mut arg_type := g.get_expr_type(base_arg).trim_space()

	if (arg_type == '' || arg_type == 'int') && base_arg is ast.Ident {
		arg_type = (g.get_local_var_c_type(base_arg.name) or { '' }).trim_space()
	}
	// For Ident args, check if the local var C type includes pointer info (e.g. from mut params)
	// that get_expr_type doesn't provide
	if base_arg is ast.Ident && !arg_type.ends_with('*') {
		if local_c := g.get_local_var_c_type(base_arg.name) {
			if local_c.ends_with('*') {
				arg_type = local_c
			}
		}
	}
	is_pointer_arg := g.expr_is_pointer(base_arg) || arg_type.ends_with('*')
	// Check interface conversion FIRST before auto-deref/address-of, since
	// derefing a Stack* to Stack doesn't help when Layout is expected.
	expected_base := expected_type.trim_right('*')
	arg_base := arg_type.trim_right('*')
	if g.is_interface_type(expected_base) && arg_base != '' && arg_base != 'int'
		&& arg_base != expected_base && !g.is_interface_type(arg_base)
		&& !arg_base.starts_with('Array_') && arg_base != 'array' && !arg_base.starts_with('Map_')
		&& arg_base != 'map' {
		if expected_type.ends_with('*') {
			if g.gen_heap_interface_cast(expected_base, base_arg) {
				return
			}
		} else {
			if g.gen_interface_cast(expected_base, base_arg) {
				return
			}
		}
	}
	if !expected_type.ends_with('*') && expected_type.starts_with('Array_')
		&& arg_type.ends_with('*') {
		g.sb.write_string('(*')
		g.expr(base_arg)
		g.sb.write_string(')')
		return
	}
	if !expected_type.ends_with('*') && is_pointer_arg && g.should_auto_deref(base_arg) {
		g.sb.write_string('(*')
		g.expr(base_arg)
		g.sb.write_string(')')
		return
	}
	if expected_type.ends_with('*') && !is_pointer_arg {
		g.gen_addr_of_expr(base_arg, expected_type.trim_right('*'))
		return
	}
	g.expr(arg)
}

fn (mut g Gen) smartcast_c_type_from_scope(expr ast.Expr) ?string {
	if mut fn_scope := g.ensure_cur_fn_scope() {
		expr_name := expr.name()
		if expr_name != '' {
			if cast_type := fn_scope.lookup_field_smartcast(expr_name) {
				c_type := g.types_type_to_c(cast_type)
				if c_type != '' && c_type != 'int' {
					return c_type
				}
			}
		}
		if expr is ast.SelectorExpr {
			lhs_name := expr.lhs.name()
			if lhs_name != '' {
				if cast_type := fn_scope.lookup_field_smartcast(lhs_name) {
					if field_type := selector_struct_field_type_from_type(cast_type, expr.rhs.name) {
						c_type := g.types_type_to_c(field_type)
						if c_type != '' && c_type != 'int' {
							return c_type
						}
					}
				}
			}
		}
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
	for i in 0 .. args.len {
		_ = g.get_expr_type(args[i])
	}
	if lhs is ast.SelectorExpr {
		mut receiver_type := g.get_expr_type(lhs.lhs)
		if (receiver_type == '' || receiver_type == 'int') && lhs.lhs is ast.SelectorExpr {
			receiver_type = g.selector_field_type(lhs.lhs)
		}
		base_receiver := receiver_type.trim_right('*')
		if g.is_interface_type(base_receiver) && lhs.rhs.name != '' {
			method_name := lhs.rhs.name
			if g.is_interface_vtable_method(base_receiver, method_name) {
				sep := if receiver_type.ends_with('*') { '->' } else { '.' }
				mut method_info := InterfaceMethodInfo{}
				mut has_method_info := false
				mut expected_total_params := 1
				if method := g.interface_method_by_name(base_receiver, method_name) {
					method_info = method
					has_method_info = true
					expected_total_params = method.param_types.len + 1
				}
				g.expr(lhs.lhs)
				g.sb.write_string('${sep}${method_name}(')
				mut emitted_args := 0
				mut param_offset := 0
				if args.len < expected_total_params {
					g.expr(lhs.lhs)
					g.sb.write_string('${sep}_object')
					emitted_args++
				} else {
					// Object is already in args — param_types start at args[1]
					param_offset = 1
				}
				for i in 0 .. args.len {
					arg := args[i]
					if emitted_args > 0 {
						g.sb.write_string(', ')
					}
					pi := i - param_offset
					if has_method_info && pi >= 0 && pi < method_info.param_types.len {
						g.gen_interface_call_arg(method_info.param_types[pi], arg)
					} else {
						g.expr(arg)
					}
					emitted_args++
				}
				g.sb.write_string(')')
				return
			}
			// Default interface method (not in vtable, has a body on the interface).
			// Call the default implementation directly: InterfaceName__method(receiver, args...)
			default_fn := '${base_receiver}__${method_name}'
			if default_fn in g.fn_param_is_ptr || default_fn in g.fn_return_types {
				call_args := g.interface_call_args_without_object(lhs.lhs, args)
				g.sb.write_string('${default_fn}(')
				ptr_params := g.fn_param_is_ptr[default_fn] or { []bool{} }
				recv_wants_ptr := ptr_params.len > 0 && ptr_params[0]
				recv_is_ptr := receiver_type.ends_with('*')
				if recv_wants_ptr && !recv_is_ptr {
					g.sb.write_string('&')
				} else if !recv_wants_ptr && recv_is_ptr {
					g.sb.write_string('*')
				}
				g.expr(lhs.lhs)
				for i in 0 .. call_args.len {
					g.sb.write_string(', ')
					if param_types := g.fn_param_types[default_fn] {
						if i + 1 < param_types.len {
							g.gen_interface_call_arg(param_types[i + 1], call_args[i])
						} else {
							g.expr(call_args[i])
						}
					} else {
						g.expr(call_args[i])
					}
				}
				g.sb.write_string(')')
				return
			}
			// Interface-to-interface: method not on current interface but exists on
			// another interface (e.g., Widget variable inside `if w is Layout`).
			// Find a concrete type that has this method and call it directly via _object.
			if method_info := g.find_method_on_any_interface(method_name) {
				suffix := '__${method_name}'
				// Find a concrete function that ends with __method_name
				mut concrete_fn := ''
				for fn_name, _ in g.fn_return_types {
					if fn_name.ends_with(suffix) && fn_name != default_fn {
						// Verify it has the right number of params
						if fn_params := g.fn_param_types[fn_name] {
							if fn_params.len == method_info.param_types.len + 1 {
								concrete_fn = fn_name
								break
							}
						}
					}
				}
				if concrete_fn != '' {
					// Generate: ((ret_type (*)(void*, ...))(concrete_fn))(w->_object, args...)
					// Actually, emit a cast call through the method signature
					sep := if receiver_type.ends_with('*') { '->' } else { '.' }
					g.sb.write_string('((${method_info.cast_signature})(${concrete_fn}))(')
					g.expr(lhs.lhs)
					g.sb.write_string('${sep}_object')
					call_args2 := g.interface_call_args_without_object(lhs.lhs, args)
					for i in 0 .. call_args2.len {
						g.sb.write_string(', ')
						g.expr(call_args2[i])
					}
					g.sb.write_string(')')
					return
				}
			}
		}
		if lhs.lhs is ast.Ident {
			lhs_ident := lhs.lhs as ast.Ident
			if lhs_ident.name == 'C' && lhs.rhs.name in ['FD_ZERO', 'FD_SET', 'FD_ISSET'] {
				g.sb.write_string('${lhs.rhs.name}(')
				for i in 0 .. args.len {
					arg := args[i]
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
	if lhs is ast.SelectorExpr {
		if lhs.rhs.name in ['hash_fn', 'key_eq_fn', 'clone_fn', 'free_fn'] {
			base_type := g.method_receiver_base_type(lhs.lhs)
			if base_type == 'map' || base_type.starts_with('Map_') {
				g.expr(lhs)
				g.sb.write_string('(')
				for i in 0 .. args.len {
					arg := args[i]
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
				for i in 0 .. args.len {
					arg := args[i]
					if i > 0 {
						g.sb.write_string(', ')
					}
					g.expr(arg)
				}
				g.sb.write_string(')')
				return
			}
			// Generic function call: arrays.uniq(res) where arrays__uniq is generic.
			// Try specializing the name (e.g. arrays__uniq -> arrays__uniq_string).
			if specialized := g.try_specialize_generic_call_name(alt, args) {
				g.sb.write_string('${specialized}(')
				for i in 0 .. args.len {
					arg := args[i]
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
			mut receiver_type := g.get_expr_type(lhs.lhs)
			if (receiver_type == '' || receiver_type == 'int') && lhs.lhs is ast.SelectorExpr {
				receiver_type = g.selector_field_type(lhs.lhs)
			}
			mut base_receiver := receiver_type.trim_right('*')
			mut is_iface_receiver := g.is_interface_type(base_receiver)
			if !is_iface_receiver {
				if raw := g.get_raw_type(lhs.lhs) {
					raw_name := g.types_type_to_c(raw).trim_right('*')
					if raw_name != '' && g.is_interface_type(raw_name) {
						base_receiver = raw_name
						if receiver_type == '' || receiver_type == 'int' {
							receiver_type = raw_name
						}
						is_iface_receiver = true
					}
				}
			}
			if !is_iface_receiver && lhs.lhs is ast.Ident {
				if local_type := g.get_local_var_c_type(lhs.lhs.name) {
					local_base := local_type.trim_right('*')
					if local_base != '' && g.is_interface_type(local_base) {
						base_receiver = local_base
						receiver_type = local_type
						is_iface_receiver = true
					}
				}
			}
			// Check global variables for interface type (e.g., __global default_rng &PRNG)
			if !is_iface_receiver && lhs.lhs is ast.Ident && g.env != unsafe { nil } {
				ident_name := lhs.lhs.name
				// Strip module prefix for scope lookup (e.g., 'rand__default_rng' → 'default_rng')
				short_name := if ident_name.contains('__') {
					ident_name.all_after_last('__')
				} else {
					ident_name
				}
				// Try looking up in the declaring module's scope
				global_mod := g.global_var_modules[short_name] or { '' }
				for mod_name in [global_mod, g.cur_module, 'builtin'] {
					if mod_name == '' {
						continue
					}
					if scope := g.env.get_scope(mod_name) {
						if obj := scope.lookup_parent(short_name, 0) {
							global_type := g.types_type_to_c(obj.typ())
							global_base := global_type.trim_right('*')
							if global_base != '' && g.is_interface_type(global_base) {
								base_receiver = global_base
								receiver_type = global_type
								is_iface_receiver = true
								break
							}
						}
					}
				}
			}
			if is_iface_receiver {
				sep := if receiver_type.ends_with('*') { '->' } else { '.' }
				method_name := lhs.rhs.name
				if g.is_interface_vtable_method(base_receiver, method_name) {
					mut cast_sig := '${c_ret} (*)(void*)'
					mut expected_total_params := 1
					if method := g.interface_method_by_name(base_receiver, method_name) {
						cast_sig = method.cast_signature
						expected_total_params = method.param_types.len + 1
					}
					g.sb.write_string('((${cast_sig})')
					g.expr(lhs)
					g.sb.write_string(')(')
					// If transformed code already passes all expected parameters
					// (including the receiver object), do not prepend `_object` again.
					mut prepend_object := args.len < expected_total_params
					mut emitted_args := 0
					if prepend_object {
						g.expr(lhs.lhs)
						g.sb.write_string('${sep}_object')
						emitted_args++
					}
					// When prepend_object is false, args[0] is _object (already provided
					// by the transformer), so param_types indices start at args[1].
					param_offset := if prepend_object { 0 } else { 1 }
					for i in 0 .. args.len {
						arg := args[i]
						if emitted_args > 0 {
							g.sb.write_string(', ')
						}
						param_idx := i - param_offset
						if method := g.interface_method_by_name(base_receiver, method_name) {
							if param_idx >= 0 && param_idx < method.param_types.len {
								g.gen_interface_call_arg(method.param_types[param_idx],
									arg)
							} else {
								g.expr(arg)
							}
						} else {
							g.expr(arg)
						}
						emitted_args++
					}
					g.sb.write_string(')')
					return
				}
				// Default interface method: not in vtable, has a body on the interface itself.
				// Call the default implementation directly: InterfaceName__method(receiver, args...)
				default_method_name := '${base_receiver}__${method_name}'
				if default_method_name in g.fn_param_is_ptr
					|| default_method_name in g.fn_return_types {
					call_args := g.interface_call_args_without_object(lhs.lhs, args)
					g.sb.write_string('${default_method_name}(')
					// Pass receiver (interface pointer or value) as first arg
					ptr_params := g.fn_param_is_ptr[default_method_name] or { []bool{} }
					receiver_as_ptr := ptr_params.len > 0 && ptr_params[0]
					if receiver_as_ptr {
						if !receiver_type.ends_with('*') {
							g.sb.write_string('&')
						}
						g.expr(lhs.lhs)
					} else {
						if receiver_type.ends_with('*') {
							g.sb.write_string('*')
						}
						g.expr(lhs.lhs)
					}
					for i, arg in call_args {
						g.sb.write_string(', ')
						if param_types := g.fn_param_types[default_method_name] {
							if i + 1 < param_types.len {
								g.gen_interface_call_arg(param_types[i + 1], arg)
							} else {
								g.expr(arg)
							}
						} else {
							g.expr(arg)
						}
					}
					g.sb.write_string(')')
					return
				}
				mut narrowed_receiver := g.get_expr_type_from_env(lhs.lhs) or { '' }
				if narrowed_receiver == '' || narrowed_receiver == 'int' {
					narrowed_receiver = g.get_env_c_type(lhs.lhs) or { narrowed_receiver }
				}
				if narrowed_receiver == '' || narrowed_receiver == 'int' {
					narrowed_receiver = g.smartcast_c_type_from_scope(lhs.lhs) or {
						narrowed_receiver
					}
				}
				narrowed_receiver = strip_pointer_type_name(unmangle_c_ptr_type(narrowed_receiver))
				if narrowed_receiver != '' && narrowed_receiver != 'int'
					&& !g.is_interface_type(narrowed_receiver) {
					if concrete_method := g.resolve_method_on_concrete_type(narrowed_receiver,
						method_name)
					{
						call_args := g.interface_call_args_without_object(lhs.lhs, args)
						g.sb.write_string('${concrete_method}(')
						ptr_params := g.fn_param_is_ptr[concrete_method] or { []bool{} }
						receiver_as_ptr := ptr_params.len > 0 && ptr_params[0]
						if receiver_as_ptr {
							g.sb.write_string('((${narrowed_receiver}*)(')
							g.expr(lhs.lhs)
							g.sb.write_string('${sep}_object))')
						} else {
							g.sb.write_string('(*(${narrowed_receiver}*)(')
							g.expr(lhs.lhs)
							g.sb.write_string('${sep}_object))')
						}
						for i in 0 .. call_args.len {
							arg := call_args[i]
							g.sb.write_string(', ')
							g.gen_call_arg(concrete_method, i + 1, arg)
						}
						g.sb.write_string(')')
						return
					}
					// Try embedded struct method resolution
					if emb := g.resolve_method_on_embedded_receiver(narrowed_receiver,
						method_name)
					{
						call_args := g.interface_call_args_without_object(lhs.lhs, args)
						g.sb.write_string('${emb.method_c_name}(')
						ptr_params := g.fn_param_is_ptr[emb.method_c_name] or { []bool{} }
						receiver_as_ptr := ptr_params.len > 0 && ptr_params[0]
						if receiver_as_ptr {
							g.sb.write_string('&((${narrowed_receiver}*)(')
							g.expr(lhs.lhs)
							g.sb.write_string('${sep}_object))->${emb.owner}')
						} else {
							g.sb.write_string('((${narrowed_receiver}*)(')
							g.expr(lhs.lhs)
							g.sb.write_string('${sep}_object))->${emb.owner}')
						}
						for i in 0 .. call_args.len {
							arg := call_args[i]
							g.sb.write_string(', ')
							g.gen_call_arg(emb.method_c_name, i + 1, arg)
						}
						g.sb.write_string(')')
						return
					}
				}
			}
			// When receiver is a concrete (non-interface) type from smartcast,
			// resolve the method on the concrete type or its embedded structs.
			if !is_iface_receiver && base_receiver != '' && base_receiver != 'int'
				&& !g.is_interface_type(base_receiver) {
				method_name := lhs.rhs.name
				if concrete_method := g.resolve_method_on_concrete_type(base_receiver,
					method_name)
				{
					ptr_params := g.fn_param_is_ptr[concrete_method] or { []bool{} }
					receiver_as_ptr := ptr_params.len > 0 && ptr_params[0]
					g.sb.write_string('${concrete_method}(')
					if receiver_as_ptr {
						g.sb.write_string('&(')
						g.expr(lhs.lhs)
						g.sb.write_string(')')
					} else {
						g.expr(lhs.lhs)
					}
					for i in 0 .. args.len {
						g.sb.write_string(', ')
						g.gen_call_arg(concrete_method, i + 1, args[i])
					}
					g.sb.write_string(')')
					return
				}
				if emb := g.resolve_method_on_embedded_receiver(base_receiver, method_name) {
					ptr_params := g.fn_param_is_ptr[emb.method_c_name] or { []bool{} }
					receiver_as_ptr := ptr_params.len > 0 && ptr_params[0]
					g.sb.write_string('${emb.method_c_name}(')
					if receiver_as_ptr {
						g.sb.write_string('&(')
						g.expr(lhs.lhs)
						g.sb.write_string(').${emb.owner}')
					} else {
						g.sb.write_string('(')
						g.expr(lhs.lhs)
						g.sb.write_string(').${emb.owner}')
					}
					for i in 0 .. args.len {
						g.sb.write_string(', ')
						g.gen_call_arg(emb.method_c_name, i + 1, args[i])
					}
					g.sb.write_string(')')
					return
				}
			}
			g.sb.write_string('((${c_ret}(*)())')
			g.expr(lhs)
			g.sb.write_string(')(')
			for i in 0 .. args.len {
				arg := args[i]
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
	mut embedded_receiver_owner := ''
	call_args << args
	if lhs is ast.Ident {
		name = sanitize_fn_ident(lhs.name)
	} else if lhs is ast.SelectorExpr {
		if lhs.rhs.name in ['hash_fn', 'key_eq_fn', 'clone_fn', 'free_fn'] {
			base_type := g.method_receiver_base_type(lhs.lhs)
			if base_type == 'map' || base_type.starts_with('Map_') {
				g.expr(lhs)
				g.sb.write_string('(')
				for i in 0 .. args.len {
					arg := args[i]
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
			for i in 0 .. args.len {
				arg := args[i]
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
			method_name := sanitize_fn_ident(lhs.rhs.name)
			mut base_type := g.method_receiver_base_type(lhs.lhs)
			if base_type == '' {
				base_type = g.get_expr_type(lhs.lhs).trim_space().trim_right('*')
			}
			if (name == '' || (name !in g.fn_param_is_ptr && name !in g.fn_return_types))
				&& base_type != '' {
				if embedded := g.resolve_method_on_embedded_receiver(base_type, method_name) {
					name = embedded.method_c_name
					embedded_receiver_owner = embedded.owner
				}
			}
			if name == '' {
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
	if name == 'int__eq_epsilon'
		&& ('f32__eq_epsilon' in g.fn_param_is_ptr || 'f32__eq_epsilon' in g.fn_return_types) {
		name = 'f32__eq_epsilon'
	}
	if name == 'os__exit' {
		name = 'exit'
	}
	// chan type methods map to sync__Channel__ methods
	if name.starts_with('chan__') {
		name = 'sync__Channel__' + name['chan__'.len..]
	}
	// strings__Builder methods are emitted directly by cheaders.v;
	// do NOT fall back to array__ methods which have different signatures.
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
	if specialized_name := g.try_specialize_generic_call_name(name, call_args) {
		name = specialized_name
	}
	for i in 0 .. call_args.len {
		arg := call_args[i]
		if arg is ast.FieldInit {
			panic('bug in v2 compiler: FieldInit call args should have been lowered in v2.transformer (${g.cur_file_name}:${g.cur_fn_name} call=${name} arg_idx=${i} field=${arg.name})')
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
	if call_args.len == 1 && name.starts_with('cgltf_') {
		arg := call_args[0]
		if is_none_like_expr(arg) || (arg is ast.BasicLiteral && arg.value == '0') {
			g.sb.write_string('((${name}){0})')
			return
		}
	}
	if name == 'panic' && call_args.len == 1 {
		arg := call_args[0]
		arg_type := g.get_expr_type(arg)
		if arg_type == 'IError' || (arg is ast.Ident && arg.name == 'err') {
			g.sb.write_string('v_panic(IError__str(')
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
			for i in 0 .. call_args.len {
				arg := call_args[i]
				if i > 0 {
					g.sb.write_string(', ')
				}
				g.expr(arg)
			}
			g.sb.write_string(')')
			return
		}
		mut arg_types := []string{cap: call_args.len}
		for i in 0 .. call_args.len {
			arg := call_args[i]
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
		for i in 0 .. call_args.len {
			arg := call_args[i]
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
	// Rename builtin `panic` to `v_panic` to avoid macOS mach.h conflict.
	// Also catch module-qualified variants (e.g. bits__panic) which occur
	// when the call originates from a non-builtin module.
	if c_name == 'panic' || c_name.ends_with('__panic') {
		c_name = 'v_panic'
	}
	// Apply @[export] name mapping: V-qualified names → export C symbols.
	if export_name := g.export_fn_names[c_name] {
		c_name = export_name
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
	// For fn-pointer local variable calls, pre-compute param pointer info
	// so we can auto-deref mut params when the fn type expects by-value.
	mut fnptr_param_is_ptr := []bool{}
	if is_local_call && c_name !in g.fn_param_is_ptr {
		fnptr_param_is_ptr = g.fn_pointer_param_is_ptr(lhs)
	}
	for i in 0 .. total_args {
		if i > 0 {
			g.sb.write_string(', ')
		}
		if i < call_args.len {
			if i == 0 && embedded_receiver_owner != '' && lhs is ast.SelectorExpr {
				g.gen_embedded_receiver_arg(lhs.lhs, embedded_receiver_owner, c_name)
				continue
			}
			if c_name == 'signal' && i == 1 {
				g.sb.write_string('((void (*)(int))')
				g.expr(call_args[i])
				g.sb.write_string(')')
				continue
			}
			// For fn-pointer calls, auto-deref mut params when the fn type expects by-value
			if fnptr_param_is_ptr.len > 0 && i < fnptr_param_is_ptr.len && !fnptr_param_is_ptr[i] {
				arg := call_args[i]
				mut arg_ident_name := ''
				if arg is ast.ModifierExpr {
					if arg.expr is ast.Ident {
						arg_ident_name = arg.expr.name
					}
				} else if arg is ast.Ident {
					arg_ident_name = arg.name
				}
				if arg_ident_name != '' && arg_ident_name in g.cur_fn_mut_params {
					g.sb.write_string('(*')
					g.sb.write_string(arg_ident_name)
					g.sb.write_string(')')
					continue
				}
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

fn ensure_ptr_suffix(typ string) string {
	if typ.ends_with('*') {
		return typ
	}
	mut sb := strings.new_builder(typ.len + 1)
	sb.write_string(typ)
	sb.write_u8(`*`)
	return sb.str()
}

fn anon_fn_name(counter int) string {
	mut sb := strings.new_builder(20)
	sb.write_string('_anon_fn_')
	sb.write_string(counter.str())
	return sb.str()
}

fn fn_literal_c_param_name(idx int) string {
	mut sb := strings.new_builder(16)
	sb.write_string('_fnlit_p')
	sb.write_string(idx.str())
	return sb.str()
}

struct FnLiteralCaptureInfo {
	name         string
	c_type       string
	assign_expr  ast.Expr
	is_mut       bool
	storage_name string
}

fn (mut g Gen) fn_literal_capture_infos(anon_name string, captured_vars []ast.Expr) []FnLiteralCaptureInfo {
	mut infos := []FnLiteralCaptureInfo{cap: captured_vars.len}
	for i, captured in captured_vars {
		mut assign_expr := captured
		mut is_mut := false
		if captured is ast.ModifierExpr {
			assign_expr = captured.expr
			is_mut = true
		}
		if assign_expr !is ast.Ident {
			continue
		}
		name := (assign_expr as ast.Ident).name
		if name == '' {
			continue
		}
		mut c_type := g.local_var_c_type_for_expr(assign_expr) or { '' }
		if c_type == '' {
			c_type = g.get_expr_type(assign_expr)
		}
		if c_type == '' || c_type == 'void' {
			c_type = 'void*'
		}
		if is_mut && !c_type.ends_with('*') {
			c_type = ensure_ptr_suffix(c_type)
		}
		infos << FnLiteralCaptureInfo{
			name:         name
			c_type:       c_type
			assign_expr:  assign_expr
			is_mut:       is_mut
			storage_name: '${anon_name}_capture_${i}'
		}
	}
	return infos
}

fn (mut g Gen) gen_fn_literal(node ast.FnLiteral) {
	anon_name := anon_fn_name(g.tmp_counter)
	g.tmp_counter++
	capture_infos := g.fn_literal_capture_infos(anon_name, node.captured_vars)

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
	saved_returned_idents := g.cur_fn_returned_idents.clone()

	// Use new builder for anon fn
	g.sb = strings.new_builder(1024)
	g.indent = 0
	g.cur_fn_name = anon_name
	g.cur_fn_ret_type = ret_type
	g.cur_fn_scope = unsafe { nil }
	g.cur_fn_mut_params = map[string]bool{}
	g.runtime_local_types = map[string]string{}
	g.cur_fn_returned_idents = g.collect_returned_idents(node.stmts)

	// Generate function definition
	for capture in capture_infos {
		g.sb.writeln('static ${capture.c_type} ${capture.storage_name};')
	}
	g.sb.write_string('static ')
	g.sb.write_string(ret_type)
	g.sb.write_u8(` `)
	g.sb.write_string(anon_name)
	g.sb.write_u8(`(`)
	for i, param in node.typ.params {
		if i > 0 {
			g.sb.write_string(', ')
		}
		param_type := g.expr_type_to_c(param.typ)
		// Use the original parameter name so body references match.
		param_name := if param.name != '' {
			param.name
		} else {
			fn_literal_c_param_name(i)
		}
		param_c_type := if param.is_mut && !param_type.ends_with('*') {
			ensure_ptr_suffix(param_type)
		} else {
			param_type
		}
		g.sb.write_string(param_c_type)
		g.sb.write_u8(` `)
		g.sb.write_string(param_name)
		if param.is_mut {
			g.cur_fn_mut_params[param_name] = true
		}
		// Register param type so expr_is_pointer and auto-deref work correctly
		ptype := if param.is_mut && !param_type.ends_with('*') {
			ensure_ptr_suffix(param_type)
		} else {
			param_c_type
		}
		g.runtime_local_types[param_name] = ptype
	}
	if node.typ.params.len == 0 {
		g.sb.write_string('void')
	}
	g.sb.writeln(') {')
	g.indent = 1
	for capture in capture_infos {
		if capture.is_mut {
			g.cur_fn_mut_params[capture.name] = true
		}
		g.runtime_local_types[capture.name] = capture.c_type
		g.sb.writeln('\t${capture.c_type} ${capture.name} = ${capture.storage_name};')
	}
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
	g.cur_fn_returned_idents = saved_returned_idents.clone()

	// Emit function name at use site
	if capture_infos.len == 0 {
		g.sb.write_string(anon_name)
		return
	}
	g.sb.write_string('({ ')
	for capture in capture_infos {
		g.sb.write_string('${capture.storage_name} = ')
		g.expr(capture.assign_expr)
		g.sb.write_string('; ')
	}
	g.sb.write_string('${anon_name}; })')
}

fn (g &Gen) alias_base_c_type(type_name string) ?string {
	if type_name == '' {
		return none
	}
	if base_name := g.alias_base_types[type_name] {
		if base_name != '' && base_name != type_name {
			return base_name
		}
	}
	short_name := type_name.all_after_last('__')
	for file in g.files {
		mod_name := file.mod.replace('.', '_')
		for stmt in file.stmts {
			if stmt is ast.TypeDecl {
				candidate := if mod_name != '' && mod_name != 'main' && mod_name != 'builtin' {
					'${mod_name}__${stmt.name}'
				} else {
					stmt.name
				}
				if candidate != type_name && stmt.name != short_name {
					continue
				}
				base_name := stmt.base_type.name().replace('.', '__')
				if base_name != '' && base_name != type_name {
					return base_name
				}
			}
		}
	}
	if g.env == unsafe { nil } {
		return none
	}
	mut modules := []string{}
	if type_name.contains('__') {
		modules << type_name.all_before_last('__')
	}
	if g.cur_module != '' && g.cur_module !in modules {
		modules << g.cur_module
	}
	if 'builtin' !in modules {
		modules << 'builtin'
	}
	for mod_name in modules {
		if mod_name == '' {
			continue
		}
		if mut scope := g.env_scope(mod_name) {
			for candidate in [type_name, short_name] {
				if candidate == '' {
					continue
				}
				if obj := scope.lookup_parent(candidate, 0) {
					if obj is types.Type && obj is types.Alias {
						alias_obj := obj as types.Alias
						base_name := g.types_type_to_c(alias_obj.base_type)
						if base_name != '' && base_name != type_name {
							return base_name
						}
					}
				}
			}
		}
	}
	for file in g.files {
		mod_name := file.mod.replace('.', '_')
		if mod_name == '' || mod_name in modules {
			continue
		}
		if mut scope := g.env_scope(mod_name) {
			if obj := scope.lookup_parent(short_name, 0) {
				if obj is types.Type && obj is types.Alias {
					alias_obj := obj as types.Alias
					base_name := g.types_type_to_c(alias_obj.base_type)
					if base_name != '' && base_name != type_name {
						return base_name
					}
				}
			}
		}
	}
	return none
}

fn (g &Gen) get_str_fn_for_type(expr_type string) ?string {
	if expr_type == ''
		|| expr_type in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'f32', 'f64', 'bool', 'string', 'char', 'rune', 'voidptr', 'byteptr', 'charptr'] {
		return none
	}
	if base_type := g.alias_base_c_type(expr_type) {
		if base_str_fn := g.get_str_fn_for_type(base_type) {
			return base_str_fn
		}
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

fn (g &Gen) has_concrete_fn_signature(fn_name string) bool {
	if ret := g.fn_return_types[fn_name] {
		if ret != '' && ret != 'array' {
			return true
		}
	}
	if params := g.fn_param_types[fn_name] {
		for p in params {
			if p != 'array' && p != 'array*' {
				return true
			}
		}
	}
	return false
}

fn expr_has_generic_placeholder(e ast.Expr) bool {
	match e {
		ast.Ident {
			return is_generic_placeholder_type_name(e.name)
		}
		ast.PrefixExpr {
			return expr_has_generic_placeholder(e.expr)
		}
		ast.ModifierExpr {
			return expr_has_generic_placeholder(e.expr)
		}
		ast.SelectorExpr {
			return expr_has_generic_placeholder(e.lhs)
				|| is_generic_placeholder_type_name(e.rhs.name)
		}
		ast.Type {
			match e {
				ast.ArrayType {
					return expr_has_generic_placeholder(e.elem_type)
				}
				ast.ArrayFixedType {
					return expr_has_generic_placeholder(e.elem_type)
						|| expr_has_generic_placeholder(e.len)
				}
				ast.MapType {
					return expr_has_generic_placeholder(e.key_type)
						|| expr_has_generic_placeholder(e.value_type)
				}
				ast.OptionType {
					return expr_has_generic_placeholder(e.base_type)
				}
				ast.ResultType {
					return expr_has_generic_placeholder(e.base_type)
				}
				ast.FnType {
					for p in e.params {
						if expr_has_generic_placeholder(p.typ) {
							return true
						}
					}
					if e.return_type !is ast.EmptyExpr {
						return expr_has_generic_placeholder(e.return_type)
					}
				}
				else {}
			}
			return false
		}
		else {
			return false
		}
	}
}

fn fn_decl_has_generic_placeholders(node ast.FnDecl) bool {
	if node.receiver.typ !is ast.EmptyExpr && expr_has_generic_placeholder(node.receiver.typ) {
		return true
	}
	for p in node.typ.params {
		if expr_has_generic_placeholder(p.typ) {
			return true
		}
	}
	if node.typ.return_type !is ast.EmptyExpr && expr_has_generic_placeholder(node.typ.return_type) {
		return true
	}
	return false
}

fn module_prefix_from_fn_name(fn_name string) string {
	idx := fn_name.index('__') or { return '' }
	if idx <= 0 {
		return ''
	}
	return fn_name[..idx]
}

fn (mut g Gen) emit_extract_attr_specialization(fn_name string, suffix string, elem_c_type string) {
	spec_fn := '${fn_name}_${suffix}'
	mod_prefix := module_prefix_from_fn_name(fn_name)
	mut result_type := g.fn_return_types[fn_name] or { '' }
	if result_type == '' || !result_type.starts_with('_result_') {
		result_type = '_result_Array_' + mangle_alias_component(elem_c_type)
	}
	mut array_type := g.result_value_type(result_type)
	if array_type == '' || !array_type.starts_with('Array_') {
		array_type = 'Array_' + mangle_alias_component(elem_c_type)
	}
	g.register_alias_type(array_type)
	g.register_alias_type(result_type)
	accessor_fn := if mod_prefix != '' {
		'${mod_prefix}__accessor_data_ptr'
	} else {
		'accessor_data_ptr'
	}
	parsed_data_sym := if mod_prefix != '' {
		'${mod_prefix}__parsed_data'
	} else {
		'parsed_data'
	}
	g.sb.writeln('${result_type} ${spec_fn}(cgltf_primitive prim, string attr_name, int attr_type, int attr_index, int expected_comp, int expected_count) {')
	g.sb.writeln('\textern struct cgltf_data* ${parsed_data_sym};')
	g.sb.writeln('\tfor (int a = 0; a < prim.attributes_count; a++) {')
	g.sb.writeln('\t\tcgltf_attribute attr = prim.attributes[a];')
	g.sb.writeln('\t\tbool matches = false;')
	g.sb.writeln('\t\tif (attr.name != NULL) {')
	g.sb.writeln('\t\t\tif (string__eq(attr_name, tos3(attr.name))) {')
	g.sb.writeln('\t\t\t\tmatches = true;')
	g.sb.writeln('\t\t\t}')
	g.sb.writeln('\t\t} else {')
	g.sb.writeln('\t\t\tif (attr_type == attr.type && (attr_index < 0 || attr.index == attr_index)) {')
	g.sb.writeln('\t\t\t\tmatches = true;')
	g.sb.writeln('\t\t\t}')
	g.sb.writeln('\t\t}')
	g.sb.writeln('\t\tif (!matches) {')
	g.sb.writeln('\t\t\tcontinue;')
	g.sb.writeln('\t\t}')
	g.sb.writeln('\t\tcgltf_accessor* acc = attr.data;')
	g.sb.writeln('\t\tif (acc->component_type != expected_comp || acc->type != expected_count) {')
	g.sb.writeln('\t\t\tcontinue;')
	g.sb.writeln('\t\t}')
	g.sb.writeln('\t\tvoid* ptr = ${accessor_fn}(${parsed_data_sym}, acc);')
	g.sb.writeln('\t\tint count = ((int)(acc->count)) * expected_count;')
	g.sb.writeln('\t\t${array_type} data = __new_array_with_default_noscan(count, 0, sizeof(${elem_c_type}), NULL);')
	g.sb.writeln('\t\tmemcpy(data.data, ptr, count * ((int)sizeof(${elem_c_type})));')
	g.sb.writeln('\t\treturn ({ ${result_type} _res = (${result_type}){0}; ${array_type} _val = data; _result_ok(&_val, (_result*)&_res, sizeof(_val)); _res; });')
	g.sb.writeln('\t}')
	g.sb.writeln('\t${array_type} empty_data = __new_array_with_default_noscan(0, 0, sizeof(${elem_c_type}), NULL);')
	g.sb.writeln('\treturn ({ ${result_type} _res = (${result_type}){0}; ${array_type} _val = empty_data; _result_ok(&_val, (_result*)&_res, sizeof(_val)); _res; });')
	g.sb.writeln('}')
}

fn (mut g Gen) emit_sampling_job_decompress_impl(fn_name string) {
	mod_prefix := module_prefix_from_fn_name(fn_name)
	job_type := if mod_prefix != '' { '${mod_prefix}__SamplingJob' } else { 'SamplingJob' }
	ctrl_type := if mod_prefix != '' { '${mod_prefix}__KeyframesCtrl' } else { 'KeyframesCtrl' }
	cache_type := if mod_prefix != '' {
		'${mod_prefix}__SamplingJobContextCache'
	} else {
		'SamplingJobContextCache'
	}
	float3_key_type := if mod_prefix != '' { '${mod_prefix}__Float3Key' } else { 'Float3Key' }
	quat_key_type := if mod_prefix != '' { '${mod_prefix}__QuaternionKey' } else { 'QuaternionKey' }
	interp_float3_type := if mod_prefix != '' {
		'${mod_prefix}__InterpSoaFloat3'
	} else {
		'InterpSoaFloat3'
	}
	interp_quat_type := if mod_prefix != '' {
		'${mod_prefix}__InterpSoaQuaternion'
	} else {
		'InterpSoaQuaternion'
	}
	soa_float3_type := if mod_prefix != '' { '${mod_prefix}__SoaFloat3' } else { 'SoaFloat3' }
	soa_quat_type := if mod_prefix != '' { '${mod_prefix}__SoaQuaternion' } else { 'SoaQuaternion' }
	simd_float4_type := if mod_prefix != '' { '${mod_prefix}__SimdFloat4' } else { 'SimdFloat4' }
	keys_ratio_fn := if mod_prefix != '' { '${mod_prefix}__keys_ratio' } else { 'keys_ratio' }
	g.sb.writeln('${simd_float4_type} ${keys_ratio_fn}(Array_f32 timepoints, Array_u8 ratios, Array_fixed_u32_4 ats);')
	g.sb.writeln('void ${fn_name}(${job_type}* job, int num_soa_tracks, Array_f32 timepoints, ${ctrl_type} ctrl, array compressed, ${cache_type}* cache, array* decompressed, void* decompress_fn) {')
	g.sb.writeln('\t(void)job;')
	g.sb.writeln('\tint num_outdated_flags = (num_soa_tracks + 7) / 8;')
	g.sb.writeln('\tu32* entries_data = (u32*)cache->entries.data;')
	g.sb.writeln('\tu16* previouses_data = (u16*)ctrl.previouses.data;')
	g.sb.writeln('\tfor (int j = 0; j < num_outdated_flags; j++) {')
	g.sb.writeln('\t\tu8 outdated = ((u8*)cache->outdated.data)[j];')
	g.sb.writeln('\t\t((u8*)cache->outdated.data)[j] = 0;')
	g.sb.writeln('\t\tfor (int i = j * 8; outdated != 0; i++) {')
	g.sb.writeln('\t\t\tif ((outdated & 1) == 0) {')
	g.sb.writeln('\t\t\t\toutdated >>= 1;')
	g.sb.writeln('\t\t\t\tcontinue;')
	g.sb.writeln('\t\t\t}')
	g.sb.writeln('\t\t\tif (i >= decompressed->len) {')
	g.sb.writeln('\t\t\t\toutdated >>= 1;')
	g.sb.writeln('\t\t\t\tcontinue;')
	g.sb.writeln('\t\t\t}')
	g.sb.writeln('\t\t\tArray_fixed_u32_4 rights = (Array_fixed_u32_4){')
	g.sb.writeln('\t\t\t\tentries_data[(i * 4) + 0],')
	g.sb.writeln('\t\t\t\tentries_data[(i * 4) + 1],')
	g.sb.writeln('\t\t\t\tentries_data[(i * 4) + 2],')
	g.sb.writeln('\t\t\t\tentries_data[(i * 4) + 3],')
	g.sb.writeln('\t\t\t};')
	g.sb.writeln('\t\t\tArray_fixed_u32_4 lefts = (Array_fixed_u32_4){')
	g.sb.writeln('\t\t\t\trights[0] - previouses_data[rights[0]],')
	g.sb.writeln('\t\t\t\trights[1] - previouses_data[rights[1]],')
	g.sb.writeln('\t\t\t\trights[2] - previouses_data[rights[2]],')
	g.sb.writeln('\t\t\t\trights[3] - previouses_data[rights[3]],')
	g.sb.writeln('\t\t\t};')
	g.sb.writeln('\t\t\tif (compressed.element_size == sizeof(${float3_key_type}) && decompressed->element_size == sizeof(${interp_float3_type})) {')
	g.sb.writeln('\t\t\t\t${float3_key_type}* compressed_data = (${float3_key_type}*)compressed.data;')
	g.sb.writeln('\t\t\t\t${interp_float3_type}* decompressed_data = (${interp_float3_type}*)decompressed->data;')
	g.sb.writeln('\t\t\t\t${float3_key_type} k00 = compressed_data[lefts[0]];')
	g.sb.writeln('\t\t\t\t${float3_key_type} k10 = compressed_data[lefts[1]];')
	g.sb.writeln('\t\t\t\t${float3_key_type} k20 = compressed_data[lefts[2]];')
	g.sb.writeln('\t\t\t\t${float3_key_type} k30 = compressed_data[lefts[3]];')
	g.sb.writeln('\t\t\t\tdecompressed_data[i].ratio[0] = ${keys_ratio_fn}(timepoints, ctrl.ratios, lefts);')
	g.sb.writeln('\t\t\t\t((void (*)(${float3_key_type}, ${float3_key_type}, ${float3_key_type}, ${float3_key_type}, ${soa_float3_type}*))(decompress_fn))(k00, k10, k20, k30, &decompressed_data[i].value[0]);')
	g.sb.writeln('\t\t\t\t${float3_key_type} k01 = compressed_data[rights[0]];')
	g.sb.writeln('\t\t\t\t${float3_key_type} k11 = compressed_data[rights[1]];')
	g.sb.writeln('\t\t\t\t${float3_key_type} k21 = compressed_data[rights[2]];')
	g.sb.writeln('\t\t\t\t${float3_key_type} k31 = compressed_data[rights[3]];')
	g.sb.writeln('\t\t\t\tArray_fixed_u32_4 rights2 = (Array_fixed_u32_4){rights[0], rights[1], rights[2], rights[3]};')
	g.sb.writeln('\t\t\t\tdecompressed_data[i].ratio[1] = ${keys_ratio_fn}(timepoints, ctrl.ratios, rights2);')
	g.sb.writeln('\t\t\t\t((void (*)(${float3_key_type}, ${float3_key_type}, ${float3_key_type}, ${float3_key_type}, ${soa_float3_type}*))(decompress_fn))(k01, k11, k21, k31, &decompressed_data[i].value[1]);')
	g.sb.writeln('\t\t\t} else if (compressed.element_size == sizeof(${quat_key_type}) && decompressed->element_size == sizeof(${interp_quat_type})) {')
	g.sb.writeln('\t\t\t\t${quat_key_type}* compressed_data = (${quat_key_type}*)compressed.data;')
	g.sb.writeln('\t\t\t\t${interp_quat_type}* decompressed_data = (${interp_quat_type}*)decompressed->data;')
	g.sb.writeln('\t\t\t\t${quat_key_type} k00 = compressed_data[lefts[0]];')
	g.sb.writeln('\t\t\t\t${quat_key_type} k10 = compressed_data[lefts[1]];')
	g.sb.writeln('\t\t\t\t${quat_key_type} k20 = compressed_data[lefts[2]];')
	g.sb.writeln('\t\t\t\t${quat_key_type} k30 = compressed_data[lefts[3]];')
	g.sb.writeln('\t\t\t\tdecompressed_data[i].ratio[0] = ${keys_ratio_fn}(timepoints, ctrl.ratios, lefts);')
	g.sb.writeln('\t\t\t\t((void (*)(${quat_key_type}, ${quat_key_type}, ${quat_key_type}, ${quat_key_type}, ${soa_quat_type}*))(decompress_fn))(k00, k10, k20, k30, &decompressed_data[i].value[0]);')
	g.sb.writeln('\t\t\t\t${quat_key_type} k01 = compressed_data[rights[0]];')
	g.sb.writeln('\t\t\t\t${quat_key_type} k11 = compressed_data[rights[1]];')
	g.sb.writeln('\t\t\t\t${quat_key_type} k21 = compressed_data[rights[2]];')
	g.sb.writeln('\t\t\t\t${quat_key_type} k31 = compressed_data[rights[3]];')
	g.sb.writeln('\t\t\t\tArray_fixed_u32_4 rights2 = (Array_fixed_u32_4){rights[0], rights[1], rights[2], rights[3]};')
	g.sb.writeln('\t\t\t\tdecompressed_data[i].ratio[1] = ${keys_ratio_fn}(timepoints, ctrl.ratios, rights2);')
	g.sb.writeln('\t\t\t\t((void (*)(${quat_key_type}, ${quat_key_type}, ${quat_key_type}, ${quat_key_type}, ${soa_quat_type}*))(decompress_fn))(k01, k11, k21, k31, &decompressed_data[i].value[1]);')
	g.sb.writeln('\t\t\t}')
	g.sb.writeln('\t\t\toutdated >>= 1;')
	g.sb.writeln('\t\t}')
	g.sb.writeln('\t}')
	g.sb.writeln('}')
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
		// If a concrete signature already exists for this symbol, do not emit
		// an array-based generic stub that would conflict at C compile time.
		should_skip_stub := g.has_concrete_fn_signature(fn_name)
			&& !fn_decl_has_generic_placeholders(node)
		if !should_skip_stub {
			// Fallback: emit a stub with array types.
			// Only emit stub bodies for modules that should be emitted in the current
			// cache bundle. Otherwise the same stub appears in both builtin.o and vlib.o.
			if !g.should_emit_module(g.cur_module) {
				return
			}
			if fn_name.ends_with('SamplingJob__decompress') {
				g.emit_sampling_job_decompress_impl(fn_name)
				return
			}
			mut stub_ret := g.fn_return_types[fn_name] or { 'array' }
			if stub_ret == '' || is_generic_placeholder_c_type_name(stub_ret)
				|| stub_ret.starts_with('Array_fixed_') {
				stub_ret = 'array'
			}
			// Emit a stub that aborts at runtime with a clear message instead
			// of silently returning a zero value.  This surfaces missing
			// generic specialization bugs immediately rather than hiding them.
			g.sb.writeln('/* unresolved generic */ ${stub_ret} ${fn_name}() {')
			g.sb.writeln('\tfputs("v2: unresolved generic call: ${fn_name}\\n", stderr);')
			g.sb.writeln('\tabort();')
			if stub_ret != 'void' {
				// Unreachable, but keeps compilers that warn about missing
				// return values happy.
				g.sb.writeln('\treturn ${zero_value_for_type(stub_ret)};')
			}
			g.sb.writeln('}')
			if fn_name.ends_with('extract_attr') {
				g.emit_extract_attr_specialization(fn_name, 'f32', 'f32')
				g.emit_extract_attr_specialization(fn_name, 'u16', 'u16')
			}
		}
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
