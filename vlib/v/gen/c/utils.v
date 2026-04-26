// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module c

import v.ast

const cgen_resolution_hash_prime = u64(1099511628211)
const cgen_resolution_hash_seed = u64(14695981039346656037)
const cgen_unwrap_generic_cache_salt = u64(0x9e3779b185ebca87)
const cgen_scope_var_type_cache_salt = u64(0xc2b2ae3d27d4eb4f)

@[inline]
fn cgen_resolution_hash_mix(key u64, value u64) u64 {
	return (key ^ value) * cgen_resolution_hash_prime
}

fn (mut g Gen) clear_type_resolution_caches() {
	g.unwrap_generic_cache.clear()
	g.resolved_scope_var_type_cache.clear()
}

fn (g &Gen) type_resolution_context_key() u64 {
	mut key := cgen_resolution_hash_seed
	if g.inside_struct_init {
		key = cgen_resolution_hash_mix(key, 1)
	}
	key = cgen_resolution_hash_mix(key, u64(g.cur_struct_init_typ))
	key = cgen_resolution_hash_mix(key, u64(g.cur_concrete_types.len))
	for concrete_type in g.cur_concrete_types {
		key = cgen_resolution_hash_mix(key, u64(concrete_type))
	}
	key = cgen_resolution_hash_mix(key, u64(g.active_call_concrete_types.len))
	for concrete_type in g.active_call_concrete_types {
		key = cgen_resolution_hash_mix(key, u64(concrete_type))
	}
	if g.comptime != unsafe { nil } {
		key = cgen_resolution_hash_mix(key, u64(g.comptime.comptime_loop_id))
		key = cgen_resolution_hash_mix(key, u64(g.comptime.comptime_for_field_type))
		key = cgen_resolution_hash_mix(key, u64(g.comptime.comptime_for_method_ret_type))
	}
	return key
}

@[inline]
fn (g &Gen) type_resolution_cache_key(typ ast.Type, salt u64) u64 {
	return cgen_resolution_hash_mix(g.type_resolution_context_key(), u64(typ)) ^ salt
}

@[inline]
fn (g &Gen) expr_resolution_cache_key(pos int, default_typ ast.Type, salt u64) u64 {
	if pos <= 0 {
		return 0
	}
	mut key := g.type_resolution_context_key()
	key = cgen_resolution_hash_mix(key, u64(g.fid + 2))
	key = cgen_resolution_hash_mix(key, u64(pos))
	key = cgen_resolution_hash_mix(key, u64(default_typ))
	return key ^ salt
}

@[inline]
fn (g &Gen) type_is_known_concrete(typ ast.Type) bool {
	if typ == 0 || typ.has_flag(.generic) {
		return false
	}
	idx := typ.idx()
	return idx <= ast.nil_type_idx
		|| (idx < g.generic_parts_cache.len && g.generic_parts_cache[idx] == 1)
}

@[inline]
fn (mut g Gen) unwrap_generic(typ ast.Type) ast.Type {
	if typ == 0 {
		return typ
	}
	if !typ.has_flag(.generic) {
		idx := typ.idx()
		if idx <= ast.nil_type_idx
			|| (idx < g.generic_parts_cache.len && g.generic_parts_cache[idx] == 1) {
			return typ
		}
	}
	return g.unwrap_generic_slow(typ)
}

fn (mut g Gen) unwrap_generic_slow(typ ast.Type) ast.Type {
	cache_key := g.type_resolution_cache_key(typ, cgen_unwrap_generic_cache_salt)
	if cached := g.unwrap_generic_cache[cache_key] {
		return cached
	}
	mut resolved_typ := g.recheck_concrete_type(typ)
	if resolved_typ == 0 {
		resolved_typ = typ
	}
	if !resolved_typ.has_flag(.generic) {
		resolved_idx := resolved_typ.idx()
		if resolved_idx <= ast.nil_type_idx
			|| (resolved_idx < g.generic_parts_cache.len
			&& g.generic_parts_cache[resolved_idx] == 1)
			|| !g.type_has_unresolved_generic_parts(resolved_typ) {
			g.unwrap_generic_cache[cache_key] = resolved_typ
			return resolved_typ
		}
	}
	// NOTE: `convert_generic_type` should not mutate the table.
	//
	// It mutates if the generic type is for example `[]T` and the concrete
	// type is an array type that has not been registered yet.
	//
	// This should have already happened in the checker, since it also calls
	// `convert_generic_type`. `g.table` is made non-mut to make sure
	// no one else can accidentally mutates the table.
	current_generic_names := g.current_fn_generic_names()
	if current_generic_names.len > 0 && current_generic_names.len == g.cur_concrete_types.len {
		if t_typ := g.table.convert_generic_type(resolved_typ, current_generic_names,
			g.cur_concrete_types)
		{
			g.unwrap_generic_cache[cache_key] = t_typ
			return t_typ
		}
	} else if g.inside_struct_init {
		if g.cur_struct_init_typ != 0 {
			sym := g.table.sym(g.cur_struct_init_typ)
			if sym.info is ast.Struct {
				if sym.info.generic_types.len > 0 {
					generic_names := sym.info.generic_types.map(g.table.sym(it).name)
					mut concrete_types := sym.info.concrete_types.clone()
					if concrete_types.len == 0 && sym.generic_types.len == generic_names.len
						&& sym.generic_types != sym.info.generic_types {
						concrete_types = sym.generic_types.clone()
					}
					if t_typ := g.table.convert_generic_type(resolved_typ, generic_names,
						concrete_types)
					{
						g.unwrap_generic_cache[cache_key] = t_typ
						return t_typ
					}
				}
			}
		}
	} else if resolved_typ != 0 && g.table.sym(resolved_typ).kind == .struct {
		// resolve selector `a.foo` where `a` is struct[T] on non generic function
		sym := g.table.sym(resolved_typ)
		if sym.info is ast.Struct {
			if sym.info.generic_types.len > 0 {
				generic_names := sym.info.generic_types.map(g.table.sym(it).name)
				mut concrete_types := sym.info.concrete_types.clone()
				if concrete_types.len == 0 && sym.generic_types.len == generic_names.len
					&& sym.generic_types != sym.info.generic_types {
					concrete_types = sym.generic_types.clone()
				}
				if t_typ := g.table.convert_generic_type(resolved_typ, generic_names,
					concrete_types)
				{
					g.unwrap_generic_cache[cache_key] = t_typ
					return t_typ
				}

				if t_typ := g.table.convert_generic_type(resolved_typ, generic_names,
					g.cur_concrete_types)
				{
					g.unwrap_generic_cache[cache_key] = t_typ
					return t_typ
				}
			}
		}
	}
	if typ.has_flag(.generic) {
		if t_typ := g.type_resolver.resolve_bound_generic_type(typ) {
			g.unwrap_generic_cache[cache_key] = t_typ
			return t_typ
		}
	}
	g.unwrap_generic_cache[cache_key] = resolved_typ
	return resolved_typ
}

// Promotes literal element types in arrays (e.g. []int_literal -> []int, []float_literal -> []f64)
// so that array comparisons use the correct registered array type.
fn (mut g Gen) promote_literal_array_type(typ ast.Type) ast.Type {
	sym := g.table.sym(typ)
	if sym.info is ast.Array {
		promoted_elem := ast.mktyp(sym.info.elem_type)
		if promoted_elem != sym.info.elem_type {
			idx := g.table.find_or_register_array(promoted_elem)
			if idx > 0 {
				return ast.new_type(idx).derive(typ)
			}
		}
	}
	return typ
}

fn (mut g Gen) infer_branch_expr_type(stmts []ast.Stmt) ast.Type {
	if stmts.len == 0 {
		return ast.void_type
	}
	last_stmt := stmts.last()
	if last_stmt !is ast.ExprStmt {
		return ast.void_type
	}
	expr_stmt := last_stmt as ast.ExprStmt
	mut default_typ := expr_stmt.typ
	if default_typ == 0 || default_typ == ast.void_type {
		default_typ = expr_stmt.expr.type()
	}
	mut resolved_typ := g.resolved_expr_type(expr_stmt.expr, default_typ)
	if resolved_typ == 0 || resolved_typ == ast.void_type {
		resolved_typ = g.type_resolver.get_type_or_default(expr_stmt.expr, default_typ)
	}
	if resolved_typ == 0 || resolved_typ == ast.void_type {
		resolved_typ = default_typ
	}
	if resolved_typ == 0 || resolved_typ == ast.void_type {
		return ast.void_type
	}
	return g.unwrap_generic(g.recheck_concrete_type(resolved_typ))
}

fn (mut g Gen) infer_if_expr_type(node ast.IfExpr) ast.Type {
	if g.inside_return && g.inside_struct_init {
		for branch in node.branches {
			branch_typ := g.infer_branch_expr_type(branch.stmts)
			if branch_typ != 0 && branch_typ != ast.void_type {
				return branch_typ
			}
		}
	}
	if node.typ != 0 && node.typ != ast.void_type {
		resolved := g.unwrap_generic(g.recheck_concrete_type(node.typ))
		// In generic functions, node.typ may have been mutated by the checker
		// to a concrete type from the last processed instantiation. When the
		// if-expr is used as a return value (g.inside_return), use the function's
		// return type instead, which correctly resolves via cur_concrete_types.
		// Only apply this override when the function's return type is actually
		// generic — otherwise the if-expression type is concrete and correct.
		if g.inside_return && !g.inside_struct_init && g.cur_fn != unsafe { nil }
			&& g.cur_concrete_types.len > 0 && g.cur_fn.return_type.has_flag(.generic) {
			fn_ret := g.unwrap_generic(g.recheck_concrete_type(g.cur_fn.return_type))
			if fn_ret != 0 && fn_ret != ast.void_type {
				if node.typ.has_flag(.result) && !fn_ret.has_flag(.result) {
					return fn_ret.set_flag(.result)
				} else if node.typ.has_flag(.option) && !fn_ret.has_flag(.option) {
					return fn_ret.set_flag(.option)
				}
				return fn_ret
			}
		}
		return resolved
	}
	for branch in node.branches {
		branch_typ := g.infer_branch_expr_type(branch.stmts)
		if branch_typ != 0 && branch_typ != ast.void_type {
			return branch_typ
		}
	}
	return ast.void_type
}

fn (mut g Gen) infer_match_expr_type(node ast.MatchExpr) ast.Type {
	if g.inside_return && g.inside_struct_init {
		for branch in node.branches {
			branch_typ := g.infer_branch_expr_type(branch.stmts)
			if branch_typ != 0 && branch_typ != ast.void_type {
				return branch_typ
			}
		}
	}
	if node.return_type != 0 && node.return_type != ast.void_type {
		resolved := g.unwrap_generic(g.recheck_concrete_type(node.return_type))
		// In generic functions, node.return_type may have been mutated by the checker
		// to a concrete type from the last processed instantiation. When the match is
		// used as a return value (g.inside_return), use the function's return type
		// instead, which correctly resolves via cur_concrete_types.
		// Only apply this override when the function's return type is actually
		// generic — otherwise the match expression type is concrete and correct.
		if g.inside_return && !g.inside_struct_init && g.cur_fn != unsafe { nil }
			&& g.cur_concrete_types.len > 0 && g.cur_fn.return_type.has_flag(.generic) {
			fn_ret := g.unwrap_generic(g.recheck_concrete_type(g.cur_fn.return_type))
			if fn_ret != 0 && fn_ret != ast.void_type {
				// Preserve option/result flags from the match's return_type
				if node.return_type.has_flag(.result) && !fn_ret.has_flag(.result) {
					return fn_ret.set_flag(.result)
				} else if node.return_type.has_flag(.option) && !fn_ret.has_flag(.option) {
					return fn_ret.set_flag(.option)
				}
				return fn_ret
			}
		}
		return resolved
	}
	for branch in node.branches {
		branch_typ := g.infer_branch_expr_type(branch.stmts)
		if branch_typ != 0 && branch_typ != ast.void_type {
			return branch_typ
		}
	}
	return ast.void_type
}

fn (mut g Gen) recheck_concrete_type(typ ast.Type) ast.Type {
	if typ == 0 {
		return typ
	}
	if g.cur_fn == unsafe { nil } || g.cur_concrete_types.len == 0 {
		return typ
	}
	idx := typ.idx()
	if idx <= ast.nil_type_idx || (!typ.has_flag(.generic) && idx < g.generic_parts_cache.len
		&& g.generic_parts_cache[idx] == 1) {
		return typ
	}
	sym := g.table.sym(typ)
	match sym.info {
		ast.Struct, ast.Interface, ast.SumType {
			if sym.info.concrete_types.len > 0
				&& !sym.info.concrete_types.any(it.has_flag(.generic)) {
				return typ
			}
		}
		ast.GenericInst {
			if sym.info.concrete_types.len > 0
				&& !sym.info.concrete_types.any(it.has_flag(.generic)) {
				return typ
			}
		}
		else {}
	}

	if !typ.has_flag(.generic) && !g.type_has_unresolved_generic_parts(typ) {
		return typ
	}
	if g.cur_fn == unsafe { nil } || g.cur_concrete_types.len == 0 {
		return typ
	}
	generic_names := g.current_fn_generic_names()
	if generic_names.len == 0 || generic_names.len != g.cur_concrete_types.len {
		return typ
	}
	concrete_types := g.cur_concrete_types
	if resolved_typ := g.table.convert_generic_type(typ, generic_names, concrete_types) {
		return resolved_typ
	}
	mut muttable := unsafe { &ast.Table(g.table) }
	unwrapped_typ := muttable.unwrap_generic_type_ex(typ, generic_names, concrete_types, true)
	if unwrapped_typ != typ {
		return unwrapped_typ
	}
	return typ
}

@[inline]
fn (g &Gen) has_current_generic_context() bool {
	return g.cur_fn != unsafe { nil } && g.cur_concrete_types.len > 0
}

@[inline]
fn (mut g Gen) type_needs_generic_resolution(typ ast.Type) bool {
	if typ == 0 {
		return false
	}
	if typ.has_flag(.generic) {
		return true
	}
	if typ.idx() <= ast.nil_type_idx {
		return false
	}
	if (g.cur_fn == unsafe { nil } || g.cur_concrete_types.len == 0)
		&& !g.has_active_call_generic_context() {
		return false
	}
	idx := typ.idx()
	if idx <= ast.nil_type_idx
		|| (idx < g.generic_parts_cache.len && g.generic_parts_cache[idx] == 1) {
		return false
	}
	return g.type_has_unresolved_generic_parts(typ)
}

// is_expr_smartcast_to_sumtype checks if expr is a smartcast variable/field
// whose original type is the given sumtype. This is used to prevent sumtype
// variant unwrapping when passing a smartcast expression to a function
// that expects the original sumtype.
fn (mut g Gen) is_expr_smartcast_to_sumtype(expr ast.Expr, expected_sumtype ast.Type) bool {
	scope := g.file.scope.innermost(expr.pos().pos)
	if expr is ast.SelectorExpr {
		v := scope.find_struct_field(expr.expr.str(), expr.expr_type, expr.field_name)
		if v != unsafe { nil } && v.smartcasts.len > 0 {
			return true
		}
	} else if expr is ast.Ident {
		if v := scope.find_var(expr.name) {
			if v.smartcasts.len > 0 && v.orig_type == expected_sumtype {
				return true
			}
		}
	}
	return false
}

// expr_has_or_block checks if an expression has an `or {}` block that
// unwraps an option/result type.
fn (g Gen) expr_has_or_block(expr ast.Expr) bool {
	return match expr {
		ast.CallExpr { expr.or_block.kind != .absent }
		ast.Ident { expr.or_expr.kind != .absent }
		ast.IndexExpr { expr.or_expr.kind != .absent }
		ast.SelectorExpr { expr.or_block.kind != .absent }
		ast.PrefixExpr { expr.or_block.kind != .absent }
		ast.InfixExpr { expr.or_block.kind != .absent }
		ast.ComptimeCall { expr.or_block.kind != .absent }
		ast.ComptimeSelector { expr.or_block.kind != .absent }
		else { false }
	}
}

fn (g &Gen) is_auto_deref_source_ident(expr ast.Expr) bool {
	if expr is ast.Ident {
		ident := expr as ast.Ident
		if ident.obj is ast.Var && ident.obj.is_auto_deref {
			return true
		}
		if source_var := ident.scope.find_var(ident.name) {
			return source_var.is_auto_deref
		}
	}
	return false
}

fn (g &Gen) auto_deref_source_type_is_pointer(expr ast.Expr) bool {
	if expr !is ast.Ident || g.cur_fn == unsafe { nil } || !expr.is_auto_deref_var() {
		return false
	}
	ident := expr as ast.Ident
	for param in g.cur_fn.params {
		if param.name == ident.name {
			source_typ := if param.orig_typ != 0 { param.orig_typ } else { param.typ }
			return source_typ.is_any_kind_of_pointer()
		}
	}
	return false
}

fn (mut g Gen) resolved_scope_var_type(expr ast.Ident) ast.Type {
	if g.has_active_call_generic_context() {
		return g.resolved_scope_var_type_uncached(expr)
	}
	cache_key := g.expr_resolution_cache_key(expr.pos.pos, 0, cgen_scope_var_type_cache_salt)
	if cache_key != 0 {
		if cached := g.resolved_scope_var_type_cache[cache_key] {
			return cached
		}
	}
	resolved := g.resolved_scope_var_type_uncached(expr)
	if cache_key != 0 && resolved != 0 {
		g.resolved_scope_var_type_cache[cache_key] = resolved
	}
	return resolved
}

fn (mut g Gen) resolved_scope_var_type_uncached(expr ast.Ident) ast.Type {
	mut scope := if expr.scope != unsafe { nil } {
		expr.scope.innermost(expr.pos.pos)
	} else {
		expr.scope
	}
	if scope == unsafe { nil } || scope.find_var(expr.name) == none {
		scope = if g.file.scope != unsafe { nil } {
			g.file.scope.innermost(expr.pos.pos)
		} else {
			expr.scope
		}
	}
	if scope == unsafe { nil } {
		return 0
	}
	if mut v := scope.find_var(expr.name) {
		mut refreshed_expr_type := ast.Type(0)
		if v.generic_typ != 0 {
			refreshed_generic_type := g.unwrap_generic(g.recheck_concrete_type(v.generic_typ))
			if refreshed_generic_type != 0 {
				v.typ = refreshed_generic_type
				v.orig_type = ast.no_type
				v.smartcasts = []
				v.is_unwrapped = false
			}
		}
		if !v.is_arg && v.expr !is ast.EmptyExpr && v.pos.pos > 0 && v.pos.pos < expr.pos.pos
			&& !(v.expr is ast.Ident && v.expr.name == expr.name)
			&& ((g.cur_fn != unsafe { nil } && g.cur_concrete_types.len > 0)
			|| v.typ.has_flag(.generic)
			|| g.type_has_unresolved_generic_parts(v.typ)) {
			resolved_expr_type := g.resolved_expr_type(v.expr, v.typ)
			if resolved_expr_type != 0 {
				refreshed_expr_type = g.unwrap_generic(g.recheck_concrete_type(resolved_expr_type))
				if g.type_has_unresolved_generic_parts(refreshed_expr_type) {
					call_like_type := g.resolved_call_like_expr_type(v.expr)
					if call_like_type != 0 && !call_like_type.has_flag(.generic)
						&& !g.type_has_unresolved_generic_parts(call_like_type) {
						refreshed_expr_type = call_like_type
					}
				}
				// Keep `mut x := param` as a value copy when re-resolving locals,
				// unless the original mut parameter type was already a pointer.
				if g.is_auto_deref_source_ident(v.expr) && refreshed_expr_type.is_ptr()
					&& !g.auto_deref_source_type_is_pointer(v.expr) {
					refreshed_expr_type = refreshed_expr_type.deref()
				}
				// If the variable was initialized with an `or {}` block that
				// unwraps the option/result, clear the flag from the resolved type
				if refreshed_expr_type.has_option_or_result() && g.expr_has_or_block(v.expr) {
					refreshed_expr_type = refreshed_expr_type.clear_option_and_result()
				}
				$if trace_ci_fixes ? {
					if g.file.path.contains('comptime_for_in_options_struct_test.v')
						&& expr.name in ['v', 'w'] {
						current_name := if v.typ == 0 { '0' } else { g.table.type_to_str(v.typ) }
						resolved_name := if refreshed_expr_type == 0 {
							'0'
						} else {
							g.table.type_to_str(refreshed_expr_type)
						}
						orig_name := if v.orig_type == 0 {
							'0'
						} else {
							g.table.type_to_str(v.orig_type)
						}
						eprintln('resolved_scope_var_type refresh ${expr.name}: current=${current_name} expr=${typeof(v.expr).name} resolved=${resolved_name} unwrapped=${v.is_unwrapped} orig=${orig_name}')
					}
				}
				if v.is_unwrapped && refreshed_expr_type.has_option_or_result() {
					v.orig_type = refreshed_expr_type
					v.typ = refreshed_expr_type.clear_option_and_result()
				} else {
					v.typ = refreshed_expr_type
					if !v.is_unwrapped && v.smartcasts.len == 0 {
						v.orig_type = ast.no_type
					}
				}
			}
		}
		if (v.is_unwrapped || v.smartcasts.len > 0) && scope.parent != unsafe { nil } {
			if mut parent_v := scope.parent.find_var(expr.name) {
				if parent_v.generic_typ != 0 {
					refreshed_parent_type :=
						g.unwrap_generic(g.recheck_concrete_type(parent_v.generic_typ))
					if refreshed_parent_type != 0 {
						parent_v.typ = refreshed_parent_type
					}
				}
				if !parent_v.is_arg && parent_v.expr !is ast.EmptyExpr && parent_v.pos.pos > 0
					&& parent_v.pos.pos < expr.pos.pos && !(parent_v.expr is ast.Ident
					&& parent_v.expr.name == expr.name)
					&& ((g.cur_fn != unsafe { nil } && g.cur_concrete_types.len > 0)
					|| parent_v.typ.has_flag(.generic)
					|| g.type_has_unresolved_generic_parts(parent_v.typ)) {
					resolved_parent_expr_type := g.resolved_expr_type(parent_v.expr, parent_v.typ)
					if resolved_parent_expr_type != 0 {
						mut refreshed_parent_type :=
							g.unwrap_generic(g.recheck_concrete_type(resolved_parent_expr_type))
						if g.type_has_unresolved_generic_parts(parent_v.typ) {
							call_like_type := g.resolved_call_like_expr_type(parent_v.expr)
							if call_like_type != 0 && !call_like_type.has_flag(.generic)
								&& !g.type_has_unresolved_generic_parts(call_like_type) {
								parent_v.typ = call_like_type
							}
						}
						if g.is_auto_deref_source_ident(parent_v.expr)
							&& refreshed_parent_type.is_ptr()
							&& !g.auto_deref_source_type_is_pointer(parent_v.expr) {
							refreshed_parent_type = refreshed_parent_type.deref()
						}
						parent_v.typ = refreshed_parent_type
					}
				}
				if v.is_unwrapped {
					parent_orig_type := if parent_v.orig_type != ast.no_type
						&& parent_v.orig_type.has_option_or_result() {
						parent_v.orig_type
					} else {
						parent_v.typ
					}
					if parent_orig_type.has_option_or_result() {
						v.orig_type = parent_orig_type
						v.typ = parent_orig_type.clear_option_and_result()
					}
				} else if v.smartcasts.len > 0 && parent_v.typ != 0 && v.orig_type == ast.no_type {
					v.orig_type = if parent_v.orig_type != ast.no_type {
						parent_v.orig_type
					} else {
						parent_v.typ
					}
				}
			}
		}
		if v.is_inherited && scope.parent != unsafe { nil } {
			if mut parent_v := scope.parent.find_var(expr.name) {
				by_value_auto_deref_capture := !v.is_auto_deref && parent_v.is_auto_deref
					&& parent_v.typ.is_ptr()
				if by_value_auto_deref_capture {
					if parent_v.generic_typ != 0 {
						refreshed_parent_type :=
							g.unwrap_generic(g.recheck_concrete_type(parent_v.generic_typ))
						if refreshed_parent_type != 0 {
							parent_v.typ = refreshed_parent_type
						}
					}
					if parent_v.expr !is ast.EmptyExpr
						&& ((g.cur_fn != unsafe { nil } && g.cur_concrete_types.len > 0)
						|| parent_v.typ.has_flag(.generic)
						|| g.type_has_unresolved_generic_parts(parent_v.typ)) {
						resolved_parent_type := g.resolved_expr_type(parent_v.expr, parent_v.typ)
						if resolved_parent_type != 0 {
							parent_v.typ =
								g.unwrap_generic(g.recheck_concrete_type(resolved_parent_type))
						}
					}
					if parent_v.typ != 0 {
						resolved_parent_type :=
							g.unwrap_generic(g.recheck_concrete_type(parent_v.typ))
						if resolved_parent_type != 0 {
							return if resolved_parent_type.is_ptr() {
								resolved_parent_type.deref()
							} else {
								resolved_parent_type
							}
						}
					}
				} else {
					if parent_v.generic_typ != 0 {
						refreshed_parent_type :=
							g.unwrap_generic(g.recheck_concrete_type(parent_v.generic_typ))
						if refreshed_parent_type != 0 {
							parent_v.typ = refreshed_parent_type
						}
					}
					if parent_v.smartcasts.len > 0 {
						smartcast_type := if parent_v.ct_type_var == .smartcast {
							g.type_resolver.get_type(expr)
						} else {
							g.exposed_smartcast_type(parent_v.orig_type,
								parent_v.smartcasts.last(), parent_v.is_mut)
						}
						return g.unwrap_generic(g.recheck_concrete_type(smartcast_type))
					}
					if parent_v.expr !is ast.EmptyExpr
						&& ((g.cur_fn != unsafe { nil } && g.cur_concrete_types.len > 0)
						|| parent_v.typ.has_flag(.generic)
						|| g.type_has_unresolved_generic_parts(parent_v.typ)) {
						resolved_parent_type := g.resolved_expr_type(parent_v.expr, parent_v.typ)
						if resolved_parent_type != 0 {
							resolved_parent :=
								g.unwrap_generic(g.recheck_concrete_type(resolved_parent_type))
							if g.type_has_unresolved_generic_parts(resolved_parent) {
								call_like_type := g.resolved_call_like_expr_type(parent_v.expr)
								if call_like_type != 0 && !call_like_type.has_flag(.generic)
									&& !g.type_has_unresolved_generic_parts(call_like_type) {
									return call_like_type
								}
							}
							return resolved_parent
						}
					}
					if parent_v.typ != 0 {
						return g.unwrap_generic(g.recheck_concrete_type(parent_v.typ))
					}
				}
			}
		}
		if v.smartcasts.len > 0 {
			smartcast_type := if v.ct_type_var == .smartcast {
				g.type_resolver.get_type(expr)
			} else {
				g.exposed_smartcast_type(v.orig_type, v.smartcasts.last(), v.is_mut)
			}
			return g.unwrap_generic(g.recheck_concrete_type(smartcast_type))
		}
		if v.is_unwrapped {
			$if trace_ci_fixes ? {
				if g.file.path.contains('comptime_for_in_options_struct_test.v')
					&& expr.name in ['v', 'w'] {
					typ_name := if v.typ == 0 { '0' } else { g.table.type_to_str(v.typ) }
					orig_name := if v.orig_type == 0 {
						'0'
					} else {
						g.table.type_to_str(v.orig_type)
					}
					refreshed_name := if refreshed_expr_type == 0 {
						'0'
					} else {
						g.table.type_to_str(refreshed_expr_type)
					}
					eprintln('resolved_scope_var_type unwrapped ${expr.name}: typ=${typ_name} orig=${orig_name} refreshed=${refreshed_name}')
				}
			}
			if refreshed_expr_type != 0 && refreshed_expr_type.has_option_or_result() {
				return g.unwrap_generic(g.recheck_concrete_type(refreshed_expr_type.clear_option_and_result()))
			}
			unwrapped_type := if v.orig_type != ast.no_type && v.orig_type.has_option_or_result() {
				v.orig_type.clear_option_and_result()
			} else {
				v.typ.clear_option_and_result()
			}
			if unwrapped_type != 0 && unwrapped_type != ast.void_type {
				return g.unwrap_generic(g.recheck_concrete_type(unwrapped_type))
			}
		}
		if v.typ != 0 {
			return g.unwrap_generic(g.recheck_concrete_type(v.typ))
		}
	}
	return 0
}

fn (mut g Gen) resolved_ident_is_auto_heap_not_stack(expr ast.Ident) bool {
	if expr.obj is ast.Var {
		if expr.obj.is_auto_heap && !expr.obj.is_stack_obj {
			return true
		}
	}
	if expr.scope != unsafe { nil } {
		if v := expr.scope.find_var(expr.name) {
			return v.is_auto_heap && !v.is_stack_obj
		}
	}
	return false
}

fn (mut g Gen) resolved_ident_is_auto_heap(expr ast.Ident) bool {
	if expr.obj is ast.Var && expr.obj.is_auto_heap {
		return true
	}
	if expr.scope != unsafe { nil } {
		if v := expr.scope.find_var(expr.name) {
			return v.is_auto_heap
		}
	}
	return false
}

fn (g &Gen) resolved_ident_is_auto_deref(expr ast.Ident) bool {
	if expr.scope != unsafe { nil } {
		if v := expr.scope.find_var(expr.name) {
			return v.is_auto_deref
		}
	}
	if expr.obj is ast.Var {
		return expr.obj.is_auto_deref
	}
	return false
}

fn (g &Gen) resolved_ident_is_by_value_auto_deref_capture(expr ast.Ident) bool {
	if expr.scope == unsafe { nil } || expr.scope.parent == unsafe { nil } {
		return false
	}
	scope_var := expr.scope.find_var(expr.name) or { return false }
	if !scope_var.is_inherited || scope_var.is_auto_deref {
		return false
	}
	parent_var := expr.scope.parent.find_var(expr.name) or { return false }
	return parent_var.is_auto_deref && parent_var.typ.is_ptr()
}

fn (g &Gen) expr_is_auto_deref_var(expr ast.Expr) bool {
	return match expr {
		ast.Ident { g.resolved_ident_is_auto_deref(expr) }
		else { expr.is_auto_deref_var() }
	}
}

// scope_ident_is_auto_heap reports whether `expr`'s scope variable has
// is_auto_heap set. Unlike `resolved_ident_is_auto_heap`, this ignores
// `expr.obj.is_auto_heap` because a use-site Ident's obj copy may have been
// toggled by `mark_as_referenced` even when the declaration was emitted as
// a value (e.g. vars declared inside nested scopes where fn_scope.find_var
// misses them).
fn (mut g Gen) scope_ident_is_auto_heap(expr ast.Ident) bool {
	if expr.scope != unsafe { nil } {
		if v := expr.scope.find_var(expr.name) {
			return v.is_auto_heap
		}
	}
	return false
}

fn (mut g Gen) resolved_ident_array_elem_type(expr ast.Ident) ast.Type {
	scope_type := g.resolved_scope_var_type(expr)
	if scope_type != 0 {
		elem_type := g.recheck_concrete_type(g.table.value_type(g.unwrap_generic(scope_type)))
		if elem_type != 0 {
			return g.unwrap_generic(elem_type)
		}
	}
	if expr.obj !is ast.Var {
		return 0
	}
	var_obj := expr.obj as ast.Var
	if var_obj.expr is ast.ArrayInit {
		array_init := var_obj.expr as ast.ArrayInit
		if array_init.elem_type != 0 {
			return g.unwrap_generic(g.recheck_concrete_type(array_init.elem_type))
		}
		if array_init.typ != 0 {
			elem_type :=
				g.recheck_concrete_type(g.table.value_type(g.unwrap_generic(array_init.typ)))
			if elem_type != 0 {
				return g.unwrap_generic(elem_type)
			}
		}
	}
	if var_obj.typ != 0 {
		elem_type := g.recheck_concrete_type(g.table.value_type(g.unwrap_generic(var_obj.typ)))
		if elem_type != 0 {
			return g.unwrap_generic(elem_type)
		}
	}
	return 0
}

fn (mut g Gen) resolved_ident_map_key_type(expr ast.Ident) ast.Type {
	scope_type := g.resolved_scope_var_type(expr)
	if scope_type != 0 {
		typ_sym := g.table.final_sym(g.unwrap_generic(scope_type))
		if typ_sym.kind == .map {
			key_type := g.recheck_concrete_type(typ_sym.map_info().key_type)
			if key_type != 0 {
				return g.unwrap_generic(key_type)
			}
		}
	}
	if expr.obj !is ast.Var {
		return 0
	}
	var_obj := expr.obj as ast.Var
	if var_obj.expr is ast.MapInit {
		map_init := var_obj.expr as ast.MapInit
		if map_init.key_type != 0 {
			return g.unwrap_generic(g.recheck_concrete_type(map_init.key_type))
		}
	}
	if var_obj.typ != 0 {
		typ_sym := g.table.final_sym(g.unwrap_generic(var_obj.typ))
		if typ_sym.kind == .map {
			key_type := g.recheck_concrete_type(typ_sym.map_info().key_type)
			if key_type != 0 {
				return g.unwrap_generic(key_type)
			}
		}
	}
	return 0
}

fn (mut g Gen) resolved_ident_map_value_type(expr ast.Ident) ast.Type {
	scope_type := g.resolved_scope_var_type(expr)
	if scope_type != 0 {
		typ_sym := g.table.final_sym(g.unwrap_generic(scope_type))
		if typ_sym.kind == .map {
			val_type := g.recheck_concrete_type(typ_sym.map_info().value_type)
			if val_type != 0 {
				return g.unwrap_generic(val_type)
			}
		}
	}
	if expr.obj !is ast.Var {
		return 0
	}
	var_obj := expr.obj as ast.Var
	if var_obj.expr is ast.MapInit {
		map_init := var_obj.expr as ast.MapInit
		if map_init.value_type != 0 {
			return g.unwrap_generic(g.recheck_concrete_type(map_init.value_type))
		}
	}
	if var_obj.typ != 0 {
		typ_sym := g.table.final_sym(g.unwrap_generic(var_obj.typ))
		if typ_sym.kind == .map {
			val_type := g.recheck_concrete_type(typ_sym.map_info().value_type)
			if val_type != 0 {
				return g.unwrap_generic(val_type)
			}
		}
	}
	return 0
}

fn (mut g Gen) resolved_array_elem_type_from_name(name string) ast.Type {
	if !name.starts_with('[]') {
		return 0
	}
	elem_type := g.table.find_type(name[2..])
	if elem_type != 0 {
		return g.unwrap_generic(g.recheck_concrete_type(elem_type))
	}
	return 0
}

fn split_map_type_name(name string) (string, string) {
	if !name.starts_with('map[') {
		return '', ''
	}
	mut depth := 1
	mut i := 4
	for i < name.len {
		ch := name[i]
		if ch == `[` {
			depth++
		} else if ch == `]` {
			depth--
			if depth == 0 {
				break
			}
		}
		i++
	}
	if depth != 0 || i >= name.len {
		return '', ''
	}
	return name[4..i], name[i + 1..]
}

fn (mut g Gen) resolved_map_types_from_name(name string) (ast.Type, ast.Type) {
	key_name, val_name := split_map_type_name(name)
	if key_name.len == 0 || val_name.len == 0 {
		return ast.Type(0), ast.Type(0)
	}
	key_type := g.table.find_type(key_name)
	val_type := g.table.find_type(val_name)
	return g.unwrap_generic(g.recheck_concrete_type(key_type)), g.unwrap_generic(g.recheck_concrete_type(val_type))
}

fn (mut g Gen) resolved_call_like_expr_type(expr ast.Expr) ast.Type {
	match expr {
		ast.CallExpr {
			resolved := g.resolve_return_type(expr)
			if resolved != ast.void_type {
				return g.unwrap_generic(g.recheck_concrete_type(resolved)).clear_option_and_result()
			}
		}
		ast.PostfixExpr {
			resolved := g.resolved_call_like_expr_type(expr.expr)
			if resolved != 0 {
				return g.unwrap_generic(resolved).clear_option_and_result()
			}
		}
		ast.UnsafeExpr {
			return g.resolved_call_like_expr_type(expr.expr)
		}
		else {}
	}

	return 0
}

// resolved_expr_type recomputes the concrete type for expr nodes that can keep
// stale generic metadata across concrete rechecks/codegen.
fn (mut g Gen) resolved_or_block_value_type(or_expr ast.OrExpr) ast.Type {
	if or_expr.stmts.len == 0 {
		return 0
	}
	last_or_stmt := or_expr.stmts.last()
	if last_or_stmt is ast.ExprStmt && last_or_stmt.typ != ast.void_type {
		resolved_or_type := g.resolved_expr_type(last_or_stmt.expr, last_or_stmt.typ)
		if resolved_or_type != 0 && resolved_or_type != ast.void_type {
			return g.unwrap_generic(g.recheck_concrete_type(resolved_or_type))
		}
	}
	return 0
}

// resolve_selector_smartcast_type resolves the final smartcast type for a
// selector expression in generic contexts. When a field like `val.field` has
// nested smartcasts (e.g., option unwrap then sumtype variant), the scope
// stores these smartcasts but they may reference stale types from a previous
// generic instantiation. This function re-resolves them using current concrete
// types to determine the correct final type.
fn (mut g Gen) resolve_selector_smartcast_type(node ast.SelectorExpr) ast.Type {
	scope := g.file.scope.innermost(node.pos.pos)
	field := scope.find_struct_field(node.expr.str(), node.expr_type, node.field_name)
	if field != unsafe { nil } && field.smartcasts.len > 0 {
		resolved_sc := g.unwrap_generic(g.recheck_concrete_type(g.exposed_smartcast_type(field.orig_type,
			field.smartcasts.last(), field.is_mut)))
		if resolved_sc != 0 {
			return resolved_sc
		}
	}
	return 0
}

fn (mut g Gen) resolved_expr_type(expr ast.Expr, default_typ ast.Type) ast.Type {
	match expr {
		ast.ParExpr {
			return g.resolved_expr_type(expr.expr, default_typ)
		}
		ast.CTempVar {
			if expr.typ != 0 {
				return g.unwrap_generic(g.recheck_concrete_type(expr.typ))
			}
			return g.resolved_expr_type(expr.orig, default_typ)
		}
		ast.Ident {
			if expr.obj is ast.Var {
				if expr.obj.typ != 0 && expr.obj.generic_typ == 0 && !expr.obj.is_inherited
					&& !expr.obj.is_unwrapped && !expr.obj.is_assignment_smartcast
					&& !expr.obj.is_or && expr.obj.orig_type == ast.no_type
					&& expr.obj.smartcasts.len == 0 && expr.obj.ct_type_var == .no_comptime
					&& !g.has_current_generic_context() && !g.has_active_call_generic_context() {
					if g.type_is_known_concrete(expr.obj.typ) {
						return expr.obj.typ
					}
				}
				if g.cur_fn != unsafe { nil } && g.cur_fn.is_method
					&& expr.name == g.cur_fn.receiver.name {
					// In generic contexts, prefer resolving from the receiver declaration
					// since scope types may be stale from a previous checker instantiation
					if g.cur_concrete_types.len > 0 && (g.cur_fn.receiver.typ.has_flag(.generic)
						|| g.type_has_unresolved_generic_parts(g.cur_fn.receiver.typ)) {
						resolved_receiver_type :=
							g.unwrap_generic(g.recheck_concrete_type(g.cur_fn.receiver.typ))
						if resolved_receiver_type != 0 {
							return resolved_receiver_type
						}
					}
					scope_type := g.resolved_scope_var_type(expr)
					if scope_type != 0 {
						return scope_type
					}
					resolved_receiver_type :=
						g.unwrap_generic(g.recheck_concrete_type(g.cur_fn.receiver.typ))
					if resolved_receiver_type != 0 {
						return resolved_receiver_type
					}
				}
				if g.cur_fn != unsafe { nil } && g.cur_concrete_types.len > 0
					&& expr.obj.expr is ast.Ident {
					ident_expr := expr.obj.expr as ast.Ident
					if ident_expr.or_expr.kind != .absent {
						resolved_or_type := g.resolved_or_block_value_type(ident_expr.or_expr)
						if resolved_or_type != 0 {
							return resolved_or_type
						}
					}
				}
				if expr.obj.ct_type_var == .generic_param {
					resolved := g.resolve_current_fn_generic_param_type(expr.name)
					if resolved != 0 {
						return g.unwrap_generic(g.recheck_concrete_type(resolved))
					}
				}
				// In generic contexts, if the variable has smartcasts with generic
				// types (preserved in node.obj), resolve from those instead of
				// relying on scope types which may be stale from a different
				// generic instantiation.
				if g.cur_fn != unsafe { nil } && g.cur_concrete_types.len > 0
					&& expr.obj.smartcasts.len > 0 && expr.obj.smartcasts.any(it.has_flag(.generic)
					|| g.type_has_unresolved_generic_parts(it)) {
					obj_smartcast_type := g.exposed_smartcast_type(expr.obj.orig_type,
						expr.obj.smartcasts.last(), expr.obj.is_mut)
					resolved_sc := g.unwrap_generic(g.recheck_concrete_type(obj_smartcast_type))
					if resolved_sc != 0 {
						return resolved_sc
					}
				}
				scope_type := g.resolved_scope_var_type(expr)
				if scope_type != 0 && !scope_type.has_flag(.generic)
					&& !g.type_has_unresolved_generic_parts(scope_type) {
					if !expr.obj.is_arg
						|| (g.cur_fn != unsafe { nil } && g.cur_concrete_types.len > 0) {
						// For generic_var in generic contexts, prefer expression-based
						// resolution first (scope types may be stale from a previous
						// instantiation), but fall back to scope type if expr resolution
						// fails.
						if g.cur_fn != unsafe { nil } && g.cur_concrete_types.len > 0
							&& expr.obj.ct_type_var == .generic_var
							&& expr.obj.typ.has_flag(.generic) {
							// Try expression-based resolution first
							if expr.obj.expr !is ast.EmptyExpr && !(expr.obj.expr is ast.Ident
								&& expr.obj.expr.name == expr.name) {
								resolved := g.resolved_expr_type(expr.obj.expr, expr.obj.typ)
								if resolved != 0 {
									return g.unwrap_generic(resolved)
								}
							}
						}
						// In generic contexts, scope types may be stale from a previous
						// checker instantiation. If the variable's init expression is a
						// struct init whose type name matches a generic parameter, re-resolve
						// using the current concrete types.
						if g.cur_fn != unsafe { nil } && g.cur_concrete_types.len > 0
							&& expr.obj.expr is ast.StructInit {
							generic_names := g.current_fn_generic_names()
							struct_init_typ_str := expr.obj.expr.typ_str.all_after_last('.')
							idx := generic_names.index(struct_init_typ_str)
							if idx >= 0 && idx < g.cur_concrete_types.len {
								return g.cur_concrete_types[idx]
							}
						}
						return scope_type
					}
				}
				if expr.obj.expr !is ast.EmptyExpr
					&& (expr.obj.ct_type_var == .generic_var || expr.obj.typ.has_flag(.generic)
					|| g.type_has_unresolved_generic_parts(expr.obj.typ)
					|| ((g.cur_fn != unsafe { nil } && g.cur_concrete_types.len > 0)
					&& expr.obj.expr !in [ast.IntegerLiteral, ast.FloatLiteral, ast.StringLiteral, ast.BoolLiteral, ast.CharLiteral])) {
					if !(expr.obj.expr is ast.Ident && expr.obj.expr.name == expr.name) {
						mut resolved := g.resolved_expr_type(expr.obj.expr, expr.obj.typ)
						if resolved != 0 {
							resolved = g.unwrap_generic(g.recheck_concrete_type(resolved))
							if expr.obj.typ != 0 {
								resolved_obj_type :=
									g.unwrap_generic(g.recheck_concrete_type(expr.obj.typ))
								if resolved_obj_type != 0
									&& !g.type_has_unresolved_generic_parts(resolved_obj_type)
									&& resolved.has_option_or_result()
									&& resolved.clear_option_and_result() == resolved_obj_type {
									return resolved_obj_type
								}
							}
							if g.type_has_unresolved_generic_parts(resolved) {
								call_like_type := g.resolved_call_like_expr_type(expr.obj.expr)
								if call_like_type != 0 && !call_like_type.has_flag(.generic)
									&& !g.type_has_unresolved_generic_parts(call_like_type) {
									return call_like_type
								}
							}
							return g.unwrap_generic(resolved)
						}
					}
				}
				if expr.obj.ct_type_var != .no_comptime && expr.obj.ct_type_var != .generic_param {
					ctyp := g.type_resolver.get_type(expr)
					if ctyp != ast.void_type {
						return g.unwrap_generic(ctyp)
					}
				}
			}
			if g.cur_fn != unsafe { nil } && g.cur_fn.generic_names.len > 0
				&& g.cur_concrete_types.len > 0 {
				resolved := g.resolve_current_fn_generic_param_type(expr.name)
				if resolved != 0 {
					return g.unwrap_generic(g.recheck_concrete_type(resolved))
				}
			}
			if expr.obj is ast.Var && expr.obj.typ != 0 {
				resolved_obj_type := g.unwrap_generic(g.recheck_concrete_type(expr.obj.typ))
				if resolved_obj_type != 0 && (expr.obj.is_arg || expr.obj.typ.has_flag(.generic)
					|| g.type_has_unresolved_generic_parts(expr.obj.typ)) {
					return resolved_obj_type
				}
			}
			scope_type := g.resolved_scope_var_type(expr)
			if scope_type != 0 && expr.obj is ast.Var
				&& (expr.obj.is_unwrapped || expr.obj.orig_type != 0 || expr.obj.smartcasts.len > 0) {
				return scope_type
			}
			default_resolved_type := g.unwrap_generic(g.recheck_concrete_type(default_typ))
			resolver_type := g.unwrap_generic(g.recheck_concrete_type(g.type_resolver.get_type_or_default(expr,
				default_typ)))
			if resolver_type != 0 && !g.type_has_unresolved_generic_parts(resolver_type)
				&& (resolver_type != default_resolved_type || (expr.obj is ast.Var
				&& (expr.obj.is_unwrapped || expr.obj.orig_type != 0
				|| expr.obj.smartcasts.len > 0))) {
				return resolver_type
			}
			if scope_type != 0 {
				return scope_type
			}
		}
		ast.SelectorExpr {
			left_default := if expr.expr_type != 0 { expr.expr_type } else { default_typ }
			left_type := g.recheck_concrete_type(g.resolved_expr_type(expr.expr, left_default))
			if left_type != 0 {
				sym := g.table.sym(g.unwrap_generic(left_type))
				if field := g.table.find_field_with_embeds(sym, expr.field_name) {
					mut field_type := field.typ
					match sym.info {
						ast.Struct, ast.Interface, ast.SumType {
							mut generic_names := sym.info.generic_types.map(g.table.sym(it).name)
							mut concrete_types := sym.info.concrete_types.clone()
							if concrete_types.len == 0 && sym.generic_types.len == generic_names.len
								&& sym.generic_types != sym.info.generic_types {
								concrete_types = sym.generic_types.clone()
							}
							mut source_field_type := field.typ
							if sym.info.parent_type.has_flag(.generic) {
								parent_sym := g.table.sym(sym.info.parent_type)
								if parent_field := g.table.find_field_with_embeds(parent_sym,
									expr.field_name)
								{
									source_field_type = parent_field.typ
									match parent_sym.info {
										ast.Struct, ast.Interface, ast.SumType {
											generic_names =
												parent_sym.info.generic_types.map(g.table.sym(it).name)
										}
										else {}
									}
								}
							}
							if generic_names.len == concrete_types.len && concrete_types.len > 0 {
								mut muttable := unsafe { &ast.Table(g.table) }
								resolved_field_type := muttable.unwrap_generic_type_ex(source_field_type,
									generic_names, concrete_types, true)
								if resolved_field_type != source_field_type {
									field_type = resolved_field_type
								} else {
									if converted_field_type := muttable.convert_generic_type(source_field_type,
										generic_names, concrete_types)
									{
										field_type = converted_field_type
									}
								}
							}
						}
						ast.GenericInst {
							parent_sym := g.table.sym(ast.new_type(sym.info.parent_idx))
							mut source_field_type := field.typ
							if parent_field := g.table.find_field_with_embeds(parent_sym,
								expr.field_name)
							{
								source_field_type = parent_field.typ
							}
							match parent_sym.info {
								ast.Struct, ast.Interface, ast.SumType {
									generic_names :=
										parent_sym.info.generic_types.map(g.table.sym(it).name)
									if generic_names.len == sym.info.concrete_types.len
										&& sym.info.concrete_types.len > 0 {
										mut muttable := unsafe { &ast.Table(g.table) }
										resolved_field_type := muttable.unwrap_generic_type_ex(source_field_type,
											generic_names, sym.info.concrete_types, true)
										if resolved_field_type != source_field_type {
											field_type = resolved_field_type
										} else {
											if converted_field_type := muttable.convert_generic_type(source_field_type,
												generic_names, sym.info.concrete_types)
											{
												field_type = converted_field_type
											}
										}
									}
								}
								else {}
							}
						}
						else {}
					}

					$if trace_ci_fixes ? {
						if g.file.path.contains('binary_search_tree.v') && expr.expr is ast.Ident
							&& expr.expr.name == 'tree' {
							eprintln('resolved selector ${expr.expr.name}.${expr.field_name} left=${g.table.type_to_str(left_type)} field=${g.table.type_to_str(field.typ)} final=${g.table.type_to_str(field_type)} expr_typ=${g.table.type_to_str(expr.typ)}')
						}
					}
					mut resolved_type := g.unwrap_generic(g.recheck_concrete_type(field_type))
					if expr.or_block.kind != .absent {
						resolved_type = resolved_type.clear_option_and_result()
					}
					return resolved_type
				}
			}
			if expr.typ != 0 {
				mut resolved_type := g.unwrap_generic(g.recheck_concrete_type(expr.typ))
				if expr.or_block.kind != .absent {
					resolved_type = resolved_type.clear_option_and_result()
				}
				return resolved_type
			}
		}
		ast.IndexExpr {
			left_default := if expr.left_type != 0 { expr.left_type } else { default_typ }
			left_type := g.recheck_concrete_type(g.resolved_expr_type(expr.left, left_default))
			if left_type != 0 {
				if expr.index is ast.RangeExpr {
					mut slice_type := ast.Type(0)
					if expr.left is ast.Ident {
						resolved_left_type :=
							g.resolve_current_fn_generic_param_type(expr.left.name)
						if resolved_left_type != 0 {
							slice_type = g.unwrap_generic(resolved_left_type)
						}
					}
					if slice_type == 0 {
						slice_type = g.unwrap_generic(left_type)
					}
					// Slicing returns a new array by value; strip pointer
					// from mut params.
					slice_type = slice_type.set_nr_muls(0)
					// Slicing a fixed array yields a dynamic array.
					slice_sym := g.table.final_sym(slice_type)
					if slice_sym.info is ast.ArrayFixed {
						return ast.new_type(g.table.find_or_register_array(slice_sym.info.elem_type))
					}
					return slice_type
				}
				if expr.left is ast.Ident {
					resolved_value_type :=
						g.resolve_current_fn_generic_param_value_type(expr.left.name)
					if resolved_value_type != 0 {
						return g.unwrap_generic(resolved_value_type)
					}
				}
				value_type :=
					g.recheck_concrete_type(g.table.value_type(g.unwrap_generic(left_type)))
				if value_type != 0 {
					return g.unwrap_generic(value_type)
				}
				if expr.typ != 0 && !expr.typ.has_flag(.generic)
					&& !g.type_has_unresolved_generic_parts(expr.typ) {
					return g.unwrap_generic(expr.typ)
				}
			}
		}
		ast.InfixExpr {
			if expr.op in [.eq, .ne, .gt, .ge, .lt, .le, .logical_or, .and, .key_in, .not_in, .key_is,
				.not_is] {
				return ast.bool_type
			}
			// In generic contexts, promoted_type may be stale from a different
			// checker instantiation pass. Compute from operand types instead.
			if expr.promoted_type != 0 && !expr.promoted_type.has_flag(.generic)
				&& !g.type_has_unresolved_generic_parts(expr.promoted_type)
				&& (g.cur_fn == unsafe { nil } || g.cur_concrete_types.len == 0) {
				return g.unwrap_generic(g.recheck_concrete_type(expr.promoted_type))
			}
			left_default := if expr.left_type != 0 { expr.left_type } else { default_typ }
			right_default := if expr.right_type != 0 { expr.right_type } else { default_typ }
			left_type := g.resolved_expr_type(expr.left, left_default)
			right_type := g.resolved_expr_type(expr.right, right_default)
			if expr.op in [.plus, .minus, .mul, .power, .div, .mod] {
				if left_type == right_type && left_type != 0
					&& left_type !in [ast.int_literal_type, ast.float_literal_type] {
					return g.unwrap_generic(left_type)
				}
				promoted := g.type_resolver.promote_type(g.unwrap_generic(left_type),
					g.unwrap_generic(right_type))
				if promoted != ast.void_type {
					return g.unwrap_generic(promoted)
				}
			}
			if expr.op in [.left_shift, .right_shift, .amp, .pipe, .xor] && left_type != 0 {
				return g.unwrap_generic(left_type)
			}
		}
		ast.IfExpr {
			inferred_typ := g.infer_if_expr_type(expr)
			if inferred_typ != 0 && inferred_typ != ast.void_type {
				return inferred_typ
			}
		}
		ast.MatchExpr {
			inferred_typ := g.infer_match_expr_type(expr)
			if inferred_typ != 0 && inferred_typ != ast.void_type {
				return inferred_typ
			}
		}
		ast.CallExpr {
			if expr.kind == .type_name {
				return ast.string_type
			}
			if expr.kind == .type_idx {
				return ast.int_type
			}
			resolved := g.resolve_return_type(expr)
			if resolved != ast.void_type {
				return if expr.or_block.kind == .absent {
					g.unwrap_generic(g.recheck_concrete_type(resolved))
				} else {
					g.unwrap_generic(g.recheck_concrete_type(resolved)).clear_option_and_result()
				}
			}
			// When resolve_return_type fails (e.g. fn field call where no method exists),
			// try resolving from return_type_generic using receiver generic type names.
			// This handles cases like Procedure[T,U].handle() calling p.function(p.value)
			// where return_type is contaminated from the last checker pass but
			// return_type_generic preserves the original generic return type (!U).
			if expr.return_type_generic != 0 && expr.return_type_generic.has_flag(.generic)
				&& g.cur_fn != unsafe { nil } && g.cur_fn.is_method
				&& g.cur_fn.receiver.typ.has_flag(.generic) && g.cur_concrete_types.len > 0 {
				receiver_generic_names := g.table.generic_type_names(g.cur_fn.receiver.typ)
				if receiver_generic_names.len == g.cur_concrete_types.len {
					if gen_type := g.table.convert_generic_type(expr.return_type_generic,
						receiver_generic_names, g.cur_concrete_types)
					{
						if !gen_type.has_flag(.generic) {
							return if expr.or_block.kind == .absent {
								gen_type
							} else {
								gen_type.clear_option_and_result()
							}
						}
					}
				}
			}
			if expr.return_type != 0 {
				return if expr.or_block.kind == .absent {
					g.unwrap_generic(g.recheck_concrete_type(expr.return_type))
				} else {
					g.unwrap_generic(g.recheck_concrete_type(expr.return_type)).clear_option_and_result()
				}
			}
		}
		ast.ComptimeCall {
			if expr.kind == .method && g.comptime.comptime_for_method != unsafe { nil } {
				sym := g.table.sym(g.unwrap_generic(expr.left_type))
				if m := sym.find_method(g.comptime.comptime_for_method.name) {
					return m.return_type
				}
			}
		}
		ast.ComptimeSelector {
			if expr.is_method {
				ctyp := g.type_resolver.get_comptime_selector_type(expr, ast.void_type)
				if ctyp != ast.void_type {
					return g.unwrap_generic(ctyp)
				}
				if expr.typ != ast.void_type && expr.typ != 0 {
					return g.unwrap_generic(expr.typ)
				}
			} else if expr.typ_key != '' {
				ctyp := g.type_resolver.get_ct_type_or_default(expr.typ_key, default_typ)
				if ctyp != ast.void_type {
					return g.unwrap_generic(ctyp)
				}
			}
			if expr.left_type != 0 {
				return g.unwrap_generic(expr.left_type)
			}
		}
		ast.CastExpr {
			if expr.typ != 0 {
				return g.unwrap_generic(g.recheck_concrete_type(expr.typ))
			}
		}
		ast.ArrayInit {
			base_array_typ := if expr.generic_typ != 0 { expr.generic_typ } else { expr.typ }
			base_elem_typ := if expr.generic_elem_type != 0 {
				expr.generic_elem_type
			} else {
				expr.elem_type
			}
			if g.cur_fn != unsafe { nil } && g.cur_concrete_types.len > 0 && base_array_typ != 0
				&& expr.exprs.len > 0 && !expr.is_fixed {
				array_type := g.unwrap_generic(g.recheck_concrete_type(base_array_typ))
				if g.table.final_sym(array_type).kind == .array {
					mut inferred_elem_type := ast.void_type
					for i, elem_expr in expr.exprs {
						mut default_elem_type := base_elem_typ
						if default_elem_type == 0 {
							default_elem_type = g.table.value_type(array_type)
						}
						if inferred_elem_type != ast.void_type {
							default_elem_type = inferred_elem_type
						}
						mut resolved_elem_type := ast.void_type
						if elem_expr is ast.Ident {
							resolved_elem_type = g.resolved_scope_var_type(elem_expr)
						}
						if resolved_elem_type == ast.void_type {
							expr_default_type := if expr.expr_types.len > i
								&& expr.expr_types[i] != 0 {
								expr.expr_types[i]
							} else {
								default_elem_type
							}
							resolved_elem_type = g.unwrap_generic(g.recheck_concrete_type(g.resolved_expr_type(elem_expr,
								expr_default_type)))
						}
						if resolved_elem_type != 0 && resolved_elem_type != ast.void_type {
							inferred_elem_type = resolved_elem_type
						}
					}
					if inferred_elem_type != ast.void_type {
						return g.table.find_or_register_array(g.unwrap_generic(inferred_elem_type))
					}
				}
			}
			if base_array_typ != 0 {
				return g.unwrap_generic(g.recheck_concrete_type(base_array_typ))
			}
		}
		ast.StructInit {
			// For `T{}` where T is a generic parameter, typ_str preserves the
			// original name (e.g. 'main.T') even after the type has been resolved
			// to a concrete type. Use it to look up the current concrete type.
			if expr.typ_str.len > 0 && g.cur_fn != unsafe { nil } && g.cur_concrete_types.len > 0 {
				generic_names := g.current_fn_generic_names()
				short_name := expr.typ_str.all_after_last('.')
				idx := generic_names.index(short_name)
				if idx >= 0 && idx < g.cur_concrete_types.len {
					return g.cur_concrete_types[idx]
				}
			}
			// In generic contexts, use generic_typ to allow resolution via
			// recheck_concrete_type/unwrap_generic only when the struct init type
			// itself still has unresolved generic parts (short syntax or generic flag).
			// For explicitly typed inits (e.g. LinkedList[StructFieldInfo]{}),
			// preserve expr.typ to avoid incorrectly substituting the inner struct's
			// generic parameter with the enclosing function's concrete type.
			base_struct_typ := if expr.generic_typ != 0 && g.cur_fn != unsafe { nil }
				&& g.cur_concrete_types.len > 0 {
				if expr.is_short_syntax || expr.typ.has_flag(.generic) || expr.typ == ast.void_type {
					expr.generic_typ
				} else {
					expr.typ
				}
			} else if expr.typ != 0 {
				expr.typ
			} else {
				expr.generic_typ
			}
			if base_struct_typ != 0 {
				return g.unwrap_generic(g.recheck_concrete_type(base_struct_typ))
			}
		}
		ast.MapInit {
			if expr.typ != 0 {
				return g.unwrap_generic(g.recheck_concrete_type(expr.typ))
			}
		}
		ast.AsCast {
			return g.unwrap_generic(g.recheck_concrete_type(expr.typ))
		}
		ast.UnsafeExpr {
			return g.resolved_expr_type(expr.expr, default_typ)
		}
		ast.PrefixExpr {
			right_default := if expr.right_type != 0 { expr.right_type } else { default_typ }
			inner_type := g.resolved_expr_type(expr.right, right_default)
			return match expr.op {
				.amp {
					// When the inner expr is an auto-deref var (e.g. mut param),
					// codegen skips emitting & since the var is already a pointer.
					// Don't add .ref() to match.
					if expr.right.is_auto_deref_var() {
						g.unwrap_generic(inner_type)
					} else {
						g.unwrap_generic(inner_type).ref()
					}
				}
				.mul {
					resolved_inner_type := g.unwrap_generic(inner_type)
					if resolved_inner_type.is_ptr() {
						resolved_inner_type.deref()
					} else {
						resolved_right_type :=
							g.unwrap_generic(g.recheck_concrete_type(right_default))
						if resolved_right_type.is_ptr() {
							resolved_right_type.deref()
						} else {
							resolved_inner_type
						}
					}
				}
				.arrow {
					right_sym := g.table.final_sym(g.unwrap_generic(inner_type))
					if right_sym.kind == .chan {
						g.unwrap_generic(right_sym.chan_info().elem_type)
					} else {
						g.unwrap_generic(inner_type)
					}
				}
				else {
					g.unwrap_generic(inner_type)
				}
			}
		}
		ast.PostfixExpr {
			inner_default := if expr.typ != 0 { expr.typ } else { default_typ }
			inner_type := g.resolved_expr_type(expr.expr, inner_default)
			return if expr.op == .question {
				mut resolved_postfix_type := g.unwrap_generic(inner_type)
				if resolved_postfix_type != 0 && g.table.sym(resolved_postfix_type).kind == .alias {
					unaliased_postfix_type := g.table.unaliased_type(resolved_postfix_type)
					if unaliased_postfix_type.has_option_or_result() {
						resolved_postfix_type = g.unwrap_generic(unaliased_postfix_type)
					}
				}
				resolved_postfix_type.clear_option_and_result()
			} else {
				g.unwrap_generic(inner_type)
			}
		}
		else {}
	}

	resolved := g.type_resolver.get_type_or_default(expr, default_typ)
	if resolved != 0 {
		return g.unwrap_generic(resolved)
	}
	return g.unwrap_generic(default_typ)
}

struct Type {
	// typ is the original type
	typ ast.Type        @[required]
	sym &ast.TypeSymbol @[required]
	// unaliased is `typ` once aliased have been resolved
	// it may not contain information such as flags and nr_muls
	unaliased     ast.Type        @[required]
	unaliased_sym &ast.TypeSymbol @[required]
}

// unwrap returns the following variants of a type:
// * generics unwrapped
// * alias unwrapped
fn (mut g Gen) unwrap(typ ast.Type) Type {
	no_generic := g.unwrap_generic(typ)
	no_generic_sym := g.table.sym(no_generic)
	if no_generic_sym.kind != .alias {
		return Type{
			typ:           no_generic
			sym:           no_generic_sym
			unaliased:     no_generic
			unaliased_sym: no_generic_sym
		}
	}
	return Type{
		typ:           no_generic
		sym:           no_generic_sym
		unaliased:     no_generic_sym.parent_idx
		unaliased_sym: g.table.sym(ast.idx_to_type(no_generic_sym.parent_idx))
	}
}

// generate function variable definition, e.g. `void (*var_name) (int, string)`
fn (mut g Gen) fn_var_signature(var_type ast.Type, return_type ast.Type, arg_types []ast.Type, var_name string) string {
	if var_type.has_flag(.option) || var_type.has_flag(.result) {
		return '${g.styp(var_type)} ${c_name(var_name)}'
	}
	ret_styp := g.styp(return_type)
	nr_muls := var_type.nr_muls()
	mut sig := '${ret_styp} (${'*'.repeat(nr_muls + 1)}${c_name(var_name)}) ('
	for j, arg_typ in arg_types {
		arg_sym := g.table.sym(arg_typ)
		if arg_sym.info is ast.FnType {
			func := arg_sym.info.func
			arg_sig := g.fn_var_signature(arg_typ, func.return_type, func.params.map(it.typ), '')
			sig += arg_sig
		} else {
			arg_styp := g.styp(arg_typ)
			sig += arg_styp
		}
		if j < arg_types.len - 1 {
			sig += ', '
		}
	}
	sig += ')'
	return sig
}

// generate anon fn cname, e.g. `anon_fn_void_int_string`, `anon_fn_void_int_ptr_string`
fn (mut g Gen) anon_fn_cname(return_type ast.Type, arg_types []ast.Type) string {
	ret_styp := g.styp(return_type).replace('*', '_ptr')
	mut sig := 'anon_fn_${ret_styp}_'
	for j, arg_typ in arg_types {
		arg_sym := g.table.sym(arg_typ)
		if arg_sym.info is ast.FnType {
			sig += g.anon_fn_cname(arg_sym.info.func.return_type,
				arg_sym.info.func.params.map(it.typ))
		} else {
			sig += g.styp(arg_typ).replace('*', '_ptr')
		}
		if j < arg_types.len - 1 {
			sig += '_'
		}
	}
	return sig
}

// escape quotes for string
fn escape_quotes(val string) string {
	bs := '\\'
	unescaped_val := val.replace('${bs}${bs}', '\x01').replace_each([
		"${bs}'",
		"'",
		'${bs}"',
		'"',
	])
	return unescaped_val.replace_each(['\x01', '${bs}${bs}', "'", "${bs}'", '"', '${bs}"'])
}

@[inline]
fn (mut g Gen) dot_or_ptr(val_type ast.Type) string {
	return if val_type.has_flag(.shared_f) {
		'->val.'
	} else if val_type.is_ptr() {
		'->'
	} else {
		'.'
	}
}

fn (mut g Gen) unwrap_option_type(typ ast.Type, name string, is_auto_heap bool) {
	styp := g.base_type(typ)
	if is_auto_heap {
		g.write('(*(${styp}*)${name}->data)')
	} else {
		type_sym := g.table.sym(typ)
		if type_sym.kind == .alias {
			// Alias to Option type
			parent_typ := (type_sym.info as ast.Alias).parent_type
			if parent_typ.has_flag(.option) {
				g.write('*((${g.base_type(parent_typ)}*)')
			}
			g.write('(*(${styp}*)${name}.data)')
			if parent_typ.has_flag(.option) {
				g.write('.data)')
			}
		} else if typ.has_flag(.option_mut_param_t) {
			g.write('(*(${styp}*)${name}->data)')
		} else {
			g.write('(*(${styp}*)${name}.data)')
		}
	}
}
