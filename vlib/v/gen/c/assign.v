// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module c

import v.ast
import v.util
import v.token

fn smartcast_selector_expr_str(expr ast.SelectorExpr) string {
	mut expr_str := expr.expr.str()
	if expr.expr is ast.ParExpr && expr.expr.expr is ast.AsCast {
		expr_str = expr.expr.expr.expr.str()
	}
	return expr_str
}

fn (g &Gen) is_smartcast_assign_lhs(expr ast.Expr) bool {
	match expr {
		ast.Ident {
			if expr.obj is ast.Var && expr.obj.smartcasts.len > 0 {
				if expr.obj.is_mut && expr.obj.orig_type != 0 {
					orig_sym := g.table.final_sym(expr.obj.orig_type)
					if orig_sym.kind == .sum_type {
						return false
					}
				}
				return true
			}
			return false
		}
		ast.SelectorExpr {
			if expr.expr_type == 0 {
				return false
			}
			scope_field := expr.scope.find_struct_field(smartcast_selector_expr_str(expr),
				expr.expr_type, expr.field_name)
			if scope_field == unsafe { nil } || scope_field.smartcasts.len == 0 {
				return false
			}
			// Option field smartcast on LHS: the assignment replaces the option
			// as a whole (e.g. `s.x = 10` or `s.x = none` inside `if s.x != none`),
			// so skip unwrap treatment that's only meant for sumtype reassignments.
			if scope_field.orig_type.has_flag(.option) {
				return false
			}
			return true
		}
		else {
			return false
		}
	}
}

fn (mut g Gen) expr_in_value_context(expr ast.Expr, value_type ast.Type, expected_type ast.Type) {
	mut expr_copy := expr
	match mut expr_copy {
		ast.IfExpr {
			if !expr_copy.is_expr || expr_copy.typ == 0 || expr_copy.typ == ast.void_type {
				expr_copy.is_expr = true
				expr_copy.typ = if value_type != 0 && value_type != ast.void_type {
					value_type
				} else {
					expected_type
				}
			}
		}
		ast.MatchExpr {
			if !expr_copy.is_expr || expr_copy.return_type == 0
				|| expr_copy.return_type == ast.void_type {
				resolved_value_type := if value_type != 0 && value_type != ast.void_type {
					value_type
				} else {
					expected_type
				}
				expr_copy.is_expr = true
				expr_copy.return_type = resolved_value_type
				if expr_copy.expected_type in [0, ast.void_type, ast.none_type] {
					expr_copy.expected_type = expected_type
				}
			}
		}
		else {}
	}

	g.expr(expr_copy)
}

fn (mut g Gen) auto_heap_assignment_uses_existing_storage(expr ast.Expr, expr_type ast.Type) bool {
	// Reusing container-backed storage avoids leaking a fresh HEAP() copy for
	// `@[heap]` values like `x := arr[i]`. Keep the old path under autofree,
	// because aliasing array storage there would hand ownership to the local.
	if g.is_autofree {
		return false
	}
	return match expr {
		ast.IndexExpr { g.auto_heap_array_index_uses_existing_storage(expr, expr_type) }
		else { false }
	}
}

fn (mut g Gen) auto_heap_array_index_uses_existing_storage(node ast.IndexExpr, expr_type ast.Type) bool {
	if node.index is ast.RangeExpr || node.or_expr.kind != .absent || node.is_option {
		return false
	}
	mut resolved_expr_type := g.recheck_concrete_type(g.resolved_expr_type(ast.Expr(node),
		expr_type))
	if resolved_expr_type == 0 || resolved_expr_type == ast.void_type {
		resolved_expr_type = g.recheck_concrete_type(expr_type)
	}
	if resolved_expr_type == 0 || resolved_expr_type == ast.void_type {
		resolved_expr_type = g.recheck_concrete_type(node.typ)
	}
	elem_type := g.unwrap_generic(resolved_expr_type)
	if elem_type == 0 || elem_type.is_ptr() || !g.table.final_sym(elem_type).is_heap() {
		return false
	}
	mut resolved_left_type := g.recheck_concrete_type(g.resolved_expr_type(node.left,
		node.left_type))
	if resolved_left_type == 0 || resolved_left_type == ast.void_type {
		resolved_left_type = g.recheck_concrete_type(node.left_type)
	}
	left_sym := g.table.final_sym(g.unwrap_generic(resolved_left_type))
	return left_sym.kind == .array
}

fn (mut g Gen) write_auto_heap_assignment_expr(expr ast.Expr, expr_type ast.Type) bool {
	return match expr {
		ast.IndexExpr { g.write_auto_heap_array_index_expr(expr, expr_type) }
		else { false }
	}
}

fn (mut g Gen) write_auto_heap_array_index_expr(node ast.IndexExpr, expr_type ast.Type) bool {
	if !g.auto_heap_array_index_uses_existing_storage(node, expr_type) {
		return false
	}
	mut resolved_expr_type := g.recheck_concrete_type(g.resolved_expr_type(ast.Expr(node),
		expr_type))
	if resolved_expr_type == 0 || resolved_expr_type == ast.void_type {
		resolved_expr_type = g.recheck_concrete_type(expr_type)
	}
	if resolved_expr_type == 0 || resolved_expr_type == ast.void_type {
		resolved_expr_type = g.recheck_concrete_type(node.typ)
	}
	elem_type := g.unwrap_generic(resolved_expr_type)
	elem_type_str := g.styp(elem_type)
	mut resolved_left_type := g.recheck_concrete_type(g.resolved_expr_type(node.left,
		node.left_type))
	if resolved_left_type == 0 || resolved_left_type == ast.void_type {
		resolved_left_type = g.recheck_concrete_type(node.left_type)
	}
	left_type := if resolved_left_type != 0 { resolved_left_type } else { node.left_type }
	left_is_ptr := left_type.is_ptr() || node.left.is_auto_deref_var()
	left_is_shared := left_type.has_flag(.shared_f)
	array_get_fn := if node.is_gated { 'builtin__array_get_ni' } else { 'builtin__array_get' }
	g.write('((${elem_type_str}*)${array_get_fn}(')
	if left_is_ptr && !left_is_shared {
		g.write('*')
	}
	if node.left is ast.IndexExpr {
		g.inside_array_index = true
		g.expr(ast.Expr(node.left))
		g.inside_array_index = false
	} else {
		g.expr(ast.Expr(node.left))
	}
	if left_is_shared {
		if left_is_ptr {
			g.write('->val')
		} else {
			g.write('.val')
		}
	}
	g.write(', ')
	g.expr(node.index)
	g.write('))')
	return true
}

fn (mut g Gen) write_assign_target_expr(left ast.Expr, var_type ast.Type) {
	if left.is_auto_deref_var() && !var_type.has_flag(.shared_f) {
		g.write('*')
	}
	g.expr(left)
	if var_type.has_flag(.shared_f) {
		g.write('->val')
	}
}

fn (mut g Gen) write_assign_value_expr(left ast.Expr, var_type ast.Type) {
	old_is_assign_lhs := g.is_assign_lhs
	g.is_assign_lhs = false
	defer {
		g.is_assign_lhs = old_is_assign_lhs
	}
	if left.is_auto_deref_var() && !var_type.has_flag(.shared_f) {
		g.write('*')
	}
	g.expr(left)
	if var_type.has_flag(.shared_f) {
		g.write('->val')
	}
}

fn (mut g Gen) gen_power_assign_expr(left ast.Expr, left_type ast.Type, right ast.Expr, right_type ast.Type) {
	power_result_type := g.normalized_power_result_type(left_type.clear_flag(.shared_f).clear_flag(.atomic_f),
		left_type.clear_flag(.shared_f).clear_flag(.atomic_f), right_type)
	builtin_power_type := g.table.unalias_num_type(power_result_type)
	result_styp := g.styp(power_result_type)
	g.uses_power = true
	if builtin_power_type == ast.f32_type {
		g.write('(${result_styp})powf((${g.styp(ast.f32_type)})(')
		g.write_assign_value_expr(left, left_type)
		g.write('), ')
		g.expr_with_cast(right, right_type, ast.f32_type)
		g.write(')')
		return
	}
	if builtin_power_type.is_float() {
		g.write('(${result_styp})pow((${g.styp(ast.f64_type)})(')
		g.write_assign_value_expr(left, left_type)
		g.write('), ')
		g.expr_with_cast(right, right_type, ast.f64_type)
		g.write(')')
		return
	}
	if builtin_power_type.is_unsigned() {
		g.uses_power_u64 = true
		g.write('(${result_styp})__v_pow_u64((${g.styp(ast.u64_type)})(')
		g.write_assign_value_expr(left, left_type)
		g.write('), ')
		g.expr_with_cast(right, right_type, ast.i64_type)
		g.write(')')
		return
	}
	g.write('(${result_styp})__v_pow_i64((${g.styp(ast.i64_type)})(')
	g.write_assign_value_expr(left, left_type)
	g.write('), ')
	g.expr_with_cast(right, right_type, ast.i64_type)
	g.write(')')
}

fn assign_expr_unwraps_option_or_result(expr ast.Expr) bool {
	return match expr {
		ast.CallExpr { expr.or_block.kind != .absent }
		ast.ComptimeCall { expr.or_block.kind != .absent }
		ast.ComptimeSelector { expr.or_block.kind != .absent }
		ast.Ident { expr.or_expr.kind != .absent }
		ast.IndexExpr { expr.or_expr.kind != .absent && !expr.typ.has_option_or_result() }
		ast.InfixExpr { expr.or_block.kind != .absent }
		ast.PostfixExpr { expr.op == .question }
		ast.PrefixExpr { expr.or_block.kind != .absent }
		ast.SelectorExpr { expr.or_block.kind != .absent }
		else { false }
	}
}

fn (mut g Gen) decl_assign_struct_init_needs_tmp(expr ast.Expr) bool {
	mut node := ast.StructInit{}
	match expr {
		ast.StructInit {
			node = expr
		}
		ast.ParExpr {
			if expr.expr !is ast.StructInit {
				return false
			}
			node = expr.expr as ast.StructInit
		}
		else {
			return false
		}
	}

	sym := g.table.final_sym(g.unwrap_generic(g.recheck_concrete_type(node.typ)))
	if sym.info !is ast.Struct {
		return false
	}
	if g.styp(node.typ) in skip_struct_init {
		return false
	}
	info := sym.info as ast.Struct
	if node.no_keys {
		return node.init_fields.len < info.fields.len
	}
	init_field_names := node.init_fields.map(it.name)
	return info.fields.any(it.name !in init_field_names)
}

fn (mut g Gen) gen_self_recursing_anon_fn_capture_patch(left ast.Expr, anon_fn ast.AnonFn) {
	if left !is ast.Ident || anon_fn.inherited_vars.len == 0 {
		return
	}
	ident := left as ast.Ident
	if ident.name !in anon_fn.inherited_vars.map(it.name) {
		return
	}
	ctx_struct := g.closure_ctx(anon_fn.decl)
	left_expr := g.expr_string(left)
	g.writeln('((${ctx_struct}*)builtin__closure__closure_data(${left_expr}))->${c_name(ident.name)} = ${left_expr};')
}

fn (mut g Gen) expr_with_opt_or_block(expr ast.Expr, expr_typ ast.Type, var_expr ast.Expr, ret_typ ast.Type,
	in_heap bool) {
	gen_or := expr is ast.Ident && expr.or_expr.kind != .absent
	if gen_or {
		old_inside_opt_or_res := g.inside_opt_or_res
		g.inside_opt_or_res = true
		expr_var := if expr is ast.Ident && expr.kind == .constant {
			g.c_const_name(expr.name)
		} else if expr is ast.Ident && expr.is_auto_heap() {
			'(*${expr.name})'
		} else {
			'${expr}'
		}
		dot_or_ptr := if !expr_typ.has_flag(.option_mut_param_t) { '.' } else { '-> ' }
		mut heap_line := ''
		if in_heap {
			// When the variable needs heap allocation, the caller has already
			// written the partial line `TYPE *var = HEAP(TYPE, (`.
			// We must NOT access the option's .data inside the HEAP macro
			// before checking the state, because the option may be `none`.
			// Pull back the partial line, emit the state check first, then
			// write the assignment with data access after the check.
			heap_line = g.go_before_last_stmt()
			g.empty_line = true
		} else {
			g.expr_with_cast(expr, expr_typ, ret_typ)
			g.writeln(';')
		}
		g.writeln('if (${c_name(expr_var)}${dot_or_ptr}state != 0) { // assign')
		if expr is ast.Ident && expr.or_expr.kind == .propagate_option {
			g.writeln('\tbuiltin__panic_option_not_set(_S("none"));')
		} else {
			g.inside_or_block = true
			defer {
				g.inside_or_block = false
			}
			or_expr := (expr as ast.Ident).or_expr
			stmts := or_expr.stmts
			scope := or_expr.scope
			last_stmt := stmts.last()
			// handles stmt block which returns something
			// e.g. { return none }
			if stmts.len > 0 && last_stmt is ast.ExprStmt && last_stmt.typ != ast.void_type {
				var_expr_name := c_name(var_expr.str())
				if last_stmt.expr is ast.Ident && last_stmt.expr.or_expr.kind != .absent {
					g.write('${var_expr_name} = ')
					g.expr_with_opt_or_block(ast.Expr(last_stmt.expr), last_stmt.typ, var_expr,
						ret_typ, in_heap)
				} else {
					g.gen_or_block_stmts(var_expr_name, '', stmts, ret_typ, false, scope,
						expr.pos())
				}
			} else {
				// handles stmt block which doesn't returns value
				// e.g. { return }
				g.stmts(stmts)
				if stmts.len > 0 && last_stmt is ast.ExprStmt {
					g.writeln(';')
				}
				g.write_defer_stmts(scope, false, expr.pos())
			}
		}
		g.writeln('}')
		if in_heap {
			// Now that the state has been checked (and we haven't returned/panicked),
			// it is safe to access .data and do the heap allocation.
			g.write(heap_line)
			g.expr_with_cast(expr, expr_typ, ret_typ)
			g.writeln('));')
		}
		g.inside_opt_or_res = old_inside_opt_or_res
	} else {
		g.expr_with_opt(expr, expr_typ, ret_typ)
		if in_heap {
			g.write('))')
		}
	}
}

// expr_opt_with_alias handles conversion from different option alias type name
fn (mut g Gen) expr_opt_with_alias(expr ast.Expr, expr_typ ast.Type, ret_typ ast.Type) string {
	styp := g.base_type(ret_typ)

	line := g.go_before_last_stmt().trim_space()
	g.empty_line = true

	ret_var := g.new_tmp_var()
	ret_styp := g.styp(ret_typ).replace('*', '_ptr')
	g.writeln('${ret_styp} ${ret_var} = {.state=2, .err=_const_none__, .data={E_STRUCT}};')

	if expr !is ast.None {
		is_option_expr := expr_typ.has_flag(.option)
		if is_option_expr {
			g.write('builtin___option_clone((${option_name}*)')
		} else {
			g.write('builtin___option_ok(&(${styp}[]){ ')
		}
		has_addr := is_option_expr && expr !in [ast.Ident, ast.SelectorExpr]
		if has_addr {
			expr_styp := g.styp(expr_typ).replace('*', '_ptr')
			g.write('ADDR(${expr_styp}, ')
		} else if is_option_expr {
			g.write('&')
		}
		g.expr(expr)
		if has_addr {
			g.write(')')
		}
		if !is_option_expr {
			g.write(' }')
		}
		g.writeln(', (${option_name}*)&${ret_var}, sizeof(${styp}));')
	}
	g.write(line)
	if g.inside_return {
		g.write(' ')
	}
	g.write(ret_var)
	return ret_var
}

// expr_opt_with_cast is used in cast expr when converting compatible option types
// e.g. ?int(?u8(0))
fn (mut g Gen) expr_opt_with_cast(expr ast.Expr, expr_typ ast.Type, ret_typ ast.Type) string {
	if !expr_typ.has_flag(.option) || !ret_typ.has_flag(.option) {
		panic('cgen: expected expr_type and ret_typ to be options')
	}

	if expr_typ.idx() == ret_typ.idx() && g.table.sym(expr_typ).kind != .alias {
		return g.expr_with_opt(expr, expr_typ, ret_typ)
	} else {
		if expr is ast.CallExpr && expr.return_type.has_flag(.option) {
			return g.expr_opt_with_alias(expr, expr_typ, ret_typ)
		} else {
			past := g.past_tmp_var_new()
			defer {
				g.past_tmp_var_done(past)
			}
			unwrapped_ret := g.unwrap_generic(ret_typ)
			// Unwrap type aliases to ensure sizeof uses the base type size, not the alias size
			// This fixes the ASAN stack-buffer-overflow issue when using type aliases like MaybeInt = ?int
			unaliased_ret := g.table.unaliased_type(unwrapped_ret)
			styp := g.base_type(unaliased_ret)
			decl_styp := g.styp(unwrapped_ret).replace('*', '_ptr')
			g.writeln('${decl_styp} ${past.tmp_var};')
			is_none := expr is ast.CastExpr && expr.expr is ast.None
			if is_none {
				g.write('builtin___option_none(&(${styp}[]) {')
			} else {
				g.write('builtin___option_ok(&(${styp}[]) {')
			}
			if expr is ast.CastExpr && expr_typ.has_flag(.option) {
				ret_sym := g.table.sym(ret_typ)
				if ret_sym.kind == .sum_type {
					exp_sym := g.table.sym(expr_typ)
					fname := g.get_sumtype_casting_fn(expr_typ, ret_typ)
					g.call_cfn_for_casting_expr(fname, expr, ret_typ, expr_typ, expr_typ,
						ret_sym.cname, expr_typ.is_ptr(), exp_sym.kind == .function,
						g.styp(expr_typ))
				} else {
					g.write('*((${g.base_type(expr_typ)}*)')
					g.expr(expr)
					g.write('.data)')
				}
			} else {
				old_inside_opt_or_res := g.inside_opt_or_res
				g.inside_opt_or_res = false
				g.expr_with_cast(expr, expr_typ, ret_typ)
				g.inside_opt_or_res = old_inside_opt_or_res
			}
			g.writeln(' }, (${option_name}*)(&${past.tmp_var}), sizeof(${styp}));')
			return past.tmp_var
		}
	}
}

// expr_with_opt is used in assigning an expression to an `option` variable
// e.g. x = y (option lhs and rhs), mut x = ?int(123), y = none
fn (mut g Gen) expr_with_opt(expr ast.Expr, expr_typ ast.Type, ret_typ ast.Type) string {
	old_inside_opt_or_res := g.inside_opt_or_res
	g.inside_opt_or_res = true
	defer {
		g.inside_opt_or_res = old_inside_opt_or_res
	}
	unwrapped_expr_typ := g.unwrap_generic(expr_typ)
	unwrapped_ret_typ := g.unwrap_generic(ret_typ)
	if unwrapped_expr_typ.has_flag(.option) && unwrapped_ret_typ.has_flag(.option)
		&& !g.is_arraymap_set
		&& expr in [ast.SelectorExpr, ast.DumpExpr, ast.Ident, ast.ComptimeSelector, ast.ComptimeCall, ast.AsCast, ast.CallExpr, ast.MatchExpr, ast.IfExpr, ast.IndexExpr, ast.UnsafeExpr, ast.CastExpr] {
		if expr in [ast.Ident, ast.CastExpr] {
			if unwrapped_expr_typ.idx() != unwrapped_ret_typ.idx()
				&& g.table.type_to_str(unwrapped_expr_typ) != g.table.type_to_str(unwrapped_ret_typ) {
				return g.expr_opt_with_cast(expr, unwrapped_expr_typ, unwrapped_ret_typ)
			}
		}
		g.expr(expr)
		if expr is ast.ComptimeSelector {
			return g.gen_comptime_selector(expr)
		} else {
			return expr.str()
		}
	} else {
		tmp_out_var := g.new_tmp_var()
		g.expr_with_tmp_var(expr, unwrapped_expr_typ, unwrapped_ret_typ, tmp_out_var, true)
		return tmp_out_var
	}
	return ''
}

fn (g &Gen) static_init_guard_name(pos token.Pos) string {
	return '_vstatic_init_${pos.pos}'
}

fn (mut g Gen) gen_static_decl_runtime_init(node ast.AssignStmt, left ast.Expr, left_type ast.Type, right ast.Expr, right_type ast.Type) bool {
	if node.left.len != 1 || node.right.len != 1 || g.inside_ternary != 0 {
		return false
	}
	if left !is ast.Ident {
		return false
	}
	guard_name := g.static_init_guard_name(left.pos())
	g.writeln(';')
	g.writeln('static bool ${guard_name};')
	g.writeln('if (!${guard_name}) {')
	g.indent++
	g.writeln('${guard_name} = true;')
	old_is_assign_lhs := g.is_assign_lhs
	g.is_assign_lhs = false
	g.assign_stmt(ast.AssignStmt{
		op:          .assign
		pos:         node.pos
		left:        [left]
		right:       [right]
		left_types:  [left_type]
		right_types: [right_type]
	})
	g.is_assign_lhs = old_is_assign_lhs
	g.indent--
	g.writeln('}')
	return true
}

fn (mut g Gen) assign_stmt(node_ ast.AssignStmt) {
	mut node := unsafe { node_ }
	if node.is_static {
		is_defer_var := node.left[0] is ast.Ident && node.left[0].name in g.defer_vars
		if is_defer_var && node.op == .decl_assign {
			return
		}
		if !is_defer_var {
			g.write('static ')
		}
	}
	if node.is_volatile && node.left[0] is ast.Ident && node.left[0].name !in g.defer_vars {
		g.write('volatile ')
	}
	mut return_type := ast.void_type
	is_decl := node.op == .decl_assign
	g.assign_op = node.op
	g.inside_assign = true
	g.arraymap_set_pos = 0
	g.is_arraymap_set = false
	g.is_assign_lhs = false
	g.is_shared = false
	defer {
		g.assign_op = .unknown
		g.inside_assign = false
		g.assign_ct_type.clear()
		g.arraymap_set_pos = 0
		g.is_arraymap_set = false
		g.is_assign_lhs = false
		g.is_shared = false
	}
	op := if is_decl { token.Kind.assign } else { node.op }
	right_expr := node.right[0]
	match right_expr {
		ast.CallExpr {
			resolved_call_type := g.resolve_return_type(right_expr)
			if resolved_call_type != ast.void_type {
				return_type = resolved_call_type
			} else {
				return_type = right_expr.return_type
			}
		}
		ast.LockExpr {
			return_type = right_expr.typ
		}
		ast.MatchExpr {
			return_type = right_expr.return_type
		}
		ast.IfExpr {
			return_type = right_expr.typ
		}
		else {}
	}

	if node.right.len == 1 && node.left.len > 1 && node.left_types.len == node.left.len {
		is_generic_context := g.cur_fn != unsafe { nil } && g.cur_concrete_types.len > 0
		concrete_left_types := if is_generic_context {
			node.left_types.map(g.unwrap_generic(g.recheck_concrete_type(it)))
		} else {
			node.left_types
		}
		if concrete_left_types.all(it != 0 && !it.has_flag(.generic)
			&& !g.type_has_unresolved_generic_parts(it))
		{
			expected_multi_return := g.table.find_or_register_multi_return(concrete_left_types)
			if return_type == ast.void_type || return_type == 0 {
				return_type = expected_multi_return
			} else if g.table.sym(return_type).kind == .multi_return
				&& return_type != expected_multi_return {
				actual_return_types := if is_generic_context {
					(g.table.sym(return_type).info as ast.MultiReturn).types.map(g.unwrap_generic(g.recheck_concrete_type(it)))
				} else {
					(g.table.sym(return_type).info as ast.MultiReturn).types
				}
				if actual_return_types.any(it == 0 || it.has_flag(.generic)
					|| g.type_has_unresolved_generic_parts(it))
				{
					return_type = expected_multi_return
				}
			}
		}
	}
	// Free the old value assigned to this string var (only if it's `str = [new value]`
	// or `x.str = [new value]` )
	mut af := g.is_autofree && !g.is_builtin_mod && !g.is_autofree_tmp && node.left_types.len == 1
		&& node.left[0] in [ast.Ident, ast.SelectorExpr] && (node.op == .assign
		|| (node.op == .plus_assign && node.left_types[0] == ast.string_type))
	if af && node.right.len == 1 && node.right[0] is ast.CallExpr {
		call_expr := node.right[0] as ast.CallExpr
		if call_expr.is_method && call_expr.left is ast.CallExpr {
			af = false
		}
	}
	mut sref_name := ''
	mut type_to_free := ''
	if af {
		first_left_type := node.left_types[0]
		first_left_sym := g.table.sym(node.left_types[0])
		if first_left_type == ast.string_type
			|| (node.op == .assign && first_left_sym.kind == .array) {
			type_to_free = if first_left_type == ast.string_type { 'string' } else { 'array' }
			mut ok := true
			left0 := node.left[0]
			if left0 is ast.Ident {
				if left0.name == '_' {
					ok = false
				}
			}
			if ok {
				sref_name = '_sref${node.pos.pos}'
				g.write('${type_to_free} ${sref_name} = (') // TODO: we are copying the entire string here, optimize
				// we can't just do `.str` since we need the extra data from the string struct
				// doing `&string` is also not an option since the stack memory with the data will be overwritten
				if left0.is_auto_deref_var() && !first_left_type.has_flag(.shared_f) {
					g.write('*')
				}
				g.expr(left0) // node.left[0])
				if first_left_type.has_flag(.shared_f) {
					g.write('->val')
				}
				g.writeln('); // free ${type_to_free} on re-assignment2')
				defer(fn) {
					if af {
						g.writeln('builtin__${type_to_free}_free(&${sref_name});')
					}
				}
			} else {
				af = false
			}
		} else {
			af = false
		}
	}
	// TODO: g.gen_assign_vars_autofree(node)
	// json_test failed w/o this check
	if node.right.len == 1 && return_type != ast.void_type && return_type != 0 {
		sym := g.table.sym(return_type)
		if sym.kind == .multi_return {
			g.gen_multi_return_assign(node, return_type, sym)
			return
		}
	}
	// TODO: non idents on left (exprs)
	if node.has_cross_var {
		g.gen_cross_var_assign(node)
	}
	// `a := 1` | `a,b := 1,2`
	if node.right.len < node.left.len {
		g.checker_bug('node.right.len < node.left.len', node.pos)
	}
	if node.right_types.len < node.left.len {
		g.checker_bug('node.right_types.len < node.left.len', node.pos)
	}
	if node.left_types.len < node.left.len {
		g.checker_bug('node.left_types.len < node.left.len', node.pos)
	}

	last_curr_var_name := g.curr_var_name.clone()
	defer {
		g.curr_var_name = last_curr_var_name
	}
	g.curr_var_name = []

	for i, mut left in node.left {
		mut is_auto_heap := false
		mut is_fn_var := false
		mut var_type := node.left_types[i]
		mut val_type := node.right_types[i]
		// Save original shared status of val_type before resolution blocks can overwrite it.
		// This is needed because `val_type = var_type` in resolution blocks can lose the
		// RHS shared flag (e.g., lock expr returning shared value to non-shared variable).
		orig_val_shared := val_type.has_flag(.shared_f)
		if g.cur_fn != unsafe { nil } && g.cur_concrete_types.len > 0 {
			resolved_left_type := g.recheck_concrete_type(var_type)
			if resolved_left_type != 0 {
				var_type = g.unwrap_generic(resolved_left_type)
			}
			resolved_right_type := g.recheck_concrete_type(val_type)
			if resolved_right_type != 0 {
				val_type = g.unwrap_generic(resolved_right_type)
			}
		}
		mut val := node.right[i]
		mut str_add_rhs_tmp := ''
		mut str_add_rhs_needs_free := false
		mut skip_str_add_rhs_clone := false
		if is_decl && g.cur_concrete_types.len > 0 && val is ast.CallExpr
			&& val.return_type_generic != 0 {
			mut resolved_val_type := g.resolve_return_type(val).clear_option_and_result()
			if resolved_val_type == ast.void_type || resolved_val_type.has_flag(.generic) {
				resolved_val_type =
					g.unwrap_generic(val.return_type_generic).clear_option_and_result()
			}
			// When unwrap_generic couldn't resolve (e.g. cur_fn.generic_names is empty
			// for methods whose generics come from the receiver), try resolving using
			// the receiver's generic type names from the current fn.
			if (resolved_val_type == ast.void_type || resolved_val_type.has_flag(.generic)
				|| g.type_has_unresolved_generic_parts(resolved_val_type))
				&& g.cur_fn != unsafe { nil } && g.cur_fn.is_method
				&& g.cur_fn.receiver.typ.has_flag(.generic) {
				receiver_generic_names := g.table.generic_type_names(g.cur_fn.receiver.typ)
				if receiver_generic_names.len == g.cur_concrete_types.len {
					if gen_type := g.table.convert_generic_type(val.return_type_generic,
						receiver_generic_names, g.cur_concrete_types)
					{
						resolved_val_type = gen_type.clear_option_and_result()
					}
				}
			}
			if resolved_val_type != ast.void_type && !resolved_val_type.has_flag(.generic) {
				var_type = ast.mktyp(resolved_val_type)
				val_type = resolved_val_type
			}
		}
		mut is_call := false
		mut gen_or := false
		mut blank_assign := false
		mut is_va_list := false // C varargs
		mut ident := ast.Ident{
			scope: unsafe { nil }
		}
		if is_decl {
			// Strip shared/atomic flags and pointer from the default type
			// so that `resolved_expr_type` doesn't propagate LHS declaration
			// properties to the resolved RHS type (e.g., for string/int literals
			// that fall through to the default).
			mut decl_default := var_type.clear_flag(.shared_f).clear_flag(.atomic_f)
			if var_type.has_flag(.shared_f) && decl_default.nr_muls() > 0 {
				decl_default = decl_default.set_nr_muls(decl_default.nr_muls() - 1)
			}
			// Check if this variable is auto-heap promoted early so we can
			// skip resolved_expr_type updates that would add extra pointer
			// levels (the HEAP macro handles the pointer promotion).
			left_is_auto_heap := left is ast.Ident && g.resolved_ident_is_auto_heap_not_stack(left)
			resolved_decl_type := g.resolved_expr_type(val, decl_default)
			if resolved_decl_type != 0 && resolved_decl_type != ast.void_type && !left_is_auto_heap {
				mut resolved_unwrapped :=
					g.unwrap_generic(g.recheck_concrete_type(resolved_decl_type))
				// Don't propagate pointer flag from auto-deref mut parameters.
				// `mut e := expr` where expr is a mut param should create a value copy.
				// Also handles the case where var_type is already a pointer (e.g.
				// `c := head` where head is `mut &Client` → var_type is &Client
				// but resolved_expr_type returns &&Client). Compare nr_muls to
				// deref only when there's an extra level from auto-deref.
				if resolved_unwrapped.is_ptr() && !var_type.is_ptr() {
					resolved_unwrapped = resolved_unwrapped.deref()
				} else if val is ast.Ident && val.is_auto_deref_var()
					&& resolved_unwrapped.nr_muls() > var_type.nr_muls() {
					resolved_unwrapped = resolved_unwrapped.set_nr_muls(var_type.nr_muls())
				}
				// Don't propagate shared/atomic flags from the resolved RHS expression
				// to a non-shared declaration. E.g., `sliced := shared_arr[..x]` inside
				// a rlock block resolves to a shared type, but `sliced` is not shared.
				if !var_type.has_flag(.shared_f) && resolved_unwrapped.has_flag(.shared_f) {
					resolved_unwrapped = resolved_unwrapped.clear_flag(.shared_f)
					if resolved_unwrapped.nr_muls() > 0 {
						resolved_unwrapped =
							resolved_unwrapped.set_nr_muls(resolved_unwrapped.nr_muls() - 1)
					}
				}
				if !var_type.has_flag(.atomic_f) && resolved_unwrapped.has_flag(.atomic_f) {
					resolved_unwrapped = resolved_unwrapped.clear_flag(.atomic_f)
				}
				// Skip when resolved type is a parent sumtype of a smartcast variant.
				// This happens in match arms where the RHS uses a smartcast variable
				// (e.g. `mut info := ts.info` inside `match ts.info { Struct { ... } }`).
				// Resolve aggregate types (from multi-branch match arms)
				// to the concrete variant type for the current iteration.
				resolved_sym_ := g.table.sym(resolved_unwrapped)
				if resolved_sym_.info is ast.Aggregate {
					resolved_unwrapped = resolved_sym_.info.types[g.aggregate_type_idx]
				}
				resolved_sym := g.table.sym(resolved_unwrapped)
				var_sym := g.table.sym(var_type)
				is_sumtype_reversal := resolved_sym.kind == .sum_type
					&& resolved_unwrapped != var_type && var_sym.kind != .sum_type
				// Don't downgrade a specific map/array type (e.g. map[int]SqlExpr)
				// to the base map/array type. This happens when .clone() etc.
				// return the base type but the checker already has a specific type.
				is_base_container_downgrade := (resolved_unwrapped == ast.map_type
					&& var_sym.kind == .map && var_type != ast.map_type)
					|| (resolved_unwrapped == ast.array_type && var_sym.kind == .array
					&& var_type != ast.array_type)
				// Don't introduce option/result flag when the checker already unwrapped it.
				// E.g., `x := *var?` where var is `?&int`: checker says x is `int`,
				// but resolved_expr_type may return `?int` because it sees the option var.
				is_option_introduction := !var_type.has_option_or_result()
					&& resolved_unwrapped.has_option_or_result()
				if !is_sumtype_reversal && !is_base_container_downgrade && !is_option_introduction {
					var_type = resolved_unwrapped
					val_type = var_type
					node.left_types[i] = var_type
					if i < node.right_types.len {
						node.right_types[i] = val_type
					}
					if mut left is ast.Ident {
						if mut left.obj is ast.Var {
							left.obj.typ = var_type
							if !var_type.has_option_or_result() {
								left.obj.orig_type = ast.no_type
								left.obj.smartcasts = []
								left.obj.is_unwrapped = false
							}
							if left.obj.ct_type_var != .no_comptime {
								g.type_resolver.update_ct_type(left.name, var_type)
							}
						}
						if left.scope != unsafe { nil } {
							if mut scope_var := left.scope.find_var(left.name) {
								scope_var.typ = var_type
								if !var_type.has_option_or_result() {
									scope_var.orig_type = ast.no_type
									scope_var.smartcasts = []
									scope_var.is_unwrapped = false
								}
							}
						}
					}
				}
			}
		}
		mut cur_indexexpr := -1
		consider_int_overflow := g.do_int_overflow_checks && g.unwrap_generic(var_type).is_int()
		consider_int_div_mod := g.table.final_sym(g.unwrap_generic(var_type)).is_int()
		is_safe_add_assign := node.op == .plus_assign && consider_int_overflow
		is_safe_sub_assign := node.op == .minus_assign && consider_int_overflow
		is_safe_mul_assign := node.op == .mult_assign && consider_int_overflow
		is_safe_div_assign := node.op == .div_assign && consider_int_div_mod
		is_safe_mod_assign := node.op == .mod_assign && consider_int_div_mod
		initial_left_sym := g.table.sym(g.unwrap_generic(var_type))
		is_va_list = initial_left_sym.language == .c && initial_left_sym.name == 'C.va_list'
		if mut left is ast.Ident {
			ident = left
			g.curr_var_name << ident.name
			// id_info := ident.var_info()
			// var_type = id_info.typ
			blank_assign = left.kind == .blank_ident
			// TODO: temporary, remove this
			left_info := left.info
			if left_info is ast.IdentVar {
				share := left_info.share
				if share == .shared_t {
					var_type = var_type.set_flag(.shared_f)
				}
				if share == .atomic_t {
					var_type = var_type.set_flag(.atomic_f)
				}
			}
			if mut left.obj is ast.Var {
				if is_decl {
					if val is ast.Ident && val.ct_expr {
						ctyp := g.unwrap_generic(g.type_resolver.get_type(val))
						if ctyp != ast.void_type {
							var_type = ctyp
							val_type = var_type
							gen_or = val.or_expr.kind != .absent
							if gen_or {
								var_type = val_type.clear_flag(.option)
							}
							left.obj.typ = var_type
							g.type_resolver.update_ct_type(left.name, var_type)
						}
					} else if val is ast.ComptimeSelector {
						if val.typ_key != '' {
							if is_decl {
								var_type = g.type_resolver.get_ct_type_or_default(val.typ_key,
									var_type)
								val_type = var_type
								left.obj.typ = var_type
								g.type_resolver.update_ct_type(left.name, var_type)
							} else {
								val_type = g.type_resolver.get_ct_type_or_default(val.typ_key,
									var_type)
							}
						}
					} else if val is ast.ComptimeCall {
						key_str := '${val.method_name}.return_type'
						var_type = g.type_resolver.get_ct_type_or_default(key_str, var_type)
						val_type = var_type
						left.obj.typ = var_type
						g.type_resolver.update_ct_type(left.name, var_type)
						g.assign_ct_type[val.pos.pos] = var_type
					} else if val is ast.Ident && val.info is ast.IdentVar {
						val_info := val.info as ast.IdentVar
						gen_or = val.or_expr.kind != .absent
						if val_info.is_option && gen_or {
							var_type = val_type.clear_flag(.option)
							left.obj.typ = var_type
						}
					} else if val is ast.DumpExpr {
						if val.expr is ast.ComptimeSelector {
							if val.expr.typ_key != '' {
								var_type = g.type_resolver.get_ct_type_or_default(val.expr.typ_key,
									var_type)
								val_type = var_type
								left.obj.typ = var_type
								g.type_resolver.update_ct_type(left.name, var_type)
							}
						}
					} else if val is ast.IndexExpr && (val.left is ast.Ident && val.left.ct_expr) {
						ctyp := g.unwrap_generic(g.type_resolver.get_type(val))
						if ctyp != ast.void_type {
							var_type = ctyp
							val_type = var_type
							left.obj.typ = var_type
							g.type_resolver.update_ct_type(left.name, var_type)
						}
					} else if left.obj.ct_type_var == .generic_var && val is ast.CallExpr {
						if val.return_type_generic != 0 {
							mut fn_ret_type := g.resolve_return_type(val).clear_option_and_result()
							if fn_ret_type == ast.void_type || fn_ret_type.has_flag(.generic) {
								fn_ret_type =
									g.unwrap_generic(val.return_type_generic).clear_option_and_result()
							}
							if fn_ret_type != ast.void_type {
								var_type = fn_ret_type
								val_type = var_type
								left.obj.typ = var_type
								g.assign_ct_type[val.pos.pos] = var_type
							}
						} else if val.is_static_method && val.left_type.has_flag(.generic) {
							fn_ret_type := g.resolve_return_type(val)
							var_type = fn_ret_type
							val_type = var_type
							left.obj.typ = var_type
							g.assign_ct_type[val.pos.pos] = var_type
						} else if val.left_type != 0 && g.table.type_kind(val.left_type) == .array
							&& val.name == 'map' && val.args.len > 0
							&& val.args[0].expr is ast.AsCast
							&& val.args[0].expr.typ.has_flag(.generic) {
							var_type =
								g.table.find_or_register_array(g.unwrap_generic((val.args[0].expr as ast.AsCast).typ))
							val_type = var_type
							left.obj.typ = var_type
							g.assign_ct_type[val.pos.pos] = var_type
						}
					} else if val is ast.InfixExpr
						&& val.op in [.plus, .minus, .mul, .power, .div, .mod] && val.left_ct_expr {
						left_ctyp := g.type_resolver.get_type_or_default(val.left, val.left_type)
						right_ctyp := g.type_resolver.get_type_or_default(val.right, val.right_type)
						ctyp := g.type_resolver.promote_type(g.unwrap_generic(left_ctyp),
							g.unwrap_generic(right_ctyp))
						if ctyp != ast.void_type {
							ct_type_var := g.comptime.get_ct_type_var(val.left)
							if ct_type_var in [.key_var, .value_var] {
								g.type_resolver.update_ct_type(left.name, g.unwrap_generic(ctyp))
							}
							var_type = ctyp
							val_type = var_type
							left.obj.typ = var_type
						}
					} else if val is ast.PostfixExpr && val.op == .question
						&& (val.expr is ast.Ident && val.expr.ct_expr) {
						ctyp := g.unwrap_generic(g.type_resolver.get_type(val))
						if ctyp != ast.void_type {
							var_type = ctyp
							val_type = var_type
							left.obj.typ = var_type

							ct_type_var := g.comptime.get_ct_type_var(val.expr)
							if ct_type_var == .field_var {
								g.type_resolver.update_ct_type(left.name, ctyp)
							}
						}
					} else if var_type.has_flag(.generic) && val is ast.StructInit
						&& val_type.has_flag(.generic) {
						val_type = g.unwrap_generic(val_type)
						var_type = val_type
					}
				}
				is_auto_heap = g.resolved_ident_is_auto_heap(left)
				// In generic instantiations, is_auto_heap may be set by the
				// checker's post_process when concrete types resolve to @[heap]
				// structs. Reset when the variable is an option type.
				if is_auto_heap && g.cur_fn != unsafe { nil } && g.cur_concrete_types.len > 0 {
					resolved_obj_typ := g.unwrap_generic(left.obj.typ)
					if resolved_obj_typ != 0 && resolved_obj_typ.has_flag(.option)
						&& !resolved_obj_typ.is_ptr() {
						is_auto_heap = false
					}
				}
				if left.obj.typ != 0 && val is ast.PrefixExpr {
					is_fn_var = g.table.final_sym(left.obj.typ).kind == .function
				}
			}
		} else if mut left is ast.ComptimeSelector {
			if left.typ_key != '' {
				var_type = g.type_resolver.get_ct_type_or_default(left.typ_key, var_type)
			}
			if val is ast.ComptimeSelector {
				if val.typ_key != '' {
					val_type = g.type_resolver.get_ct_type_or_default(val.typ_key, var_type)
				}
			} else if val is ast.CallExpr && val.return_type_generic.has_flag(.generic) {
				g.assign_ct_type[val.pos.pos] = g.comptime.comptime_for_field_type
				val_type = var_type
			}
		} else if mut left is ast.IndexExpr && val is ast.ComptimeSelector {
			if val.typ_key != '' {
				val_type = g.type_resolver.get_ct_type_or_default(val.typ_key, var_type)
			}
		}
		if is_decl && val is ast.CallExpr && val.or_block.kind != .absent {
			mut resolved_call_type := g.resolve_return_type(val)
			if resolved_call_type == ast.void_type {
				resolved_call_type = val.return_type
			}
			if g.table.sym(resolved_call_type).kind == .alias {
				unaliased_call_type := g.table.unaliased_type(resolved_call_type)
				if unaliased_call_type.has_option_or_result() {
					resolved_call_type = g.unwrap_generic(unaliased_call_type)
				}
			}
			var_type = resolved_call_type.clear_option_and_result()
			val_type = var_type
			if mut left is ast.Ident && mut left.obj is ast.Var {
				left.obj.typ = var_type
			}
		}
		if is_decl && val is ast.PostfixExpr && val.op == .question {
			mut resolved_val_type := g.resolved_expr_type(val.expr, val_type)
			if g.table.sym(resolved_val_type).kind == .alias {
				unaliased_val_type := g.table.unaliased_type(resolved_val_type)
				if unaliased_val_type.has_option_or_result() {
					resolved_val_type = g.unwrap_generic(unaliased_val_type)
				}
			}
			resolved_val_type = resolved_val_type.clear_option_and_result()
			if resolved_val_type != 0 && resolved_val_type != ast.void_type {
				var_type = resolved_val_type
				val_type = resolved_val_type
				if mut left is ast.Ident && mut left.obj is ast.Var {
					left.obj.typ = var_type
				}
			}
		}
		if is_decl && val is ast.Ident && val.or_expr.kind != .absent && g.cur_fn != unsafe { nil }
			&& g.cur_concrete_types.len > 0 && val.or_expr.stmts.len > 0 {
			last_or_stmt := val.or_expr.stmts.last()
			if last_or_stmt is ast.ExprStmt && last_or_stmt.typ != ast.void_type {
				resolved_or_type := g.resolved_expr_type(last_or_stmt.expr, last_or_stmt.typ)
				if resolved_or_type != 0 && resolved_or_type != ast.void_type {
					var_type = g.unwrap_generic(g.recheck_concrete_type(resolved_or_type))
					val_type = var_type
					if mut left is ast.Ident && mut left.obj is ast.Var {
						left.obj.typ = var_type
					}
				}
			}
		}
		if is_decl && assign_expr_unwraps_option_or_result(val) {
			var_type = var_type.clear_option_and_result()
			val_type = val_type.clear_option_and_result()
			if mut left is ast.Ident && mut left.obj is ast.Var {
				left.obj.typ = var_type
			}
		}
		if is_decl && var_type.has_option_or_result() {
			if (val is ast.CallExpr && val.or_block.kind != .absent)
				|| (val is ast.PostfixExpr && val.op == .question) {
				var_type = var_type.clear_option_and_result()
				val_type = val_type.clear_option_and_result()
				if mut left is ast.Ident && mut left.obj is ast.Var {
					left.obj.typ = var_type
				}
			}
		}
		if is_decl && mut left is ast.Ident && mut left.obj is ast.Var {
			mut should_recompute_decl_type := false
			match val {
				ast.Ident {
					should_recompute_decl_type = val.ct_expr
						|| (val.obj is ast.Var && val.obj.ct_type_var != .no_comptime)
						|| (val.obj is ast.Var && (val.obj.typ.has_flag(.generic)
						|| g.type_has_unresolved_generic_parts(val.obj.typ)))
				}
				ast.SelectorExpr {
					should_recompute_decl_type = val.typ.has_flag(.generic)
						|| g.type_has_unresolved_generic_parts(val.typ)
						|| val.expr_type.has_flag(.generic)
						|| g.type_has_unresolved_generic_parts(val.expr_type)
				}
				ast.IndexExpr {
					should_recompute_decl_type = val.typ.has_flag(.generic)
						|| g.type_has_unresolved_generic_parts(val.typ)
						|| val.left_type.has_flag(.generic)
						|| g.type_has_unresolved_generic_parts(val.left_type)
						|| (val.left is ast.Ident && val.left.ct_expr)
				}
				ast.ComptimeSelector {
					should_recompute_decl_type = true
				}
				ast.CastExpr {
					should_recompute_decl_type = val.typ.has_flag(.generic)
						|| g.type_has_unresolved_generic_parts(val.typ)
				}
				ast.PostfixExpr {
					should_recompute_decl_type = val.op == .question
				}
				ast.PrefixExpr {
					should_recompute_decl_type = val.op == .arrow
				}
				else {}
			}

			if g.cur_fn != unsafe { nil } && g.cur_concrete_types.len > 0 {
				should_recompute_decl_type = true
			}
			if val is ast.CallExpr {
				should_recompute_decl_type = val.return_type_generic != 0
					|| val.is_static_method || val.concrete_types.len > 0
					|| val.raw_concrete_types.len > 0
					|| (g.cur_fn != unsafe { nil } && g.cur_concrete_types.len > 0)
					|| (val.is_method && (val.left_type.has_flag(.generic)
					|| g.type_has_unresolved_generic_parts(val.left_type)
					|| val.receiver_type.has_flag(.generic)
					|| g.type_has_unresolved_generic_parts(val.receiver_type)))
			}
			mut resolved_val_type := if val is ast.Ident && val.or_expr.kind != .absent
				&& g.cur_fn != unsafe { nil } && g.cur_concrete_types.len > 0 {
				or_value_type := g.resolved_or_block_value_type(val.or_expr)
				if or_value_type != 0 {
					or_value_type
				} else {
					g.resolved_expr_type(val, val_type)
				}
			} else {
				g.resolved_expr_type(val, val_type)
			}
			if should_recompute_decl_type && resolved_val_type != 0
				&& resolved_val_type != ast.void_type {
				if assign_expr_unwraps_option_or_result(val) {
					resolved_val_type = resolved_val_type.clear_option_and_result()
				}
				resolved_val_type = g.unwrap_generic(g.recheck_concrete_type(resolved_val_type))
				// Resolve aggregate types (from multi-branch match arms)
				// to the concrete variant type for the current iteration.
				resolved_val_sym2 := g.table.sym(resolved_val_type)
				if resolved_val_sym2.info is ast.Aggregate {
					resolved_val_type = resolved_val_sym2.info.types[g.aggregate_type_idx]
				}
				// For SelectorExpr with scope smartcast (e.g. `if w.check != none`),
				// the resolved field type has the option flag, but the smartcast
				// unwraps it. Clear the option flag in that case.
				if val is ast.SelectorExpr && resolved_val_type.has_flag(.option) {
					scope := g.file.scope.innermost(val.pos.pos)
					field := scope.find_struct_field(val.expr.str(), val.expr_type, val.field_name)
					if field != unsafe { nil } && field.smartcasts.len > 0 {
						resolved_val_type = resolved_val_type.clear_flag(.option)
					}
				}
				resolved_val_sym := g.table.final_sym(resolved_val_type)
				if resolved_val_sym.kind == .array && !resolved_val_type.is_ptr()
					&& g.table.sym(resolved_val_type).kind != .alias {
					mut resolved_elem_type :=
						g.unwrap_generic(g.recheck_concrete_type(resolved_val_sym.array_info().elem_type))
					if resolved_elem_type == ast.int_literal_type {
						resolved_elem_type = ast.int_type
					} else if resolved_elem_type == ast.float_literal_type {
						resolved_elem_type = ast.f64_type
					}
					if resolved_elem_type != 0 && !resolved_elem_type.has_flag(.generic)
						&& !g.type_has_unresolved_generic_parts(resolved_elem_type) {
						mut new_arr_type :=
							ast.idx_to_type(g.table.find_or_register_array(resolved_elem_type))
						// Preserve option/result flags from the original resolved type
						if resolved_val_type.has_flag(.option) {
							new_arr_type = new_arr_type.set_flag(.option)
						}
						if resolved_val_type.has_flag(.result) {
							new_arr_type = new_arr_type.set_flag(.result)
						}
						resolved_val_type = new_arr_type
					}
				}
				// When assigning from an auto-deref variable (e.g. mut ref param),
				// the resolved type from scope includes the extra pointer level.
				// Deref it to match the V value semantics.
				if val is ast.Ident && val.is_auto_deref_var() && resolved_val_type.is_ptr()
					&& !g.auto_deref_source_type_is_pointer(val) {
					resolved_val_type = resolved_val_type.deref()
				}
				// Preserve shared/atomic flags from the original declaration.
				if var_type.has_flag(.shared_f) {
					resolved_val_type = resolved_val_type.set_flag(.shared_f)
				}
				if var_type.has_flag(.atomic_f) {
					resolved_val_type = resolved_val_type.set_flag(.atomic_f)
				}
				var_type = resolved_val_type
				// val_type represents the RHS expression type, which is NOT shared/atomic.
				// Only var_type (the LHS declaration type) should carry those flags.
				val_type = resolved_val_type.clear_flag(.shared_f).clear_flag(.atomic_f)
				left.obj.typ = var_type
			}
		}
		// Various resolution blocks above may overwrite var_type, stripping the
		// shared/atomic flags and nr_muls that the checker originally set.
		// Re-apply them based on the left-hand identifier's share attribute,
		// which is the authoritative source for whether the declaration is shared.
		if is_decl && mut left is ast.Ident {
			left_info := left.info
			if left_info is ast.IdentVar {
				if left_info.share == .shared_t && !var_type.has_flag(.shared_f) {
					var_type = var_type.set_flag(.shared_f)
				}
				if left_info.share == .atomic_t && !var_type.has_flag(.atomic_f) {
					var_type = var_type.set_flag(.atomic_f)
				}
			}
		}
		if is_decl && var_type.has_flag(.shared_f) && var_type.nr_muls() == 0 {
			var_type = var_type.set_nr_muls(1)
		}
		if is_decl && mut left is ast.Ident && mut left.obj is ast.Var {
			left.obj.typ = var_type
			if mut scope_var := left.scope.find_var(left.name) {
				scope_var.typ = var_type
				scope_var.orig_type = ast.no_type
				scope_var.smartcasts = []
				scope_var.is_unwrapped = false
			}
		}
		if is_decl && val is ast.CallExpr && val.kind == .clone && val.left is ast.IndexExpr {
			left_idx := val.left as ast.IndexExpr
			if left_idx.index is ast.RangeExpr && g.table.final_sym(var_type).kind == .array {
				is_auto_heap = false
			}
		}
		mut styp := g.styp(var_type)
		if is_decl && val is ast.CallExpr && val.kind == .clone && val.left is ast.IndexExpr {
			left_idx := val.left as ast.IndexExpr
			if left_idx.index is ast.RangeExpr && g.table.final_sym(val.return_type).kind == .array {
				styp = styp.trim('*')
			}
		}
		mut is_fixed_array_init := false
		mut has_val := false
		match val {
			ast.ArrayInit {
				is_fixed_array_init = val.is_fixed
				has_val = val.has_val
			}
			ast.ParExpr {
				if val.expr is ast.ArrayInit {
					array_init := val.expr as ast.ArrayInit
					is_fixed_array_init = array_init.is_fixed
					has_val = array_init.has_val
				}
			}
			ast.CallExpr {
				is_call = true
				if val.comptime_ret_val {
					return_type = g.comptime.comptime_for_field_type
					styp = g.styp(return_type)
				} else {
					return_type = val.return_type
				}
			}
			// TODO: no buffer fiddling
			ast.AnonFn {
				if !var_type.has_option_or_result() {
					if blank_assign {
						g.write('{')
					}
					// if it's a decl assign (`:=`) or a blank assignment `_ =`/`_ :=` then generate `void (*ident) (args) =`
					if (is_decl || blank_assign) && left is ast.Ident {
						sig := g.fn_var_signature(val.typ, val.decl.return_type,
							val.decl.params.map(it.typ), ident.name)
						g.write(sig + ' = ')
					} else {
						g.is_assign_lhs = true
						g.assign_op = node.op
						g.expr(left)
						g.is_assign_lhs = false
						g.is_arraymap_set = false
						if mut left is ast.IndexExpr {
							sym := g.table.final_sym(left.left_type)
							if sym.kind in [.map, .array] {
								g.expr(val)
								g.writeln('});')
								continue
							}
						}
						g.write(' = ')
					}
					g.expr(val)
					g.writeln(';')
					g.gen_self_recursing_anon_fn_capture_patch(left, val)
					if blank_assign {
						g.write('}')
					}
					continue
				}
			}
			else {}
		}

		if g.cur_fn != unsafe { nil } && g.cur_concrete_types.len > 0 {
			orig_var_option := var_type.has_flag(.option)
			resolved_left_type := g.resolved_expr_type(left, var_type)
			if resolved_left_type != 0 {
				var_type = g.unwrap_generic(g.recheck_concrete_type(resolved_left_type))
			}
			// Preserve the option flag when the variable was option-smartcasted.
			// `resolved_expr_type` returns the smartcasted (unwrapped) type for variables
			// inside `if x != none` blocks, but the C variable is still the option type
			// and needs option wrapping on assignment.
			if orig_var_option && !var_type.has_flag(.option) {
				var_type = var_type.set_flag(.option)
			}
			resolved_val_type := g.resolved_expr_type(val, val_type)
			if resolved_val_type != 0 {
				new_val_type := g.unwrap_generic(g.recheck_concrete_type(resolved_val_type))
				// Preserve option/result flag clearing from earlier unwrap
				if !val_type.has_flag(.option) && new_val_type.has_flag(.option) {
					val_type = new_val_type.clear_option_and_result()
				} else if !val_type.has_flag(.result) && new_val_type.has_flag(.result) {
					val_type = new_val_type.clear_option_and_result()
				} else {
					val_type = new_val_type
				}
			}
		}
		styp = g.styp(var_type)
		if is_decl && val is ast.CallExpr && val.kind == .clone && val.left is ast.IndexExpr {
			left_idx := val.left as ast.IndexExpr
			if left_idx.index is ast.RangeExpr && g.table.final_sym(val.return_type).kind == .array {
				styp = styp.trim('*')
			}
		}
		left_sym := g.table.sym(g.unwrap_generic(var_type))
		is_va_list = left_sym.language == .c && left_sym.name == 'C.va_list'
		unwrapped_val_type := g.unwrap_generic(val_type)
		right_sym := g.table.sym(unwrapped_val_type)
		unaliased_right_sym := g.table.final_sym(unwrapped_val_type)
		unaliased_left_sym := g.table.final_sym(g.unwrap_generic(var_type))
		is_fixed_array_var := unaliased_right_sym.kind == .array_fixed && val !is ast.ArrayInit
			&& (val in [ast.Ident, ast.IndexExpr, ast.CallExpr, ast.SelectorExpr, ast.ComptimeSelector, ast.DumpExpr, ast.InfixExpr]
			|| (val is ast.CastExpr && val.expr !is ast.ArrayInit)
			|| (val is ast.PrefixExpr && val.op == .arrow)
			|| (val is ast.UnsafeExpr && val.expr in [ast.SelectorExpr, ast.Ident, ast.CallExpr]))
			&& !((g.pref.translated || g.file.is_translated)
			&& unaliased_left_sym.kind != .array_fixed)
		g.is_assign_lhs = true
		g.assign_op = node.op

		g.left_is_opt = var_type.has_option_or_result()
		g.right_is_opt = val_type.has_option_or_result()
		defer(fn) {
			g.left_is_opt = false
			g.right_is_opt = false
		}
		if !is_decl && node.op == .assign && var_type.has_flag(.option_mut_param_t) {
			// For `mut ?T` parameters / for-loop variables, the C type is
			// `_option_T*` (pointer to option). Write through the pointer
			// by copying the entire option struct (state + data).
			mut target_option_type := g.resolve_current_fn_generic_param_type(left.str())
			if target_option_type == 0 || !target_option_type.has_flag(.option) {
				target_option_type = var_type.clear_flag(.option_mut_param_t)
			}
			if target_option_type.has_flag(.option) {
				target_inner_type := target_option_type.clear_option_and_result()
				if target_inner_type.is_ptr() {
					target_option_type = target_inner_type.deref().set_flag(.option)
				}
			}
			target_option_type = g.unwrap_generic(g.recheck_concrete_type(target_option_type))
			tmp_var := g.new_tmp_var()
			g.expr_with_tmp_var(val, val_type, target_option_type, tmp_var, false)
			left_name := c_name(left.str())
			is_fn_param := left is ast.Ident && left.is_auto_deref_var()
			if is_fn_param {
				// Function params use _option_T_ptr* type, copy data through pointer
				val_base_type := g.base_type(val_type)
				g.writeln('${left_name}->state = ${tmp_var}.state;')
				g.writeln('memcpy(&${left_name}->data, ${tmp_var}.data, sizeof(${val_base_type}));')
			} else {
				// For-loop variables use _option_T* type, copy whole struct
				g.writeln('*${left_name} = ${tmp_var};')
			}
			continue
		}

		if blank_assign {
			if val is ast.IndexExpr {
				g.assign_op = .decl_assign
			}
			g.is_assign_lhs = false
			if is_call {
				old_is_void_expr_stmt := g.is_void_expr_stmt
				g.is_void_expr_stmt = true
				g.expr(val)
				g.is_void_expr_stmt = old_is_void_expr_stmt
			} else if g.inside_for_c_stmt {
				g.expr(val)
			} else if var_type.has_flag(.option) {
				g.expr_with_opt(val, val_type, var_type)
			} else {
				if left_sym.kind == .function {
					g.write('{void* _ = ')
				} else {
					// For blank idents, use val_type to determine the C type
					// instead of var_type (styp), because in generic functions
					// the checker's left_types[i] for blank idents can be
					// overwritten by a later generic instantiation.
					mut blank_styp := g.styp(val_type)
					if val is ast.Ident && val.is_auto_deref_var()
						&& !g.auto_deref_source_type_is_pointer(val) {
						blank_styp = '${blank_styp}*'
					}
					if blank_styp.ends_with('*') {
						blank_styp = 'void*'
					}
					g.write('{${blank_styp} _ = ')
				}
				if (val in [ast.MatchExpr, ast.IfExpr, ast.ComptimeSelector] || is_fixed_array_var)
					&& unaliased_right_sym.info is ast.ArrayFixed {
					tmp_var := g.expr_with_var(val, var_type, false)
					// When the temp var is a return wrapper struct (_v_Array_fixed_...),
					// access .ret_arr to get the actual C array for subscripting.
					init_expr := if unaliased_right_sym.info.is_fn_ret {
						'${tmp_var}.ret_arr'
					} else {
						tmp_var
					}
					g.fixed_array_var_init(init_expr, false, unaliased_right_sym.info.elem_type,
						unaliased_right_sym.info.size)
				} else {
					g.expr(val)
				}
				g.writeln(';}')
			}
		} else if node.op == .assign && (is_fixed_array_init || is_fixed_array_var
			|| (unaliased_right_sym.kind == .array_fixed && val is ast.CastExpr)) {
			// Fixed arrays
			if unaliased_left_sym.kind != .array_fixed && unaliased_right_sym.kind == .array_fixed
				&& (g.pref.translated || g.file.is_translated) {
				// translated:
				// arr = [5]u8{}
				// ptr = arr   => ptr = &arr[0]
				g.expr(left)
				g.write(' = ')
				g.expr(val)
			} else if is_fixed_array_init && var_type.has_flag(.option) {
				g.expr(left)
				g.write(' = ')
				g.expr_with_opt(val, val_type, var_type)
			} else if unaliased_right_sym.kind == .array_fixed && val is ast.CastExpr {
				if var_type.has_flag(.option) {
					g.expr(left)
					g.writeln('.state = 0;')
					g.write('memcpy(')
					g.expr(left)
					g.write('.data, ')
					g.expr(val)
					g.writeln(', sizeof(${g.styp(var_type.clear_flag(.option))}));')
				} else {
					g.write('memcpy(')
					g.expr(left)
					g.write(', ')
					g.expr(val)
					g.writeln(', sizeof(${g.styp(var_type)}));')
				}
			} else {
				arr_typ := styp.trim('*')
				old_is_assign_lhs := g.is_assign_lhs
				// For map IndexExpr LHS, keep is_assign_lhs = true so the index
				// generator emits `map_get_and_set` (which inserts missing keys)
				// instead of `map_get` (which returns a zero-default buffer).
				left_is_map_index := left is ast.IndexExpr
					&& g.table.final_sym(left.left_type).kind == .map
				g.is_assign_lhs = left_is_map_index
				left_expr := g.expr_string(left)
				if !is_fixed_array_init && assign_expr_unwraps_option_or_result(val) {
					// val has an or-block — its code generator emits unwrap
					// statements via go_before_last_stmt(), so we must emit
					// memcpy() inline. Capturing val with expr_string() would
					// slurp those statements into the memcpy() argument list.
					g.write('memcpy(${left_expr}, ')
					g.expr(val)
					g.writeln(', sizeof(${arr_typ}));')
				} else {
					mut fixed_right_expr := ''
					if is_fixed_array_init {
						right := val as ast.ArrayInit
						right_var := g.new_tmp_var()
						g.write('${arr_typ} ${right_var} = ')
						g.expr(right)
						g.writeln(';')
						fixed_right_expr = right_var
					} else {
						fixed_right_expr = g.expr_string(val)
					}
					g.writeln('')
					g.writeln('memcpy(${left_expr}, ${fixed_right_expr}, sizeof(${arr_typ}));')
				}
				g.is_assign_lhs = old_is_assign_lhs
				g.is_assign_lhs = false
			}
		} else {
			is_inside_ternary := g.inside_ternary != 0
			cur_line := if is_inside_ternary && is_decl {
				g.register_ternary_name(ident.name)
				g.empty_line = false
				g.go_before_ternary()
			} else {
				''
			}
			if mut left is ast.IndexExpr && left.is_index_operator {
				g.write(cur_line)
				if node.op == .assign {
					g.index_operator_call(left.left, left.left_type, left.index, left.index_type,
						'[]=', val, val_type)
				} else {
					infix_op := token.assign_op_to_infix_op(node.op)
					op_expr := ast.InfixExpr{
						left:          ast.Expr(left)
						right:         val
						op:            infix_op
						pos:           node.pos
						left_type:     left.typ
						right_type:    val_type
						promoted_type: g.type_resolver.promote_type(left.typ, val_type)
					}
					g.index_operator_call(left.left, left.left_type, left.index, left.index_type,
						'[]=', ast.Expr(op_expr), left.typ)
				}
				if !g.inside_for_c_stmt {
					g.writeln(';')
				}
				continue
			}
			mut str_add := false
			mut op_overloaded := false
			mut op_expected_left := ast.no_type
			mut op_expected_right := ast.no_type
			is_shared_re_assign := !is_decl && node.left_types[i].has_flag(.shared_f)
				&& left is ast.Ident && left_sym.kind in [.array, .map, .struct]
			mut is_mut_arg_pointer_rebind := false
			// Keep pointer traversal assignments on auto-deref vars as local pointer rebinds,
			// but only for function arguments (is_arg), not for loop iteration variables.
			if !is_decl && !is_shared_re_assign && !var_type.has_flag(.option)
				&& var_type.is_any_kind_of_pointer() && unwrapped_val_type.is_any_kind_of_pointer()
				&& left.is_auto_deref_var() && !val.is_auto_deref_var() && node.op == .assign
				&& left.is_auto_deref_arg() {
				is_mut_arg_pointer_rebind = true
			}
			if node.op == .plus_assign && unaliased_right_sym.kind == .string {
				if g.is_autofree && !g.is_builtin_mod && !g.is_autofree_tmp
					&& val !in [ast.Ident, ast.StringLiteral, ast.SelectorExpr, ast.ComptimeSelector] {
					str_add_rhs_tmp = '_str_add_rhs_${node.pos.pos}_${i}'
					g.writeln(g.autofree_tmp_arg_init_stmt('string ${str_add_rhs_tmp} = ', val))
					val = ast.Expr(ast.Ident{
						mod:  g.cur_mod.name
						name: str_add_rhs_tmp
					})
					str_add_rhs_needs_free = true
					skip_str_add_rhs_clone = true
					defer(fn) {
						if str_add_rhs_needs_free {
							g.writeln('builtin__string_free(&${str_add_rhs_tmp});')
						}
					}
				}
				if mut left is ast.IndexExpr {
					if g.table.sym(left.left_type).kind == .array_fixed {
						// strs[0] += str2 => `strs[0] = _string__plus(strs[0], str2)`
						g.expr(left)
						g.write(' = builtin__string__plus(')
					} else {
						// a[0] += str => `builtin__array_set(&a, 0, &(string[]) {_string__plus(...))})`
						g.expr(left)
						g.write('builtin__string__plus(')
					}
				} else {
					// allow literal values to auto deref var (e.g.`for mut v in values { v += 1.0 }`)
					if left.is_auto_deref_var() {
						g.write('*')
					}
					// str += str2 => `str = builtin__string__plus(str, str2)`
					g.expr(left)
					g.write(' = builtin__string__plus(')
				}
				g.is_assign_lhs = false
				str_add = true
			}
			// Assignment Operator Overloading
			has_power_assign_overload := node.op == .power_assign
				&& left_sym.find_method_with_generic_parent('**') != none
			if (((left_sym.kind == .struct && right_sym.kind == .struct)
				|| (left_sym.kind == .alias && right_sym.kind == .alias))
				&& node.op in [.plus_assign, .minus_assign, .div_assign, .mult_assign, .power_assign, .mod_assign])
				|| has_power_assign_overload {
				extracted_op := match node.op {
					.plus_assign { '+' }
					.minus_assign { '-' }
					.div_assign { '/' }
					.mod_assign { '%' }
					.mult_assign { '*' }
					.power_assign { '**' }
					else { 'unknown op' }
				}

				pos := g.out.len
				g.expr(left)
				struct_info := g.table.final_sym(var_type)
				if left_sym.info is ast.Struct && left_sym.info.generic_types.len > 0 {
					concrete_types := left_sym.info.concrete_types
					mut method_name := left_sym.cname + '_' + util.replace_op(extracted_op)
					specialized_suffix := g.generic_fn_name(concrete_types, '')
					if specialized_suffix != '' && !method_name.ends_with(specialized_suffix) {
						method_name = g.generic_fn_name(concrete_types, method_name)
					}
					g.write(' = ${method_name}(')
					g.expr(left)
					g.write(', ')
					g.expr(val)
					g.writeln(');')
					return
				} else if left_sym.kind == .alias
					&& g.table.final_sym(g.unwrap_generic(var_type)).is_number()
					&& !left_sym.has_method(extracted_op) {
					g.write(' = ')
					if node.op == .power_assign {
						g.gen_power_assign_expr(left, var_type, val, val_type)
					} else {
						g.expr(left)
						g.write(' ${extracted_op} ')
						g.expr(val)
					}
					if !g.inside_for_c_stmt {
						g.write(';')
					}
					return
				} else if left_sym.kind == .alias && struct_info.kind == .struct
					&& struct_info.info is ast.Struct && struct_info.info.generic_types.len > 0 {
					mut method_name := struct_info.cname + '_' + util.replace_op(extracted_op)
					specialized_suffix := g.generic_fn_name(struct_info.info.concrete_types, '')
					if specialized_suffix != '' && !method_name.ends_with(specialized_suffix) {
						method_name = g.generic_fn_name(struct_info.info.concrete_types,
							method_name)
					}
					g.write(' = ${method_name}(')
					g.expr(left)
					g.write(', ')
					g.expr(val)
					g.writeln(');')
					return
				} else {
					if g.table.final_sym(g.unwrap_generic(var_type)).kind == .array_fixed {
						g.go_back_to(pos)
						g.empty_line = true
						g.write('memcpy(')
						g.expr(left)
						g.write(', ${styp}_${util.replace_op(extracted_op)}(')
					} else {
						g.write(' = ${styp}_${util.replace_op(extracted_op)}(')
					}
					method := g.table.find_method(left_sym, extracted_op) or {
						// the checker will most likely have found this, already...
						g.error('assignment operator `${extracted_op}=` used but no `${extracted_op}` method defined',
							node.pos)
						ast.Fn{}
					}
					op_expected_left = method.params[0].typ
					op_expected_right = method.params[1].typ
					op_overloaded = true
				}
			}
			final_left_sym := g.table.final_sym(g.unwrap_generic(var_type))
			final_right_sym := g.table.final_sym(unwrapped_val_type)
			mut aligned := 0
			is_safe_shift_assign := !g.pref.translated && !g.file.is_translated
				&& node.op in [.left_shift_assign, .right_shift_assign] && final_left_sym.is_int()
			safe_shift_fn_name := if is_safe_shift_assign {
				g.safe_shift_fn_name(var_type, token.assign_op_to_infix_op(node.op))
			} else {
				''
			}
			if final_left_sym.info is ast.Struct {
				if attr := final_left_sym.info.attrs.find_first('aligned') {
					aligned = if attr.arg == '' { 0 } else { attr.arg.int() }
				}
			}

			if final_left_sym.kind == .bool && final_right_sym.kind == .bool
				&& node.op in [.boolean_or_assign, .boolean_and_assign] {
				extracted_op := match node.op {
					.boolean_or_assign {
						'||'
					}
					.boolean_and_assign {
						'&&'
					}
					else {
						'unknown op'
					}
				}

				g.expr(left)
				g.write(' = ')
				g.expr(left)
				g.write(' ${extracted_op} ')
				g.expr(val)
				g.writeln(';')
				return
			}
			if node.op == .power_assign && !op_overloaded {
				g.write_assign_target_expr(left, var_type)
				g.write(' = ')
				g.gen_power_assign_expr(left, var_type, val, val_type)
				g.writeln(';')
				return
			}
			if right_sym.info is ast.FnType && is_decl {
				if is_inside_ternary {
					g.out.write_string(util.tabs(g.indent - g.inside_ternary))
				}
				fn_name := c_fn_name(g.get_ternary_name(ident.name))

				if val_type.has_flag(.option) {
					ret_styp := g.styp(g.unwrap_generic(val_type))
					g.write('${ret_styp} ${fn_name}')
				} else {
					g.write_fntype_decl(fn_name, right_sym.info, var_type.nr_muls())
				}
			} else {
				if is_decl {
					if is_inside_ternary {
						g.out.write_string(util.tabs(g.indent - g.inside_ternary))
					}
					mut is_used_var_styp := false
					if ident.name !in g.defer_vars {
						val_sym := g.table.sym(val_type)
						if val_sym.info is ast.Struct && val_sym.info.generic_types.len > 0 {
							if val is ast.StructInit {
								var_styp := g.styp(val.typ)
								if var_type.has_flag(.shared_f) {
									g.write('__shared__${var_styp}* ')
								} else {
									g.write('${var_styp} ')
								}
								is_used_var_styp = true
							} else if val is ast.PrefixExpr {
								if val.op == .amp && val.right is ast.StructInit {
									var_styp := g.styp(val.right.typ.ref())
									if var_type.has_flag(.shared_f) {
										g.write('__shared__')
									}
									g.write('${var_styp} ')
									is_used_var_styp = true
								}
							}
						}
						if !is_used_var_styp {
							if !val_type.has_flag(.option) && left_sym.is_array_fixed() {
								if left_sym.info is ast.Alias {
									parent_sym := g.table.final_sym(left_sym.info.parent_type)
									styp = g.styp(left_sym.info.parent_type)
									if !parent_sym.is_array_fixed_ret() {
										g.write('${styp} ')
									} else {
										g.write('${styp[3..]} ')
									}
								} else {
									if !left_sym.is_array_fixed_ret() {
										g.write('${styp} ')
									} else {
										g.write('${styp[3..]} ')
									}
								}
							} else {
								g.write('${styp} ')
							}
						}
						if is_auto_heap && !(val_type.is_ptr() && val_type.has_flag(.option)) {
							g.write('*')
						}
					}
				}
				if left in [ast.Ident, ast.SelectorExpr] && !g.is_smartcast_assign_lhs(left) {
					g.prevent_sum_type_unwrapping_once = true
				}
				if !is_fixed_array_var || is_decl || is_shared_re_assign {
					if op_overloaded {
						g.op_arg(left, op_expected_left, var_type)
					} else {
						if !is_decl && !is_shared_re_assign
							&& (left.is_auto_deref_var() || is_auto_heap)
							&& !is_mut_arg_pointer_rebind
							&& (!var_type.has_flag(.option) || is_auto_heap) {
							g.write('*')
						}
						if var_type.has_flag(.option_mut_param_t) {
							g.expr(left)
							g.write(' = ')
						} else {
							g.expr(left)
						}
						if !is_decl && var_type.has_flag(.shared_f) {
							g.write('->val') // don't reset the mutex, just change the value
						}
					}
				}
			}
			if is_inside_ternary && is_decl {
				g.write(';\n${cur_line}')
				g.out.write_string(util.tabs(g.indent))
				g.expr(left)
			}
			if is_decl && node.is_static
				&& g.gen_static_decl_runtime_init(node, left, var_type, val, val_type) {
				continue
			}
			g.is_assign_lhs = false
			left_expr := left
			if left_expr is ast.IndexExpr && g.cur_indexexpr.len > 0 {
				cur_indexexpr = g.cur_indexexpr.len - 1
			}
			if is_fixed_array_var || is_va_list {
				if is_decl {
					g.writeln(';')
					if is_va_list {
						continue
					}
				}
			} else if !var_type.has_flag(.option_mut_param_t) && cur_indexexpr == -1 && !str_add
				&& !op_overloaded && !is_safe_add_assign && !is_safe_sub_assign
				&& !is_safe_mul_assign && !is_safe_div_assign && !is_safe_mod_assign
				&& !is_safe_shift_assign {
				g.write(' ${op} ')
			} else if (str_add || op_overloaded) && !is_safe_add_assign && !is_safe_sub_assign
				&& !is_safe_mul_assign && !is_safe_div_assign && !is_safe_mod_assign {
				g.write(', ')
			} else if is_safe_shift_assign {
				g.write(' = ${safe_shift_fn_name}(')
				g.expr(left)
				g.write(', (u64)')
			} else if is_safe_add_assign || is_safe_sub_assign || is_safe_mul_assign
				|| is_safe_div_assign || is_safe_mod_assign {
				overflow_styp := g.styp(get_overflow_fn_type(var_type))
				div_mod_styp :=
					g.styp(g.unwrap_generic(var_type).clear_flag(.shared_f).clear_flag(.atomic_f))
				vsafe_fn_name := match true {
					is_safe_add_assign { 'builtin__overflow__add_${overflow_styp}' }
					is_safe_sub_assign { 'builtin__overflow__sub_${overflow_styp}' }
					is_safe_mul_assign { 'builtin__overflow__mul_${overflow_styp}' }
					is_safe_div_assign { 'VSAFE_DIV_${div_mod_styp}' }
					is_safe_mod_assign { 'VSAFE_MOD_${div_mod_styp}' }
					else { '' }
				}

				if is_safe_div_assign || is_safe_mod_assign {
					g.vsafe_arithmetic_ops[vsafe_fn_name] = VSafeArithmeticOp{
						typ: g.unwrap_generic(var_type).clear_flag(.shared_f).clear_flag(.atomic_f)
						op:  token.assign_op_to_infix_op(node.op)
					}
				}
				g.write(' = ${vsafe_fn_name}(')
				g.expr(left)
				g.write(',')
			}
			mut decl_tmp_var := ''
			mut decl_stmt_str := ''
			if is_decl && g.inside_ternary == 0 && !node.is_static && !var_type.has_flag(.shared_f)
				&& g.decl_assign_struct_init_needs_tmp(val) {
				decl_stmt_str = g.go_before_last_stmt().trim_space()
				g.empty_line = true
				decl_tmp_var = g.new_tmp_var()
				g.write('${styp} ')
				if is_auto_heap && !(val_type.is_ptr() && val_type.has_flag(.option)) {
					g.write('*')
				}
				g.write('${decl_tmp_var} ${op} ')
			}
			mut cloned := false
			if g.is_autofree {
				if right_sym.kind in [.array, .string] && !unwrapped_val_type.has_flag(.shared_f) {
					if !skip_str_add_rhs_clone
						&& g.gen_clone_assignment(var_type, val, unwrapped_val_type, false) {
						cloned = true
					}
				} else if right_sym.info is ast.Interface && var_type != ast.error_type {
					g.register_free_method(var_type)
				}
			}
			if !cloned {
				if g.comptime.comptime_for_field_var == ''
					&& ((var_type.has_flag(.option) && !val_type.has_flag(.option))
					|| (var_type.has_flag(.result) && !val_type.has_flag(.result))) {
					old_inside_opt_or_res := g.inside_opt_or_res
					defer(fn) {
						g.inside_opt_or_res = old_inside_opt_or_res
					}
					g.inside_opt_or_res = true
					if is_decl && is_auto_heap && var_type.has_flag(.option) {
						g.write('&')
					}
					tmp_var := g.new_tmp_var()
					g.expr_with_tmp_var(val, val_type, var_type, tmp_var, true)
				} else if is_fixed_array_var {
					// TODO: Instead of the translated check, check if it's a pointer already
					// and don't generate memcpy &
					typ_str := g.styp(val_type).trim('*')
					final_typ_str := if is_fixed_array_var { '' } else { '(${typ_str}*)' }
					final_ref_str := if is_fixed_array_var {
						''
					} else if val_type.is_ptr() {
						'(byte*)'
					} else {
						'(byte*)&'
					}
					if val_type.has_flag(.option) {
						g.expr(left)
						g.write(' = ')
						g.expr(val)
					} else {
						if op_overloaded {
							g.expr(left)
							g.write(', ')
							g.expr(val)
							g.write(').ret_arr, sizeof(${typ_str})')
						} else {
							g.write('memcpy(${final_typ_str}')
							g.expr(left)
							g.write(', ${final_ref_str}')
							g.expr(val)
							g.write(', sizeof(${typ_str}))')
						}
					}
				} else if is_decl {
					g.is_shared = var_type.has_flag(.shared_f)
					if is_fixed_array_init && !has_val {
						if val is ast.ArrayInit {
							g.array_init(val, g.ident_cname(ident))
						} else {
							g.write('{0}')
						}
					} else {
						is_option_unwrapped := assign_expr_unwraps_option_or_result(val)
						is_option_auto_heap := is_auto_heap && is_option_unwrapped
						auto_heap_uses_existing_storage := is_auto_heap && !is_fn_var
							&& !is_option_auto_heap
							&& g.auto_heap_assignment_uses_existing_storage(val, val_type)
						// For large structs (with large fixed arrays), avoid stack-allocated
						// compound literals which can cause stack overflow. Use vcalloc directly.
						mut is_large_struct_heap := false
						if is_auto_heap && !is_fn_var && val is ast.StructInit
							&& g.struct_has_large_fixed_array(val.typ) {
							is_large_struct_heap = true
						}
						if is_auto_heap && !is_fn_var && !is_large_struct_heap
							&& !auto_heap_uses_existing_storage {
							if aligned != 0 {
								g.write('HEAP_align(${styp}, (')
							} else {
								ptrmap_o, _ := g.vgc_ptrmap(var_type.set_nr_muls(0))
								if ptrmap_o.len > 0 {
									g.write('HEAP_vgc(${styp}, (')
								} else {
									g.write('HEAP(${styp}, (')
								}
							}
						}
						if !is_fn_var && val.is_auto_deref_var() && !is_option_unwrapped
							&& !g.auto_deref_source_type_is_pointer(val) {
							g.write('*')
						}
						if (var_type.has_flag(.option) && val !in [ast.Ident, ast.SelectorExpr])
							|| gen_or {
							g.expr_with_opt_or_block(val, val_type, left, var_type,
								is_option_auto_heap)
						} else if val is ast.ArrayInit {
							cvar_name := g.ident_cname(ident)
							if val.is_fixed && ident.name in g.defer_vars {
								g.go_before_last_stmt()
								g.empty_line = true
								g.write('memcpy(${cvar_name}, ')
								g.write('(${styp})')
								g.array_init(val, cvar_name)
								g.write(', sizeof(${styp}))')
							} else {
								g.array_init(val, cvar_name)
							}
						} else if val is ast.ParExpr && val.expr is ast.ArrayInit {
							array_init := val.expr as ast.ArrayInit
							cvar_name := g.ident_cname(ident)
							if array_init.is_fixed && ident.name in g.defer_vars {
								g.go_before_last_stmt()
								g.empty_line = true
								g.write('memcpy(${cvar_name}, ')
								g.write('(${styp})')
								g.array_init(array_init, cvar_name)
								g.write(', sizeof(${styp}))')
							} else {
								g.array_init(array_init, cvar_name)
							}
						} else if var_type.has_flag(.shared_f) && !val_type.has_flag(.shared_f)
							&& !val_type.is_ptr() {
							g.expr_with_cast(val, val_type, var_type)
						} else if val_type.has_flag(.shared_f) || orig_val_shared {
							g.expr_with_cast(val, val_type.set_flag(.shared_f), var_type)
						} else if val in [ast.MatchExpr, ast.IfExpr]
							&& unaliased_right_sym.info is ast.ArrayFixed {
							tmp_var := g.expr_with_var(val, var_type, false)
							// When the temp var is a return wrapper struct (_v_Array_fixed_...),
							// access .ret_arr to get the actual C array for subscripting.
							init_expr := if unaliased_right_sym.info.is_fn_ret {
								'${tmp_var}.ret_arr'
							} else {
								tmp_var
							}
							g.fixed_array_var_init(init_expr, false,
								unaliased_right_sym.info.elem_type, unaliased_right_sym.info.size)
						} else if is_large_struct_heap && val is ast.StructInit {
							// For large structs, use vcalloc directly to avoid stack overflow
							// from compound literals on the stack
							tmp_var := g.new_tmp_var()
							stmt_str := g.go_before_last_stmt()
							g.empty_line = true
							g.writeln('${styp}* ${tmp_var} = (${styp}*)builtin__vcalloc(sizeof(${styp}));')
							// Initialize non-zero fields
							val_sym := g.table.final_sym(val.typ)
							if val_sym.info is ast.Struct {
								for init_field in val.init_fields {
									if init_field.typ == 0 {
										continue
									}
									field_name := c_name(init_field.name)
									g.write('${tmp_var}->${field_name} = ')
									g.expr(init_field.expr)
									g.writeln(';')
								}
								// Handle fields with default values
								for field in val_sym.info.fields {
									mut found := false
									for init_field in val.init_fields {
										if init_field.name == field.name {
											found = true
											break
										}
									}
									if !found && field.has_default_expr {
										field_name := c_name(field.name)
										g.write('${tmp_var}->${field_name} = ')
										g.expr(field.default_expr)
										g.writeln(';')
									}
								}
							}
							g.empty_line = false
							g.write2(stmt_str, tmp_var)
						} else {
							old_inside_assign_fn_var := g.inside_assign_fn_var
							g.inside_assign_fn_var = val is ast.PrefixExpr && val.op == .amp
								&& is_fn_var
							mut nval := val
							if val is ast.PrefixExpr && val.right is ast.CallExpr {
								call_expr := val.right as ast.CallExpr
								if call_expr.name == 'new_array_from_c_array' {
									nval = call_expr
									if !var_type.has_flag(.shared_f) {
										g.write('HEAP(${g.styp(var_type.clear_ref())}, ')
									}
									g.expr(nval)
									if !var_type.has_flag(.shared_f) {
										g.write(')')
									}
								}
							}
							if nval == val {
								if auto_heap_uses_existing_storage {
									g.write_auto_heap_assignment_expr(nval, val_type)
								} else {
									g.expr_in_value_context(nval, val_type, var_type)
								}
								if !is_fn_var && is_auto_heap && is_option_auto_heap
									&& !is_large_struct_heap {
									if aligned != 0 {
										g.write('), ${aligned})')
									} else {
										g.write('))')
									}
								}
							}
							g.inside_assign_fn_var = old_inside_assign_fn_var
						}
						if !is_fn_var && is_auto_heap && !is_option_auto_heap
							&& !auto_heap_uses_existing_storage && !is_large_struct_heap {
							if aligned != 0 {
								g.write('), ${aligned})')
							} else {
								ptrmap, nptrs := g.vgc_ptrmap(var_type.set_nr_muls(0))
								if ptrmap.len > 0 {
									g.write('), ${ptrmap}, ${nptrs})')
								} else {
									g.write('))')
								}
							}
						}
					}
				} else {
					// var = &auto_heap_var
					old_is_auto_heap := g.is_option_auto_heap
					defer(fn) {
						g.is_option_auto_heap = old_is_auto_heap
					}
					if val is ast.Ident && val.is_mut() && var_type.is_ptr() {
						if var_type.nr_muls() < val_type.nr_muls() {
							g.write('*'.repeat(var_type.nr_muls()))
						}
					}
					g.is_option_auto_heap = val_type.has_flag(.option) && val is ast.PrefixExpr
						&& val.right is ast.Ident && (val.right as ast.Ident).is_auto_heap()
					if var_type.has_flag(.option) || gen_or {
						g.expr_with_opt_or_block(val, val_type, left, var_type, false)
					} else if node.has_cross_var {
						g.gen_cross_tmp_variable(node.left, val)
					} else if val is ast.None {
						if var_type.has_flag(.generic)
							&& g.unwrap_generic(var_type).has_flag(.option)
							&& var_type.nr_muls() > 0 {
							g.gen_option_error(var_type.set_nr_muls(0), ast.None{})
						} else {
							g.gen_option_error(var_type, ast.None{})
						}
					} else {
						if op_overloaded {
							g.op_arg(val, op_expected_right, val_type)
						} else {
							exp_type := if is_mut_arg_pointer_rebind {
								var_type
							} else if var_type.is_ptr()
								&& (left.is_auto_deref_var() || var_type.has_flag(.shared_f)) {
								var_type.deref()
							} else {
								var_type
							}.clear_flag(.shared_f) // don't reset the mutex, just change the value
							mut use_heap_pointed_ident := false
							mut use_raw_auto_heap_ident := false
							if val is ast.PrefixExpr && val.op == .amp && val.right is ast.Ident {
								right_ident := val.right as ast.Ident
								mut resolved_right_type := g.resolved_expr_type(ast.Expr(right_ident),
									val.right_type)
								if resolved_right_type == 0 {
									resolved_right_type = val.right_type
								}
								resolved_right_type =
									g.unwrap_generic(g.recheck_concrete_type(resolved_right_type))
								rhs_sym_ := g.table.final_sym(resolved_right_type)
								if rhs_sym_.kind != .function {
									right_type_for_compare :=
										resolved_right_type.clear_flag(.shared_f).clear_flag(.atomic_f)
									exp_type_for_compare :=
										exp_type.clear_flag(.shared_f).clear_flag(.atomic_f)
									right_points_to_heap := resolved_right_type.is_ptr()
										&& g.table.final_sym(resolved_right_type.deref()).is_heap()
									use_heap_pointed_ident = resolved_right_type != 0
										&& right_points_to_heap
										&& right_type_for_compare == exp_type_for_compare
									right_is_auto_heap := right_ident.is_auto_heap()
										|| g.resolved_ident_is_auto_heap(right_ident)
									use_raw_auto_heap_ident = exp_type.is_ptr()
										&& right_is_auto_heap && !use_heap_pointed_ident
										&& resolved_right_type != 0 && !resolved_right_type.is_ptr()
								}
							}
							if use_heap_pointed_ident {
								g.expr(ast.Expr((val as ast.PrefixExpr).right))
							} else if use_raw_auto_heap_ident {
								old_inside_assign_fn_var := g.inside_assign_fn_var
								g.inside_assign_fn_var = true
								g.expr(ast.Expr((val as ast.PrefixExpr).right))
								g.inside_assign_fn_var = old_inside_assign_fn_var
							} else {
								g.expr_with_cast(val, val_type, exp_type)
							}
						}
					}
				}
			}
			if str_add || op_overloaded || is_safe_add_assign || is_safe_sub_assign
				|| is_safe_mul_assign || is_safe_div_assign || is_safe_mod_assign {
				g.write(')')
			} else if is_safe_shift_assign {
				g.write(')')
			}
			if node_.op == .assign && var_type.has_flag(.option_mut_param_t) {
				g.write('.data, sizeof(${g.base_type(val_type)}))')
			}
			if cur_indexexpr != -1 {
				g.cur_indexexpr.delete(cur_indexexpr)
				g.write(' })')
				g.is_arraymap_set = g.cur_indexexpr.len > 0
			}
			if decl_tmp_var != '' {
				g.writeln(';')
				g.write2(decl_stmt_str, ' ')
				g.write(decl_tmp_var)
			}
			g.is_shared = false
		}
		g.right_is_opt = false
		if g.inside_ternary == 0 && (node.left.len > 1 || !node.is_simple) {
			g.writeln(';')
		}
	}
}

fn (mut g Gen) gen_multi_return_assign(node &ast.AssignStmt, return_type ast.Type, return_sym ast.TypeSymbol) {
	// multi return
	// TODO: Handle in if_expr
	mut ret_type := return_type
	mut ret_sym := return_sym
	mut suffix := ''
	if g.comptime.inside_comptime_for && node.right[0] is ast.CallExpr {
		call_expr := node.right[0] as ast.CallExpr
		if call_expr.concrete_types.len > 0 && return_sym.info is ast.MultiReturn
			&& g.comptime.comptime_for_field_var != '' {
			field_type := g.comptime.comptime_for_field_type
			field_sym := g.table.sym(field_type)
			if field_sym.info is ast.Map {
				map_info := field_sym.info as ast.Map
				ret_type =
					g.table.find_or_register_multi_return([map_info.key_type, map_info.value_type])
				ret_sym = *g.table.sym(ret_type)
			}
		}
		if g.comptime.comptime_for_field_var != '' {
			suffix = '_${g.comptime.comptime_for_field_value.name}'
		}
	}
	if node.right[0] is ast.CallExpr {
		call_expr := node.right[0] as ast.CallExpr
		mut fn_var_type := ast.void_type
		lookup_name := if call_expr.left is ast.Ident {
			call_expr.left.name
		} else {
			call_expr.name
		}
		resolved_current_type := g.resolve_current_fn_generic_param_type(lookup_name)
		if resolved_current_type != 0
			&& g.table.final_sym(g.unwrap_generic(resolved_current_type)).kind == .function {
			fn_var_type = g.unwrap_generic(g.recheck_concrete_type(resolved_current_type))
		} else if call_expr.is_fn_var {
			fn_var_type = g.unwrap_generic(g.recheck_concrete_type(call_expr.fn_var_type))
		}
		if fn_var_type == 0 {
			if obj := call_expr.scope.find_var(lookup_name) {
				if g.table.final_sym(g.unwrap_generic(obj.typ)).kind == .function {
					fn_var_type = g.unwrap_generic(g.recheck_concrete_type(obj.typ))
				}
			}
		}
		if fn_var_type != 0 {
			fn_sym := g.table.final_sym(fn_var_type)
			if fn_sym.info is ast.FnType {
				resolved_ret_type :=
					g.unwrap_generic(g.recheck_concrete_type(fn_sym.info.func.return_type))
				if resolved_ret_type != 0 && g.table.sym(resolved_ret_type).kind == .multi_return {
					ret_type = resolved_ret_type
					ret_sym = *g.table.sym(ret_type)
				}
			}
		}
	}
	mr_var_name := 'mr_${node.pos.pos}${suffix}'
	mut is_option := ret_type.has_flag(.option)
	mut mr_styp := g.styp(ret_type.clear_flag(.result))
	if node.right[0] is ast.CallExpr && node.right[0].or_block.kind != .absent {
		is_option = false
		mr_styp = g.styp(ret_type.clear_option_and_result())
	}
	g.write('${mr_styp} ${mr_var_name} = ')
	g.expr(node.right[0])
	g.writeln(';')
	raw_mr_types := (ret_sym.info as ast.MultiReturn).types
	is_generic_context := g.cur_fn != unsafe { nil } && g.cur_concrete_types.len > 0
	mr_types := if is_generic_context {
		raw_mr_types.map(g.unwrap_generic(g.recheck_concrete_type(it)))
	} else if node.right_types.len == raw_mr_types.len {
		node.right_types.clone()
	} else {
		raw_mr_types.clone()
	}
	mut recompute_types := node.op == .decl_assign || ret_type != return_type
	if g.comptime.inside_comptime_for && node.right[0] is ast.CallExpr {
		call_expr := node.right[0] as ast.CallExpr
		if call_expr.concrete_types.len > 0 && g.comptime.comptime_for_field_var != ''
			&& return_sym.info is ast.MultiReturn {
			recompute_types = true
			for i, mut lx in &node.left {
				if mut lx is ast.Ident && lx.kind != .blank_ident {
					if mut lx.obj is ast.Var {
						lx.obj.typ = mr_types[i]
					}
				}
			}
		}
	}
	if recompute_types {
		for i, mut lx in &node.left {
			if mut lx is ast.Ident && lx.kind != .blank_ident {
				if mut lx.obj is ast.Var && i < mr_types.len {
					lx.obj.typ = mr_types[i]
					if !mr_types[i].has_option_or_result() {
						lx.obj.orig_type = ast.no_type
						lx.obj.smartcasts = []
						lx.obj.is_unwrapped = false
					}
					if lx.obj.ct_type_var != .no_comptime {
						g.type_resolver.update_ct_type(lx.name, mr_types[i])
					}
				}
				if i < mr_types.len && lx.scope != unsafe { nil } {
					if mut scope_var := lx.scope.find_var(lx.name) {
						scope_var.typ = mr_types[i]
						if !mr_types[i].has_option_or_result() {
							scope_var.orig_type = ast.no_type
							scope_var.smartcasts = []
							scope_var.is_unwrapped = false
						}
					}
				}
			}
		}
	}
	for i, lx in node.left {
		mut cur_indexexpr := -1
		mut is_auto_heap := false
		mut ident := ast.Ident{
			scope: unsafe { nil }
		}
		if lx is ast.Ident {
			ident = lx
			if lx.kind == .blank_ident {
				continue
			}
			if lx.obj is ast.Var {
				is_auto_heap = lx.obj.is_auto_heap
			}
		}
		if lx is ast.IndexExpr && g.cur_indexexpr.len > 0 {
			cur_indexexpr = g.cur_indexexpr.index(lx.pos.pos)
		}
		left_type := if recompute_types { mr_types[i] } else { node.left_types[i] }
		styp := if ident.name in g.defer_vars { '' } else { g.styp(left_type) }
		needs_auto_heap_alloc := is_auto_heap && node.op == .decl_assign
		if node.op == .decl_assign {
			g.write('${styp} ')
		}
		if lx.is_auto_deref_var() {
			g.write('*')
		}
		noscan := if is_auto_heap { g.check_noscan(return_type) } else { '' }
		mut aligned := 0
		sym := g.table.final_sym(left_type)
		if sym.info is ast.Struct {
			if attr := sym.info.attrs.find_first('aligned') {
				aligned = if attr.arg == '' { 0 } else { attr.arg.int() }
			}
		}
		if left_type.has_flag(.option) {
			base_typ := g.base_type(left_type)
			tmp_var := if needs_auto_heap_alloc {
				if aligned != 0 {
					'HEAP_align(${styp}, ${mr_var_name}.arg${i}, ${aligned})'
				} else {
					'HEAP${noscan}(${styp}, ${mr_var_name}.arg${i})'
				}
			} else if is_option {
				'(*((${g.base_type(return_type)}*)${mr_var_name}.data)).arg${i}'
			} else {
				'${mr_var_name}.arg${i}'
			}
			if mr_types[i].has_flag(.option) {
				old_left_is_opt := g.left_is_opt
				g.left_is_opt = true
				g.expr(lx)
				g.writeln(' = ${tmp_var};')
				g.left_is_opt = old_left_is_opt
			} else {
				g.write('builtin___option_ok(&(${base_typ}[]) { ${tmp_var} }, (${option_name}*)(&')
				tmp_left_is_opt := g.left_is_opt
				g.left_is_opt = true
				g.expr(lx)
				g.left_is_opt = tmp_left_is_opt
				g.writeln('), sizeof(${base_typ}));')
			}
		} else {
			g.expr(lx)
			if sym.kind == .array_fixed {
				g.writeln2(';',
					'memcpy(&${g.expr_string(lx)}, &${mr_var_name}.arg${i}, sizeof(${styp}));')
			} else {
				if cur_indexexpr != -1 {
					if needs_auto_heap_alloc {
						if aligned != 0 {
							g.writeln('HEAP_align(${styp}, ${mr_var_name}.arg${i}, ${aligned}) });')
						} else {
							g.writeln('HEAP${noscan}(${styp}, ${mr_var_name}.arg${i}) });')
						}
					} else if is_option {
						g.writeln('(*((${g.base_type(return_type)}*)${mr_var_name}.data)).arg${i} });')
					} else {
						g.writeln('${mr_var_name}.arg${i} });')
					}
					g.cur_indexexpr.delete(cur_indexexpr)
				} else {
					if needs_auto_heap_alloc {
						if aligned != 0 {
							g.writeln(' = HEAP_align(${styp}, ${mr_var_name}.arg${i}, ${aligned});')
						} else {
							g.writeln(' = HEAP${noscan}(${styp}, ${mr_var_name}.arg${i});')
						}
					} else if is_option {
						g.writeln(' = (*((${g.base_type(return_type)}*)${mr_var_name}.data)).arg${i};')
					} else {
						g.writeln(' = ${mr_var_name}.arg${i};')
					}
				}
			}
		}
	}
	if g.is_arraymap_set {
		g.is_arraymap_set = false
	}
}

fn (mut g Gen) gen_cross_var_assign(node &ast.AssignStmt) {
	for i, left in node.left {
		left_is_auto_deref_var := left.is_auto_deref_var()
		match left {
			ast.Ident {
				left_typ := node.left_types[i]
				left_sym := g.table.sym(left_typ)
				mut anon_ctx := ''
				if g.anon_fn != unsafe { nil } {
					if obj := left.scope.find_var(left.name) {
						if obj.is_inherited {
							anon_ctx = '${closure_ctx}->'
						}
					}
				}
				if left_sym.info is ast.FnType {
					g.write_fn_ptr_decl(&left_sym.info, '_var_${left.pos.pos}')
					g.writeln(' = ${anon_ctx}${g.ident_cname(left)};')
				} else if left_is_auto_deref_var {
					styp := g.styp(left_typ).trim('*')
					if left_sym.kind == .array {
						g.writeln('${styp} _var_${left.pos.pos} = builtin__array_clone(${anon_ctx}${g.ident_cname(left)});')
					} else {
						g.writeln('${styp} _var_${left.pos.pos} = *${anon_ctx}${g.ident_cname(left)};')
					}
				} else {
					styp := g.styp(left_typ)
					if left_sym.kind == .array {
						g.writeln('${styp} _var_${left.pos.pos} = builtin__array_clone(&${anon_ctx}${g.ident_cname(left)});')
					} else {
						g.writeln('${styp} _var_${left.pos.pos} = ${anon_ctx}${g.ident_cname(left)};')
					}
				}
			}
			ast.IndexExpr {
				if left.is_index_operator {
					styp := g.styp(node.left_types[i])
					g.write('${styp} _var_${left.pos.pos} = ')
					g.expr(ast.Expr(left))
					g.writeln(';')
					continue
				}
				mut container_type := left.left_type
				if g.cur_fn != unsafe { nil } && g.cur_concrete_types.len > 0 {
					resolved_container_type := g.resolved_expr_type(left.left, left.left_type)
					if resolved_container_type != 0 {
						container_type =
							g.unwrap_generic(g.recheck_concrete_type(resolved_container_type))
					}
				}
				sym := g.table.sym(g.table.unaliased_type(container_type))
				if sym.kind == .array {
					info := sym.info as ast.Array
					elem_sym := g.table.sym(info.elem_type)
					needs_clone := info.elem_type == ast.string_type && g.is_autofree

					if elem_sym.kind == .function {
						left_typ := node.left_types[i]
						left_sym := g.table.sym(left_typ)
						g.write_fn_ptr_decl(left_sym.info as ast.FnType, '_var_${left.pos.pos}')
						g.write(' = *(voidptr*)builtin__array_get(')
					} else {
						styp := g.styp(info.elem_type)
						string_clone := if needs_clone { 'builtin__string_clone(' } else { '' }

						g.write('${styp} _var_${left.pos.pos} = ${string_clone}*(${styp}*)builtin__array_get(')
					}

					if left.left_type.is_ptr() || left.left.is_auto_deref_var() {
						g.write('*')
					}
					g.expr(left.left)
					g.write(', ')
					g.expr(left.index)
					if needs_clone {
						g.write(')')
					}
					g.writeln(');')
				} else if sym.kind == .array_fixed {
					info := sym.info as ast.ArrayFixed
					elem_sym := g.table.sym(info.elem_type)
					if elem_sym.kind == .function {
						left_typ := node.left_types[i]
						left_sym := g.table.sym(left_typ)
						g.write_fn_ptr_decl(left_sym.info as ast.FnType, '_var_${left.pos.pos}')
						g.write(' = *(voidptr*)')
					} else {
						styp := g.styp(info.elem_type)
						g.write('${styp} _var_${left.pos.pos} = ')
					}
					if left.left_type.is_ptr() {
						g.write('*')
					}
					needs_clone := info.elem_type == ast.string_type && g.is_autofree
					if needs_clone {
						g.write('builtin__string_clone(')
					}
					g.expr(left)
					if needs_clone {
						g.write(')')
					}
					g.writeln(';')
				} else if sym.kind == .map {
					info := sym.info as ast.Map
					styp := g.styp(info.value_type)
					zero := g.type_default(info.value_type)
					val_sym := g.table.sym(info.value_type)
					if val_sym.kind == .function {
						left_type := node.left_types[i]
						left_sym := g.table.sym(left_type)
						g.write_fn_ptr_decl(left_sym.info as ast.FnType, '_var_${left.pos.pos}')
						g.write(' = *(voidptr*)builtin__map_get(')
					} else {
						g.write('${styp} _var_${left.pos.pos} = *(${styp}*)builtin__map_get(')
					}
					if !left.left_type.is_ptr() {
						g.write('ADDR(map, ')
						g.expr(left.left)
						g.write(')')
					} else {
						g.expr(left.left)
					}
					g.write(', ')
					g.write_map_key_arg(left.index, info.key_type)
					if val_sym.kind == .function {
						g.writeln(', &(voidptr[]){ ${zero} });')
					} else {
						g.writeln(', &(${styp}[]){ ${zero} });')
					}
				}
			}
			ast.SelectorExpr {
				styp := g.styp(left.typ)
				g.write('${styp} _var_${left.pos.pos} = ')
				g.expr(left.expr)
				sel := g.dot_or_ptr(left.expr_type)
				g.writeln('${sel}${left.field_name};')
			}
			else {}
		}
	}
}

fn (mut g Gen) gen_cross_tmp_variable(left []ast.Expr, val ast.Expr) {
	val_ := val
	match val {
		ast.Ident {
			mut has_var := false
			for lx in left {
				if lx is ast.Ident {
					if val.name == lx.name {
						g.write2('_var_', lx.pos.pos.str())
						has_var = true
						break
					}
				}
			}
			if !has_var {
				g.expr(val_)
			}
		}
		ast.IndexExpr {
			mut has_var := false
			for lx in left {
				if val_.str() == lx.str() {
					g.write2('_var_', lx.pos().pos.str())
					has_var = true
					break
				}
			}
			if !has_var {
				g.expr(val_)
			}
		}
		ast.InfixExpr {
			sym := g.table.sym(val.left_type)
			svalop := val.op.str()
			if _ := g.table.find_method(sym, svalop) {
				left_styp := g.styp(val.left_type.set_nr_muls(0))
				g.write2(left_styp, '_')
				g.write2(util.replace_op(svalop), '(')
				g.gen_cross_tmp_variable(left, val.left)
				g.write(', ')
				g.gen_cross_tmp_variable(left, val.right)
				g.write(')')
			} else {
				g.gen_cross_tmp_variable(left, val.left)
				g.write(svalop)
				g.gen_cross_tmp_variable(left, val.right)
			}
		}
		ast.ParExpr {
			g.write('(')
			g.gen_cross_tmp_variable(left, val.expr)
			g.write(')')
		}
		ast.CallExpr {
			if val.is_method {
				unwrapped_rec_type, typ_sym := g.unwrap_receiver_type(val)
				left_type := g.unwrap_generic(val.left_type)
				left_sym := g.table.sym(left_type)
				final_left_sym := g.table.final_sym(left_type)
				rec_typ_name := g.resolve_receiver_name(val, unwrapped_rec_type, final_left_sym,
					left_sym, typ_sym)
				mut fn_name := util.no_dots('${rec_typ_name}_${val.name}')
				if resolved_sym := g.table.find_sym(rec_typ_name) {
					if resolved_sym.is_builtin() && !fn_name.starts_with('builtin__') {
						fn_name = 'builtin__${fn_name}'
					}
				} else if rec_typ_name in ['int_literal', 'float_literal', 'vint_t'] {
					fn_name = 'builtin__${fn_name}'
				}
				g.write('${fn_name}(&')
				g.gen_cross_tmp_variable(left, val.left)
				for i, arg in val.args {
					g.gen_cross_tmp_variable(left, arg.expr)
					if i != val.args.len - 1 {
						g.write(', ')
					}
				}
				g.write(')')
			} else {
				mut fn_name := val.name.replace('.', '__')
				if val.concrete_types.len > 0 {
					fn_name = g.generic_fn_name(val.concrete_types, fn_name)
				}
				g.write('${fn_name}(')
				for i, arg in val.args {
					g.gen_cross_tmp_variable(left, arg.expr)
					if i != val.args.len - 1 {
						g.write(', ')
					}
				}
				g.write(')')
			}
		}
		ast.PrefixExpr {
			g.write(val.op.str())
			g.gen_cross_tmp_variable(left, val.right)
		}
		ast.PostfixExpr {
			g.gen_cross_tmp_variable(left, val.expr)
			g.write(val.op.str())
		}
		ast.SelectorExpr {
			mut has_var := false
			for lx in left {
				if val_.str() == lx.str() {
					g.write2('_var_', lx.pos().pos.str())
					has_var = true
					break
				}
			}
			if !has_var {
				g.expr(val_)
			}
		}
		else {
			g.expr(val_)
		}
	}
}
