// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module transformer

import v2.ast
import v2.token
import v2.types

fn (mut t Transformer) register_if_guard_temp_and_payload_types(temp_name string, guard_lhs []ast.Expr, rhs ast.Expr) {
	wrapper_type := t.expr_wrapper_type_for_or(rhs) or { return }
	t.register_temp_var(temp_name, wrapper_type)
	mut data_type := types.Type(types.voidptr_)
	mut has_data_type := false
	match wrapper_type {
		types.ResultType {
			data_type = wrapper_type.base_type
			has_data_type = true
		}
		types.OptionType {
			data_type = wrapper_type.base_type
			has_data_type = true
		}
		else {}
	}

	if !has_data_type {
		return
	}
	for lhs_expr in guard_lhs {
		if lhs_name := assign_lhs_ident_name(lhs_expr) {
			if lhs_name != '_' {
				t.register_temp_var(lhs_name, data_type)
			}
		}
	}
}

fn (mut t Transformer) transform_if_guard_else_expr(expr ast.Expr) ast.Expr {
	if expr is ast.EmptyExpr {
		return expr
	}
	transformed := t.transform_expr(expr)
	mut else_stmts := []ast.Stmt{}
	if t.pending_stmts.len > 0 {
		else_stmts << t.pending_stmts
		t.pending_stmts.clear()
	}
	if transformed is ast.UnsafeExpr {
		else_stmts << transformed.stmts
	} else {
		if else_stmts.len == 0 {
			return transformed
		}
		else_stmts << ast.ExprStmt{
			expr: transformed
		}
	}
	if else_stmts.len == 0 {
		return ast.empty_expr
	}
	return ast.IfExpr{
		cond:  ast.empty_expr
		stmts: else_stmts
		pos:   expr.pos()
	}
}

// try_expand_if_guard_assign_stmts expands an if-guard assignment to multiple statements.
// Transforms: x := if r := map[key] { r } else { default }
// Into (for maps):
//   x := if key in map { map[key] } else { default }
// Into (for other cases):
//   r := expr
//   x := if r { r } else { default }
fn (mut t Transformer) try_expand_if_guard_assign_stmts(stmt ast.AssignStmt) ?[]ast.Stmt {
	// Check for single assignment with IfExpr RHS
	if stmt.rhs.len != 1 || stmt.lhs.len != 1 {
		return none
	}
	rhs_expr := stmt.rhs[0]
	// Check if RHS is an IfExpr with IfGuardExpr condition
	if rhs_expr !is ast.IfExpr {
		return none
	}
	if_expr := rhs_expr as ast.IfExpr
	if if_expr.cond !is ast.IfGuardExpr {
		return none
	}
	guard := if_expr.cond as ast.IfGuardExpr

	// Extract guard variable name from LHS of the guard assignment
	mut guard_var_name := ''
	for lhs_expr in guard.stmt.lhs {
		if lhs_expr is ast.Ident {
			guard_var_name = lhs_expr.name
			break
		} else if lhs_expr is ast.ModifierExpr {
			if lhs_expr.expr is ast.Ident {
				guard_var_name = lhs_expr.expr.name
				break
			}
		}
	}
	if guard_var_name == '' || guard.stmt.rhs.len == 0 {
		return none
	}

	guard_rhs := guard.stmt.rhs[0]
	synth_pos := t.next_synth_pos()

	// Check if RHS is a map index expression - use "key in map" condition
	if guard_rhs is ast.IndexExpr {
		if _ := t.get_map_type_for_expr(guard_rhs.lhs) {
			// This is a map access - transform using "key in map" check
			// x := if key in map { map[key] } else { default }
			key_in_map := t.make_infix_expr_at(.key_in, guard_rhs.expr, guard_rhs.lhs,
				guard_rhs.pos)

			// Build new stmts for the then-branch: guard_var := map[key]; <original stmts>
			mut new_then_stmts := []ast.Stmt{cap: if_expr.stmts.len + 1}
			new_then_stmts << ast.AssignStmt{
				op:  .decl_assign
				lhs: guard.stmt.lhs
				rhs: guard.stmt.rhs
				pos: guard.stmt.pos
			}
			for s in if_expr.stmts {
				new_then_stmts << s
			}

			modified_if := ast.IfExpr{
				cond:      key_in_map
				stmts:     new_then_stmts
				else_expr: if_expr.else_expr
				pos:       synth_pos
			}
			// Propagate the original IfExpr type to the synthesized node
			if orig_type := t.get_expr_type(ast.Expr(if_expr)) {
				t.register_synth_type(synth_pos, orig_type)
			}

			return [
				ast.Stmt(ast.AssignStmt{
					op:  stmt.op
					lhs: stmt.lhs
					rhs: [ast.Expr(modified_if)]
					pos: stmt.pos
				}),
			]
		}
	}

	mut is_result := t.expr_returns_result(guard_rhs)
	mut is_option := t.expr_returns_option(guard_rhs)
	if !is_result && !is_option {
		fn_name := t.get_call_fn_name(guard_rhs)
		is_result = fn_name != '' && t.fn_returns_result(fn_name)
		is_option = fn_name != '' && t.fn_returns_option(fn_name)
	}
	if is_result || is_option {
		temp_name := t.gen_temp_name()
		temp_ident := ast.Ident{
			name: temp_name
			pos:  synth_pos
		}
		mut data_type := types.Type(types.voidptr_)
		mut has_data_type := false
		if wrapper_type := t.expr_wrapper_type_for_or(guard_rhs) {
			t.register_temp_var(temp_name, wrapper_type)
			match wrapper_type {
				types.ResultType {
					data_type = wrapper_type.base_type
					has_data_type = true
				}
				types.OptionType {
					data_type = wrapper_type.base_type
					has_data_type = true
				}
				else {}
			}
		}
		if !has_data_type {
			ret_type_name := t.get_call_return_type(guard_rhs)
			if ret_type_name.starts_with('_option_') {
				base_name := ret_type_name['_option_'.len..]
				if base_type := t.c_return_base_type_to_type(base_name) {
					data_type = base_type
					has_data_type = true
				}
			} else if ret_type_name.starts_with('_result_') {
				base_name := ret_type_name['_result_'.len..]
				if base_type := t.c_return_base_type_to_type(base_name) {
					data_type = base_type
					has_data_type = true
				}
			}
		}
		if has_data_type && guard_var_name != '' {
			t.register_temp_var(guard_var_name, data_type)
		}
		success_cond := if is_result {
			ast.Expr(ast.PrefixExpr{
				op:   .not
				expr: t.synth_selector(temp_ident, 'is_error', types.Type(types.bool_))
			})
		} else {
			t.make_infix_expr(.eq, t.synth_selector(temp_ident, 'state', types.Type(types.int_)),
				t.make_number_expr('0'))
		}
		mut then_stmts := []ast.Stmt{}
		then_stmts << ast.AssignStmt{
			op:  .decl_assign
			lhs: guard.stmt.lhs
			rhs: [t.synth_selector(temp_ident, 'data', data_type)]
			pos: guard.stmt.pos
		}
		for s in if_expr.stmts {
			then_stmts << s
		}
		mut else_expr := if_expr.else_expr
		if is_result && else_expr is ast.IfExpr {
			else_if := else_expr as ast.IfExpr
			if else_if.cond is ast.EmptyExpr {
				mut else_stmts := []ast.Stmt{cap: else_if.stmts.len + 1}
				else_stmts << ast.AssignStmt{
					op:  .decl_assign
					lhs: [ast.Expr(ast.Ident{
						name: 'err'
					})]
					rhs: [
						t.synth_selector(temp_ident, 'err', types.Type(types.Struct{
							name: 'IError'
						})),
					]
					pos: guard.stmt.pos
				}
				for s in else_if.stmts {
					else_stmts << s
				}
				else_expr = ast.Expr(ast.IfExpr{
					cond:  ast.empty_expr
					stmts: else_stmts
					pos:   else_if.pos
				})
			}
		}
		modified_if := ast.IfExpr{
			cond:      success_cond
			stmts:     then_stmts
			else_expr: else_expr
			pos:       synth_pos
		}
		if orig_type := t.get_expr_type(ast.Expr(if_expr)) {
			t.register_synth_type(synth_pos, orig_type)
		}
		return [
			ast.Stmt(ast.AssignStmt{
				op:  .decl_assign
				lhs: [ast.Expr(temp_ident)]
				rhs: [guard_rhs]
				pos: synth_pos
			}),
			ast.Stmt(ast.AssignStmt{
				op:  stmt.op
				lhs: stmt.lhs
				rhs: [ast.Expr(modified_if)]
				pos: stmt.pos
			}),
		]
	}

	// Non-map case: use original approach
	mut stmts := []ast.Stmt{}

	// 1. Guard variable declaration: r := expr
	stmts << ast.AssignStmt{
		op:  .decl_assign
		lhs: guard.stmt.lhs
		rhs: guard.stmt.rhs
		pos: guard.stmt.pos
	}

	// 2. Modified if expression with guard variable as condition
	//    x := if r { r } else { default }
	// Use synthesized position to avoid inheriting wrong type from original IfGuardExpr
	guard_ident := ast.Ident{
		name: guard_var_name
		pos:  synth_pos
	}
	modified_if := ast.IfExpr{
		cond:      guard_ident
		stmts:     if_expr.stmts
		else_expr: if_expr.else_expr
		pos:       synth_pos
	}
	// Propagate the original IfExpr type to the synthesized node
	if orig_type := t.get_expr_type(ast.Expr(if_expr)) {
		t.register_synth_type(synth_pos, orig_type)
	}
	stmts << ast.AssignStmt{
		op:  stmt.op
		lhs: stmt.lhs
		rhs: [ast.Expr(modified_if)]
		pos: stmt.pos
	}

	return stmts
}

// try_expand_if_guard_stmt expands a statement-level if-guard.
// Transforms: if attr := table[name] { use(attr) }
// Into: if (table[name]) { attr := table[name]; use(attr) }
// For Result types: if attr := fn_call() { use(attr) }
// Into: { _tmp := fn_call(); if (!_tmp.is_error) { attr := *(_tmp.data); use(attr) } else { else_body } }
fn (mut t Transformer) try_expand_if_guard_stmt(stmt ast.ExprStmt) ?[]ast.Stmt {
	// Check if this is an IfExpr with IfGuardExpr condition
	if stmt.expr !is ast.IfExpr {
		return none
	}
	if_expr := stmt.expr as ast.IfExpr
	if if_expr.cond !is ast.IfGuardExpr {
		return none
	}
	guard := if_expr.cond as ast.IfGuardExpr

	if guard.stmt.rhs.len == 0 {
		return none
	}

	rhs := guard.stmt.rhs[0]
	synth_pos := t.next_synth_pos()

	mut rhs_is_map_index := false
	mut map_index_expr := ast.IndexExpr{}
	if idx := t.map_index_expr_from_guard_rhs(rhs) {
		rhs_is_map_index = true
		map_index_expr = idx
	}

	// Check if RHS is a call that returns Result/Option
	// First try expression-based lookup (works for both function and method calls)
	mut is_result := false
	mut is_option := false
	if !rhs_is_map_index {
		is_result = t.expr_returns_result(rhs)
		is_option = t.expr_returns_option(rhs)
	}

	// Fallback to function name lookup for simple function calls
	if !rhs_is_map_index && !is_result && !is_option {
		fn_name := t.get_call_fn_name(rhs)
		is_result = fn_name != '' && t.fn_returns_result(fn_name)
		is_option = fn_name != '' && t.fn_returns_option(fn_name)
	}

	if is_result || is_option {
		// Handle Result/Option if-guard
		// Generate: { _tmp := call(); if (!_tmp.is_error) { attr := extractValue(_tmp); body } else { else } }
		temp_name := t.gen_temp_name()
		temp_ident := ast.Ident{
			name: temp_name
			pos:  synth_pos
		}

		mut stmts := []ast.Stmt{}

		mut data_type := types.Type(types.voidptr_)
		mut has_data_type := false
		// Register temp variable type so cleanc can look up its type for .data unwrapping
		if wrapper_type := t.expr_wrapper_type_for_or(rhs) {
			t.register_temp_var(temp_name, wrapper_type)
			match wrapper_type {
				types.ResultType {
					data_type = wrapper_type.base_type
					has_data_type = true
				}
				types.OptionType {
					data_type = wrapper_type.base_type
					has_data_type = true
				}
				else {}
			}
		} else if wrapper_type := t.get_expr_type(rhs) {
			t.register_temp_var(temp_name, wrapper_type)
			match wrapper_type {
				types.ResultType {
					data_type = wrapper_type.base_type
					has_data_type = true
				}
				types.OptionType {
					data_type = wrapper_type.base_type
					has_data_type = true
				}
				else {}
			}
		}
		if !has_data_type {
			ret_type_name := t.get_call_return_type(rhs)
			if ret_type_name.starts_with('_option_') {
				base_name := ret_type_name['_option_'.len..]
				if base_type := t.c_return_base_type_to_type(base_name) {
					data_type = base_type
					has_data_type = true
				}
			} else if ret_type_name.starts_with('_result_') {
				base_name := ret_type_name['_result_'.len..]
				if base_type := t.c_return_base_type_to_type(base_name) {
					data_type = base_type
					has_data_type = true
				}
			}
		}
		mut guard_var_name := ''
		for lhs_expr in guard.stmt.lhs {
			if lhs_expr is ast.Ident {
				guard_var_name = lhs_expr.name
				break
			}
			if lhs_expr is ast.ModifierExpr && lhs_expr.expr is ast.Ident {
				guard_var_name = (lhs_expr.expr as ast.Ident).name
				break
			}
		}
		if has_data_type && guard_var_name != '' {
			t.register_temp_var(guard_var_name, data_type)
		}

		// 1. _tmp := call()
		transformed_rhs := t.transform_expr(rhs)
		if t.pending_stmts.len > 0 {
			stmts << t.pending_stmts
			t.pending_stmts.clear()
		}
		stmts << ast.AssignStmt{
			op:  .decl_assign
			lhs: [ast.Expr(temp_ident)]
			rhs: [transformed_rhs]
			pos: synth_pos
		}

		// 2. Build condition: !_tmp.is_error (for Result) or _tmp.state == 0 (for Option)
		success_cond := if is_result {
			ast.Expr(ast.PrefixExpr{
				op:   .not
				expr: t.synth_selector(temp_ident, 'is_error', types.Type(types.bool_))
			})
		} else {
			t.make_infix_expr(.eq, t.synth_selector(temp_ident, 'state', types.Type(types.int_)),
				t.make_number_expr('0'))
		}

		// 3. Build if-body: attr := _tmp.data; original_body
		// cleanc handles the cast and dereference when it sees .data on Result type
		mut if_stmts := []ast.Stmt{}
		data_access := t.synth_selector(temp_ident, 'data', data_type)
		if_stmts << ast.AssignStmt{
			op:  .decl_assign
			lhs: guard.stmt.lhs
			rhs: [data_access]
			pos: guard.stmt.pos
		}
		for s in if_expr.stmts {
			if_stmts << s
		}

		mut else_expr := if_expr.else_expr
		if is_result && else_expr is ast.IfExpr {
			else_if := else_expr as ast.IfExpr
			if else_if.cond is ast.EmptyExpr {
				mut else_stmts := []ast.Stmt{cap: else_if.stmts.len + 1}
				else_stmts << ast.AssignStmt{
					op:  .decl_assign
					lhs: [ast.Expr(ast.Ident{
						name: 'err'
					})]
					rhs: [
						t.synth_selector(temp_ident, 'err', types.Type(types.Struct{
							name: 'IError'
						})),
					]
					pos: else_if.pos
				}
				else_stmts << else_if.stmts
				else_expr = ast.IfExpr{
					cond:      else_if.cond
					stmts:     else_stmts
					else_expr: else_if.else_expr
					pos:       else_if.pos
				}
			}
		}

		// 4. Build the if expression
		modified_if := ast.IfExpr{
			cond:      success_cond
			stmts:     t.transform_stmts(if_stmts)
			else_expr: t.transform_if_guard_else_expr(else_expr)
			pos:       synth_pos
		}
		// Propagate the original IfExpr type to the synthesized node
		if orig_type := t.get_expr_type(ast.Expr(if_expr)) {
			t.register_synth_type(synth_pos, orig_type)
		}
		stmts << ast.ExprStmt{
			expr: modified_if
		}

		return stmts
	}

	// Non-Result/Option if-guard
	// Check if RHS is an index expression (map or array lookup)
	// For map lookups: if x := map[key] { use(x) }
	// Transform to: { _tmp := map__get_check(&map, &key); if (_tmp != nil) { x := *_tmp; use(x) } }
	// For array lookups: if x := arr[i] { use(x) }
	// Transform to: if (i < arr.len) { x := arr[i]; use(x) }
	if rhs_is_map_index {
		rhs_idx := map_index_expr
		if map_expr_typ := t.get_expr_type(rhs_idx.lhs) {
			if map_type := t.unwrap_map_type(map_expr_typ) {
				// This is a map lookup - use map__get_check pattern
				temp_name := t.gen_temp_name()
				temp_ident := ast.Ident{
					name: temp_name
					pos:  synth_pos
				}

				// Register temp variable type: map__get_check returns pointer to value type
				t.register_temp_var(temp_name, types.Pointer{
					base_type: map_type.value_type
				})

				map_arg := if t.is_pointer_type(map_expr_typ) {
					t.transform_expr(rhs_idx.lhs)
				} else {
					t.addr_of_expr_with_temp(rhs_idx.lhs, map_expr_typ)
				}

				get_check_call := ast.CallExpr{
					lhs:  ast.Ident{
						name: 'map__get_check'
					}
					args: [
						map_arg,
						t.voidptr_cast(t.addr_of_expr_with_temp(rhs_idx.expr, t.map_key_type_for_expr(rhs_idx.expr,
							map_type))),
					]
				}
				temp_assign := ast.AssignStmt{
					op:  .decl_assign
					lhs: [ast.Expr(temp_ident)]
					rhs: [ast.Expr(get_check_call)]
					pos: synth_pos
				}

				// Build if body: guard_var := *_tmp; original_body
				// Use typed_deref to cast voidptr to correct pointer type
				// before dereference (map__get_check returns voidptr)
				mut if_stmts := []ast.Stmt{}
				deref_tmp := t.typed_deref(temp_ident, map_type.value_type)
				if_stmts << ast.AssignStmt{
					op:  .decl_assign
					lhs: guard.stmt.lhs
					rhs: [deref_tmp]
					pos: guard.stmt.pos
				}
				for s in if_expr.stmts {
					if_stmts << s
				}

				// Build condition: _tmp != nil
				null_check := t.make_infix_expr(.ne, ast.Expr(temp_ident), ast.Expr(ast.Ident{
					name: 'nil'
				}))

				// Build the if expression
				modified_if := ast.IfExpr{
					cond:      null_check
					stmts:     t.transform_stmts(if_stmts)
					else_expr: t.transform_expr(if_expr.else_expr)
					pos:       synth_pos
				}
				// Propagate the original IfExpr type to the synthesized node
				if orig_type := t.get_expr_type(ast.Expr(if_expr)) {
					t.register_synth_type(synth_pos, orig_type)
				}

				return [
					ast.Stmt(temp_assign),
					ast.Stmt(ast.ExprStmt{
						expr: modified_if
					}),
				]
			}
		}
	} else if rhs is ast.IndexExpr {
		rhs_idx := rhs
		// This is an array lookup - generate bounds check: index < array.len
		bounds_check := t.make_infix_expr_at(.lt, t.transform_expr(rhs_idx.expr), t.synth_selector(t.transform_expr(rhs_idx.lhs),
			'len', types.Type(types.int_)), rhs_idx.pos)

		// Build if body: guard_var := arr[i]; original_body
		mut if_stmts := []ast.Stmt{}
		if_stmts << ast.AssignStmt{
			op:  .decl_assign
			lhs: guard.stmt.lhs
			rhs: guard.stmt.rhs
			pos: guard.stmt.pos
		}
		for s in if_expr.stmts {
			if_stmts << s
		}

		// Build the if expression
		modified_if := ast.IfExpr{
			cond:      bounds_check
			stmts:     t.transform_stmts(if_stmts)
			else_expr: t.transform_expr(if_expr.else_expr)
			pos:       synth_pos
		}
		// Propagate the original IfExpr type to the synthesized node
		if orig_type := t.get_expr_type(ast.Expr(if_expr)) {
			t.register_synth_type(synth_pos, orig_type)
		}

		return [
			ast.Stmt(ast.ExprStmt{
				expr: modified_if
			}),
		]
	}

	rhs_expr := t.transform_expr(rhs)

	// For map lookups returning arrays, generate:
	// { arr := map[key]; if (arr.data != nil) { ... } }
	// This handles the case where "key exists" = "non-nil data"
	map_returns_array := t.is_map_lookup_returning_array(rhs)

	// Prepend guard variable assignment to stmts (inside the if body)
	guard_assign := ast.AssignStmt{
		op:  .decl_assign
		lhs: guard.stmt.lhs
		rhs: guard.stmt.rhs
		pos: guard.stmt.pos
	}
	mut new_stmts := []ast.Stmt{cap: if_expr.stmts.len + 1}
	new_stmts << guard_assign
	for s in if_expr.stmts {
		new_stmts << s
	}

	// Determine the condition to use
	mut cond_expr := ast.Expr(rhs_expr)
	if map_returns_array {
		// For arrays, check .data != nil (indicates key existed in map)
		// Extract guard variable name
		mut guard_var_name := ''
		for lhs_expr in guard.stmt.lhs {
			if lhs_expr is ast.Ident {
				guard_var_name = lhs_expr.name
				break
			}
		}
		// Check if it's a blank identifier - if so, use a temp variable
		is_blank := guard_var_name == '_'
		if is_blank {
			guard_var_name = t.gen_temp_name()
		}
		if guard_var_name != '' {
			// Generate: guard_var.data != nil
			// But we need to declare the variable first, so we generate:
			// { arr := map[key]; if (arr.data) { ... } }
			// Put assignment before the if, then use arr.data as condition
			// When blank, use temp variable instead of _
			temp_lhs := if is_blank {
				[
					ast.Expr(ast.Ident{
						name: guard_var_name
						pos:  synth_pos
					}),
				]
			} else {
				guard.stmt.lhs
			}
			temp_assign := ast.AssignStmt{
				op:  .decl_assign
				lhs: temp_lhs
				rhs: guard.stmt.rhs
				pos: guard.stmt.pos
			}
			// Remove the guard_assign from new_stmts since we're putting it before the if
			new_stmts = []ast.Stmt{cap: if_expr.stmts.len}
			for s in if_expr.stmts {
				new_stmts << s
			}
			// Use arr.data as condition
			cond_expr = t.synth_selector(ast.Ident{
				name: guard_var_name
				pos:  synth_pos
			}, 'data', types.Type(types.voidptr_))
			modified_if := ast.IfExpr{
				cond:      cond_expr
				stmts:     t.transform_stmts(new_stmts)
				else_expr: t.transform_expr(if_expr.else_expr)
				pos:       synth_pos
			}
			// Propagate the original IfExpr type to the synthesized node
			if orig_type := t.get_expr_type(ast.Expr(if_expr)) {
				t.register_synth_type(synth_pos, orig_type)
			}
			return [
				ast.Stmt(temp_assign),
				ast.Stmt(ast.ExprStmt{
					expr: modified_if
				}),
			]
		}
	}

	modified_if := ast.IfExpr{
		cond:      cond_expr
		stmts:     t.transform_stmts(new_stmts)
		else_expr: t.transform_expr(if_expr.else_expr)
		pos:       synth_pos
	}
	// Propagate the original IfExpr type to the synthesized node
	if orig_type := t.get_expr_type(ast.Expr(if_expr)) {
		t.register_synth_type(synth_pos, orig_type)
	}

	return [ast.Stmt(ast.ExprStmt{
		expr: modified_if
	})]
}

// try_expand_return_if_expr handles IfExpr in return statements
// Transforms: return if cond { a } else { b }
// Into: if cond { return a } else { return b }
fn (mut t Transformer) try_expand_return_if_expr(stmt ast.ReturnStmt) ?[]ast.Stmt {
	// Only handle single-expression returns with IfExpr
	if stmt.exprs.len != 1 {
		return none
	}
	if_expr := stmt.exprs[0]
	if if_expr !is ast.IfExpr {
		return none
	}
	ie := if_expr as ast.IfExpr
	// Must have an else branch to be a valid expression form
	if ie.else_expr is ast.EmptyExpr {
		return none
	}
	// Transform into if-statement with return in each branch
	return t.expand_return_if_expr(ie)
}

// expand_return_if_expr recursively expands an if-expression into if-statements with returns
fn (mut t Transformer) expand_return_if_expr(ie ast.IfExpr) []ast.Stmt {
	// Build the then-branch statements with return
	mut then_stmts := []ast.Stmt{}
	for i, s in ie.stmts {
		if i == ie.stmts.len - 1 {
			// Last statement - wrap in return if it's an expression
			if s is ast.ExprStmt {
				then_stmts << ast.ReturnStmt{
					exprs: [s.expr]
				}
			} else {
				then_stmts << s
			}
		} else {
			then_stmts << s
		}
	}

	// Build the else-branch
	mut else_expr := ast.empty_expr
	if ie.else_expr is ast.IfExpr {
		else_ie := ie.else_expr as ast.IfExpr
		// Check if this is a pure else block (no condition)
		if else_ie.cond is ast.EmptyExpr {
			// Pure else - create an if block with just the else stmts that include return
			mut else_stmts := []ast.Stmt{}
			for i, s in else_ie.stmts {
				if i == else_ie.stmts.len - 1 {
					if s is ast.ExprStmt {
						else_stmts << ast.ReturnStmt{
							exprs: [s.expr]
						}
					} else {
						else_stmts << s
					}
				} else {
					else_stmts << s
				}
			}
			else_expr = ast.Expr(ast.IfExpr{
				cond:      ast.empty_expr
				stmts:     else_stmts
				else_expr: ast.empty_expr
			})
		} else {
			// else-if chain - recursively expand
			expanded_else := t.expand_return_if_expr(else_ie)
			// The expanded result should be an ExprStmt containing an IfExpr
			if expanded_else.len > 0 && expanded_else[0] is ast.ExprStmt {
				expr_stmt := expanded_else[0] as ast.ExprStmt
				else_expr = expr_stmt.expr
			} else {
				// Fallback: wrap in else block
				else_expr = ast.Expr(ast.IfExpr{
					cond:      ast.empty_expr
					stmts:     expanded_else
					else_expr: ast.empty_expr
				})
			}
		}
	} else {
		// Simple else - wrap the expression in return
		else_expr = ast.Expr(ast.IfExpr{
			cond:      ast.empty_expr
			stmts:     [ast.Stmt(ast.ReturnStmt{
				exprs: [ie.else_expr]
			})]
			else_expr: ast.empty_expr
		})
	}

	// Create the transformed if expression (used as statement)
	transformed_if := ast.IfExpr{
		cond:      ie.cond
		stmts:     then_stmts
		else_expr: else_expr
	}
	// Wrap in ExprStmt to make it a valid statement
	return [ast.Stmt(ast.ExprStmt{
		expr: transformed_if
	})]
}

// try_expand_if_expr_assign_stmts handles assignment with IfExpr RHS.
// Transforms: lhs = if cond { a } else { b }
// Into: if cond { lhs = a } else { lhs = b }
fn (mut t Transformer) try_expand_if_expr_assign_stmts(stmt ast.AssignStmt) ?[]ast.Stmt {
	if stmt.op !in [.decl_assign, .assign] || stmt.lhs.len != 1 || stmt.rhs.len != 1 {
		return none
	}
	rhs := stmt.rhs[0]
	if rhs !is ast.IfExpr {
		return none
	}
	ie := rhs as ast.IfExpr
	// Expression-form if must have else branch.
	if ie.else_expr is ast.EmptyExpr {
		return none
	}
	expanded_if := t.expand_assign_if_expr(stmt.lhs[0], ie)
	if stmt.op == .assign {
		return expanded_if
	}
	decl_type := t.get_expr_type(rhs) or { return none }
	decl_base := t.unwrap_alias_and_pointer_type(decl_type)
	if decl_base !is types.Enum {
		return none
	}
	mut result := []ast.Stmt{cap: expanded_if.len + 1}
	result << ast.Stmt(ast.AssignStmt{
		op:  .decl_assign
		lhs: stmt.lhs
		rhs: [t.zero_value_expr_for_type(decl_type)]
		pos: stmt.pos
	})
	result << expanded_if
	return result
}

// expand_assign_if_expr recursively expands an if-expression into if-statements
// that perform assignment in each branch.
fn (mut t Transformer) expand_assign_if_expr(lhs ast.Expr, ie ast.IfExpr) []ast.Stmt {
	mut then_stmts := []ast.Stmt{}
	for i, s in ie.stmts {
		if i == ie.stmts.len - 1 && s is ast.ExprStmt {
			then_stmts << ast.AssignStmt{
				op:  .assign
				lhs: [lhs]
				rhs: [s.expr]
			}
		} else {
			then_stmts << s
		}
	}

	mut else_expr := ast.empty_expr
	if ie.else_expr is ast.IfExpr {
		else_ie := ie.else_expr as ast.IfExpr
		// else { ... }
		if else_ie.cond is ast.EmptyExpr {
			mut else_stmts := []ast.Stmt{}
			for i, s in else_ie.stmts {
				if i == else_ie.stmts.len - 1 && s is ast.ExprStmt {
					else_stmts << ast.AssignStmt{
						op:  .assign
						lhs: [lhs]
						rhs: [s.expr]
					}
				} else {
					else_stmts << s
				}
			}
			else_expr = ast.Expr(ast.IfExpr{
				cond:      ast.empty_expr
				stmts:     else_stmts
				else_expr: ast.empty_expr
			})
		} else {
			expanded_else := t.expand_assign_if_expr(lhs, else_ie)
			if expanded_else.len > 0 && expanded_else[0] is ast.ExprStmt {
				else_expr = (expanded_else[0] as ast.ExprStmt).expr
			} else {
				else_expr = ast.Expr(ast.IfExpr{
					cond:      ast.empty_expr
					stmts:     expanded_else
					else_expr: ast.empty_expr
				})
			}
		}
	} else {
		else_expr = ast.Expr(ast.IfExpr{
			cond:      ast.empty_expr
			stmts:     [
				ast.Stmt(ast.AssignStmt{
					op:  .assign
					lhs: [lhs]
					rhs: [ie.else_expr]
				}),
			]
			else_expr: ast.empty_expr
		})
	}

	transformed_if := ast.IfExpr{
		cond:      ie.cond
		stmts:     then_stmts
		else_expr: else_expr
	}
	return [ast.Stmt(ast.ExprStmt{
		expr: transformed_if
	})]
}

// if_expr_is_value returns true if the IfExpr produces a value (i.e., the body's
// last statement is an ExprStmt whose expression has a non-void type).
// This distinguishes value-position ifs from statement-position ifs that happen
// to have an else branch.
fn (t &Transformer) if_expr_is_value(ie ast.IfExpr) bool {
	if ie.stmts.len == 0 {
		return false
	}
	last := ie.stmts[ie.stmts.len - 1]
	if last !is ast.ExprStmt {
		return false
	}
	// Check that the expression actually produces a non-void value.
	// Statement-form ifs may end with void function calls or postfix ops
	// used for side effects (i++, println(...), etc).
	last_expr := (last as ast.ExprStmt).expr
	if typ := t.get_expr_type(last_expr) {
		type_name := t.type_to_c_name(typ)
		if type_name == '' || type_name == 'void' {
			return false
		}
	}
	return true
}

// try_expand_tuple_if_assign_stmts expands `x, y, w, h := if cond { a, b, c, d } else { e, f, g, h }`
// into individual declarations + if-statement with assignments:
//   x := 0; y := 0; w := 0; h := 0;
//   if cond { x = a; y = b; w = c; h = d } else { x = e; y = f; w = g; h = h2 }
fn (mut t Transformer) try_expand_tuple_if_assign_stmts(stmt ast.AssignStmt) ?[]ast.Stmt {
	if stmt.op !in [.decl_assign, .assign] {
		return none
	}
	// Must have tuple LHS
	is_tuple_lhs := stmt.lhs.len > 1 || (stmt.lhs.len == 1 && stmt.lhs[0] is ast.Tuple)
	if !is_tuple_lhs {
		return none
	}
	// Must have single IfExpr RHS
	if stmt.rhs.len != 1 {
		return none
	}
	if stmt.rhs[0] !is ast.IfExpr {
		// Check if RHS might be Tuple containing an IfExpr
		return none
	}
	tuple_lhs := if stmt.lhs.len > 1 {
		stmt.lhs
	} else if stmt.lhs[0] is ast.Tuple {
		(stmt.lhs[0] as ast.Tuple).exprs
	} else {
		stmt.lhs
	}
	n := tuple_lhs.len
	if n == 0 {
		return none
	}
	if_expr := stmt.rhs[0] as ast.IfExpr
	// Extract tuple values from then branch
	then_stmts := t.build_tuple_branch_assigns(if_expr.stmts, tuple_lhs, n, stmt.pos) or {
		return none
	}
	// Process else branch
	mut else_expr := ast.Expr(ast.empty_expr)
	if if_expr.else_expr is ast.IfExpr {
		else_if := if_expr.else_expr as ast.IfExpr
		if else_if.cond is ast.EmptyExpr {
			// Plain else block
			else_stmts := t.build_tuple_branch_assigns(else_if.stmts, tuple_lhs, n, stmt.pos) or {
				return none
			}
			else_expr = ast.IfExpr{
				stmts: else_stmts
				pos:   else_if.pos
			}
		} else {
			// else-if chain
			else_if_stmts := t.build_tuple_branch_assigns(else_if.stmts, tuple_lhs, n, stmt.pos) or {
				return none
			}
			else_expr = ast.IfExpr{
				cond:      else_if.cond
				stmts:     else_if_stmts
				else_expr: else_if.else_expr
				pos:       else_if.pos
			}
		}
	}
	// Build result: declarations for each variable when needed, then the if-statement.
	mut result := []ast.Stmt{cap: n + 1}
	if stmt.op == .decl_assign {
		for lhs_expr in tuple_lhs {
			idx := result.len
			zero_expr := t.tuple_if_decl_zero_value(if_expr, idx, n)
			result << ast.Stmt(ast.AssignStmt{
				op:  .decl_assign
				lhs: [lhs_expr]
				rhs: [zero_expr]
				pos: stmt.pos
			})
		}
	}
	result << ast.Stmt(ast.ExprStmt{
		expr: ast.IfExpr{
			cond:      if_expr.cond
			stmts:     then_stmts
			else_expr: else_expr
			pos:       if_expr.pos
		}
	})
	return result
}

fn (t &Transformer) tuple_if_decl_zero_value(if_expr ast.IfExpr, idx int, n int) ast.Expr {
	if values := t.extract_branch_tuple_values(if_expr.stmts, n) {
		if idx < values.len {
			if expr_is_string_literal(values[idx]) {
				return t.zero_value_expr_for_type(types.string_)
			}
			if typ := t.get_expr_type(values[idx]) {
				return t.zero_value_expr_for_type(typ)
			}
		}
	}
	if if_expr.else_expr is ast.IfExpr {
		else_if := if_expr.else_expr as ast.IfExpr
		if values := t.extract_branch_tuple_values(else_if.stmts, n) {
			if idx < values.len {
				if expr_is_string_literal(values[idx]) {
					return t.zero_value_expr_for_type(types.string_)
				}
				if typ := t.get_expr_type(values[idx]) {
					return t.zero_value_expr_for_type(typ)
				}
			}
		}
	}
	return ast.Expr(ast.BasicLiteral{
		kind:  .number
		value: '0'
	})
}

fn expr_is_string_literal(expr ast.Expr) bool {
	return expr is ast.StringLiteral || (expr is ast.BasicLiteral && expr.kind == .string)
}

// extract_branch_tuple_values extracts N values from a branch's last statement.
// The last statement should be an ExprStmt containing either a Tuple or a single expression.
fn (t &Transformer) extract_branch_tuple_values(stmts []ast.Stmt, n int) ?[]ast.Expr {
	if stmts.len == 0 {
		return none
	}
	// Find last non-empty statement (skip trailing EmptyStmt)
	mut last_idx := stmts.len - 1
	for last_idx >= 0 && stmts[last_idx] is ast.EmptyStmt {
		last_idx--
	}
	if last_idx < 0 {
		return none
	}
	last := stmts[last_idx]
	if last !is ast.ExprStmt {
		return none
	}
	last_expr := (last as ast.ExprStmt).expr
	if last_expr is ast.Tuple {
		if last_expr.exprs.len == n {
			return last_expr.exprs
		}
		return none
	}
	// Single expression, only valid for n == 1
	if n == 1 {
		return [last_expr]
	}
	return none
}

// build_tuple_branch_assigns builds assignment statements for a tuple if-expression branch.
// If the branch ends with a Tuple literal (a, b, c), it assigns each element directly.
// If the branch ends with a single call expression returning a tuple, it assigns the call
// to a temp variable and extracts .arg0, .arg1, etc.
fn (mut t Transformer) build_tuple_branch_assigns(stmts []ast.Stmt, tuple_lhs []ast.Expr, n int, pos token.Pos) ?[]ast.Stmt {
	if tuple_values := t.extract_branch_tuple_values(stmts, n) {
		// Branch has explicit tuple values — assign each one directly
		mut result := []ast.Stmt{cap: n}
		for i in 0 .. n {
			result << ast.Stmt(ast.AssignStmt{
				op:  .assign
				lhs: [tuple_lhs[i]]
				rhs: [tuple_values[i]]
				pos: pos
			})
		}
		return result
	}
	// Branch has a single non-tuple expression (e.g. a function call returning a tuple).
	// Extract it and assign via temp: _tuple_tN = call(); x = _tuple_tN.arg0; y = _tuple_tN.arg1
	if stmts.len == 0 {
		return none
	}
	mut last_idx := stmts.len - 1
	for last_idx >= 0 && stmts[last_idx] is ast.EmptyStmt {
		last_idx--
	}
	if last_idx < 0 {
		return none
	}
	last := stmts[last_idx]
	if last !is ast.ExprStmt {
		return none
	}
	last_expr := (last as ast.ExprStmt).expr
	if last_expr is ast.Tuple {
		return none // Tuple case was already handled above
	}
	// Single expression returning a tuple (e.g. function call)
	t.temp_counter++
	tmp_name := '_tuple_t${t.temp_counter}'
	tmp_ident := ast.Ident{
		name: tmp_name
	}
	mut result := []ast.Stmt{cap: n + 1}
	// Include any preceding statements from the branch
	for i in 0 .. last_idx {
		if stmts[i] !is ast.EmptyStmt {
			result << stmts[i]
		}
	}
	// Assign call result to temp
	result << ast.Stmt(ast.AssignStmt{
		op:  .decl_assign
		lhs: [ast.Expr(tmp_ident)]
		rhs: [last_expr]
		pos: pos
	})
	// Extract .arg0, .arg1, etc.
	for i in 0 .. n {
		result << ast.Stmt(ast.AssignStmt{
			op:  .assign
			lhs: [tuple_lhs[i]]
			rhs: [
				ast.Expr(ast.SelectorExpr{
					lhs: tmp_ident
					rhs: ast.Ident{
						name: 'arg${i}'
					}
				}),
			]
			pos: pos
		})
	}
	return result
}

// lower_if_expr_value lowers a value-position IfExpr into a temp variable + statement-form if.
// Generates: _if_t<N> := if cond { a } else { b }
// Hoists the decl_assign via pending_stmts and returns the temp ident as replacement.
fn (mut t Transformer) lower_if_expr_value(ie ast.IfExpr) ast.Expr {
	t.temp_counter++
	tmp_name := '_if_t${t.temp_counter}'
	tmp_ident := ast.Ident{
		name: tmp_name
	}
	// Register temp variable type so cleanc can resolve it from scope
	if typ := t.get_expr_type(ast.Expr(ie)) {
		t.register_temp_var(tmp_name, typ)
	}
	// Hoist the decl_assign with the IfExpr as RHS.
	// cleanc's gen_assign_stmt already handles decl_assign + IfExpr RHS
	// by emitting: Type tmp; if (cond) { tmp = a; } else { tmp = b; }
	t.pending_stmts << ast.Stmt(ast.AssignStmt{
		op:  .decl_assign
		lhs: [ast.Expr(tmp_ident)]
		rhs: [ast.Expr(ie)]
		pos: ie.pos
	})
	return ast.Expr(tmp_ident)
}

fn (mut t Transformer) collect_defers_in_if(node ast.IfExpr, mut defer_bodies [][]ast.Stmt) ast.Expr {
	new_else := t.collect_defers_in_else(node.else_expr, mut defer_bodies)
	return ast.IfExpr{
		cond:      node.cond
		stmts:     t.collect_and_remove_defers(node.stmts, mut defer_bodies)
		else_expr: new_else
	}
}

fn (mut t Transformer) collect_defers_in_else(else_expr ast.Expr, mut defer_bodies [][]ast.Stmt) ast.Expr {
	if else_expr is ast.IfExpr {
		return t.collect_defers_in_if(else_expr, mut defer_bodies)
	}
	return else_expr
}

// inject_defer_before_returns walks the statement list and replaces return statements
// with: { defer_body; return expr; } — saving the return value in a temp var if needed.
fn (mut t Transformer) inject_defer_before_returns(stmts []ast.Stmt, defer_stmts []ast.Stmt, has_return_type bool) []ast.Stmt {
	mut result := []ast.Stmt{cap: stmts.len}
	for stmt in stmts {
		match stmt {
			ast.ReturnStmt {
				if has_return_type && stmt.exprs.len > 0 {
					// Save return value to temp, run defers, return temp
					t.temp_counter++
					temp_name := '_defer_t${t.temp_counter}'
					if expr_type := t.get_expr_type(stmt.exprs[0]) {
						t.register_temp_var(temp_name, expr_type)
					}
					ret_expr := ast.Expr(stmt.exprs[0])
					result << ast.Stmt(ast.AssignStmt{
						op:  .decl_assign
						lhs: [ast.Expr(ast.Ident{
							name: temp_name
						})]
						rhs: [ret_expr]
					})
					result << defer_stmts
					result << ast.Stmt(ast.ReturnStmt{
						exprs: [ast.Expr(ast.Ident{
							name: temp_name
						})]
					})
				} else {
					result << defer_stmts
					result << ast.Stmt(stmt)
				}
			}
			ast.ExprStmt {
				expr := stmt.expr
				if expr is ast.IfExpr {
					result << ast.Stmt(ast.ExprStmt{
						expr: t.inject_defer_in_if_expr(expr, defer_stmts, has_return_type)
					})
				} else {
					result << ast.Stmt(stmt)
				}
			}
			ast.ForStmt {
				result << ast.Stmt(ast.ForStmt{
					init:  stmt.init
					cond:  stmt.cond
					post:  stmt.post
					stmts: t.inject_defer_before_returns(stmt.stmts, defer_stmts, has_return_type)
				})
			}
			ast.BlockStmt {
				result << ast.Stmt(ast.BlockStmt{
					stmts: t.inject_defer_before_returns(stmt.stmts, defer_stmts, has_return_type)
				})
			}
			else {
				result << stmt
			}
		}
	}
	return result
}

fn (mut t Transformer) inject_defer_in_if_expr(node ast.IfExpr, defer_stmts []ast.Stmt, has_return_type bool) ast.Expr {
	new_else := t.inject_defer_in_else(node.else_expr, defer_stmts, has_return_type)
	return ast.IfExpr{
		cond:      node.cond
		stmts:     t.inject_defer_before_returns(node.stmts, defer_stmts, has_return_type)
		else_expr: new_else
	}
}

fn (mut t Transformer) inject_defer_in_else(else_expr ast.Expr, defer_stmts []ast.Stmt, has_return_type bool) ast.Expr {
	match else_expr {
		ast.IfExpr {
			// else if: recurse
			return t.inject_defer_in_if_expr(else_expr, defer_stmts, has_return_type)
		}
		ast.EmptyExpr {
			return else_expr
		}
		else {
			return else_expr
		}
	}
}

// transform_comptime_expr evaluates compile-time conditionals and returns the selected branch
fn (mut t Transformer) eval_comptime_if(node ast.IfExpr) ast.Expr {
	cond_result := t.eval_comptime_cond(node.cond)

	if cond_result {
		// Condition is true - return the then branch with transformed statements
		if node.stmts.len == 1 {
			stmt := node.stmts[0]
			if stmt is ast.ExprStmt {
				return t.transform_expr(stmt.expr)
			}
		}
		// Multi-statement branch at expression level can't be represented;
		// statement-level expansion handles these properly
		return ast.empty_expr
	} else {
		// Condition is false - evaluate else branch
		else_e := node.else_expr
		if else_e !is ast.EmptyExpr {
			if else_e is ast.IfExpr {
				if else_e.cond is ast.EmptyExpr {
					// Plain $else block
					if else_e.stmts.len == 1 {
						stmt := else_e.stmts[0]
						if stmt is ast.ExprStmt {
							return t.transform_expr(stmt.expr)
						}
					}
					// Multi-statement $else at expression level
					return ast.empty_expr
				} else {
					// $else $if - recursive evaluation
					return t.eval_comptime_if(else_e)
				}
			}
		}
	}
	// Condition is false and no else branch - return empty (comptime block is skipped)
	return ast.empty_expr
}

// resolve_comptime_if_stmts evaluates a compile-time $if condition and returns
// the selected branch's statements, fully resolving the comptime at statement level.
fn (mut t Transformer) resolve_comptime_if_stmts(node ast.IfExpr) []ast.Stmt {
	cond_result := t.eval_comptime_cond(node.cond)
	if cond_result {
		return node.stmts
	}
	// Condition is false - evaluate else branch
	else_e := node.else_expr
	if else_e is ast.IfExpr {
		if else_e.cond is ast.EmptyExpr {
			// Plain $else block
			return else_e.stmts
		}
		// $else $if - recursive evaluation
		return t.resolve_comptime_if_stmts(else_e)
	}
	return []
}

// can_eval_comptime_cond returns true if the transformer can evaluate this
// comptime condition. Returns false for conditions involving generic type
// checks (key_is/not_is) that need to be resolved by the backend.
fn (t &Transformer) can_eval_comptime_cond(cond ast.Expr) bool {
	match cond {
		ast.Ident {
			return true
		}
		ast.PrefixExpr {
			return t.can_eval_comptime_cond(cond.expr)
		}
		ast.InfixExpr {
			if cond.op in [.key_is, .key_isreftype, .not_is] {
				return false
			}
			return t.can_eval_comptime_cond(cond.lhs) && t.can_eval_comptime_cond(cond.rhs)
		}
		ast.PostfixExpr {
			return true
		}
		ast.ParenExpr {
			return t.can_eval_comptime_cond(cond.expr)
		}
		else {
			return false
		}
	}
}

// transform_comptime_if_bodies recursively transforms the body stmts of each
// branch in a comptime $if, without evaluating the condition. This is used when
// the condition can't be evaluated at transform time (e.g., generic type checks).
fn (mut t Transformer) transform_comptime_if_bodies(node ast.IfExpr) ast.IfExpr {
	transformed_stmts := t.transform_stmts(node.stmts)
	mut transformed_else := node.else_expr
	if node.else_expr is ast.IfExpr {
		else_if := node.else_expr as ast.IfExpr
		transformed_else_if := t.transform_comptime_if_bodies(else_if)
		transformed_else = ast.Expr(transformed_else_if)
	}
	return ast.IfExpr{
		cond:      node.cond
		stmts:     transformed_stmts
		else_expr: transformed_else
	}
}

// eval_comptime_cond evaluates a compile-time condition expression
fn (t &Transformer) eval_comptime_cond(cond ast.Expr) bool {
	match cond {
		ast.Ident {
			return t.eval_comptime_flag(cond.name)
		}
		ast.PrefixExpr {
			if cond.op == .not {
				return !t.eval_comptime_cond(cond.expr)
			}
		}
		ast.InfixExpr {
			if cond.op == .and {
				return t.eval_comptime_cond(cond.lhs) && t.eval_comptime_cond(cond.rhs)
			}
			if cond.op == .logical_or {
				return t.eval_comptime_cond(cond.lhs) || t.eval_comptime_cond(cond.rhs)
			}
		}
		ast.PostfixExpr {
			// Handle optional feature check: feature?
			if cond.op == .question {
				inner := cond.expr
				if inner is ast.Ident {
					return t.eval_comptime_flag(inner.name)
				}
			}
		}
		ast.ParenExpr {
			return t.eval_comptime_cond(cond.expr)
		}
		else {}
	}

	return false
}

// eval_comptime_flag evaluates a single comptime flag/identifier
fn (t &Transformer) eval_comptime_flag(name string) bool {
	match name {
		'macos', 'darwin' {
			$if macos {
				return true
			}
			return false
		}
		'linux' {
			$if linux {
				return true
			}
			return false
		}
		'windows' {
			$if windows {
				return true
			}
			return false
		}
		'freebsd' {
			$if freebsd {
				return true
			}
			return false
		}
		'x64', 'amd64' {
			$if amd64 {
				return true
			}
			return false
		}
		'arm64', 'aarch64' {
			$if arm64 {
				return true
			}
			return false
		}
		'little_endian' {
			$if little_endian {
				return true
			}
			return false
		}
		'big_endian' {
			$if big_endian {
				return true
			}
			return false
		}
		'debug' {
			$if debug {
				return true
			}
			return false
		}
		'native' {
			return t.pref != unsafe { nil } && (t.pref.backend == .arm64 || t.pref.backend == .x64)
		}
		// Native backend cannot resolve C.stdout/C.stderr data symbols through GOT,
		// so use C.write() instead of fwrite() for I/O operations.
		'builtin_write_buf_to_fd_should_use_c_write' {
			return t.pref != unsafe { nil } && (t.pref.backend == .arm64 || t.pref.backend == .x64)
		}
		'tinyc' {
			// For native backends, inline assembly from V source is not supported
			// by the SSA builder. Pretend we're TinyCC so that $if arm64 && !tinyc
			// guards select the software fallback path instead of inline asm.
			return t.pref != unsafe { nil } && (t.pref.backend == .arm64 || t.pref.backend == .x64)
		}
		'prealloc' {
			return t.pref != unsafe { nil } && t.pref.prealloc
		}
		// Feature flags that are typically false
		'new_int', 'gcboehm', 'autofree', 'ppc64' {
			return false
		}
		else {
			// Check user-defined comptime flags from -d <name>
			if t.pref != unsafe { nil } && name in t.pref.user_defines {
				return true
			}
			return false
		}
	}
}
