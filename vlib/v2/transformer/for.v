// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module transformer

import v2.ast
import v2.token
import v2.types

// gen_map_iter_temp_name generates unique temporary variable names for map iteration
fn (mut t Transformer) gen_map_iter_temp_name(suffix string) string {
	t.temp_counter++
	return '_map_${suffix}_${t.temp_counter}'
}

fn (t &Transformer) iter_expr_needs_deref(expr ast.Expr) bool {
	if expr is ast.Ident {
		if obj := t.scope.lookup_parent(expr.name, 0) {
			mut base := obj.typ()
			for {
				if base is types.Pointer {
					return true
				}
				if base is types.Alias {
					alias_t := base as types.Alias
					base = alias_t.base_type
					continue
				}
				break
			}
		}
	}
	if iter_type := t.get_expr_type(expr) {
		mut base := iter_type
		for {
			if base is types.Pointer {
				return true
			}
			if base is types.Alias {
				alias_t := base as types.Alias
				base = alias_t.base_type
				continue
			}
			break
		}
	}
	return false
}

fn (mut t Transformer) iter_value_expr(orig ast.Expr, transformed ast.Expr, pos token.Pos, value_type types.Type) ast.Expr {
	t.register_synth_type(pos, value_type)
	base_expr := ast.Expr(ast.ParenExpr{
		expr: transformed
		pos:  pos
	})
	if t.iter_expr_needs_deref(orig) {
		deref_expr := ast.Expr(ast.PrefixExpr{
			op:   .mul
			expr: base_expr
			pos:  pos
		})
		return ast.Expr(ast.ParenExpr{
			expr: deref_expr
			pos:  pos
		})
	}
	return base_expr
}

// try_expand_for_in_map expands map iteration to lower-level constructs.
// Transforms: for k, v in map_expr { body }
// Into:
//   {
//       mut _map_len := map_expr.key_values.len
//       for _map_idx := 0; _map_idx < _map_len; _map_idx++ {
//           _map_delta := map_expr.key_values.len - _map_len
//           _map_len = map_expr.key_values.len
//           if _map_delta < 0 { _map_idx = -1; continue }
//           if !DenseArray__has_index(&map_expr.key_values, _map_idx) { continue }
//           k := *(KeyType*)DenseArray__key(&map_expr.key_values, _map_idx)
//           v := *(ValueType*)DenseArray__value(&map_expr.key_values, _map_idx)
//           body
//       }
//   }
fn (mut t Transformer) try_expand_for_in_map(stmt ast.ForStmt) ?[]ast.Stmt {
	if t.is_eval_backend() {
		return none
	}
	// Check if this is a for-in statement
	if stmt.init !is ast.ForInStmt {
		return none
	}
	for_in := stmt.init as ast.ForInStmt

	// Get the type of the iterable expression
	iter_type := t.get_expr_type(for_in.expr) or { return none }

	// Check if it's a map type (allow alias/pointer wrappers).
	map_type := t.unwrap_map_type(iter_type) or { return none }

	// Get key variable name
	mut key_name := ''
	mut key_is_blank := false
	if for_in.key !is ast.EmptyExpr {
		if for_in.key is ast.Ident {
			key_name = for_in.key.name
			key_is_blank = key_name == '_'
		} else if for_in.key is ast.ModifierExpr {
			if for_in.key.expr is ast.Ident {
				key_name = for_in.key.expr.name
				key_is_blank = key_name == '_'
			}
		}
	}

	// Get value variable name
	mut value_name := ''
	if for_in.value is ast.Ident {
		value_name = for_in.value.name
	} else if for_in.value is ast.ModifierExpr {
		if for_in.value.expr is ast.Ident {
			value_name = for_in.value.expr.name
		}
	}

	// Get C-compatible type names for key and value (using C declaration syntax with *)
	key_type_name := t.type_to_c_decl_name(map_type.key_type)
	value_type_name := t.type_to_c_decl_name(map_type.value_type)

	// Generate unique temp variable names
	idx_name := t.gen_map_iter_temp_name('idx')
	len_name := t.gen_map_iter_temp_name('len')
	delta_name := t.gen_map_iter_temp_name('delta')

	idx_ident := ast.Ident{
		name: idx_name
	}
	len_ident := ast.Ident{
		name: len_name
	}
	delta_ident := ast.Ident{
		name: delta_name
	}

	// For lvalue expressions (simple Ident or SelectorExpr), transform and use directly
	// so mutations during iteration (delete/set) are visible.
	// For rvalue expressions (function calls, map literals), store in a temp variable.
	// NOTE: rvalue expressions must NOT be pre-transformed here, because the expansion
	// result goes through transform_stmt again. Pre-transforming would cause double
	// transformation (e.g., ArrayInitExpr args in new_map_init become full array
	// construction calls instead of raw data arrays).
	is_lvalue := for_in.expr is ast.Ident || for_in.expr is ast.SelectorExpr
	mut map_ref := ast.Expr(ast.Ident{})
	mut stmts := []ast.Stmt{}
	if is_lvalue {
		map_ref = for_in.expr
	} else {
		map_tmp_name := t.gen_map_iter_temp_name('map')
		map_tmp_ident := ast.Ident{
			name: map_tmp_name
		}
		stmts << ast.AssignStmt{
			op:  .decl_assign
			lhs: [ast.Expr(map_tmp_ident)]
			rhs: [ast.Expr(for_in.expr)]
		}
		map_ref = ast.Expr(map_tmp_ident)
	}

	// key_values selector: map_ref.key_values
	key_values_expr := t.synth_selector(map_ref, 'key_values', types.Type(types.Struct{
		name: 'DenseArray'
	}))

	// key_values.len selector: map_ref.key_values.len
	key_values_len_expr := t.synth_selector(ast.Expr(key_values_expr), 'len', types.Type(types.int_))

	// 1. mut _map_len := map_ref.key_values.len
	stmts << ast.AssignStmt{
		op:  .decl_assign
		lhs: [ast.Expr(ast.ModifierExpr{
			kind: .key_mut
			expr: len_ident
		})]
		rhs: [ast.Expr(key_values_len_expr)]
	}

	// Build the inner loop body
	mut loop_body := []ast.Stmt{}

	// _map_delta := map_expr.key_values.len - _map_len
	loop_body << ast.AssignStmt{
		op:  .decl_assign
		lhs: [ast.Expr(delta_ident)]
		rhs: [t.make_infix_expr(.minus, key_values_len_expr, ast.Expr(len_ident))]
	}

	// _map_len = map_expr.key_values.len
	loop_body << ast.AssignStmt{
		op:  .assign
		lhs: [ast.Expr(len_ident)]
		rhs: [key_values_len_expr]
	}

	// if _map_delta < 0 { _map_idx = -1; continue }
	delta_lt_zero := t.make_infix_expr(.lt, ast.Expr(delta_ident), t.make_number_expr('0'))
	loop_body << ast.ExprStmt{
		expr: ast.IfExpr{
			cond:  delta_lt_zero
			stmts: [
				ast.Stmt(ast.AssignStmt{
					op:  .assign
					lhs: [ast.Expr(idx_ident)]
					rhs: [
						ast.Expr(ast.PrefixExpr{
							op:   .minus
							expr: ast.BasicLiteral{
								kind:  .number
								value: '1'
							}
						}),
					]
				}),
				ast.Stmt(ast.FlowControlStmt{
					op: .key_continue
				}),
			]
		}
	}

	// if !DenseArray__has_index(&map_expr.key_values, _map_idx) { continue }
	has_index_call := ast.CallExpr{
		lhs:  ast.Ident{
			name: 'DenseArray__has_index'
		}
		args: [
			ast.Expr(ast.PrefixExpr{
				op:   .amp
				expr: key_values_expr
			}),
			ast.Expr(idx_ident),
		]
	}
	loop_body << ast.ExprStmt{
		expr: ast.IfExpr{
			cond:  ast.PrefixExpr{
				op:   .not
				expr: has_index_call
			}
			stmts: [ast.Stmt(ast.FlowControlStmt{
				op: .key_continue
			})]
		}
	}

	// k := *(KeyType*)DenseArray__key(&map_expr.key_values, _map_idx)
	// This is represented as a cast expression wrapping the call
	if !key_is_blank && key_name != '' {
		key_call := ast.CallExpr{
			lhs:  ast.Ident{
				name: 'DenseArray__key'
			}
			args: [
				ast.Expr(ast.PrefixExpr{
					op:   .amp
					expr: key_values_expr
				}),
				ast.Expr(idx_ident),
			]
		}
		// Cast to KeyType* then dereference: *(KeyType*)call
		key_cast := ast.CastExpr{
			typ:  ast.Ident{
				name: '${key_type_name}*'
			}
			expr: key_call
		}
		key_deref := ast.PrefixExpr{
			op:   .mul
			expr: key_cast
		}
		loop_body << ast.AssignStmt{
			op:  .decl_assign
			lhs: [ast.Expr(ast.Ident{
				name: key_name
			})]
			rhs: [ast.Expr(key_deref)]
		}
		// Clone string keys to avoid use-after-free when map mutations
		// (delete/set) free the underlying string data during iteration.
		if map_type.key_type is types.String {
			loop_body << ast.AssignStmt{
				op:  .assign
				lhs: [ast.Expr(ast.Ident{
					name: key_name
				})]
				rhs: [
					ast.Expr(ast.CallExpr{
						lhs:  ast.Ident{
							name: 'string__clone'
						}
						args: [ast.Expr(ast.Ident{
							name: key_name
						})]
					}),
				]
			}
		}
		// Register key variable type in scope for later string detection
		t.scope.insert(key_name, map_type.key_type)
	}

	// v := *(ValueType*)DenseArray__value(&map_expr.key_values, _map_idx)
	if value_name != '' && value_name != '_' {
		value_call := ast.CallExpr{
			lhs:  ast.Ident{
				name: 'DenseArray__value'
			}
			args: [
				ast.Expr(ast.PrefixExpr{
					op:   .amp
					expr: key_values_expr
				}),
				ast.Expr(idx_ident),
			]
		}
		// Cast to ValueType* then dereference: *(ValueType*)call
		value_cast := ast.CastExpr{
			typ:  ast.Ident{
				name: '${value_type_name}*'
			}
			expr: value_call
		}
		value_deref := ast.PrefixExpr{
			op:   .mul
			expr: value_cast
		}
		loop_body << ast.AssignStmt{
			op:  .decl_assign
			lhs: [ast.Expr(ast.Ident{
				name: value_name
			})]
			rhs: [ast.Expr(value_deref)]
		}
		// Register value variable type in scope for later type detection
		t.scope.insert(value_name, map_type.value_type)
	}

	// Add the original body statements (NOT transformed here - transform_stmts will do it)
	for body_stmt in stmt.stmts {
		loop_body << body_stmt
	}

	// 2. Build the for loop:
	// for _map_idx := 0; _map_idx < _map_len; _map_idx++ { ... }
	loop_cond := t.make_infix_expr(.lt, ast.Expr(idx_ident), ast.Expr(len_ident))
	next_idx := t.make_infix_expr(.plus, ast.Expr(idx_ident), t.make_number_expr('1'))
	for_stmt := ast.ForStmt{
		init:  ast.AssignStmt{
			op:  .decl_assign
			lhs: [ast.Expr(idx_ident)]
			rhs: [ast.Expr(ast.BasicLiteral{
				kind:  .number
				value: '0'
			})]
		}
		cond:  loop_cond
		post:  ast.AssignStmt{
			op:  .assign
			lhs: [ast.Expr(idx_ident)]
			rhs: [next_idx]
		}
		stmts: loop_body
	}
	stmts << for_stmt

	return stmts
}

fn (mut t Transformer) transform_for_stmt(stmt ast.ForStmt) ast.ForStmt {
	// Open a child scope for loop variables
	t.open_scope()

	// Check if this is a for-in loop (init is ForInStmt)
	if stmt.init is ast.ForInStmt {
		for_in := stmt.init as ast.ForInStmt
		// Check for range expression: for i in 0..n
		if for_in.expr is ast.RangeExpr {
			result := t.transform_range_for_in(stmt, for_in, for_in.expr)
			t.close_scope()
			return result
		}
		// `for r in s.runes_iterator()` - lower as indexed string iteration.
		if iter_base := t.runes_iterator_base_expr(for_in.expr) {
			if base_type := t.get_expr_type(iter_base) {
				result := t.transform_array_for_in(stmt, ast.ForInStmt{
					key:   for_in.key
					value: for_in.value
					expr:  iter_base
				}, base_type)
				t.close_scope()
				return result
			}
		}
		if iter_type := t.get_expr_type(for_in.expr) {
			// Normalize pointer/alias wrappers so for-in lowering works for
			// method receivers like `mut a []T` and aliased array types.
			mut iter_base_type := iter_type
			for {
				if iter_base_type is types.Pointer {
					ptr := iter_base_type as types.Pointer
					iter_base_type = ptr.base_type
					continue
				}
				if iter_base_type is types.Alias {
					alias_t := iter_base_type as types.Alias
					iter_base_type = alias_t.base_type
					continue
				}
				break
			}
			// Fixed array - transform to indexed for loop with literal size
			if iter_base_type is types.ArrayFixed {
				arr_fixed := iter_base_type as types.ArrayFixed
				result := t.transform_fixed_array_for_in(stmt, for_in, arr_fixed)
				t.close_scope()
				return result
			}
			// Dynamic array or string - transform to indexed for loop with .len.
			// Keep these as separate type checks because `is A || is B` currently
			// lowers incorrectly in cleanc self-host output.
			if iter_base_type is types.Array {
				result := t.transform_array_for_in(stmt, for_in, iter_base_type)
				t.close_scope()
				return result
			}
			if iter_base_type is types.String {
				result := t.transform_array_for_in(stmt, for_in, iter_base_type)
				t.close_scope()
				return result
			}
			// Other iterable types (maps, channels, etc): keep the ForInStmt form.
			// The untyped indexed lowering below is only valid for array-like iterables.
			value_type := iter_type.value_type()
			if for_in.value is ast.Ident {
				value_name := (for_in.value as ast.Ident).name
				if value_name != '' && value_name != '_' {
					t.scope.insert(value_name, value_type)
				}
			}
			key_type := iter_type.key_type()
			if for_in.key is ast.Ident {
				key_name := (for_in.key as ast.Ident).name
				if key_name != '' && key_name != '_' {
					t.scope.insert(key_name, key_type)
				}
			}
			transformed_stmts := t.transform_stmts(stmt.stmts)
			result := ast.ForStmt{
				init:  ast.Stmt(ast.ForInStmt{
					key:   for_in.key
					value: for_in.value
					expr:  t.transform_expr(for_in.expr)
				})
				cond:  t.transform_expr(stmt.cond)
				post:  t.transform_stmt(stmt.post)
				stmts: transformed_stmts
			}
			t.close_scope()
			return result
		}
		// Keep lowering deterministic even when type info lookup fails for the
		// iterable expression. This avoids leaking raw ForInStmt nodes to cleanc.
		result := t.transform_untyped_for_in(stmt, for_in)
		t.close_scope()
		return result
	}

	// Check if the for-loop condition is an `is` check (e.g., `for x is Type { ... }`)
	// and push smartcast for the loop body
	mut loop_smartcasts := []SmartcastContext{}
	for term in t.flatten_and_terms(stmt.cond) {
		if term is ast.InfixExpr {
			if ctx := t.smartcast_context_from_is_check(term) {
				loop_smartcasts << ctx
			}
		}
	}
	for ctx in loop_smartcasts {
		t.push_smartcast_full(ctx.expr, ctx.variant, ctx.variant_full, ctx.sumtype)
	}
	transformed_stmts := t.transform_stmts(stmt.stmts)
	for _ in loop_smartcasts {
		t.pop_smartcast()
	}

	result := ast.ForStmt{
		init:  t.transform_stmt(stmt.init)
		cond:  t.transform_expr(stmt.cond)
		post:  t.transform_stmt(stmt.post)
		stmts: transformed_stmts
	}
	t.close_scope()
	return result
}

// transform_untyped_for_in lowers for-in loops when iterable type lookup fails.
// It generates an indexed loop and leaves element type inference to later stages.
fn (mut t Transformer) transform_untyped_for_in(stmt ast.ForStmt, for_in ast.ForInStmt) ast.ForStmt {
	mut value_name := '_elem'
	mut value_lhs := ast.Expr(ast.Ident{
		name: value_name
	})
	mut is_mut_value := false
	if for_in.value is ast.Ident {
		value_name = for_in.value.name
		value_lhs = ast.Expr(for_in.value)
	} else if for_in.value is ast.ModifierExpr {
		if for_in.value.expr is ast.Ident {
			value_name = for_in.value.expr.name
			value_lhs = ast.Expr(for_in.value.expr)
			if for_in.value.kind == .key_mut {
				is_mut_value = true
			}
		}
	}

	mut key_name := '_idx'
	mut has_explicit_key := false
	if for_in.key is ast.Ident {
		if for_in.key.name != '_' {
			key_name = for_in.key.name
			has_explicit_key = true
		}
	} else if for_in.key is ast.ModifierExpr {
		if for_in.key.expr is ast.Ident {
			if for_in.key.expr.name != '_' {
				key_name = for_in.key.expr.name
				has_explicit_key = true
			}
		}
	}
	if !has_explicit_key {
		key_name = '_idx_${value_name}'
	}

	idx_pos := t.next_synth_pos()
	key_ident := ast.Ident{
		name: key_name
		pos:  idx_pos
	}
	if int_obj := t.scope.lookup_parent('int', 0) {
		t.scope.insert(key_name, int_obj)
		t.register_synth_type(idx_pos, int_obj.typ())
	}
	iter_typ := t.get_expr_type(for_in.expr)
	iter_pos := t.next_synth_pos()
	mut transformed_expr := ast.Expr(ast.ParenExpr{
		expr: t.transform_expr(for_in.expr)
		pos:  iter_pos
	})
	if typ := iter_typ {
		t.register_synth_type(iter_pos, typ)
	}

	index_pos := t.next_synth_pos()
	if typ := t.get_expr_type(for_in.value) {
		t.register_synth_type(index_pos, typ)
	}

	index_expr := ast.Expr(ast.IndexExpr{
		lhs:  transformed_expr
		expr: key_ident
		pos:  index_pos
	})
	value_rhs := if is_mut_value {
		ptr_pos := t.next_synth_pos()
		if typ := t.get_expr_type(for_in.value) {
			t.register_synth_type(ptr_pos, types.Type(types.Pointer{
				base_type: typ
			}))
		}
		ast.Expr(ast.PrefixExpr{
			op:   .amp
			expr: index_expr
			pos:  ptr_pos
		})
	} else {
		index_expr
	}
	value_assign := ast.AssignStmt{
		op:  .decl_assign
		lhs: [value_lhs]
		rhs: [value_rhs]
	}

	mut new_stmts := []ast.Stmt{cap: stmt.stmts.len + 1}
	new_stmts << value_assign
	transformed_body := t.transform_stmts(stmt.stmts)
	new_stmts << transformed_body

	loop_cond := t.make_infix_expr(.lt, ast.Expr(key_ident), t.synth_selector(transformed_expr,
		'len', types.Type(types.int_)))
	return ast.ForStmt{
		init:  ast.AssignStmt{
			op:  .decl_assign
			lhs: [ast.Expr(key_ident)]
			rhs: [ast.Expr(ast.BasicLiteral{
				value: '0'
				kind:  .number
			})]
		}
		cond:  loop_cond
		post:  ast.AssignStmt{
			op:  .plus_assign
			lhs: [ast.Expr(key_ident)]
			rhs: [ast.Expr(ast.BasicLiteral{
				value: '1'
				kind:  .number
			})]
		}
		stmts: new_stmts
	}
}

fn (t &Transformer) runes_iterator_base_expr(expr ast.Expr) ?ast.Expr {
	if expr is ast.CallExpr {
		if expr.args.len == 0 && expr.lhs is ast.SelectorExpr
			&& expr.lhs.rhs.name == 'runes_iterator' {
			return expr.lhs.lhs
		}
	}
	if expr is ast.CallOrCastExpr {
		if expr.lhs is ast.SelectorExpr && expr.lhs.rhs.name == 'runes_iterator'
			&& expr.expr is ast.EmptyExpr {
			return expr.lhs.lhs
		}
	}
	return none
}

// transform_array_for_in transforms `for x in arr` / `for i, x in arr` / `for c in str`
// into: for (int _idx = 0; _idx < arr.len; _idx++) { T x = arr[_idx]; ... }
fn (mut t Transformer) transform_array_for_in(stmt ast.ForStmt, for_in ast.ForInStmt, iter_type types.Type) ast.ForStmt {
	mut value_name := '_elem'
	mut value_lhs := ast.Expr(ast.Ident{
		name: value_name
	})
	mut is_mut_value := false
	if for_in.value is ast.Ident {
		value_name = for_in.value.name
		value_lhs = ast.Expr(for_in.value)
	} else if for_in.value is ast.ModifierExpr {
		if for_in.value.expr is ast.Ident {
			value_name = for_in.value.expr.name
			value_lhs = ast.Expr(for_in.value.expr)
			if for_in.value.kind == .key_mut {
				is_mut_value = true
			}
		}
	}

	mut key_name := '_idx'
	mut has_explicit_key := false
	if for_in.key is ast.Ident {
		if for_in.key.name != '_' {
			key_name = for_in.key.name
			has_explicit_key = true
		}
	} else if for_in.key is ast.ModifierExpr {
		if for_in.key.expr is ast.Ident {
			if for_in.key.expr.name != '_' {
				key_name = for_in.key.expr.name
				has_explicit_key = true
			}
		}
	}
	if !has_explicit_key {
		key_name = '_idx_${value_name}'
	}

	idx_pos := t.next_synth_pos()
	key_ident := ast.Ident{
		name: key_name
		pos:  idx_pos
	}

	// Register loop variables in scope
	key_type := iter_type.key_type()
	value_type := iter_type.value_type()
	t.scope.insert(key_name, key_type)
	t.scope.insert(value_name, value_type)
	t.register_synth_type(idx_pos, key_type)

	iter_pos := t.next_synth_pos()
	transformed_expr := t.iter_value_expr(for_in.expr, t.transform_expr(for_in.expr),
		iter_pos, iter_type)

	index_pos := t.next_synth_pos()
	t.register_synth_type(index_pos, value_type)

	// Build: elem := arr[_idx] (or elem := &arr[_idx] for mut)
	index_expr := ast.Expr(ast.IndexExpr{
		lhs:  transformed_expr
		expr: key_ident
		pos:  index_pos
	})
	value_rhs := if is_mut_value {
		ptr_pos := t.next_synth_pos()
		t.register_synth_type(ptr_pos, types.Type(types.Pointer{
			base_type: value_type
		}))
		// mut loop variable: take address for in-place mutation
		ast.Expr(ast.PrefixExpr{
			op:   .amp
			expr: index_expr
			pos:  ptr_pos
		})
	} else {
		index_expr
	}
	value_assign := ast.AssignStmt{
		op:  .decl_assign
		lhs: [value_lhs]
		rhs: [value_rhs]
	}

	mut new_stmts := []ast.Stmt{cap: stmt.stmts.len + 1}
	new_stmts << value_assign
	transformed_body := t.transform_stmts(stmt.stmts)
	new_stmts << transformed_body

	// Build: for (_idx := 0; _idx < arr.len; _idx++) { ... }
	loop_cond := t.make_infix_expr(.lt, ast.Expr(key_ident), t.synth_selector(transformed_expr,
		'len', types.Type(types.int_)))
	return ast.ForStmt{
		init:  ast.AssignStmt{
			op:  .decl_assign
			lhs: [ast.Expr(key_ident)]
			rhs: [ast.Expr(ast.BasicLiteral{
				value: '0'
				kind:  .number
			})]
		}
		cond:  loop_cond
		post:  ast.AssignStmt{
			op:  .plus_assign
			lhs: [ast.Expr(key_ident)]
			rhs: [ast.Expr(ast.BasicLiteral{
				value: '1'
				kind:  .number
			})]
		}
		stmts: new_stmts
	}
}

// transform_range_for_in transforms `for i in start..end` into
// for (int i = start; i < end; i++) { ... }
fn (mut t Transformer) transform_range_for_in(stmt ast.ForStmt, for_in ast.ForInStmt, range ast.RangeExpr) ast.ForStmt {
	mut value_name := '_i'
	if for_in.value is ast.Ident {
		value_name = for_in.value.name
	} else if for_in.value is ast.ModifierExpr {
		if for_in.value.expr is ast.Ident {
			value_name = for_in.value.expr.name
		}
	}

	if int_obj := t.scope.lookup_parent('int', 0) {
		t.scope.insert(value_name, int_obj)
	}

	cmp_op := if range.op == .ellipsis { token.Token.le } else { token.Token.lt } // `...` inclusive, `..` exclusive

	mut new_stmts := []ast.Stmt{cap: stmt.stmts.len}
	transformed_body := t.transform_stmts(stmt.stmts)
	new_stmts << transformed_body

	// Use the start/end expressions but strip original positions to avoid
	// env type misattribution (checker may register iterable type at start pos)
	start_expr := t.strip_pos(t.transform_expr(range.start))
	end_expr := t.strip_pos(t.transform_expr(range.end))

	range_cond := t.make_infix_expr(cmp_op, ast.Expr(ast.Ident{
		name: value_name
	}), end_expr)
	return ast.ForStmt{
		init:  ast.AssignStmt{
			op:  .decl_assign
			lhs: [ast.Expr(ast.Ident{
				name: value_name
			})]
			rhs: [start_expr]
		}
		cond:  range_cond
		post:  ast.AssignStmt{
			op:  .plus_assign
			lhs: [ast.Expr(ast.Ident{
				name: value_name
			})]
			rhs: [ast.Expr(ast.BasicLiteral{
				value: '1'
				kind:  .number
			})]
		}
		stmts: new_stmts
	}
}

// transform_fixed_array_for_in transforms `for elem in fixed_arr` to indexed for loop
// for i := 0; i < SIZE; i++ { elem := fixed_arr[i]; ... }
fn (mut t Transformer) transform_fixed_array_for_in(stmt ast.ForStmt, for_in ast.ForInStmt, arr_type types.ArrayFixed) ast.ForStmt {
	// Get value variable name
	mut value_name := '_elem'
	mut value_lhs := ast.Expr(ast.Ident{
		name: value_name
	})
	if for_in.value is ast.Ident {
		value_name = for_in.value.name
		value_lhs = ast.Expr(for_in.value)
	} else if for_in.value is ast.ModifierExpr {
		if for_in.value.expr is ast.Ident {
			value_name = for_in.value.expr.name
			value_lhs = ast.Expr(for_in.value.expr)
		}
	}

	// Get key variable name (index)
	mut key_name := '_idx'
	mut has_explicit_key := false
	if for_in.key is ast.Ident {
		if for_in.key.name != '_' {
			key_name = for_in.key.name
			has_explicit_key = true
		}
	} else if for_in.key is ast.ModifierExpr {
		if for_in.key.expr is ast.Ident {
			if for_in.key.expr.name != '_' {
				key_name = for_in.key.expr.name
				has_explicit_key = true
			}
		}
	}

	// Use unique hidden index if no key specified
	if !has_explicit_key {
		key_name = '_idx_${value_name}'
	}
	idx_pos := t.next_synth_pos()
	key_ident := ast.Ident{
		name: key_name
		pos:  idx_pos
	}

	// Register loop variables in scope
	key_type := types.Type(arr_type).key_type()
	value_type := types.Type(arr_type).value_type()
	t.scope.insert(key_name, key_type)
	t.scope.insert(value_name, value_type)
	t.register_synth_type(idx_pos, key_type)

	// Transform the iterable expression
	iter_pos := t.next_synth_pos()
	transformed_expr := t.iter_value_expr(for_in.expr, t.transform_expr(for_in.expr),
		iter_pos, types.Type(arr_type))

	index_pos := t.next_synth_pos()
	t.register_synth_type(index_pos, value_type)

	// Build: elem := fixed_arr[i]
	value_assign := ast.AssignStmt{
		op:  .decl_assign
		lhs: [value_lhs]
		rhs: [
			ast.Expr(ast.IndexExpr{
				lhs:  transformed_expr
				expr: key_ident
				pos:  index_pos
			}),
		]
	}

	// Prepend value assignment to loop body
	mut new_stmts := []ast.Stmt{cap: stmt.stmts.len + 1}
	new_stmts << value_assign
	transformed_body := t.transform_stmts(stmt.stmts)
	new_stmts << transformed_body

	// Build: for i := 0; i < SIZE; i++ { ... }
	fixed_cond := t.make_infix_expr(.lt, ast.Expr(key_ident), t.make_number_expr('${arr_type.len}'))
	return ast.ForStmt{
		init:  ast.AssignStmt{
			op:  .decl_assign
			lhs: [ast.Expr(key_ident)]
			rhs: [ast.Expr(ast.BasicLiteral{
				value: '0'
				kind:  .number
			})]
		}
		cond:  fixed_cond
		post:  ast.AssignStmt{
			op:  .plus_assign
			lhs: [ast.Expr(key_ident)]
			rhs: [ast.Expr(ast.BasicLiteral{
				value: '1'
				kind:  .number
			})]
		}
		stmts: new_stmts
	}
}

// strip_pos creates a copy of a simple expression with pos=0 so that
// the cleanc env type lookup won't misattribute the original checker type.
fn (t &Transformer) strip_pos(e ast.Expr) ast.Expr {
	match e {
		ast.BasicLiteral {
			return ast.BasicLiteral{
				value: e.value
				kind:  e.kind
			}
		}
		ast.Ident {
			return ast.Ident{
				name: e.name
			}
		}
		ast.PrefixExpr {
			return ast.PrefixExpr{
				op:   e.op
				expr: t.strip_pos(e.expr)
			}
		}
		ast.CastExpr {
			return ast.CastExpr{
				typ:  e.typ
				expr: t.strip_pos(e.expr)
			}
		}
		else {
			return e
		}
	}
}

fn (mut t Transformer) transform_for_in_stmt(stmt ast.ForInStmt) ast.ForStmt {
	// ForInStmt is only a ForStmt initializer in v2 AST; lower stray ForInStmt
	// nodes through the regular for-loop transformer path.
	return t.transform_for_stmt(ast.ForStmt{
		init: ast.Stmt(stmt)
	})
}
