// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module transformer

import v2.ast
import v2.token
import v2.types

// propagate_types walks the transformed AST and fills in missing expression types
// for expressions that the checker didn't visit (e.g., inside unselected $if branches
// that were later resolved by the transformer, or inside complex lowered constructs).
fn (mut t Transformer) propagate_types(files []ast.File) {
	for file in files {
		// Set module scope for Ident resolution
		if mod_scope := t.cached_scopes[file.mod] {
			t.scope = mod_scope
		}
		t.cur_module = file.mod
		for stmt in file.stmts {
			t.prop_stmt(stmt)
		}
	}
}

// propagate_types_from_flat is the FlatAst-input counterpart to
// `propagate_types`. It walks `flat.files` directly and decodes one top-level
// statement at a time, avoiding whole-program legacy []ast.File and per-file
// []ast.Stmt allocations in flat-output transformer paths.
//
// Lets `apply_post_pass_tail_from_flat` (s167) take `&FlatAst` instead of
// `[]ast.File`, which closes the last `[]ast.File` consumer in the
// post_pass tail. The flat path also fixes a latent staleness bug in the
// `_via_driver` wedges: legacy `post_pass(result)` mutates `result` BEFORE
// `propagate_types` runs, whereas s162/s163 wedges passed un-post_pass'd
// `result` to `apply_post_pass_tail` so `propagate_types` saw stale stmts.
// With this helper plus `apply_post_pass_tail_from_flat`, the wedge passes
// `&builder.flat` (already post_pass'd by `post_pass_to_flat`) so non-arm64
// propagation sees the same post_pass'd stmts as legacy.
fn (mut t Transformer) propagate_types_from_flat(flat &ast.FlatAst) {
	if flat.files.len == 0 {
		return
	}
	for i in 0 .. flat.files.len {
		fc := flat.file_cursor(i)
		mod_name := fc.mod()
		if mod_scope := t.cached_scopes[mod_name] {
			t.scope = mod_scope
		}
		t.cur_module = mod_name
		stmts := fc.stmts()
		for j in 0 .. stmts.len() {
			t.prop_stmt_cursor(stmts.at(j))
		}
	}
}

fn (mut t Transformer) prop_expr_cursor(c ast.Cursor) ?types.Type {
	if !c.is_valid() {
		return none
	}
	t.prop_expr_cursor_children(c)
	pos := c.pos()
	if pos.is_valid() && !t.has_prop_type(pos.id) {
		if typ := t.infer_prop_type_cursor(c) {
			t.env.set_expr_type(pos.id, typ)
		}
	}
	if typ := t.get_expr_type_cursor(c) {
		return typ
	}
	return none
}

fn (mut t Transformer) prop_stmt_list_cursor(list ast.CursorList) {
	for i in 0 .. list.len() {
		t.prop_stmt_cursor(list.at(i))
	}
}

fn (mut t Transformer) prop_stmt_edges_cursor(stmt ast.Cursor, start int) {
	for i in start .. stmt.edge_count() {
		t.prop_stmt_cursor(stmt.edge(i))
	}
}

fn (mut t Transformer) prop_expr_edges_cursor(stmt ast.Cursor, start int) {
	for i in start .. stmt.edge_count() {
		t.prop_expr_cursor(stmt.edge(i))
	}
}

fn (mut t Transformer) prop_field_value_list_cursor(fields ast.CursorList, value_edge int) {
	for i in 0 .. fields.len() {
		t.prop_expr_cursor(fields.at(i).edge(value_edge))
	}
}

fn (mut t Transformer) prop_expr_cursor_edges(c ast.Cursor, start int) {
	for i in start .. c.edge_count() {
		t.prop_expr_cursor(c.edge(i))
	}
}

fn (mut t Transformer) prop_expr_cursor_list(list ast.CursorList) {
	for i in 0 .. list.len() {
		t.prop_expr_cursor(list.at(i))
	}
}

fn (mut t Transformer) prop_field_init_values_cursor(c ast.Cursor, start int) {
	for i in start .. c.edge_count() {
		t.prop_expr_cursor(c.edge(i).edge(0))
	}
}

fn (mut t Transformer) prop_field_decl_values_cursor(list ast.CursorList) {
	for i in 0 .. list.len() {
		field := list.at(i)
		t.prop_expr_cursor(field.edge(0))
		t.prop_expr_cursor(field.edge(1))
	}
}

fn (mut t Transformer) prop_expr_cursor_children(c ast.Cursor) {
	match c.kind() {
		.expr_array_init, .expr_as_cast, .expr_call, .expr_call_or_cast, .expr_cast,
		.expr_comptime, .expr_generic_arg_or_index, .expr_generic_args, .expr_index, .expr_infix,
		.expr_keyword_operator, .expr_lambda, .expr_map_init, .expr_modifier, .expr_paren,
		.expr_postfix, .expr_prefix, .expr_range, .expr_selector, .expr_sql, .expr_tuple {
			t.prop_expr_cursor_edges(c, 0)
		}
		.expr_assoc {
			t.prop_expr_cursor(c.edge(0))
			t.prop_expr_cursor(c.edge(1))
			t.prop_field_init_values_cursor(c, 2)
		}
		.expr_fn_literal {
			t.prop_expr_cursor(c.edge(0))
			captured_len := c.extra_int()
			stmt_start := 1 + captured_len
			for i in 1 .. stmt_start {
				t.prop_expr_cursor(c.edge(i))
			}
			for i in stmt_start .. c.edge_count() {
				t.prop_stmt_cursor(c.edge(i))
			}
		}
		.expr_if {
			t.prop_expr_cursor(c.edge(0))
			t.prop_expr_cursor(c.edge(1))
			for i in 2 .. c.edge_count() {
				t.prop_stmt_cursor(c.edge(i))
			}
		}
		.expr_if_guard {
			t.prop_stmt_cursor(c.edge(0))
		}
		.expr_init {
			t.prop_expr_cursor(c.edge(0))
			t.prop_field_init_values_cursor(c, 1)
		}
		.expr_lock {
			packed := c.extra_int()
			lock_len := packed & 0xffff
			rlock_len := (packed >> 16) & 0xffff
			stmt_start := lock_len + rlock_len
			for i in 0 .. stmt_start {
				t.prop_expr_cursor(c.edge(i))
			}
			for i in stmt_start .. c.edge_count() {
				t.prop_stmt_cursor(c.edge(i))
			}
		}
		.expr_match {
			t.prop_expr_cursor(c.edge(0))
			for i in 1 .. c.edge_count() {
				branch := c.edge(i)
				t.prop_expr_cursor_list(branch.list_at(0))
				t.prop_stmt_list_cursor(branch.list_at(1))
			}
		}
		.expr_or {
			t.prop_expr_cursor(c.edge(0))
			for i in 1 .. c.edge_count() {
				t.prop_stmt_cursor(c.edge(i))
			}
		}
		.expr_select {
			t.prop_stmt_cursor(c.edge(0))
			t.prop_expr_cursor(c.edge(1))
			for i in 2 .. c.edge_count() {
				t.prop_stmt_cursor(c.edge(i))
			}
		}
		.expr_string_inter {
			inters := c.list_at(1)
			for i in 0 .. inters.len() {
				inter := inters.at(i)
				t.prop_expr_cursor(inter.edge(0))
				t.prop_expr_cursor(inter.edge(1))
			}
		}
		.expr_unsafe {
			for i in 0 .. c.edge_count() {
				t.prop_stmt_cursor(c.edge(i))
			}
		}
		.typ_anon_struct {
			t.prop_expr_cursor_list(c.list_at(0))
			t.prop_expr_cursor_list(c.list_at(1))
			t.prop_field_decl_values_cursor(c.list_at(2))
		}
		.typ_fn {
			t.prop_expr_cursor_list(c.list_at(0))
			params := c.list_at(1)
			for i in 0 .. params.len() {
				t.prop_expr_cursor(params.at(i).edge(0))
			}
			t.prop_expr_cursor(c.edge(2))
		}
		.typ_array_fixed, .typ_channel, .typ_generic, .typ_map, .typ_pointer, .typ_result,
		.typ_thread, .typ_tuple, .typ_array, .typ_option {
			t.prop_expr_cursor_edges(c, 0)
		}
		else {}
	}
}

fn (mut t Transformer) prop_stmt_cursor(stmt ast.Cursor) {
	if !stmt.is_valid() {
		return
	}
	match stmt.kind() {
		.stmt_assert {
			t.prop_expr_cursor(stmt.edge(0))
			t.prop_expr_cursor(stmt.edge(1))
		}
		.stmt_assign {
			lhs_len := stmt.extra_int()
			mut rhs_types := map[int]types.Type{}
			for i in lhs_len .. stmt.edge_count() {
				if rhs_type := t.prop_expr_cursor(stmt.edge(i)) {
					rhs_types[i - lhs_len] = rhs_type
				}
			}
			for i in 0 .. lhs_len {
				lhs := stmt.edge(i)
				t.prop_expr_cursor(lhs)
				lhs_pos := lhs.pos()
				if lhs_pos.is_valid() && !t.has_prop_type(lhs_pos.id) {
					if rhs_type := rhs_types[i] {
						t.env.set_expr_type(lhs_pos.id, rhs_type)
					}
				}
			}
		}
		.stmt_block {
			t.prop_stmt_edges_cursor(stmt, 0)
		}
		.stmt_comptime {
			t.prop_stmt_cursor(stmt.edge(0))
		}
		.stmt_const_decl {
			t.prop_field_value_list_cursor(stmt.list_at(0), 0)
		}
		.stmt_defer {
			t.prop_stmt_edges_cursor(stmt, 0)
		}
		.stmt_expr {
			t.prop_expr_cursor(stmt.edge(0))
		}
		.stmt_fn_decl {
			old_scope := t.scope
			scope_key := t.prop_fn_scope_key_cursor(stmt)
			if fn_scope := t.cached_fn_scopes[scope_key] {
				t.scope = fn_scope
			}
			t.prop_stmt_list_cursor(stmt.list_at(3))
			t.scope = old_scope
		}
		.stmt_for {
			t.prop_stmt_cursor(stmt.edge(0))
			t.prop_expr_cursor(stmt.edge(1))
			t.prop_stmt_cursor(stmt.edge(2))
			t.prop_stmt_edges_cursor(stmt, 3)
		}
		.stmt_for_in {
			t.prop_expr_cursor(stmt.edge(0))
			t.prop_expr_cursor(stmt.edge(1))
			t.prop_expr_cursor(stmt.edge(2))
		}
		.stmt_global_decl {
			t.prop_field_value_list_cursor(stmt.list_at(1), 1)
		}
		.stmt_label {
			t.prop_stmt_cursor(stmt.edge(0))
		}
		.stmt_return {
			t.prop_expr_edges_cursor(stmt, 0)
		}
		.stmt_enum_decl {
			t.prop_field_value_list_cursor(stmt.list_at(2), 1)
		}
		else {}
	}
}

fn (mut t Transformer) prop_exprs(exprs []ast.Expr) {
	if !expr_array_has_valid_data(exprs) {
		return
	}
	for expr in exprs {
		t.prop_expr(expr)
	}
}

fn (mut t Transformer) prop_stmt(stmt ast.Stmt) {
	if !stmt_has_valid_data(stmt) {
		return
	}
	match stmt {
		ast.AssertStmt {
			t.prop_expr(stmt.expr)
			t.prop_expr(stmt.extra)
		}
		ast.AssignStmt {
			// Process RHS first so types are available for LHS propagation
			t.prop_exprs(stmt.rhs)
			if expr_array_has_valid_data(stmt.lhs) {
				for i, e in stmt.lhs {
					t.prop_expr(e)
					// Propagate type from RHS to LHS if LHS is still untyped
					lhs_pos := e.pos()
					if lhs_pos.is_valid() && !t.has_prop_type(lhs_pos.id) && i < stmt.rhs.len {
						if rhs_type := t.get_expr_type(stmt.rhs[i]) {
							t.env.set_expr_type(lhs_pos.id, rhs_type)
						}
					}
				}
			}
		}
		ast.BlockStmt {
			for s in stmt.stmts {
				t.prop_stmt(s)
			}
		}
		ast.ComptimeStmt {
			t.prop_stmt(stmt.stmt)
		}
		ast.ConstDecl {
			for f in stmt.fields {
				t.prop_expr(f.value)
			}
		}
		ast.DeferStmt {
			for s in stmt.stmts {
				t.prop_stmt(s)
			}
		}
		ast.ExprStmt {
			t.prop_expr(stmt.expr)
		}
		ast.FnDecl {
			// Enter function scope for Ident resolution
			old_scope := t.scope
			scope_key := t.prop_fn_scope_key(stmt)
			if fn_scope := t.cached_fn_scopes[scope_key] {
				t.scope = fn_scope
			}
			for s in stmt.stmts {
				t.prop_stmt(s)
			}
			t.scope = old_scope
		}
		ast.ForStmt {
			t.prop_stmt(stmt.init)
			t.prop_expr(stmt.cond)
			t.prop_stmt(stmt.post)
			for s in stmt.stmts {
				t.prop_stmt(s)
			}
		}
		ast.ForInStmt {
			t.prop_expr(stmt.key)
			t.prop_expr(stmt.value)
			t.prop_expr(stmt.expr)
		}
		ast.GlobalDecl {
			for f in stmt.fields {
				t.prop_expr(f.value)
			}
		}
		ast.LabelStmt {
			t.prop_stmt(stmt.stmt)
		}
		ast.ReturnStmt {
			t.prop_exprs(stmt.exprs)
		}
		ast.EnumDecl {
			for f in stmt.fields {
				t.prop_expr(f.value)
			}
		}
		else {}
	}
}

fn (mut t Transformer) prop_expr(expr ast.Expr) {
	if !expr_has_valid_data(expr) {
		return
	}
	// Recurse into sub-expressions FIRST so children have types before parent inference
	match expr {
		ast.ArrayInitExpr {
			t.prop_expr(expr.typ)
			t.prop_exprs(expr.exprs)
			t.prop_expr(expr.init)
			t.prop_expr(expr.cap)
			t.prop_expr(expr.len)
		}
		ast.AsCastExpr {
			t.prop_expr(expr.expr)
			t.prop_expr(expr.typ)
		}
		ast.AssocExpr {
			t.prop_expr(expr.typ)
			t.prop_expr(expr.expr)
			for f in expr.fields {
				t.prop_expr(f.value)
			}
		}
		ast.CallExpr {
			t.prop_expr(expr.lhs)
			t.prop_exprs(expr.args)
		}
		ast.CallOrCastExpr {
			t.prop_expr(expr.lhs)
			t.prop_expr(expr.expr)
		}
		ast.CastExpr {
			t.prop_expr(expr.typ)
			t.prop_expr(expr.expr)
		}
		ast.ComptimeExpr {
			t.prop_expr(expr.expr)
		}
		ast.FnLiteral {
			for cv in expr.captured_vars {
				t.prop_expr(cv)
			}
			for s in expr.stmts {
				t.prop_stmt(s)
			}
		}
		ast.GenericArgs {
			t.prop_expr(expr.lhs)
			t.prop_exprs(expr.args)
		}
		ast.GenericArgOrIndexExpr {
			t.prop_expr(expr.lhs)
			t.prop_expr(expr.expr)
		}
		ast.IfExpr {
			t.prop_expr(expr.cond)
			for s in expr.stmts {
				t.prop_stmt(s)
			}
			t.prop_expr(expr.else_expr)
		}
		ast.IfGuardExpr {
			t.prop_stmt(ast.Stmt(expr.stmt))
		}
		ast.InfixExpr {
			t.prop_expr(expr.lhs)
			t.prop_expr(expr.rhs)
		}
		ast.IndexExpr {
			t.prop_expr(expr.lhs)
			t.prop_expr(expr.expr)
		}
		ast.InitExpr {
			t.prop_expr(expr.typ)
			for f in expr.fields {
				t.prop_expr(f.value)
			}
		}
		ast.KeywordOperator {
			t.prop_exprs(expr.exprs)
		}
		ast.LambdaExpr {
			t.prop_expr(expr.expr)
		}
		ast.LockExpr {
			t.prop_exprs(expr.lock_exprs)
			t.prop_exprs(expr.rlock_exprs)
			for s in expr.stmts {
				t.prop_stmt(s)
			}
		}
		ast.MapInitExpr {
			t.prop_expr(expr.typ)
			t.prop_exprs(expr.keys)
			t.prop_exprs(expr.vals)
		}
		ast.MatchExpr {
			t.prop_expr(expr.expr)
			for br in expr.branches {
				for cond in br.cond {
					t.prop_expr(cond)
				}
				for s in br.stmts {
					t.prop_stmt(s)
				}
			}
		}
		ast.ModifierExpr {
			t.prop_expr(expr.expr)
		}
		ast.OrExpr {
			t.prop_expr(expr.expr)
			for s in expr.stmts {
				t.prop_stmt(s)
			}
		}
		ast.ParenExpr {
			t.prop_expr(expr.expr)
		}
		ast.PostfixExpr {
			t.prop_expr(expr.expr)
		}
		ast.PrefixExpr {
			t.prop_expr(expr.expr)
		}
		ast.RangeExpr {
			t.prop_expr(expr.start)
			t.prop_expr(expr.end)
		}
		ast.SelectExpr {
			t.prop_stmt(expr.stmt)
			for s in expr.stmts {
				t.prop_stmt(s)
			}
			t.prop_expr(expr.next)
		}
		ast.SelectorExpr {
			t.prop_expr(expr.lhs)
		}
		ast.SqlExpr {
			t.prop_expr(expr.expr)
		}
		ast.StringInterLiteral {
			for inter in expr.inters {
				t.prop_expr(inter.expr)
				t.prop_expr(inter.format_expr)
			}
		}
		ast.Tuple {
			t.prop_exprs(expr.exprs)
		}
		ast.UnsafeExpr {
			for s in expr.stmts {
				t.prop_stmt(s)
			}
		}
		else {}
	}

	// Now try to infer and set the type for this expression
	if !expr_has_valid_data(expr) {
		return
	}
	pos := expr.pos()
	if pos.is_valid() && !t.has_prop_type(pos.id) {
		if typ := t.infer_prop_type(expr) {
			t.env.set_expr_type(pos.id, typ)
		}
	}
}

// has_prop_type checks if the environment has a type set for the given expression ID.
fn (t &Transformer) has_prop_type(id int) bool {
	return t.env.has_expr_type(id)
}

// infer_prop_type tries to determine the type of an expression from its content.
fn (mut t Transformer) infer_prop_type(expr ast.Expr) ?types.Type {
	if typ := t.get_expr_type(expr) {
		return typ
	}
	match expr {
		ast.BasicLiteral {
			match expr.kind {
				.key_true, .key_false {
					return types.Type(types.bool_)
				}
				.number {
					if expr.value.contains('.') {
						return types.Type(types.Primitive{
							props: .untyped | .float
						})
					}
					return types.Type(types.Primitive{
						props: .untyped | .integer
					})
				}
				.char {
					return types.Type(types.int_)
				}
				else {}
			}
		}
		ast.StringLiteral {
			return types.Type(types.string_)
		}
		ast.StringInterLiteral {
			return types.Type(types.string_)
		}
		ast.Ident {
			// Try to look up the identifier in the scope (including parent scopes)
			if t.scope != unsafe { nil } {
				if obj := t.scope.lookup_parent(expr.name, 0) {
					return obj.typ()
				}
			}
			// Check if it's a type name or C identifier
			if typ := t.c_name_to_type(expr.name) {
				return typ
			}
			// Try module scope as fallback
			if typ := t.lookup_var_type(expr.name) {
				return typ
			}
			return none
		}
		ast.InfixExpr {
			// Comparison operators return bool
			if expr.op in [.eq, .ne, .lt, .gt, .le, .ge, .key_is, .not_is, .key_in, .not_in, .and,
				.logical_or] {
				return types.Type(types.bool_)
			}
			// For arithmetic, return the type of the left operand
			if lhs_type := t.get_expr_type(expr.lhs) {
				return lhs_type
			}
			return none
		}
		ast.PrefixExpr {
			if expr.op == .not {
				return types.Type(types.bool_)
			}
			if expr.op == .amp {
				if inner_type := t.get_expr_type(expr.expr) {
					return types.Type(types.Pointer{
						base_type: inner_type
					})
				}
			}
			if inner_type := t.get_expr_type(expr.expr) {
				return inner_type
			}
			return none
		}
		ast.PostfixExpr {
			if inner_type := t.get_expr_type(expr.expr) {
				return inner_type
			}
			return none
		}
		ast.ParenExpr {
			if inner_type := t.get_expr_type(expr.expr) {
				return inner_type
			}
			return none
		}
		ast.ModifierExpr {
			if inner_type := t.get_expr_type(expr.expr) {
				return inner_type
			}
			return none
		}
		ast.CastExpr {
			if inner_type := t.get_expr_type(expr.typ) {
				return inner_type
			}
			return none
		}
		ast.SelectorExpr {
			// Try to look up the field type using the LHS type
			if lhs_type := t.get_expr_type(expr.lhs) {
				base := t.unwrap_alias_and_pointer_type(lhs_type)
				// Handle built-in type fields directly
				if base is types.Array {
					match expr.rhs.name {
						'len', 'cap', 'element_size' {
							return types.Type(types.int_)
						}
						'data' {
							return types.Type(types.Pointer{
								base_type: types.Type(types.void_)
							})
						}
						else {}
					}
				}
				if base is types.String {
					if expr.rhs.name == 'len' || expr.rhs.name == 'is_lit' {
						return types.Type(types.int_)
					}
					if expr.rhs.name == 'str' {
						return types.Type(types.Pointer{
							base_type: types.Type(types.Primitive{
								props: .integer | .unsigned
								size:  8
							})
						})
					}
				}
				if base is types.Map {
					if expr.rhs.name == 'len' {
						return types.Type(types.int_)
					}
				}
				// Try struct field lookup with the original type name
				type_name := lhs_type.name()
				if field_type := t.lookup_struct_field_type(type_name, expr.rhs.name) {
					return field_type
				}
				// Try with the unwrapped base type name
				base_name := base.name()
				if base_name != type_name {
					if field_type := t.lookup_struct_field_type(base_name, expr.rhs.name) {
						return field_type
					}
				}
			}
			return none
		}
		ast.CallExpr {
			// Try method return type resolution first
			if ret := t.get_method_return_type(expr) {
				return ret
			}
			// Try to get the return type from the function type
			if fn_type := t.get_expr_type(expr.lhs) {
				if fn_type is types.FnType {
					if ret := fn_type.get_return_type() {
						return ret
					}
					return types.Type(types.void_)
				}
			}
			// Handle transformer-generated builtin calls
			if expr.lhs is ast.Ident {
				call_name := expr.lhs.name
				// Known transformer-generated calls with known return types
				if call_name.starts_with('string__') {
					if call_name == 'string__plus' || call_name == 'string__clone'
						|| call_name == 'string__substr' || call_name == 'string__trim'
						|| call_name == 'string__replace' {
						return types.Type(types.string_)
					}
					if call_name == 'string__eq' || call_name == 'string__ne'
						|| call_name == 'string__lt' || call_name == 'string__le'
						|| call_name == 'string__gt' || call_name == 'string__ge'
						|| call_name == 'string__contains' || call_name == 'string__starts_with'
						|| call_name == 'string__ends_with' {
						return types.Type(types.bool_)
					}
				}
				if call_name == 'map__set' || call_name == 'array__push'
					|| call_name == 'array__push_many' || call_name == 'array__set' {
					return types.Type(types.void_)
				}
				// Try generic mangled call resolution (module__Type__method)
				if call_name.contains('__') {
					parts_type := call_name.all_before_last('__')
					parts_method := call_name.all_after_last('__')
					if parts_method != '' {
						type_name := if parts_type.contains('__') {
							parts_type.all_after_last('__')
						} else {
							parts_type
						}
						if ret := t.lookup_method_return_type([type_name], parts_method) {
							return ret
						}
					}
				}
			}
			return none
		}
		ast.IndexExpr {
			if container_type := t.get_expr_type(expr.lhs) {
				return container_type.value_type()
			}
			return none
		}
		ast.KeywordOperator {
			if expr.op == .key_sizeof || expr.op == .key_isreftype {
				return types.Type(types.int_)
			}
			return none
		}
		ast.IfExpr {
			// Try to infer from the then branch's last statement
			if expr.stmts.len > 0 {
				last_stmt := expr.stmts.last()
				if last_stmt is ast.ExprStmt {
					if inner_type := t.get_expr_type(last_stmt.expr) {
						return inner_type
					}
				}
			}
			return none
		}
		else {
			return none
		}
	}

	return none
}

fn (t &Transformer) cursor_pos_type(c ast.Cursor) ?types.Type {
	pos := c.pos()
	if typ := t.get_synth_type(pos) {
		return typ
	}
	if pos.is_valid() {
		if typ := t.env.get_expr_type(pos.id) {
			return t.normalize_type(typ)
		}
	}
	return none
}

fn (t &Transformer) get_expr_type_cursor(c ast.Cursor) ?types.Type {
	if !c.is_valid() {
		return none
	}
	if c.kind() == .expr_string {
		lit_kind := unsafe { ast.StringLiteralKind(int(c.aux())) }
		if lit_kind == .c {
			if typ := types.builtin_type('charptr') {
				return typ
			}
		}
	}
	match c.kind() {
		.expr_ident {
			if typ := t.smartcast_type_for_expr_cursor(c) {
				return typ
			}
			if typ := t.lookup_local_decl_type(c.name()) {
				return typ
			}
			if typ := t.lookup_var_type(c.name()) {
				return typ
			}
			return t.cursor_pos_type(c)
		}
		.expr_unsafe {
			if typ := t.cursor_pos_type(c) {
				return typ
			}
			if c.edge_count() > 0 {
				last := c.edge(c.edge_count() - 1)
				if last.kind() == .stmt_expr {
					return t.get_expr_type_cursor(last.edge(0))
				}
			}
			return none
		}
		.expr_call, .expr_call_or_cast {
			if typ := t.cursor_pos_type(c) {
				return typ
			}
			if c.kind() == .expr_call_or_cast {
				lhs := c.edge(0).type_expr()
				if t.call_or_cast_lhs_is_type(lhs) {
					if typ := t.type_from_param_type_expr(lhs, []) {
						return t.normalize_type(typ)
					}
				}
			}
			if fn_type := t.get_expr_type_cursor(c.edge(0)) {
				if fn_type is types.FnType {
					if ret := fn_type.get_return_type() {
						return t.normalize_type(ret)
					}
					return types.Type(types.void_)
				}
			}
			if typ := t.resolve_call_return_type_cursor(c) {
				return t.normalize_type(typ)
			}
			return none
		}
		.expr_cast {
			if typ := t.type_from_param_type_expr(c.edge(0).type_expr(), []) {
				return t.normalize_type(typ)
			}
			return t.cursor_pos_type(c)
		}
		.expr_as_cast {
			if typ := t.type_from_param_type_expr(c.edge(1).type_expr(), []) {
				return t.normalize_type(typ)
			}
			return t.cursor_pos_type(c)
		}
		.expr_prefix {
			op := unsafe { token.Token(int(c.aux())) }
			if op == .mul {
				if inner_type := t.get_expr_type_cursor(c.edge(0)) {
					if inner_type is types.Pointer {
						return t.normalize_type(inner_type.base_type)
					}
				}
			}
			return t.cursor_pos_type(c)
		}
		.expr_array_init {
			if typ := t.cursor_pos_type(c) {
				return concrete_literal_array_type(t.normalize_type(typ))
			}
			return t.get_array_init_expr_type_cursor(c)
		}
		.expr_init {
			if typ := t.type_from_init_expr_cursor(c) {
				return typ
			}
			return t.cursor_pos_type(c)
		}
		.expr_selector {
			if typ := t.smartcast_type_for_expr_cursor(c) {
				return typ
			}
			rhs_name := selector_rhs_name_cursor(c)
			lhs := c.edge(0)
			if rhs_name == 'name' && lhs.kind() == .expr_ident
				&& (lhs.name() in t.cur_fn_generic_params
				|| lhs.name() in t.generic_var_type_params) {
				return types.Type(types.string_)
			}
			if lhs_type := t.get_expr_type_cursor(lhs) {
				if field_typ := t.field_type_from_receiver_type(lhs_type, rhs_name) {
					return t.normalize_type(field_typ)
				}
			}
			return t.cursor_pos_type(c)
		}
		.expr_index {
			if c.edge(1).kind() == .expr_range {
				if lhs_type := t.get_expr_type_cursor(c.edge(0)) {
					return t.normalize_type(lhs_type)
				}
			}
			if lhs_type := t.get_expr_type_cursor(c.edge(0)) {
				if map_type := t.unwrap_map_type(lhs_type) {
					return map_type.value_type
				}
				mut base_type := lhs_type
				for {
					if base_type is types.Pointer {
						base_type = base_type.base_type
						continue
					}
					if base_type is types.Alias {
						base_type = t.live_alias_base_type(base_type) or { break }
						continue
					}
					break
				}
				if base_type is types.Array {
					return t.normalize_type(base_type.elem_type)
				}
				if base_type is types.ArrayFixed {
					return t.normalize_type(base_type.elem_type)
				}
				if t.is_string_iterable_type(base_type) {
					return string_iter_value_type()
				}
			}
			return t.cursor_pos_type(c)
		}
		else {
			return t.cursor_pos_type(c)
		}
	}
}

fn (mut t Transformer) infer_prop_type_cursor(c ast.Cursor) ?types.Type {
	if typ := t.get_expr_type_cursor(c) {
		return typ
	}
	match c.kind() {
		.expr_basic_literal {
			kind := unsafe { token.Token(int(c.aux())) }
			match kind {
				.key_true, .key_false {
					return types.Type(types.bool_)
				}
				.number {
					value := c.name()
					if value.contains('.') {
						return types.Type(types.Primitive{
							props: .untyped | .float
						})
					}
					return types.Type(types.Primitive{
						props: .untyped | .integer
					})
				}
				.char {
					return types.Type(types.int_)
				}
				else {}
			}
		}
		.expr_string, .expr_string_inter {
			return types.Type(types.string_)
		}
		.expr_ident {
			if t.scope != unsafe { nil } {
				if obj := t.scope.lookup_parent(c.name(), 0) {
					return obj.typ()
				}
			}
			if typ := t.c_name_to_type(c.name()) {
				return typ
			}
			if typ := t.lookup_var_type(c.name()) {
				return typ
			}
		}
		.expr_infix {
			op := unsafe { token.Token(int(c.aux())) }
			if op in [.eq, .ne, .lt, .gt, .le, .ge, .key_is, .not_is, .key_in, .not_in, .and,
				.logical_or] {
				return types.Type(types.bool_)
			}
			if lhs_type := t.get_expr_type_cursor(c.edge(0)) {
				return lhs_type
			}
		}
		.expr_prefix {
			op := unsafe { token.Token(int(c.aux())) }
			if op == .not {
				return types.Type(types.bool_)
			}
			if op == .amp {
				if inner_type := t.get_expr_type_cursor(c.edge(0)) {
					return types.Type(types.Pointer{
						base_type: inner_type
					})
				}
			}
			if inner_type := t.get_expr_type_cursor(c.edge(0)) {
				return inner_type
			}
		}
		.expr_postfix, .expr_paren, .expr_modifier {
			if inner_type := t.get_expr_type_cursor(c.edge(0)) {
				return inner_type
			}
		}
		.expr_cast {
			if inner_type := t.get_expr_type_cursor(c.edge(0)) {
				return inner_type
			}
		}
		.expr_selector {
			rhs_name := selector_rhs_name_cursor(c)
			if lhs_type := t.get_expr_type_cursor(c.edge(0)) {
				base := t.unwrap_alias_and_pointer_type(lhs_type)
				if base is types.Array {
					match rhs_name {
						'len', 'cap', 'element_size' {
							return types.Type(types.int_)
						}
						'data' {
							return types.Type(types.Pointer{
								base_type: types.Type(types.void_)
							})
						}
						else {}
					}
				}
				if base is types.String {
					if rhs_name == 'len' || rhs_name == 'is_lit' {
						return types.Type(types.int_)
					}
					if rhs_name == 'str' {
						return types.Type(types.Pointer{
							base_type: types.Type(types.Primitive{
								props: .integer | .unsigned
								size:  8
							})
						})
					}
				}
				if base is types.Map && rhs_name == 'len' {
					return types.Type(types.int_)
				}
				type_name := lhs_type.name()
				if field_type := t.lookup_struct_field_type(type_name, rhs_name) {
					return field_type
				}
				base_name := base.name()
				if base_name != type_name {
					if field_type := t.lookup_struct_field_type(base_name, rhs_name) {
						return field_type
					}
				}
			}
		}
		.expr_call {
			if ret := t.get_method_return_type_cursor(c) {
				return ret
			}
			if fn_type := t.get_expr_type_cursor(c.edge(0)) {
				if fn_type is types.FnType {
					if ret := fn_type.get_return_type() {
						return ret
					}
					return types.Type(types.void_)
				}
			}
			call_name := call_ident_name_cursor(c.edge(0))
			if ret := transformer_generated_call_return_type(call_name) {
				return ret
			}
			if call_name.contains('__') {
				parts_type := call_name.all_before_last('__')
				parts_method := call_name.all_after_last('__')
				if parts_method != '' {
					type_name := if parts_type.contains('__') {
						parts_type.all_after_last('__')
					} else {
						parts_type
					}
					if ret := t.lookup_method_return_type([type_name], parts_method) {
						return ret
					}
				}
			}
		}
		.expr_index {
			if container_type := t.get_expr_type_cursor(c.edge(0)) {
				return container_type.value_type()
			}
		}
		.expr_keyword_operator {
			op := unsafe { token.Token(int(c.aux())) }
			if op == .key_sizeof || op == .key_isreftype {
				return types.Type(types.int_)
			}
		}
		.expr_if {
			if c.edge_count() > 2 {
				last_stmt := c.edge(c.edge_count() - 1)
				if last_stmt.kind() == .stmt_expr {
					if inner_type := t.get_expr_type_cursor(last_stmt.edge(0)) {
						return inner_type
					}
				}
			}
		}
		else {}
	}

	return none
}

fn transformer_generated_call_return_type(call_name string) ?types.Type {
	if call_name.starts_with('string__') {
		match call_name {
			'string__plus', 'string__clone', 'string__substr', 'string__trim', 'string__replace' {
				return types.Type(types.string_)
			}
			'string__eq', 'string__ne', 'string__lt', 'string__le', 'string__gt', 'string__ge',
			'string__contains', 'string__starts_with', 'string__ends_with' {
				return types.Type(types.bool_)
			}
			else {}
		}
	}
	if call_name == 'map__set' || call_name == 'array__push' || call_name == 'array__push_many'
		|| call_name == 'array__set' {
		return types.Type(types.void_)
	}
	return none
}

fn (t &Transformer) get_array_init_expr_type_cursor(c ast.Cursor) ?types.Type {
	is_fixed := array_init_cursor_has_fixed_len_marker(c)
	typ_c := c.edge(0)
	if typ_c.is_valid() && typ_c.kind() != .expr_empty {
		if typ := t.type_from_param_type_expr(typ_c.type_expr(), []) {
			if is_fixed {
				base := t.unwrap_alias_and_pointer_type(typ)
				if base is types.Array {
					return types.Type(types.ArrayFixed{
						len:       c.edge_count() - 5
						elem_type: base.elem_type
					})
				}
			}
			return typ
		}
	}
	for i in 5 .. c.edge_count() {
		if typ := t.get_array_init_elem_expr_type_cursor(c.edge(i)) {
			if is_fixed {
				return types.Type(types.ArrayFixed{
					len:       c.edge_count() - 5
					elem_type: typ
				})
			}
			return types.Type(types.Array{
				elem_type: typ
			})
		}
	}
	return none
}

fn (t &Transformer) get_array_init_elem_expr_type_cursor(c ast.Cursor) ?types.Type {
	match c.kind() {
		.expr_array_init {
			return t.get_array_init_expr_type_cursor(c)
		}
		.expr_string, .expr_string_inter {
			return types.Type(types.string_)
		}
		.expr_basic_literal {
			kind := unsafe { token.Token(int(c.aux())) }
			match kind {
				.string {
					return types.Type(types.string_)
				}
				.key_true, .key_false {
					return types.Type(types.bool_)
				}
				.number {
					value := c.name()
					if value.contains('.') || value.contains('e') || value.contains('E') {
						return types.Type(types.f64_)
					}
					return types.Type(types.int_)
				}
				.char {
					return types.Type(types.int_)
				}
				else {}
			}
		}
		else {}
	}

	if typ := t.get_expr_type_cursor(c) {
		return concrete_literal_array_elem_type(typ)
	}
	return none
}

fn array_init_cursor_has_fixed_len_marker(c ast.Cursor) bool {
	len_c := c.edge(3)
	if len_c.kind() != .expr_postfix {
		return false
	}
	op := unsafe { token.Token(int(len_c.aux())) }
	return op == .not && len_c.edge(0).kind() == .expr_empty
}

fn (t &Transformer) type_from_init_expr_cursor(c ast.Cursor) ?types.Type {
	typ_c := c.edge(0)
	if generic_type_name := t.generic_init_type_name_cursor(typ_c) {
		if typ := t.lookup_type(generic_type_name) {
			return t.normalize_type(typ)
		}
		return types.Type(types.Struct{
			name: generic_type_name
		})
	}
	typ_expr := typ_c.type_expr()
	if typ := t.type_from_param_type_expr(typ_expr, []) {
		normalized := t.normalize_type(typ)
		if t.type_to_c_name(normalized) != '' {
			return normalized
		}
	}
	if typ := t.get_expr_type_cursor(typ_c) {
		normalized := t.normalize_type(typ)
		if t.type_to_c_name(normalized) != '' {
			return normalized
		}
	}
	type_name := t.expr_to_type_name_cursor(typ_c)
	if type_name == '' {
		return none
	}
	if typ := t.c_name_to_type(type_name) {
		normalized := t.normalize_type(typ)
		if t.type_to_c_name(normalized) != '' {
			return normalized
		}
	}
	if typ := t.lookup_type(type_name) {
		normalized := t.normalize_type(typ)
		if t.type_to_c_name(normalized) != '' {
			return normalized
		}
	}
	c_type_name := t.v_type_name_to_c_name(type_name)
	return types.Type(types.Struct{
		name: if c_type_name != '' { c_type_name } else { type_name }
	})
}

fn (t &Transformer) generic_init_type_name_cursor(c ast.Cursor) ?string {
	match c.kind() {
		.expr_generic_args, .expr_generic_arg_or_index, .expr_index {
			typ_expr := c.type_expr()
			return t.generic_init_type_name(typ_expr)
		}
		else {}
	}

	return none
}

fn (t &Transformer) expr_to_type_name_cursor(c ast.Cursor) string {
	if c.kind() == .expr_ident {
		if typ := t.get_synth_type(c.pos()) {
			c_name := t.type_to_c_name(typ)
			if c_name != '' {
				return c_name
			}
		}
		return c.name()
	}
	typ_expr := c.type_expr()
	if typ_expr !is ast.EmptyExpr {
		return t.expr_to_type_name(typ_expr)
	}
	return ''
}

fn selector_rhs_name_cursor(c ast.Cursor) string {
	rhs := c.edge(1)
	if rhs.kind() == .expr_ident {
		return rhs.name()
	}
	return ''
}

fn call_ident_name_cursor(c ast.Cursor) string {
	target := unwrap_call_target_lhs_cursor(c)
	if target.kind() == .expr_ident {
		return target.name()
	}
	return ''
}

fn unwrap_call_target_lhs_cursor(c ast.Cursor) ast.Cursor {
	mut cur := c
	for cur.is_valid() {
		match cur.kind() {
			.expr_paren, .expr_modifier, .expr_generic_args, .expr_generic_arg_or_index {
				cur = cur.edge(0)
			}
			else {
				return cur
			}
		}
	}
	return cur
}

fn (t &Transformer) resolve_call_return_type_cursor(c ast.Cursor) ?types.Type {
	if c.kind() != .expr_call && c.kind() != .expr_call_or_cast {
		return none
	}
	call_lhs := unwrap_call_target_lhs_cursor(c.edge(0))
	match call_lhs.kind() {
		.expr_ident {
			return t.get_fn_return_type(call_lhs.name())
		}
		.expr_selector {
			sel_lhs := call_lhs.edge(0)
			method_name := selector_rhs_name_cursor(call_lhs)
			if sel_lhs.kind() == .expr_ident {
				mod_name := sel_lhs.name()
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
					if fn_type := t.lookup_fn_cached(module_name, method_name) {
						if ret_type := fn_type.get_return_type() {
							return ret_type
						}
					}
				}
			}
			return t.get_method_return_type_cursor(c)
		}
		else {
			return none
		}
	}
}

fn (t &Transformer) get_method_return_type_cursor(c ast.Cursor) ?types.Type {
	call_lhs := if c.kind() == .expr_selector {
		c
	} else if c.kind() == .expr_call || c.kind() == .expr_call_or_cast {
		unwrap_call_target_lhs_cursor(c.edge(0))
	} else {
		return none
	}
	if call_lhs.kind() != .expr_selector {
		return none
	}
	method_name := selector_rhs_name_cursor(call_lhs)
	receiver := call_lhs.edge(0)
	mut lookup_type_names := []string{}
	if receiver_type := t.get_expr_type_cursor(receiver) {
		t.append_method_lookup_type_name(mut lookup_type_names, receiver_type.name())
		base_type := t.unwrap_alias_and_pointer_type(receiver_type)
		t.append_method_lookup_type_name(mut lookup_type_names, base_type.name())
		if ret_type := t.interface_method_return_type(receiver_type, method_name) {
			return ret_type
		}
	}
	if t.is_string_expr_cursor(receiver) {
		if ret_type := builtin_string_method_return_type(method_name) {
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
	return none
}

fn (t &Transformer) is_string_expr_cursor(c ast.Cursor) bool {
	match c.kind() {
		.expr_string, .expr_string_inter {
			return true
		}
		.expr_basic_literal {
			kind := unsafe { token.Token(int(c.aux())) }
			return kind == .string
		}
		else {}
	}

	if typ := t.get_expr_type_cursor(c) {
		base := t.unwrap_alias_and_pointer_type(typ)
		return base is types.String || base.name() == 'string'
	}
	return false
}

fn (t &Transformer) smartcast_type_for_expr_cursor(c ast.Cursor) ?types.Type {
	expr_str := expr_cursor_to_string(c)
	if expr_str == '' {
		return none
	}
	ctx := t.find_smartcast_for_expr(expr_str) or { return none }
	if ctx.variant_full != '' {
		if typ := t.c_name_to_type(ctx.variant_full) {
			return t.normalize_type(typ)
		}
		if typ := t.lookup_type(ctx.variant_full) {
			return t.normalize_type(typ)
		}
	}
	if ctx.variant != '' {
		if typ := t.c_name_to_type(ctx.variant) {
			return t.normalize_type(typ)
		}
		if typ := t.lookup_type(ctx.variant) {
			return t.normalize_type(typ)
		}
	}
	return none
}

fn expr_cursor_to_string(c ast.Cursor) string {
	if !c.is_valid() {
		return ''
	}
	match c.kind() {
		.expr_ident {
			return c.name()
		}
		.expr_selector {
			lhs := expr_cursor_to_string(c.edge(0))
			rhs := selector_rhs_name_cursor(c)
			if lhs == '' || rhs == '' {
				return ''
			}
			return '${lhs}.${rhs}'
		}
		.expr_paren, .expr_modifier {
			return expr_cursor_to_string(c.edge(0))
		}
		else {
			return ''
		}
	}
}

// prop_fn_scope_key builds the scope key for a function declaration,
// matching the format used by the checker when storing fn scopes.
fn (t &Transformer) prop_fn_scope_key(decl ast.FnDecl) string {
	scope_fn_name := if decl.is_method {
		recv_name := t.get_receiver_type_name(decl.receiver.typ)
		'${recv_name}__${decl.name}'
	} else {
		decl.name
	}
	if t.cur_module == '' {
		return scope_fn_name
	}
	return '${t.cur_module}__${scope_fn_name}'
}

fn (t &Transformer) prop_fn_scope_key_cursor(decl ast.Cursor) string {
	scope_fn_name := if decl.flag(ast.flag_is_method) {
		recv_name := t.get_receiver_type_name_cursor(decl.edge(0).edge(0))
		'${recv_name}__${decl.name()}'
	} else {
		decl.name()
	}
	if t.cur_module == '' {
		return scope_fn_name
	}
	return '${t.cur_module}__${scope_fn_name}'
}
