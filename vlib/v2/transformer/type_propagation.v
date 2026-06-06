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
// `propagate_types`. Today it's a thin wrapper that rehydrates `flat`
// back into `[]ast.File` via `flat.to_files_range(0, flat.files.len)` and
// delegates to the legacy walker — identical behaviour by construction.
//
// Lets `apply_post_pass_tail_from_flat` (s167) take `&FlatAst` instead of
// `[]ast.File`, which closes the last `[]ast.File` consumer in the
// post_pass tail. The wrapper does NOT save memory under -gc none (the
// rehydrated array is the same size as legacy `result`), but it
// straightens the call graph ahead of the SSA migration and fixes a
// latent staleness bug in the `_via_driver` wedges: legacy
// `post_pass(result)` mutates `result` BEFORE `propagate_types` runs,
// whereas s162/s163 wedges passed un-post_pass'd `result` to
// `apply_post_pass_tail` so `propagate_types` saw stale stmts. With this
// helper plus the s167 `apply_post_pass_tail_from_flat`, the wedge passes
// `&builder.flat` (already post_pass'd by `post_pass_to_flat`) so the
// non-arm64 propagation sees the same post_pass'd stmts as legacy.
fn (mut t Transformer) propagate_types_from_flat(flat &ast.FlatAst) {
	if flat.files.len == 0 {
		return
	}
	// Cursor-native walk: handled stmt/expr kinds recurse straight over the flat
	// (no decode), un-ported kinds fall back to decode_stmt/decode_expr + the
	// legacy walker. Behaviour is identical to propagate_types (guarded by the
	// propagate parity test), but the whole-program rehydrate is gone — only the
	// un-ported subtrees are still decoded, shrinking the -gc none footprint as
	// more kinds are ported. Mirrors propagate_types' per-file scope setup.
	for i in 0 .. flat.files.len {
		fc := flat.file_cursor(i)
		mod := fc.mod()
		if mod_scope := t.cached_scopes[mod] {
			t.scope = mod_scope
		}
		t.cur_module = mod
		stmts := fc.stmts()
		for j in 0 .. stmts.len() {
			t.prop_stmt_from_flat(stmts.at(j))
		}
	}
}

// prop_stmt_from_flat is the cursor-native counterpart of prop_stmt. Handled
// kinds recurse via the cursor; const/global/enum decls fall back to the legacy
// walker on the decoded subtree (their field-value exprs are not ported yet);
// kinds prop_stmt ignores stay a no-op. Recursion mirrors prop_stmt exactly.
fn (mut t Transformer) prop_stmt_from_flat(c ast.Cursor) {
	if !c.is_valid() {
		return
	}
	match c.kind() {
		.stmt_assert {
			t.prop_expr_from_flat(c.edge(0))
			t.prop_expr_from_flat(c.edge(1))
		}
		.stmt_assign {
			// edges 0..lhs_len = lhs, lhs_len..edge_count = rhs (aux = op).
			lhs_len := c.extra_int()
			ec := c.edge_count()
			// RHS first so types are available for LHS propagation (legacy order).
			for k in lhs_len .. ec {
				t.prop_expr_from_flat(c.edge(k))
			}
			for k in 0 .. lhs_len {
				lhs_c := c.edge(k)
				t.prop_expr_from_flat(lhs_c)
				lhs_pos := lhs_c.pos()
				rhs_idx := lhs_len + k
				if lhs_pos.is_valid() && !t.has_prop_type(lhs_pos.id) && rhs_idx < ec {
					if rhs_type := t.prop_get_type_from_flat(c.edge(rhs_idx)) {
						t.env.set_expr_type(lhs_pos.id, rhs_type)
					}
				}
			}
		}
		.stmt_block, .stmt_defer {
			for k in 0 .. c.edge_count() {
				t.prop_stmt_from_flat(c.edge(k))
			}
		}
		.stmt_comptime, .stmt_label {
			t.prop_stmt_from_flat(c.edge(0))
		}
		.stmt_expr {
			t.prop_expr_from_flat(c.edge(0))
		}
		.stmt_fn_decl {
			// Enter function scope for Ident resolution (mirror prop_fn_scope_key).
			old_scope := t.scope
			name := c.name()
			scope_fn_name := if c.flag(ast.flag_is_method) {
				recv_type := c.edge(0).edge(0) // receiver param -> its type expr
				recv_name := t.get_receiver_type_name(c.flat.decode_expr(recv_type.id))
				'${recv_name}__${name}'
			} else {
				name
			}
			scope_key := if t.cur_module == '' {
				scope_fn_name
			} else {
				'${t.cur_module}__${scope_fn_name}'
			}
			if fn_scope := t.cached_fn_scopes[scope_key] {
				t.scope = fn_scope
			}
			body := c.list_at(3)
			for k in 0 .. body.len() {
				t.prop_stmt_from_flat(body.at(k))
			}
			t.scope = old_scope
		}
		.stmt_for {
			t.prop_stmt_from_flat(c.edge(0)) // init
			t.prop_expr_from_flat(c.edge(1)) // cond
			t.prop_stmt_from_flat(c.edge(2)) // post
			for k in 3 .. c.edge_count() {
				t.prop_stmt_from_flat(c.edge(k))
			}
		}
		.stmt_for_in {
			t.prop_expr_from_flat(c.edge(0)) // key
			t.prop_expr_from_flat(c.edge(1)) // value
			t.prop_expr_from_flat(c.edge(2)) // expr
		}
		.stmt_return {
			for k in 0 .. c.edge_count() {
				t.prop_expr_from_flat(c.edge(k))
			}
		}
		.stmt_const_decl, .stmt_global_decl, .stmt_enum_decl {
			// Field-value exprs not ported yet — fall back to the legacy walker.
			t.prop_stmt(c.flat.decode_stmt(c.id))
		}
		else {}
	}
}

// prop_expr_from_flat is the cursor-native counterpart of prop_expr. Ported
// kinds recurse over the cursor (children first) then infer + set the node's
// type; un-ported kinds (call, cast, struct/array/map init, string interp,
// match, lock, lambda, ...) fall back to the legacy walker on the decoded
// subtree, which sets their types in env by id so any ported parent can read
// them back. Recursion order matches prop_expr.
fn (mut t Transformer) prop_expr_from_flat(c ast.Cursor) {
	if !c.is_valid() {
		return
	}
	match c.kind() {
		.expr_basic_literal, .expr_string, .expr_ident {
			// leaves — no recursion
		}
		.expr_infix, .expr_index {
			t.prop_expr_from_flat(c.edge(0))
			t.prop_expr_from_flat(c.edge(1))
		}
		.expr_prefix, .expr_postfix, .expr_paren, .expr_modifier {
			t.prop_expr_from_flat(c.edge(0))
		}
		.expr_selector {
			// legacy recurses only the lhs; the rhs is the field Ident.
			t.prop_expr_from_flat(c.edge(0))
		}
		.expr_keyword_operator {
			for k in 0 .. c.edge_count() {
				t.prop_expr_from_flat(c.edge(k))
			}
		}
		.expr_if {
			// edges: cond(0), else_expr(1), then-stmts(2..). Legacy order is
			// cond, then-stmts, else_expr.
			t.prop_expr_from_flat(c.edge(0))
			for k in 2 .. c.edge_count() {
				t.prop_stmt_from_flat(c.edge(k))
			}
			t.prop_expr_from_flat(c.edge(1))
		}
		else {
			// un-ported: legacy walker handles recursion + inference + env writes.
			t.prop_expr(c.flat.decode_expr(c.id))
			return
		}
	}

	pos := c.pos()
	if pos.is_valid() && !t.has_prop_type(pos.id) {
		if typ := t.infer_prop_type_from_flat(c) {
			t.env.set_expr_type(pos.id, typ)
		}
	}
}

// prop_get_type_from_flat is the cursor-native counterpart of get_expr_type for
// the propagate context. Children are visited before parents, so a typed child
// is already in env by id; Idents additionally resolve by name (smartcast /
// local / module var), matching get_expr_type's ordering. Un-ported children
// (call/cast/...) were typed by the legacy fallback via the SAME get_expr_type
// path, so their env-by-id value equals what get_expr_type would return.
fn (t &Transformer) prop_get_type_from_flat(c ast.Cursor) ?types.Type {
	if !c.is_valid() {
		return none
	}
	pos := c.pos()
	if c.kind() == .expr_ident {
		id := ast.Ident{
			name: c.name()
			pos:  pos
		}
		if typ := t.smartcast_type_for_expr(id) {
			return typ
		}
		if typ := t.lookup_local_decl_type(id.name) {
			return typ
		}
		if typ := t.lookup_var_type(id.name) {
			return typ
		}
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

// infer_prop_type_from_flat is the cursor-native counterpart of infer_prop_type
// for the ported expr kinds. Mirrors it case-for-case.
fn (mut t Transformer) infer_prop_type_from_flat(c ast.Cursor) ?types.Type {
	if typ := t.prop_get_type_from_flat(c) {
		return typ
	}
	match c.kind() {
		.expr_basic_literal {
			match unsafe { token.Token(int(c.aux())) } {
				.key_true, .key_false {
					return types.Type(types.bool_)
				}
				.number {
					if c.name().contains('.') {
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
		.expr_string {
			return types.Type(types.string_)
		}
		.expr_ident {
			name := c.name()
			if t.scope != unsafe { nil } {
				if obj := t.scope.lookup_parent(name, 0) {
					return obj.typ()
				}
			}
			if typ := t.c_name_to_type(name) {
				return typ
			}
			if typ := t.lookup_var_type(name) {
				return typ
			}
			return none
		}
		.expr_infix {
			op := unsafe { token.Token(int(c.aux())) }
			if op in [.eq, .ne, .lt, .gt, .le, .ge, .key_is, .not_is, .key_in, .not_in, .and,
				.logical_or] {
				return types.Type(types.bool_)
			}
			if lhs_type := t.prop_get_type_from_flat(c.edge(0)) {
				return lhs_type
			}
			return none
		}
		.expr_prefix {
			op := unsafe { token.Token(int(c.aux())) }
			if op == .not {
				return types.Type(types.bool_)
			}
			if op == .amp {
				if inner := t.prop_get_type_from_flat(c.edge(0)) {
					return types.Type(types.Pointer{
						base_type: inner
					})
				}
			}
			if inner := t.prop_get_type_from_flat(c.edge(0)) {
				return inner
			}
			return none
		}
		.expr_postfix, .expr_paren, .expr_modifier {
			if inner := t.prop_get_type_from_flat(c.edge(0)) {
				return inner
			}
			return none
		}
		.expr_index {
			if container_type := t.prop_get_type_from_flat(c.edge(0)) {
				return container_type.value_type()
			}
			return none
		}
		.expr_keyword_operator {
			op := unsafe { token.Token(int(c.aux())) }
			if op == .key_sizeof || op == .key_isreftype {
				return types.Type(types.int_)
			}
			return none
		}
		.expr_selector {
			if lhs_type := t.prop_get_type_from_flat(c.edge(0)) {
				base := t.unwrap_alias_and_pointer_type(lhs_type)
				rhs_name := c.edge(1).name()
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
				if base is types.Map {
					if rhs_name == 'len' {
						return types.Type(types.int_)
					}
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
			return none
		}
		.expr_if {
			// then-branch is edges 2..; infer from the last then ExprStmt.
			ec := c.edge_count()
			if ec > 2 {
				last := c.edge(ec - 1)
				if last.kind() == .stmt_expr {
					if inner := t.prop_get_type_from_flat(last.edge(0)) {
						return inner
					}
				}
			}
			return none
		}
		else {}
	}

	return none
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
