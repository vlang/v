// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module transformer

import v2.ast
import v2.types

// propagate_types walks the transformed AST and fills in missing expression types
// for expressions that the checker didn't visit (e.g., inside unselected $if branches
// that were later resolved by the transformer, or inside complex lowered constructs).
fn (mut t Transformer) propagate_types(files []ast.File) {
	for file in files {
		// Set module scope for Ident resolution
		if mod_scope := t.env.get_scope(file.mod) {
			t.scope = mod_scope
		}
		t.cur_module = file.mod
		for stmt in file.stmts {
			t.prop_stmt(stmt)
		}
	}
}

fn (mut t Transformer) prop_stmt(stmt ast.Stmt) {
	match stmt {
		ast.AssertStmt {
			t.prop_expr(stmt.expr)
			t.prop_expr(stmt.extra)
		}
		ast.AssignStmt {
			for e in stmt.lhs {
				t.prop_expr(e)
			}
			for e in stmt.rhs {
				t.prop_expr(e)
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
			if fn_scope := t.env.get_fn_scope_by_key(scope_key) {
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
			for e in stmt.exprs {
				t.prop_expr(e)
			}
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
	// Recurse into sub-expressions FIRST so children have types before parent inference
	match expr {
		ast.ArrayInitExpr {
			t.prop_expr(expr.typ)
			for e in expr.exprs {
				t.prop_expr(e)
			}
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
			for arg in expr.args {
				t.prop_expr(arg)
			}
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
			for arg in expr.args {
				t.prop_expr(arg)
			}
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
			for e in expr.exprs {
				t.prop_expr(e)
			}
		}
		ast.LambdaExpr {
			t.prop_expr(expr.expr)
		}
		ast.LockExpr {
			for e in expr.lock_exprs {
				t.prop_expr(e)
			}
			for e in expr.rlock_exprs {
				t.prop_expr(e)
			}
			for s in expr.stmts {
				t.prop_stmt(s)
			}
		}
		ast.MapInitExpr {
			t.prop_expr(expr.typ)
			for k in expr.keys {
				t.prop_expr(k)
			}
			for v in expr.vals {
				t.prop_expr(v)
			}
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
			for e in expr.exprs {
				t.prop_expr(e)
			}
		}
		ast.UnsafeExpr {
			for s in expr.stmts {
				t.prop_stmt(s)
			}
		}
		else {}
	}

	// Now try to infer and set the type for this expression
	pos := expr.pos()
	if pos.is_valid() && !t.has_prop_type(pos.id) {
		if typ := t.infer_prop_type(expr) {
			t.env.set_expr_type(pos.id, typ)
		}
	}
}

// has_prop_type checks if the environment has a type set for the given expression ID.
fn (t &Transformer) has_prop_type(id int) bool {
	if id > 0 && id < t.env.expr_type_values.len {
		typ := t.env.expr_type_values[id]
		if typ is types.Void {
			return u8(typ) != 1
		}
		return true
	} else if id < 0 {
		idx := -id
		if idx < t.env.expr_type_neg_values.len {
			typ := t.env.expr_type_neg_values[idx]
			if typ is types.Void {
				return u8(typ) != 1
			}
			return true
		}
	}
	return false
}

// infer_prop_type tries to determine the type of an expression from its content.
fn (mut t Transformer) infer_prop_type(expr ast.Expr) ?types.Type {
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
			return t.get_expr_type(expr.expr)
		}
		ast.PostfixExpr {
			return t.get_expr_type(expr.expr)
		}
		ast.ParenExpr {
			return t.get_expr_type(expr.expr)
		}
		ast.ModifierExpr {
			return t.get_expr_type(expr.expr)
		}
		ast.CastExpr {
			return t.get_expr_type(expr.typ)
		}
		ast.SelectorExpr {
			// Try to look up the field type using the LHS type
			if lhs_type := t.get_expr_type(expr.lhs) {
				type_name := lhs_type.name()
				if field_type := t.lookup_struct_field_type(type_name, expr.rhs.name) {
					return field_type
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
					return t.get_expr_type(last_stmt.expr)
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
