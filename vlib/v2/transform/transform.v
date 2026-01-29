// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// The transform module provides AST-level transformations that lower
// complex language constructs into simpler forms. This avoids duplicating
// lowering logic across multiple backends (C, x64, arm64, etc).
module transform

import v2.ast

pub struct Transformer {
mut:
	// Track array literals that need lowering
	array_temps int
	// Track variable types for type inference (var_name -> type_name)
	var_types map[string]string
}

pub fn Transformer.new() &Transformer {
	return &Transformer{}
}

// transform processes an entire file, lowering complex constructs
// Returns a new file with transformed statements
pub fn (mut t Transformer) transform(file ast.File) ast.File {
	mut new_stmts := []ast.Stmt{cap: file.stmts.len}
	for stmt in file.stmts {
		new_stmts << t.stmt(stmt)
	}
	return ast.File{
		attributes: file.attributes
		mod:        file.mod
		name:       file.name
		stmts:      new_stmts
		imports:    file.imports
	}
}

fn (mut t Transformer) stmt(node ast.Stmt) ast.Stmt {
	match node {
		ast.AssignStmt {
			return t.assign_stmt(node)
		}
		ast.FnDecl {
			return t.fn_decl(node)
		}
		ast.ForStmt {
			return t.for_stmt(node)
		}
		ast.BlockStmt {
			return t.block_stmt(node)
		}
		ast.ReturnStmt {
			return t.return_stmt(node)
		}
		ast.ExprStmt {
			return ast.ExprStmt{
				expr: t.expr(node.expr)
			}
		}
		else {
			return node
		}
	}
}

fn (mut t Transformer) assign_stmt(node ast.AssignStmt) ast.Stmt {
	// Transform RHS expressions
	mut new_rhs := []ast.Expr{cap: node.rhs.len}
	for i, rhs_expr in node.rhs {
		transformed := t.expr(rhs_expr)
		new_rhs << transformed

		// Track variable types for type inference
		if node.op == .decl_assign && i < node.lhs.len {
			if node.lhs[i] is ast.Ident {
				var_name := (node.lhs[i] as ast.Ident).name
				inferred_type := t.infer_type(transformed)
				if inferred_type != '' {
					t.var_types[var_name] = inferred_type
				}
			}
		}
	}
	return ast.AssignStmt{
		op:  node.op
		lhs: node.lhs
		rhs: new_rhs
		pos: node.pos
	}
}

fn (mut t Transformer) fn_decl(node ast.FnDecl) ast.Stmt {
	mut new_stmts := []ast.Stmt{cap: node.stmts.len}
	for stmt in node.stmts {
		new_stmts << t.stmt(stmt)
	}
	return ast.FnDecl{
		attributes: node.attributes
		is_public:  node.is_public
		is_method:  node.is_method
		is_static:  node.is_static
		receiver:   node.receiver
		language:   node.language
		name:       node.name
		typ:        node.typ
		stmts:      new_stmts
		pos:        node.pos
	}
}

fn (mut t Transformer) for_stmt(node ast.ForStmt) ast.Stmt {
	mut new_stmts := []ast.Stmt{cap: node.stmts.len}
	for stmt in node.stmts {
		new_stmts << t.stmt(stmt)
	}
	return ast.ForStmt{
		init:  t.stmt(node.init)
		cond:  t.expr(node.cond)
		post:  t.stmt(node.post)
		stmts: new_stmts
	}
}

fn (mut t Transformer) block_stmt(node ast.BlockStmt) ast.Stmt {
	mut new_stmts := []ast.Stmt{cap: node.stmts.len}
	for stmt in node.stmts {
		new_stmts << t.stmt(stmt)
	}
	return ast.BlockStmt{
		stmts: new_stmts
	}
}

fn (mut t Transformer) return_stmt(node ast.ReturnStmt) ast.Stmt {
	mut new_exprs := []ast.Expr{cap: node.exprs.len}
	for expr in node.exprs {
		new_exprs << t.expr(expr)
	}
	return ast.ReturnStmt{
		exprs: new_exprs
	}
}

fn (mut t Transformer) expr(node ast.Expr) ast.Expr {
	match node {
		ast.ArrayInitExpr {
			return t.lower_array_init(node)
		}
		ast.StringInterLiteral {
			return t.lower_string_inter(node)
		}
		ast.InfixExpr {
			return t.infix_expr(node)
		}
		ast.PrefixExpr {
			return ast.PrefixExpr{
				op:   node.op
				expr: t.expr(node.expr)
				pos:  node.pos
			}
		}
		ast.CallExpr {
			return t.call_expr(node)
		}
		ast.IndexExpr {
			return ast.IndexExpr{
				lhs:      t.expr(node.lhs)
				expr:     t.expr(node.expr)
				is_gated: node.is_gated
			}
		}
		ast.IfExpr {
			return t.if_expr(node)
		}
		ast.IfGuardExpr {
			return t.if_guard_expr(node)
		}
		else {
			return node
		}
	}
}

fn (mut t Transformer) call_expr(node ast.CallExpr) ast.Expr {
	mut new_args := []ast.Expr{cap: node.args.len}
	for arg in node.args {
		new_args << t.expr(arg)
	}
	return ast.CallExpr{
		lhs:  node.lhs
		args: new_args
		pos:  node.pos
	}
}

fn (mut t Transformer) if_expr(node ast.IfExpr) ast.Expr {
	mut new_stmts := []ast.Stmt{cap: node.stmts.len}
	for stmt in node.stmts {
		new_stmts << t.stmt(stmt)
	}
	return ast.IfExpr{
		cond:      t.expr(node.cond)
		stmts:     new_stmts
		else_expr: t.expr(node.else_expr)
	}
}

fn (mut t Transformer) if_guard_expr(node ast.IfGuardExpr) ast.Expr {
	// Transform the RHS expressions in the guard's assignment statement
	mut new_rhs := []ast.Expr{cap: node.stmt.rhs.len}
	for rhs_expr in node.stmt.rhs {
		new_rhs << t.expr(rhs_expr)
	}
	return ast.IfGuardExpr{
		stmt: ast.AssignStmt{
			op:  node.stmt.op
			lhs: node.stmt.lhs
			rhs: new_rhs
			pos: node.stmt.pos
		}
	}
}

// lower_array_init transforms ArrayInitExpr into a lowered form.
// For literal arrays like [1, 2, 3], we keep the ArrayInitExpr but ensure
// all element expressions are transformed.
// The SSA builder handles the actual lowering to alloca + stores.
fn (mut t Transformer) lower_array_init(node ast.ArrayInitExpr) ast.Expr {
	// Transform all element expressions
	mut new_exprs := []ast.Expr{cap: node.exprs.len}
	for expr in node.exprs {
		new_exprs << t.expr(expr)
	}

	// Transform len/cap/init expressions if present
	new_len := t.expr(node.len)
	new_cap := t.expr(node.cap)
	new_init := t.expr(node.init)

	return ast.ArrayInitExpr{
		typ:   node.typ
		exprs: new_exprs
		init:  new_init
		cap:   new_cap
		len:   new_len
		pos:   node.pos
	}
}

// lower_string_inter transforms StringInterLiteral.
// For now, we just transform the interpolated expressions.
// The SSA builder handles the actual lowering to sprintf calls.
// Later, this can be transformed into strconv function calls.
fn (mut t Transformer) lower_string_inter(node ast.StringInterLiteral) ast.Expr {
	// Transform all interpolated expressions
	mut new_inters := []ast.StringInter{cap: node.inters.len}
	for inter in node.inters {
		new_inters << ast.StringInter{
			format:      inter.format
			width:       inter.width
			precision:   inter.precision
			expr:        t.expr(inter.expr)
			format_expr: t.expr(inter.format_expr)
		}
	}
	return ast.StringInterLiteral{
		kind:   node.kind
		values: node.values
		inters: new_inters
	}
}

// infix_expr handles binary operations, transforming string concatenation
// into method calls: `a + b` -> `a.+(b)` which becomes `string__plus(a, b)`
fn (mut t Transformer) infix_expr(node ast.InfixExpr) ast.Expr {
	lhs := t.expr(node.lhs)
	rhs := t.expr(node.rhs)

	// Check if this is string concatenation
	if node.op == .plus {
		lhs_type := t.infer_type(lhs)
		rhs_type := t.infer_type(rhs)
		if lhs_type == 'string' && rhs_type == 'string' {
			// Transform `a + b` into method call syntax
			// This will be compiled as string__plus(a, b)
			return ast.CallExpr{
				lhs:  ast.SelectorExpr{
					lhs: lhs
					rhs: ast.Ident{
						name: '+'
					}
				}
				args: [rhs]
				pos:  node.pos
			}
		}
	}

	// Default: keep as infix expression
	return ast.InfixExpr{
		op:  node.op
		lhs: lhs
		rhs: rhs
		pos: node.pos
	}
}

// infer_type attempts to determine the type of an expression
// Returns the type name or '' if unknown
fn (t &Transformer) infer_type(node ast.Expr) string {
	match node {
		ast.StringLiteral {
			return 'string'
		}
		ast.StringInterLiteral {
			return 'string'
		}
		ast.BasicLiteral {
			if node.kind == .number {
				if node.value.contains('.') || node.value.contains('e') || node.value.contains('E') {
					return 'f64'
				}
				return 'int'
			}
			if node.kind in [.key_true, .key_false] {
				return 'bool'
			}
		}
		ast.Ident {
			// Look up tracked variable type
			if typ := t.var_types[node.name] {
				return typ
			}
		}
		ast.CallExpr {
			// Method call on string returns string for + operator
			if node.lhs is ast.SelectorExpr {
				sel := node.lhs as ast.SelectorExpr
				if sel.rhs.name == '+' && t.infer_type(sel.lhs) == 'string' {
					return 'string'
				}
			}
		}
		ast.InfixExpr {
			// String + string = string
			if node.op == .plus {
				if t.infer_type(node.lhs) == 'string' && t.infer_type(node.rhs) == 'string' {
					return 'string'
				}
			}
		}
		else {}
	}
	return ''
}
