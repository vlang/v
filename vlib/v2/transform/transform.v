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
	for rhs_expr in node.rhs {
		new_rhs << t.expr(rhs_expr)
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
			return ast.InfixExpr{
				op:  node.op
				lhs: t.expr(node.lhs)
				rhs: t.expr(node.rhs)
				pos: node.pos
			}
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
