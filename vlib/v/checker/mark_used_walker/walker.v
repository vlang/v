// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module mark_used_walker

// This module walks the entire program starting at fn main and marks used (called) functions.
// Unused functions can be safely skipped by the backends to save CPU time and space.
import v.ast

pub struct Walker {
pub mut:
	used_fns map[string]bool // used_fns['println'] == true
mut:
	files []ast.File
}

/*
fn (mut w Walker) walk_files(ast_files []ast.File) {
		t := time.ticks()
*/

pub fn (mut w Walker) stmt(node ast.Stmt) {
	match mut node {
		ast.AssignStmt {
			for l in node.left {
				w.expr(l)
			}
			for r in node.right {
				w.expr(r)
			}
		}
		ast.ExprStmt {
			w.expr(node.expr)
		}
		ast.FnDecl {
			w.fn_decl(mut node)
		}
		ast.ForStmt {
			w.expr(node.cond)
			for stmt in node.stmts {
				w.stmt(stmt)
			}
		}
		else {}
	}
}

fn (mut w Walker) expr(node ast.Expr) {
	match mut node {
		ast.CallExpr {
			w.call_expr(mut node)
		}
		ast.GoExpr {
			w.expr(node.go_stmt.call_expr)
		}
		ast.IndexExpr {
			w.expr(node.left)
			w.expr(node.index)
		}
		ast.IfExpr {
			for b in node.branches {
				w.expr(b.cond)
				for stmt in b.stmts {
					w.stmt(stmt)
				}
			}
		}
		ast.MatchExpr {
			w.expr(node.cond)
			for b in node.branches {
				for expr in b.exprs {
					w.expr(expr)
				}
				for stmt in b.stmts {
					w.stmt(stmt)
				}
			}
		}
		else {}
	}
}

pub fn (mut w Walker) fn_decl(mut node ast.FnDecl) {
	fkey := if node.is_method { '${int(node.receiver.typ)}.$node.name' } else { node.name }
	if w.used_fns[fkey] {
		// This function is already known to be called, meaning it has been processed already.
		// Save CPU time and do nothing.
		return
	}
	if node.language == .c {
		return
	}
	w.used_fns[fkey] = true
	for stmt in node.stmts {
		w.stmt(stmt)
	}
}

pub fn (mut w Walker) call_expr(mut node ast.CallExpr) {
	fn_name := if node.is_method { node.receiver_type.str() + '.' + node.name } else { node.name }
	// fn_name := node.name
	// println('call_expr $fn_name')
	// if node.is_method {
	// println('M $node.name $node.receiver_type')
	//}
	if w.used_fns[fn_name] {
		return
	}
	// w.used_fns[fn_name] = true
	// Find the FnDecl for this CallExpr, mark the function as used, and process
	// all its statements.
	loop: for file in w.files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl {
				if stmt.name == node.name
					&& (!node.is_method || (node.receiver_type == stmt.receiver.typ)) {
					w.used_fns[fn_name] = true
					for fn_stmt in stmt.stmts {
						w.stmt(fn_stmt)
					}
					break loop
				}
			}
		}
	}
}
