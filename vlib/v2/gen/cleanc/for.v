// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module cleanc

import v2.ast

fn (mut g Gen) gen_for_stmt(node ast.ForStmt) {
	g.write_indent()
	has_init := !is_empty_stmt(node.init)
	has_cond := !is_empty_expr(node.cond)
	has_post := !is_empty_stmt(node.post)

	if has_init || has_post {
		// C-style for loop: for (init; cond; post)
		g.sb.write_string('for (')
		if has_init {
			g.gen_stmt_inline(node.init)
		}
		g.sb.write_string('; ')
		if has_cond {
			g.expr(node.cond)
		}
		g.sb.write_string('; ')
		if has_post {
			g.gen_stmt_inline(node.post)
		}
		g.sb.writeln(') {')
	} else if has_cond {
		// while-style: for cond {
		g.sb.write_string('while (')
		g.expr(node.cond)
		g.sb.writeln(') {')
	} else {
		// Infinite loop: for {
		g.sb.writeln('for (;;) {')
	}

	g.indent++
	g.gen_stmts(node.stmts)
	g.indent--
	g.write_indent()
	g.sb.writeln('}')
}

fn (mut g Gen) gen_stmt_inline(node ast.Stmt) {
	match node {
		ast.AssignStmt {
			lhs := node.lhs[0]
			rhs := node.rhs[0]
			if node.op == .decl_assign {
				mut name := ''
				if lhs is ast.Ident {
					name = lhs.name
				}
				typ := g.get_expr_type(rhs)
				g.sb.write_string('${typ} ${name} = ')
				g.expr(rhs)
			} else {
				g.expr(lhs)
				op_str := match node.op {
					.assign { '=' }
					.plus_assign { '+=' }
					.minus_assign { '-=' }
					else { '=' }
				}
				g.sb.write_string(' ${op_str} ')
				g.expr(rhs)
			}
		}
		ast.ExprStmt {
			g.expr(node.expr)
		}
		else {}
	}
}
