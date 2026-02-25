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
	// Save runtime_local_types: variables declared in the loop body
	// (e.g. _filter_it in nested map/filter) can shadow outer variables.
	// Without save/restore, the inner type overwrites the outer in the flat map.
	saved_local_types := g.runtime_local_types.clone()
	g.gen_stmts(node.stmts)
	g.runtime_local_types = saved_local_types.clone()
	g.not_local_var_cache = map[string]bool{}
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
				// Register the for-loop init variable so assignments inside
				// the loop body don't re-declare it.
				if name != '' && typ != '' {
					g.runtime_local_types[name] = typ
				}
			} else {
				g.expr(lhs)
				op_str := match node.op {
					.assign { '=' }
					.plus_assign { '+=' }
					.minus_assign { '-=' }
					.mul_assign { '*=' }
					.div_assign { '/=' }
					.mod_assign { '%=' }
					.and_assign { '&=' }
					.or_assign { '|=' }
					.xor_assign { '^=' }
					.left_shift_assign { '<<=' }
					.right_shift_assign { '>>=' }
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
