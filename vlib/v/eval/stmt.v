module eval

import v.ast

pub fn (mut e Eval) stmts(stmts []ast.Stmt) {
	e.open_scope()
	for stmt in stmts {
		e.stmt(stmt)
		if e.returning {
			break
		}
	}
	e.close_scope()
}

pub fn (mut e Eval) stmt(stmt ast.Stmt) {
	match stmt {
		ast.ExprStmt {
			e.expr(stmt.expr, stmt.typ)
		}
		ast.AssignStmt {
			if stmt.left.len != 1 {
				e.error('multiple values assignments are not supported')
			}
			right_expr := stmt.right[0]
			mut right := e.expr(right_expr, stmt.right_types[0])
			if right_expr is ast.CallExpr { // right is []Object and needs to be unpacked
				right = (right as []Object)[0]
			}
			match stmt.op {
				.decl_assign {
					e.set(stmt.left[0], right, true)
				}
				.assign {
					e.set(stmt.left[0], right, false)
				}
				else {
					e.error('unknown assign statment: $stmt.op')
				}
			}
		}
		ast.Return {
			e.returning = true
			e.return_values = []
			for i, expr in stmt.exprs {
				e.return_values << e.expr(expr, stmt.types[i])
			}
		}
		ast.ForInStmt {
			if !stmt.is_range {
				e.error('only range for in statements are supported')
			}
			if stmt.key_var != '' {
				e.error('keys are not supported in for in statements')
			}
			e.open_scope()
			e.set(ast.Ident{ name: stmt.val_var, scope: 0 }, Int{-1, 32}, true)
			for i in (e.expr(stmt.cond, stmt.cond_type) as Int).val .. (e.expr(stmt.high,
				ast.int_type_idx) as Int).val {
				e.set(ast.Ident{ name: stmt.val_var, scope: 0 }, Int{i, 32}, false)
				e.stmts(stmt.stmts)
			}
			e.close_scope()
		}
		ast.Block {
			e.stmts(stmt.stmts)
		}
		else {
			e.error('unhandled statement $stmt.type_name()')
		}
	}
}
