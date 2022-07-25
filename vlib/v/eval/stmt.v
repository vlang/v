module eval

import v.ast
import v.token

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
			// if stmt.left.len != 1 {
			// 	e.error('multiple values assignments are not supported')
			// }
			mut rights := []Object{}
			for i, right in stmt.right {
				rights << e.expr(right, stmt.right_types[i])
			}
			if rights[0] is []Object { // needs to be unpacked
				e.error('multiple assignment from function is not supported')
			}
			match stmt.op {
				.decl_assign {
					for i, left in stmt.left {
						e.set(left, rights[i], true, stmt.left_types[i])
					}
				}
				.assign {
					for i, left in stmt.left {
						e.set(left, rights[i], false, stmt.left_types[i])
					}
				}
				.plus_assign, .minus_assign, .mult_assign, .div_assign, .xor_assign, .mod_assign,
				.or_assign, .and_assign, .right_shift_assign, .unsigned_right_shift_assign,
				.left_shift_assign {
					infix_op := token.assign_op_to_infix_op(stmt.op)
					for i, left in stmt.left {
						res := e.infix_expr(e.expr(left, stmt.left_types[i]), rights[i],
							infix_op, stmt.left_types[i])
						e.set(left, res, false, stmt.left_types[i])
					}
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
			underscore := stmt.val_var == '_'
			if !underscore {
				e.set(ast.Ident{ name: stmt.val_var, scope: 0 }, Int{-1, 32}, true, stmt.val_type)
			}
			fstart := e.expr(stmt.cond, ast.int_type_idx).as_i64() or {
				e.error('invalid integer for start of range')
				0
			}
			fend := e.expr(stmt.high, ast.int_type_idx).as_i64() or {
				e.error('invalid integer for end of range')
				0
			}
			for i in fstart .. fend {
				if !underscore {
					e.set(ast.Ident{ name: stmt.val_var, scope: 0 }, Int{i, 32}, false,
						stmt.val_type)
				}
				e.stmts(stmt.stmts)
			}
			e.close_scope()
		}
		ast.ForStmt {
			for {
				should_break := e.expr(stmt.cond, ast.bool_type_idx)
				if !(should_break as bool) {
					break
				}
				e.stmts(stmt.stmts)
			}
		}
		ast.Block {
			e.stmts(stmt.stmts)
		}
		else {
			e.error('unhandled statement $stmt.type_name()')
		}
	}
}
