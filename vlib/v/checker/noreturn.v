module checker

import v.ast

fn (mut c Checker) check_noreturn_fn_decl(mut node ast.FnDecl) {
	if !node.is_noreturn {
		return
	}
	if node.no_body {
		return
	}
	if node.return_type != ast.void_type {
		c.error('[noreturn] functions cannot have return types', node.pos)
	}
	if uses_return_stmt(node.stmts) {
		c.error('[noreturn] functions cannot use return statements', node.pos)
	}
	if node.stmts.len != 0 {
		mut is_valid_end_of_noreturn_fn := false
		last_stmt := node.stmts.last()
		match last_stmt {
			ast.ExprStmt {
				if last_stmt.expr is ast.CallExpr {
					if last_stmt.expr.should_be_skipped {
						c.error('[noreturn] functions cannot end with a skippable `[if ..]` call',
							last_stmt.pos)
						return
					}
					if last_stmt.expr.is_noreturn {
						is_valid_end_of_noreturn_fn = true
					}
				}
			}
			ast.ForStmt {
				if last_stmt.is_inf && last_stmt.stmts.len == 0 {
					is_valid_end_of_noreturn_fn = true
				}
			}
			else {}
		}
		if !is_valid_end_of_noreturn_fn {
			c.error('[noreturn] functions should end with a call to another [noreturn] function, or with an infinite `for {}` loop',
				last_stmt.pos)
			return
		}
	}
}

fn uses_return_stmt(stmts []ast.Stmt) bool {
	if stmts.len == 0 {
		return false
	}
	for stmt in stmts {
		match stmt {
			ast.Return {
				return true
			}
			ast.Block {
				if uses_return_stmt(stmt.stmts) {
					return true
				}
			}
			ast.ExprStmt {
				match stmt.expr {
					ast.CallExpr {
						if uses_return_stmt(stmt.expr.or_block.stmts) {
							return true
						}
					}
					ast.MatchExpr {
						for b in stmt.expr.branches {
							if uses_return_stmt(b.stmts) {
								return true
							}
						}
					}
					ast.SelectExpr {
						for b in stmt.expr.branches {
							if uses_return_stmt(b.stmts) {
								return true
							}
						}
					}
					ast.IfExpr {
						for b in stmt.expr.branches {
							if uses_return_stmt(b.stmts) {
								return true
							}
						}
					}
					else {}
				}
			}
			ast.ForStmt {
				if uses_return_stmt(stmt.stmts) {
					return true
				}
			}
			ast.ForCStmt {
				if uses_return_stmt(stmt.stmts) {
					return true
				}
			}
			ast.ForInStmt {
				if uses_return_stmt(stmt.stmts) {
					return true
				}
			}
			else {}
		}
	}
	return false
}
