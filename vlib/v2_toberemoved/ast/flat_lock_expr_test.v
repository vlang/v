// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ast

fn lock_expr_ident(name string) Expr {
	return Expr(Ident{
		name: name
	})
}

fn lock_expr_stmt(value string) Stmt {
	return Stmt(ExprStmt{
		expr: Expr(BasicLiteral{
			kind:  .number
			value: value
		})
	})
}

fn test_lock_expr_counts_survive_flat_roundtrip() {
	mut b := new_flat_builder()
	b.append_file(File{
		name:  'lock_expr.v'
		mod:   'lock_expr'
		stmts: [
			Stmt(ModuleStmt{
				name: 'lock_expr'
			}),
			Stmt(ExprStmt{
				expr: Expr(LockExpr{
					lock_exprs:  [lock_expr_ident('a'), lock_expr_ident('b')]
					rlock_exprs: [lock_expr_ident('c')]
					stmts:       [lock_expr_stmt('1'), lock_expr_stmt('2')]
				})
			}),
		]
	})
	files := b.flat.to_files()
	assert files.len == 1
	assert files[0].stmts.len == 2
	assert files[0].stmts[1] is ExprStmt
	expr_stmt := files[0].stmts[1] as ExprStmt
	assert expr_stmt.expr is LockExpr
	lock_expr := expr_stmt.expr as LockExpr
	assert lock_expr.lock_exprs.len == 2
	assert lock_expr.rlock_exprs.len == 1
	assert lock_expr.stmts.len == 2
}
