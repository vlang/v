// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ast

import v2.token

fn string_inter_test_expr() Expr {
	return Expr(Ident{
		name: 'value'
	})
}

fn string_inter_test_literal(width int, precision int) Expr {
	return Expr(StringInterLiteral{
		kind:   .v
		values: ['value=', '']
		inters: [
			StringInter{
				format:       .decimal
				width:        width
				precision:    precision
				expr:         string_inter_test_expr()
				format_expr:  empty_expr
				resolved_fmt: '%d'
			},
		]
	})
}

fn assert_string_inter_roundtrip(files []File, width int, precision int) {
	assert files.len == 1
	assert files[0].stmts.len == 2
	assert files[0].stmts[1] is ExprStmt
	expr_stmt := files[0].stmts[1] as ExprStmt
	assert expr_stmt.expr is StringInterLiteral
	lit := expr_stmt.expr as StringInterLiteral
	assert lit.inters.len == 1
	assert lit.inters[0].width == width
	assert lit.inters[0].precision == precision
}

fn test_string_inter_width_precision_survive_flat_roundtrip_losslessly() {
	width := 1_000_000
	precision := -1_000_000
	mut b := new_flat_builder()
	b.append_file(File{
		name:  'string_inter.v'
		mod:   'string_inter'
		stmts: [
			Stmt(ModuleStmt{
				name: 'string_inter'
			}),
			Stmt(ExprStmt{
				expr: string_inter_test_literal(width, precision)
			}),
		]
	})
	assert_string_inter_roundtrip(b.flat.to_files(), width, precision)
}

fn test_emit_string_inter_by_ids_preserves_large_width_precision() {
	width := 900_000
	precision := -900_000
	mut b := new_flat_builder()
	b.append_file(File{
		name:  'string_inter_direct.v'
		mod:   'string_inter'
		stmts: [
			Stmt(ModuleStmt{
				name: 'string_inter'
			}),
		]
	})
	expr_id := b.emit_expr(string_inter_test_expr())
	format_expr_id := b.emit_expr(empty_expr)
	inter_id :=
		b.emit_string_inter_by_ids(.decimal, width, precision, expr_id, format_expr_id, '%d')
	lit_id := b.emit_string_inter_literal_by_ids(.v, ['value=', ''], [inter_id], token.Pos{})
	stmt_id := b.emit_expr_stmt_by_id(lit_id)
	b.append_file_stmts(0, [stmt_id])
	assert_string_inter_roundtrip(b.flat.to_files(), width, precision)
}
