import math
import v.ast
import v.pref
import v.parser
import v.checker
import v.transformer

fn test_const_par_expr_and_infix_expr() {
	println(@LOCATION)
	source_text := '
const k = 2
fn main() {
	x := [(k+1)-2+(2*(k))]int{}
}
'
	mut table := ast.new_table()
	vpref := &pref.Preferences{}
	mut prog := parser.parse_text(source_text, '', mut table, .skip_comments, vpref)
	mut checker_ := checker.new_checker(table, vpref)
	checker_.check(mut prog)
	mut t := transformer.new_transformer_with_table(table, vpref)

	// get the `InfixExpr`(`(k+1)-2+(2*(k))`) from table
	main_fn := table.cur_fn
	assign_stmt := main_fn.stmts[0] as ast.AssignStmt
	array_init_expr := assign_stmt.right[0] as ast.ArrayInit
	mut dim_expr := array_init_expr.exprs[0] as ast.InfixExpr
	dump(dim_expr)

	// verify `infix_expr` and `par_expr` work as expected
	folded_expr := t.infix_expr(mut dim_expr)
	dump(folded_expr)
	assert '${folded_expr}' == '5'
}

fn test_float_infix_expr_keeps_roundtrip_precision() {
	println(@LOCATION)
	source_text := '
fn main() {
	x := 9.765625000000004e-04 + 0.0
}
'
	mut table := ast.new_table()
	vpref := &pref.Preferences{}
	mut prog := parser.parse_text(source_text, '', mut table, .skip_comments, vpref)
	mut checker_ := checker.new_checker(table, vpref)
	checker_.check(mut prog)
	mut t := transformer.new_transformer_with_table(table, vpref)

	main_fn := table.cur_fn
	assign_stmt := main_fn.stmts[0] as ast.AssignStmt
	mut infix_expr := assign_stmt.right[0] as ast.InfixExpr
	folded_expr := t.infix_expr(mut infix_expr)

	assert folded_expr is ast.FloatLiteral
	literal := folded_expr as ast.FloatLiteral
	assert math.f64_bits(literal.val.f64()) == math.f64_bits('9.765625000000004e-04'.f64())
}
