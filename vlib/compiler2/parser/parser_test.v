module parser

import (
	compiler2.ast
	compiler2.cgen
	compiler2.table

)

fn test_parser() {
	//if true { return }
	//expr := ast.IntegerExpr {val:10}
	//expr := ast.BinaryExpr{}

	// print using walk
	//expr := parse_expr('3 + 7')
	//println('\n')

	text_expr := [
		'1 += 2',
		'1.2 + 3.4',
		'4 + 4',
		'1 + 2 * 5',
		'(2 * 3) / 2',
		'3 + (7 * 6)',
		'2 ^ 8 * (7 * 6)',
		'20 + (10 * 15) / 5', // 50
		'(2) + (17*2-30) * (5)+2 - (8/2)*4', // 8
		'2 + "hi"',
		'x := 10'
	]

	table := &table.Table{}
	for s in text_expr {
		// print using str method
		x := parse_expr(s, table)
		println('source: $s')
		println('parsed: $x')
		println('===================')
	}
}
/*
fn test_cgen2() {
	s := '2 + 3
	5+7
//x := 100
'
	table := &table.Table{}
	prog := parse_file(s, table)
	cgen.gen(prog)
	println('done')
}
*/


fn test_cgen() {
	//if true { return }
	s := [
		'x := 10',
		//'x := 10'
	]
	//expr := parse_expr('3 + 7 * 2')
	//expr2 := parse_stmt('a := 3 + "f"')
	mut e := []ast.Expr
	table := &table.Table{}
	for ss in s {
	//expr2 := parse_expr('x := 10')
	//program := ast.Program{
		e << parse_expr(ss, table)
		//exprs: [
			//expr2,
			//parse_expr('2 * 2'),
		//]
	}
	program := ast.Program{exprs:e}
	cgen.gen(program)
	//cgen.save()
}

