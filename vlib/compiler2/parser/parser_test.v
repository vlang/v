module parser

import (
	compiler2.ast
	compiler2.cgen
)

fn test_parser() {
	//if true { return }
	//expr := ast.IntegerExpr {val:10}
	//expr := ast.BinaryExpr{}

	// print using walk
	expr := parse_expr('3 + 7')
	println('\n')

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
		'2 + "hi"'
	]

	for s in text_expr {
		// print using str method
		x := parse_expr(s)
		println('source: $s')
		println('parsed: $x')
		println('===================')
	}
}


fn test_cgen() {
	//expr := parse_expr('3 + 7 * 2')
	//expr2 := parse_stmt('a := 3 + "f"')
	expr2 := parse_expr('2 +3 ')//"helo"')
	program := ast.Program{
		exprs: [
			expr2,
			//parse_expr('2 * 2'),
		]
	}
	cgen.gen(program)
	//cgen.save()
}

