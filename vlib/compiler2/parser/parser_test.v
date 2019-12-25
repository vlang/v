module parser

import compiler2.ast

fn test_parser() {
	//expr := ast.IntegerExpr {val:10}
	//expr := ast.BinaryExpr{}

	// print using walk
	expr := parse_expr('3 + 7')
	walk(expr)
	println('\n')
	
	text_expr := [
		'1.2 + 3.4',
		'4 + 4',
		'1 + 2 * 5',
		'(2 * 3) / 2',
		'3 + (7 * 6)',
		'2 ^ 8 * (7 * 6)',
		'(2) + (17*2-30) * (5)+2 - (8/2)*4'
	]
	for s in text_expr {
		// print using str method
		x := parse_expr(s)
		println('source: $s')
		println('parsed: $x')
		println('===================')
	}
}


fn walk(node ast.Expr) {
	//println('walk()')
	match node {
		ast.BinaryExpr {
			print(' (')
			walk(it.left)
			// print('$it.op.str()')
			match it.op {
				.plus {
					print(' + ')
				}
				.minus {
					print(' - ')
				}
				else {}

			}
			walk(it.right)
			print(') ')
		}
		ast.ScalarExpr {
			walk(it.left)
			print(' $it.val ')
		}
		ast.UnaryExpr {
			walk(it.left)
			print(' $it.op ')
		}
		else { }
	}
}