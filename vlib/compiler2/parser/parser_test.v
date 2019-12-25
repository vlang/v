module parser

import compiler2.ast

fn test_parser() {
	//expr := ast.IntegerExpr {val:10}
	//expr := ast.BinaryExpr{}
	text_expr := [
		'4 + 4',
		'1 + 2 * 5',
		'(2 * 3) / 2',
		'3 + (7 * 6)'
	]
	for expr in text_expr {
		// print using walk
		// expr := parse_expr('3 + 7')
		// walk(expr)
		// println('')
		
		// print using str methods
		x := parse_expr(expr)
		println('source: $expr')
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