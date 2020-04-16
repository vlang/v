type Expr = IfExpr | IntegerLiteral

struct IfExpr {
	pos int
}

struct IntegerLiteral {
	val string
}

fn handle(e Expr) string {
	assert e is IntegerLiteral
	if e is IntegerLiteral {
		println('int')
	}
	match e {
		IntegerLiteral {
			return 'int'
		}
		IfExpr {
			return 'if'
		}
	}
	return ''
}

fn test_expr() {
	expr := IntegerLiteral{'12'}
	assert handle(expr) == 'int'
	// assert expr is IntegerLiteral
}
