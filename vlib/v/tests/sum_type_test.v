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
			assert it.val == '12'
			// assert e.val == '12' // TODO
			return 'int'
		}
		IfExpr {
			return 'if'
		}
	}
	return ''
}

fn test_expr() {
	expr := IntegerLiteral{
		val: '12'
	}
	assert handle(expr) == 'int'
	// assert expr is IntegerLiteral // TODO
}

fn test_assignment_and_push() {
	mut expr1 := Expr{}
	mut arr1 := []Expr{}
	expr := IntegerLiteral{
        val: '111'
    }
	arr1 << expr
	match arr1[0] {
		IntegerLiteral {
			arr1 << it
			// should ref/dereference on assignent be made automatic?
			// currently it is done for return stmt and fn args
			expr1 = *it
		}
		else {}
	}
}
