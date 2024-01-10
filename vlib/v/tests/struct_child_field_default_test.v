struct Position {
	line_nr int = 1
}

struct Statement {
	pos Position
}

struct Expression {
	pos Position
}

fn test_child_struct_field_default() {
	stmt := Statement{
		pos: Position{}
	}
	expr := Expression{}
	println(stmt)
	println(expr)
	assert stmt.pos.line_nr == 1
	assert expr.pos.line_nr == 1
}
