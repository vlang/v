module main

type AstNode = BoolCondition | Expression

struct Expression {
	value int
}

struct BoolCondition {
mut:
	con_simple &AstNode
	is_simple  bool
}

fn parse_expression() Expression {
	return Expression{
		value: 1
	}
}

fn parse_simple_boolean_expr() BoolCondition {
	mut value := AstNode(parse_expression() as Expression)
	return BoolCondition{ con_simple: &value, is_simple: true }
}

fn test_sumtype_as_cast_of_fn_call() {
	result := parse_simple_boolean_expr()
	assert result.is_simple
	assert (result.con_simple as Expression).value == 1
}
