
fn test_if_expression_precedence_false_condition(){
	b := 10
	c := 20
	res := 1 + if b > c { b } else { c } + 1
	assert res == c + 2
}

fn test_if_expression_precedence_true_condition(){
	b := 20
	c := 10
	res := 1 + if b > c { b } else { c } + 1
	assert res == b + 2
}
