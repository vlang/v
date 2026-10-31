// A negated right operand after a binary minus, as accepted by V 0.5.2. The parser used to
// reject `5 - -a` with "invalid expression: unexpected token `-`".

const negated_literal = 5 - -3
const spaced_negated_literal = 5 - -3

fn test_negated_right_operand() {
	a := 5
	b := 3
	c := f32(-1.2)
	assert 5 - -a == 10
	assert c - -1.2 == 0
	assert a - -b == 8
	assert a - (-b) == 8
	assert -a - -b == -2
	assert a - -b - -b == 11
	assert '${a - -b} ${c - -1.2}' == '8 0.0'
	assert negated_literal == 8
	assert spaced_negated_literal == 8
}

fn test_postfix_decrement_is_unchanged() {
	mut x := 10
	y := 2
	x--
	assert x - y == 7
	assert x - -y == 11
}
