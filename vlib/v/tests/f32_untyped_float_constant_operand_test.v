// An untyped float constant compared with or combined with an f32 is converted to f32 as a
// whole, as V 0.5.2 does by folding it. Parentheses and constant arithmetic used to leave it
// a C double, so `f32(1.2) == (1.2)` and `f32(1.3) == 1.2 + 0.1` were false.

fn res_f32() !f32 {
	return f32(-1.2)
}

fn test_parenthesized_literals() {
	c := f32(-1.2)
	p := f32(1.2)
	assert p == (1.2)
	assert (1.2) == p
	assert c == (-1.2)
	assert c == -(1.2)
	assert c == (-(1.2))
	assert (-1.2) == c
	assert !(p != (1.2))
	assert !(c < (-1.2))
	assert c >= (-1.2)
	assert res_f32()! == (-1.2)
	assert res_f32()! == -(1.2)
}

fn test_constant_arithmetic() {
	q := f32(1.3)
	assert q == 1.2 + 0.1
	assert q == (1.2 + 0.1)
	assert 1.2 + 0.1 == q
	assert q == 2.6 / 2
	assert !(q != 1.2 + 0.1)
	assert f32(2.4) == 1.2 * 2
	assert f32(-1.2) == -(0.6 + 0.6)
}

fn test_arithmetic_operands() {
	c := f32(-1.2)
	p := f32(1.2)
	assert c + (1.2) == 0
	assert p * (2.0) == 2.4
	assert p - (0.6 + 0.6) == 0
}
