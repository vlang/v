// An operand that hoists statements (`f()!`, `f()?`, `f() or {}`, value `if`/`match`)
// next to a negated float literal used to spill the literal into an f64 temp, so
// `res_f32()! == -1.2` compared the f32 against the f64 value of `-1.2`.

fn res_f32() !f32 {
	return f32(-1.2)
}

fn opt_f32() ?f32 {
	return f32(-1.2)
}

fn decode[T](v f32) !T {
	return T(v)
}

fn test_result_and_option_operands() {
	assert res_f32()! == -1.2
	assert -1.2 == res_f32()!
	assert opt_f32()? == -1.2
	assert (res_f32() or { 0 }) == -1.2
	assert decode[f32](-1.2)! == -1.2
	assert !(res_f32()! != -1.2)
}

fn test_ordering_operands() {
	assert !(res_f32()! < -1.2)
	assert res_f32()! >= -1.2
	assert -1.2 >= res_f32()!
}

fn test_value_branch_operands() {
	c := f32(-1.2)
	cond := c < 0
	assert (if cond { c } else { f32(0) }) == -1.2
	assert -1.2 == (if cond { c } else { f32(0) })
	assert (match cond {
		true { c }
		else { f32(0) }
	}) == -1.2
}

fn test_assigned_comparison() {
	ok := res_f32()! == -1.2
	assert ok
}
