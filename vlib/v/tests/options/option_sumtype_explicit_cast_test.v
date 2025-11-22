type SumType = u16 | u32

fn wrap[T](x T) ?T {
	return ?T(x)
}

fn test_sumtype_explicit_to_option_sumtype() {
	value := SumType(u16(0))
	a := ?SumType(value)
	assert a != none
	assert a? == SumType(u16(0))
} 

fn test_sumtype_explicit_to_option_sumtype_generic() {
	value := SumType(u16(1))
	a := wrap[SumType](value)
	assert a != none
	assert a? == SumType(u16(1))
}
