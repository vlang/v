import math.unsigned

fn test_str() {
	x := unsigned.uint128_from_dec_str('170141183460469231713240559642174554112') or { panic('') }
	assert x.str() == '170141183460469231713240559642174554112'
	y := unsigned.uint128_from_64(42)
	assert y.str() == '42'
}

fn test_ops() {
	x := unsigned.uint128_from_64(18446744073709551615)
	y := unsigned.uint128_from_64(18446744073709551615)
	z := unsigned.uint128_from_dec_str('340282366920938463426481119284349108225') or {
		assert false
		panic('')
	}
	assert (x * y).str() == '340282366920938463426481119284349108225'
	assert (x + y).str() == '36893488147419103230'
	assert (z / unsigned.uint128_from_64(2)).str() == '170141183460469231713240559642174554112'
	assert (unsigned.uint128_from_dec_str('170141183460469231713240559642174554112') or {
		panic('')
	} - unsigned.uint128_from_64(2)).str() == '170141183460469231713240559642174554110'

	assert x == y
	assert unsigned.uint128_from_dec_str('340282366920938463426481119284349108225') or {
		assert false
		panic('')
	} > y
}
