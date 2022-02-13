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

struct LeadingZeros {
	l     unsigned.Uint128
	r     unsigned.Uint128
	zeros int
}

fn new(x u64, y u64) unsigned.Uint128 {
	return unsigned.Uint128{x, y}
}

fn test_leading_zeros() {
	tcs := [
		LeadingZeros{
			l: new(0x00, 0xf000000000000000)
			r: new(0x00, 0x8000000000000000)
			zeros: 1
		},
		LeadingZeros{
			l: new(0x00, 0xf000000000000000)
			r: new(0x00, 0xc000000000000000)
			zeros: 2
		},
		LeadingZeros{
			l: new(0x00, 0xf000000000000000)
			r: new(0x00, 0xe000000000000000)
			zeros: 3
		},
		LeadingZeros{
			l: new(0x00, 0xffff000000000000)
			r: new(0x00, 0xff00000000000000)
			zeros: 8
		},
		LeadingZeros{
			l: new(0x00, 0x000000000000ffff)
			r: new(0x00, 0x000000000000ff00)
			zeros: 56
		},
		LeadingZeros{
			l: new(0xf000000000000000, 0x01)
			r: new(0x4000000000000000, 0x00)
			zeros: 63
		},
		LeadingZeros{
			l: new(0xf000000000000000, 0x00)
			r: new(0x4000000000000000, 0x00)
			zeros: 64
		},
		LeadingZeros{
			l: new(0xf000000000000000, 0x00)
			r: new(0x8000000000000000, 0x00)
			zeros: 65
		},
		LeadingZeros{
			l: new(0x00, 0x00)
			r: new(0x00, 0x00)
			zeros: 128
		},
		LeadingZeros{
			l: new(0x01, 0x00)
			r: new(0x00, 0x00)
			zeros: 127
		},
	]

	for tc in tcs {
		zeros := tc.l.xor(tc.r).leading_zeros()
		assert zeros == tc.zeros
	}
}
