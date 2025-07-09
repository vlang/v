import math.big
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
			l:     new(0x00, 0xf000000000000000)
			r:     new(0x00, 0x8000000000000000)
			zeros: 1
		},
		LeadingZeros{
			l:     new(0x00, 0xf000000000000000)
			r:     new(0x00, 0xc000000000000000)
			zeros: 2
		},
		LeadingZeros{
			l:     new(0x00, 0xf000000000000000)
			r:     new(0x00, 0xe000000000000000)
			zeros: 3
		},
		LeadingZeros{
			l:     new(0x00, 0xffff000000000000)
			r:     new(0x00, 0xff00000000000000)
			zeros: 8
		},
		LeadingZeros{
			l:     new(0x00, 0x000000000000ffff)
			r:     new(0x00, 0x000000000000ff00)
			zeros: 56
		},
		LeadingZeros{
			l:     new(0xf000000000000000, 0x01)
			r:     new(0x4000000000000000, 0x00)
			zeros: 63
		},
		LeadingZeros{
			l:     new(0xf000000000000000, 0x00)
			r:     new(0x4000000000000000, 0x00)
			zeros: 64
		},
		LeadingZeros{
			l:     new(0xf000000000000000, 0x00)
			r:     new(0x8000000000000000, 0x00)
			zeros: 65
		},
		LeadingZeros{
			l:     new(0x00, 0x00)
			r:     new(0x00, 0x00)
			zeros: 128
		},
		LeadingZeros{
			l:     new(0x01, 0x00)
			r:     new(0x00, 0x00)
			zeros: 127
		},
	]

	for tc in tcs {
		zeros := tc.l.xor(tc.r).leading_zeros()
		assert zeros == tc.zeros
	}
}

fn test_separators() {
	// numbers of varying lengths and a random
	// scattering of '_' throughout.
	test_strings := [
		'_',
		'__',
		'_0',
		'0_',
		'_0_',
		'_1',
		'1_',
		'_1_',
		'1_2',
		'_12',
		'12_',
		'12_3',
		'1_23_4',
		'12_345',
		'_123_456_',
		'1_234_567',
		'1234_5678',
		'_123456789',
		'1234567890_',
		'0_123_456_789_0',
		'90_12_345_67890',
		'8901_234_567890_',
		'_7890_123456789_0',
		'678901234_567890',
		'567890_1234567890',
		'4567890123__4567890',
		'_34567890123_4567890',
		'2345678_90123_4567890',
		'123456789_01_234567890',
		'01234567_8901234567890',
		'9012345678901_234567890__',
		'8_90123456_78901234567890',
		'78901234567890_123_4567890',
		'___67890123_4_5_6_78901234567890___',
		'567890123_45678901234567890',
		'45_67890123456789_01234567890',
		'3456789012_345678901234567890',
		'234_567890_1234_5__67890_1234567890_',
		'12345678_90123_456789_0123_4567890',
		'0_123_456_789_012_345_678_901_234_567_890',
		'90_123456__78901__234567_8901_234567890',
		'890123456789012345678901234567_890',
		'7890_1234567_8901234567890_1234567890',
		'67890123_45678_901234567_8901234567890',
		'567890_1234567890_12345678901_234567890',
		'45678_9012345_6789012345678901234567890_',
		'34567890123456_789012345678901234567890',
		'234567_890_1234567890_1234567890_1234567890',
		'334567890_1234567890_1234567890_1234567890',
		'340282360_0000000000_0000000000_0000000000',
		'340282366_9209384634_2648111928_4349108225',
		'340282366_9209384634_6337460743_1768211455',
	]

	for ts in test_strings {
		with := unsigned.uint128_from_dec_str(ts) or {
			assert false, 'invalid Uint128 string ${ts}'
			panic('')
		}

		without := unsigned.uint128_from_dec_str(ts.replace('_', '')) or {
			assert false, 'invalid Uint128 string ${ts.replace('_', '')}'
			panic('')
		}

		assert with == without
	}
}

fn test_new() {
	assert unsigned.uint128_new(max_u64, max_u64) == unsigned.uint128_max
}

fn test_lsh() {
	a := unsigned.uint128_from_dec_str('123456789012345678901234567890')!
	assert a.str() == a.lsh(0).str()
	assert '246913578024691357802469135780' == a.lsh(1).str()
	assert '259801135457060040952792416454273138688' == a.lsh(64).str()
	assert '302984417681386893975453667670529933312' == a.lsh(100).str()
	assert unsigned.uint128_zero == a.lsh(200)
}

fn test_rsh() {
	a := unsigned.uint128_from_dec_str('279625844435276397900870454226348864638')!
	assert a.str() == a.rsh(0).str()
	assert '139812922217638198950435227113174432319' == a.rsh(1).str()
	assert '15158547400991018568' == a.rsh(64).str()
	assert '220585896' == a.rsh(100).str()
	assert '1' == a.rsh(127).str()
	assert unsigned.uint128_zero == a.rsh(200)
}

fn test_quo_rem() {
	for a_lo in 0 .. 16 {
		for a_hi in 0 .. 16 {
			for b_lo in 0 .. 16 {
				for b_hi in 0 .. 16 {
					a := unsigned.uint128_new(u64(1 << a_lo), u64(1 << a_hi))
					b := unsigned.uint128_new(u64(1 << b_lo), u64(1 << b_hi))
					big_a := big.integer_from_string(a.str())!
					big_b := big.integer_from_string(b.str())!
					q, r := a.quo_rem(b)
					big_q, big_r := big_a.div_mod(big_b)
					assert q.str() == big_q.str()
					assert r.str() == big_r.str()
				}
			}
		}
	}
}

fn test_div_128() {
	for a_lo in 0 .. 4 {
		for a_hi in 0 .. 4 {
			for b_lo in 0 .. 4 {
				for b_hi in 0 .. 4 {
					for c_lo in 0 .. 4 {
						for c_hi in 0 .. 4 {
							a := unsigned.uint128_new(u64(1 << a_lo), u64(1 << a_hi))
							b := unsigned.uint128_new(u64(1 << b_lo), u64(1 << b_hi))
							c := unsigned.uint128_new(u64(1 << c_lo), u64(1 << c_hi))
							if c.cmp(a) <= 0 {
								continue
							}
							aa := unsigned.uint256_new(b, a)
							big_a := big.integer_from_string(aa.str())!
							big_b := big.integer_from_string(c.str())!
							q, r := unsigned.div_128(a, b, c)
							big_q, big_r := big_a.div_mod(big_b)
							assert q.str() == big_q.str()
							assert r.str() == big_r.str()
						}
					}
				}
			}
		}
	}
}
