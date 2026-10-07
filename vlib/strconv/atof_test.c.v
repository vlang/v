import strconv
import math

/**********************************************************************
*
* String to float Test
*
**********************************************************************/

fn test_atof() {
	//
	// test set
	//

	// float64
	src_num := [
		f64(0.3),
		-0.3,
		0.004,
		-0.004,
		0.0,
		-0.0,
		31234567890123,
		0.01,
		2000,
		-300,
	]

	// strings
	src_num_str := [
		'0.3',
		'-0.3',
		'0.004',
		'-0.004',
		'0.0',
		'-0.0',
		'31234567890123',
		'1e-2',
		'+2e+3',
		'-3.0e+2',
	]

	// check conversion case 1 string <=> string
	for c, x in src_num {
		// slow atof
		val := strconv.atof64(src_num_str[c]) or { panic(err) }
		assert val.strlong() == x.strlong()

		// quick atof
		mut s1 := (strconv.atof_quick(src_num_str[c]).str())
		mut s2 := (x.str())
		delta := s1.f64() - s2.f64()
		// println("${s1} ${s2} ${delta}")
		assert delta < f64(1e-16)

		// test C.atof
		n1 := x.strsci(18)
		n2 := f64(C.atof(&char(src_num_str[c].str))).strsci(18)
		// println("${n1} ${n2}")
		assert n1 == n2
	}

	// check conversion case 2 string <==> f64
	// we don't test atof_quick because we already know the rounding error
	for c, x in src_num_str {
		b := src_num[c].strlong()
		value := strconv.atof64(x) or { panic(err) }
		a1 := value.strlong()
		assert a1 == b
	}

	// special cases
	mut f1 := f64(0.0)
	mut ptr := unsafe { &u64(&f1) }
	ptr = unsafe { &u64(&f1) }

	// double_plus_zero
	f1 = 0.0
	assert *ptr == u64(0x0000000000000000)
	// double_minus_zero
	f1 = -0.0
	assert *ptr == u64(0x8000000000000000)
	println('DONE!')
}

fn test_atof_subnormal() {
	// Test subnormal (denormalized) float numbers and edge cases
	// These are very small numbers close to the f64 minimum
	// IMPORTANT: Compare with hardcoded f64 literals, not .f64() which uses the same parser

	// Normal numbers
	assert strconv.atof64('1.0e-250') or { panic('parse error') } == 1.0e-250
	assert strconv.atof64('2.5e-260') or { panic('parse error') } == 2.5e-260

	// Transition zone
	assert strconv.atof64('1.0e-300') or { panic('parse error') } == 1.0e-300
	assert strconv.atof64('2.2250738585072014e-308') or { panic('parse error') } == 2.2250738585072014e-308

	// Subnormal numbers (these fail without the fix)
	assert strconv.atof64('1.23e-308') or { panic('parse error') } == 1.23e-308
	assert strconv.atof64('1.0e-310') or { panic('parse error') } == 1.0e-310
	assert strconv.atof64('5.0e-320') or { panic('parse error') } == 5.0e-320
	assert strconv.atof64('5e-324') or { panic('parse error') } == 5e-324

	// Negative subnormal
	assert strconv.atof64('-1.0e-320') or { panic('parse error') } == -1.0e-320
}

fn test_atof_small_decimal_with_many_leading_zeroes() {
	assert strconv.atof64('0.0000000000000000005') or { panic('parse error') } == strconv.atof64('5e-19') or {
		panic('parse error')
	}
	assert strconv.atof64('-0.0000000000000000005') or { panic('parse error') } == strconv.atof64('-5e-19') or {
		panic('parse error')
	}
}

fn test_atof_errors() {
	if x := strconv.atof64('') {
		eprintln('> x: ${x}')
		assert false // strconv.atof64 should have failed
	} else {
		assert err.str() == 'expected a number found an empty string'
	}
	if x := strconv.atof64('####') {
		eprintln('> x: ${x}')
		assert false // strconv.atof64 should have failed
	} else {
		assert err.str() == 'not a number'
	}
	if x := strconv.atof64('uu577.01') {
		eprintln('> x: ${x}')
		assert false // strconv.atof64 should have failed
	} else {
		assert err.str() == 'not a number'
	}
	if x := strconv.atof64('123.33xyz') {
		eprintln('> x: ${x}')
		assert false // strconv.atof64 should have failed
	} else {
		assert err.str() == 'extra char after number'
	}
}

fn test_atof_special_values() {
	for input in ['inf', '+inf', 'Inf', 'INF', 'infinity', '+Infinity', 'iNfInItY'] {
		assert math.is_inf(strconv.atof64(input)!, 1)
	}
	for input in ['-inf', '-INF', '-Infinity'] {
		assert math.is_inf(strconv.atof64(input)!, -1)
	}
	for input in ['nan', 'NaN', 'NAN'] {
		assert math.is_nan(strconv.atof64(input)!)
	}
}

fn test_atof_digit_separators() {
	assert strconv.atof64('1_000')! == 1000
	assert strconv.atof64('-1_234.5_6')! == -1234.56
	assert strconv.atof64('1_0e+1_0')! == 1e11
	assert strconv.atof64('0_0.0_1')! == 0.01
	assert strconv.atof64('.1_2')! == 0.12
}

fn test_atof_invalid_syntax() {
	for input in [' ', ' 1', '\t1', '\n1', '+', '-', '.', '+.', '-.', '-+1', '1e', '1e+', '1e-',
		'.e1', '_1', '1_', '1__0', '1_.0', '1._0', '1_e1', '1e_1', '1e1_', '+nan', '-nan', 'infx',
		'nanx', '1 2', '1\x00'] {
		if value := strconv.atof64(input) {
			assert false, '${input} parsed as ${value}'
		}
	}
	for input in ['+', '-', '.', ' ', ' 1', '-+1', '1e', '1e-'] {
		if value := strconv.atof64(input, allow_extra_chars: true) {
			assert false, '${input} parsed as ${value}'
		}
	}
	assert strconv.atof64('1.5units', allow_extra_chars: true)! == 1.5
	assert strconv.atof64('-0')!.str() == '-0.0'
}

fn test_atof_rounds_once_with_guard_and_sticky_bits() {
	// Compare IEEE 754 bits directly, without parsing expected floating-point literals.
	cases := {
		'9007199254740993':        u64(0x4340000000000000)
		'9007199254740995':        u64(0x4340000000000002)
		'9007199254740997':        u64(0x4340000000000002)
		'1e23':                    u64(0x44b52d02c7e14af6)
		'9007199254740993.01':     u64(0x4340000000000001)
		'9007199254740992.99':     u64(0x4340000000000000)
		'9007199254740994.99':     u64(0x4340000000000001)
		'18014398509481983':       u64(0x4350000000000000)
		'2.2250738585072012e-308': u64(0x0010000000000000)
		'2.4703282292062327e-324': u64(0x0000000000000000)
		'2.4703282292062328e-324': u64(0x0000000000000001)
		'1.7976931348623157e308':  u64(0x7fefffffffffffff)
	}
	for input, expected in cases {
		assert math.f64_bits(strconv.atof64(input)!) == expected, input
		assert math.f64_bits(strconv.atof64('-' + input)!) == expected | (u64(1) << 63), input
	}
	assert math.f64_bits(strconv.atof64('9_007_199_254_740_993')!) == u64(0x4340000000000000)
	assert math.f64_bits(strconv.atof64('1e23 units', allow_extra_chars: true)!) ==
		u64(0x44b52d02c7e14af6)
}
