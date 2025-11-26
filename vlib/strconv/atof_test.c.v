import strconv

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
		// println("$s1 $s2 $delta")
		assert delta < f64(1e-16)

		// test C.atof
		n1 := x.strsci(18)
		n2 := f64(C.atof(&char(src_num_str[c].str))).strsci(18)
		// println("$n1 $n2")
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
