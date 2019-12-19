/**********************************************************************
*
* String to float Test
*
**********************************************************************/
import strconv

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
		31234567890123
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
	]

	// check conversion case 1 string <=> string
	for c,x in src_num {
		// slow atof
		assert strconv.atof64(src_num_str[c]).strlong() == x.strlong()
		
		// quick atof
		mut s1 := (strconv.atof_quick(src_num_str[c]).str())
		s1 = s1[..src_num_str[c].len]
		mut s2 := (x.str())
		s2 = s2[..src_num_str[c].len]
		assert s1 == s2

		// test C.atof
		assert x.strsci(18) == f64(C.atof(src_num_str[c].str)).strsci(18)
	}

	// check conversion case 2 string <==> f64
	// we don't test atof_quick beacuse we already know the rounding error
	for c,x  in src_num_str {
		b := src_num[c].strlong()
		a1 := strconv.atof64(x).strlong()
		assert a1 == b
	}

	// special cases
	mut f1 := f64(0.0)
	mut ptr := *u64(&f1)
	ptr = *u64(&f1)

	// DOUBLE_PLUS_ZERO
	f1=0.0
	assert *ptr == u64(0x0000000000000000)
	// DOUBLE_MINUS_ZERO
	f1=-0.0
	assert *ptr == u64(0x8000000000000000)
	
}