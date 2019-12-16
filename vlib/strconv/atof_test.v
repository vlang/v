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
		-0.3 , 
		0.004, 
		-0.004, 
		0.0 , 
		-0.0
	]

	// strings
	src_num_str := [
		'0.3',
		'-0.3',
		'0.004',
		'-0.004',
		'0.0',
		'-0.0'
	]

	// check conversion1 string <=> string
	for c,x in src_num {
		assert strconv.atof64(src_num_str[c]).strlong() == x.strlong()
		assert x.strsci(18) == f64(C.atof(src_num_str[c].str)).strsci(18)
	}

	// check conversion2 string <==> f64
	for c,x  in src_num_str {
		a:=strconv.atof64(x)
		b:=src_num[c]
		assert a.strlong() == b.strlong()
	}

	// special cases
	mut f1 := f64(0.0)
	f1=0.0
	mut ptr := *u64(&f1)

	// DOUBLE_PLUS_ZERO
	assert *ptr == u64(0x0000000000000000)
	// DOUBLE_MINUS_ZERO
	f1=-0.0
	assert *ptr == u64(0x8000000000000000)
	// DOUBLE_PLUS_INFINITY
	*ptr=u64(0x7FF0000000000000)
	assert f1.strlong() == 'inf'
	// DOUBLE_MINUS_INFINITY
	*ptr=u64(0xFFF0000000000000)
	assert f1.strlong() == '-inf'
	f1=0.0
}
