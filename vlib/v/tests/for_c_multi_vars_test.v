fn test_for_c_multi_init_vars() {
	mut rets := []string{}
	for a,b := 0,1; a < 5; a++ {
		if a == 3 {
			continue
		}
		b = a + 1
		println('$a, $b')
		rets << '$a, $b'
	}
	println(rets)
	assert rets[0] == '0, 1'
	assert rets[1] == '1, 2'
	assert rets[2] == '2, 3'
	assert rets[3] == '4, 5'
}

fn test_for_c_multi_inc_vars() {
	mut rets := []string{}
	mut b := 1
	for a := 0; a < 10; a,b = b,a+b {
		if a in [2, 3] {
			continue
		}
		println('$a, $b')
		rets << '$a, $b'
	}
	println(rets)
	assert rets[0] == '0, 1'
	assert rets[1] == '1, 1'
	assert rets[2] == '1, 2'
	assert rets[3] == '5, 8'
	assert rets[4] == '8, 13'
}
