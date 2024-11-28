import aa

fn test_assign_with_in_module_sumtype() {
	node := aa.MySumType(aa.S1{})

	cond := node in [aa.S1, aa.S2]
	mut b := 'b'
	mut c := 'c'

	if cond {
		println('${cond} --- ${b} --- ${c}')
		assert '${cond} --- ${b} --- ${c}' == 'true --- b --- c'
	} else {
		assert false
	}
}
