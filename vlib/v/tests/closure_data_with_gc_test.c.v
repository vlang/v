fn create_closure() fn () int {
	x := 1234
	c := fn [x] () int {
		println(' >> x = ${x}')
		return x
	}
	return c
}

fn test_closure_data_is_kept_alive() {
	c := create_closure()
	assert c() == 1234
	$if gcboehm ? {
		C.GC_gcollect()
	}
	for _ in 0 .. 1000 {
		unsafe {
			p := malloc(8)
			C.memset(p, 0x33, 8)
		}
	}
	$if gcboehm ? {
		C.GC_gcollect()
	}
	assert c() == 1234
}
