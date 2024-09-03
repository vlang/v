fn test_volatile_var() {
	mut volatile zzz := 123
	assert zzz == 123
}

fn test_volatile_pointer() {
	x := 123
	y := 456
	mut volatile p := unsafe { &x }
	println(p)
	p = unsafe { &y }
	println(p)
	assert unsafe { *p == y }
}
