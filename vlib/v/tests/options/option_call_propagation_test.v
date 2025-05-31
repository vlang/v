fn a() ?int {
	return none
}

fn abc() ?int {
	varz := a()
	dump(varz)
	assert varz == none
	return varz
}

fn test_option_call_propagation() {
	var := abc() or { 1 }
	assert var == 1
	println(var)
}
