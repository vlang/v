struct Example {
}

fn test_str_reference_struct() {
	c1 := Example{}
	println((&c1).str())
	assert (&c1).str() == '&Example{}'

	c2 := &Example{}
	println(c2.str())
	assert c2.str() == '&Example{}'
}
