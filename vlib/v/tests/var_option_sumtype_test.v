type Abc = f64 | int

fn test_main() {
	mut v := ?Abc(none)
	assert v == none
	println(v)
	v = Abc(1)
	assert v != none
	v = 1.2
	println(v)
	assert v != none
}
