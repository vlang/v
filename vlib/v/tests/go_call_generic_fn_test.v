fn test<T>(c chan int, s T) {
	println('hi from generic fn test, T: ' + typeof(s).name)
	println('s: $s')
	assert true
	c <- 123
}

fn test_go_generic_fn() {
	mut c := chan int{}
	spawn test<string>(c, 'abcd')
	x := <-c
	assert x == 123
	println('bye')
}
