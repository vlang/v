struct St1 {
	val int = 5
}

interface In1 {
	val int
}

fn test_go_wait_with_fn_of_interface_type() {
	mut var := &St1{}
	(spawn fn1(mut var)).wait()
}

fn fn1(mut v In1) {
	println(v)
	assert v.val == 5
}
