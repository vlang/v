fn takes_quad_ref(x &&&&int) {
	unsafe {
		****x = 5
	}
}

fn takes_triple_ref(x &&&int) {
	unsafe {
		***x = 7
	}
}

fn test_call_arg_with_multi_ref_from_value() {
	mut y := 4
	takes_quad_ref(y)
	assert y == 5
}

fn test_call_arg_with_multi_ref_from_ref() {
	mut y := 4
	p := &y
	takes_triple_ref(p)
	assert y == 7
}
