fn unwrap[T](val ?T) T {
	if u_val := val {
		return u_val
	}

	return T{}
}

fn test_option_generic_function_call_can_infer_type_implicitly() {
	toto := ?int(1)
	implicit := unwrap(toto)
	explicit := unwrap[int](toto)

	assert implicit == 1
	assert explicit == 1
}
