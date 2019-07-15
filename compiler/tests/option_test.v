fn opt_err() ?string {return error('hi')}

fn test_err(){
	v := opt_err() or {
		assert err == 'hi'
		return
	}
	assert false
	println(v) // suppress not used error
}

fn err_call(ok bool) ?int {
	if !ok {
		return error('Not ok!')
	}
	return 42
}

fn test_option_for_base_type_without_variable() {
	val := err_call(true) or {
		panic(err)
		return
	}
	assert val == 42
}
