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

fn ret_none() ?int {
	//return error('wtf') //none
	return none
}	

fn test_option_for_base_type_without_variable() {
	val := err_call(true) or {
		panic(err)
	}
	assert val == 42
	println('hm')
	val2 := ret_none() or {
		println('yep')
		return
	}	
	println('nice')
	println(val2)
}

fn test_if_opt() {
	if val := err_call(false) {
		assert val == 42
	}	
	assert 1 == 1
	println('nice')
}	
