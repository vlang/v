fn opt_err() ?string {return error('hi')}

fn test_err(){
	v := opt_err() or {
		assert err == 'hi'
		return
	}
	assert false
	println(v) // suppress not used error
}
