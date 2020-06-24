fn foo() ? {
	return error('something')
}

fn test_optional_void() {
	foo() or {
		println(err)
		assert err == 'something'
		return
	}
}

fn bar() ? {
	return error('bar error')
}

fn test_optional_void_only_question() {
	bar() or {
		println(err)
		assert err == 'bar error'
		return
	}
}

fn test_optional_void_with_empty_or() {
	foo() or {}
	assert true
}

fn option_void(a int) ? {
	if a != 0 {
		return
	} else {
		return error('zero error')
	}
}

fn test_optional_void_with_return() {
	option_void(0) or {
		println(err)
		assert err == 'zero error'
		return
	}
	option_void(-1) or {
		println(err)
		assert err == 'zero error'
		return
	}
	assert true
}
