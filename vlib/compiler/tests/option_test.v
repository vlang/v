
fn opt_err_with_code() ?string {return error_with_code('hi',137)}
fn test_err_with_code(){
	v := opt_err_with_code() or {
		assert err == 'hi'
		assert errcode == 137
		return
	}
	assert false
	println(v) // suppress not used error
}

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
	println('$val2 should have been `none`')
	assert false
	// This is invalid:
	//	x := 5 or {
	//		return
	//	}
}

fn test_if_opt() {
	if val := err_call(false) {
		assert val == 42
	}
	assert 1 == 1
	println('nice')
}

fn for_opt_default() ?string {
        return error('awww')
}

fn test_opt_default() {
	a := for_opt_default() or {
			// panic(err)
			'default'
	}
	assert a == 'default'
}

fn foo_ok() ?int {
	return 777
}

fn foo_str() ?string {
	return 'something'
}

fn test_q() {
	//assert foo_ok()? == true
}

struct Person {
mut:
	name string
	age int
	title ?string
}

fn test_field_or() {
	name := foo_str() or {
		'nada'
	}
	assert name == 'something'

	mut p := Person {}
	p.name = foo_str() or {
		'nothing'
	}
	assert p.name == 'something'

	p.age = foo_ok() or {
		panic('no age')
	}
	assert p.age == 777

	mytitle := p.title or {
		'default'
	}
	assert mytitle == 'default'
}

struct Thing {
mut:
	opt ?int
}

fn test_opt_field() {
	mut t := Thing{}
	t.opt = 5
	val := t.opt or { return }
	assert val == 5
}

fn opt_ptr(a &int) ?&int {
	if isnil(a) {
		return none
	}
	return a
}

fn test_opt_ptr() {
	a := 3
	r1 := opt_ptr(&a) or {
		&int(0)
	}
	assert r1 == &a
	r2 := opt_ptr(&int(0)) or {
		return
	}
	println('`$r2` should be none')
	assert false
}

fn multi_return_opt(err bool) ?(string, string) {
	if err {
		return error('oops')
	}
	return 'hello', 'v'
}

fn test_multi_return_opt() {
	a, b := multi_return_opt(false) or {
		panic(err)
	}
	assert a == 'hello' && b == 'v'
	_, _ := multi_return_opt(true) or {
		assert err == 'oops'
		return
	}
}