type HandlerFn = fn ()

fn new_static_router(route_not_found_handler ?fn ()) fn () {
	return route_not_found_handler or {
		fn () {}
	}
}

fn new_static_router2(route_not_found_handler ?HandlerFn) fn () {
	return route_not_found_handler or {
		fn () {}
	}
}

fn test_option_fn_alias_decl_with_none() {
	b := new_static_router2(none)
	$if b is $function {
		assert true
	} $else {
		assert false
	}
}

fn test_option_fn_decl_with_none() {
	a := new_static_router(none)
	$if a is $function {
		assert true
	} $else {
		assert false
	}
}

fn test_option_fn_passing_normal() {
	anon_1 := fn () {
		println(1)
	}
	c := new_static_router(anon_1)
	assert c == anon_1
}

fn test_option_fn_passing_to_alias() {
	anon_2 := fn () {
		println(2)
	}
	d := new_static_router(anon_2)
	assert d == anon_2
}
