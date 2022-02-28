interface TheInterface {
mut:
	an_interface() ?
}

struct Implementation {
}

fn (mut i Implementation) an_interface() ? {
	return
}

fn maker() ?TheInterface {
	inner := Implementation{}
	return inner
}

fn do(mut inter TheInterface) string {
	return 'ok'
}

fn test_fn_mut_arg_of_interface() ? {
	mut inner := maker() ?
	ret := do(mut inner)
	println(ret)
	assert ret == 'ok'
}
