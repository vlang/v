fn start(f fn ()) string {
	f()
	return 'ok!!'
}

fn on_connect() {
	println('fn ok!!')
}

fn test_go_call_fn_with_anon_fn_arg() {
	g := spawn start(on_connect)
	ret := g.wait()
	assert ret == 'ok!!'
}
