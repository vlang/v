fn test_anon_fn_decl_with_anon_fn_params() {
	a := fn (p1 fn () int, p2 string) string {
		n := p1()
		return '${n}' + p2
	}
	ret := a(aaa, 'hello')
	println(ret)
	assert ret == '22hello'
}

fn aaa() int {
	return 22
}
