module main

fn test_anon_fn_with_nested_anon_fn_args() {
	mut xa := fn (x fn (int) string, y int) string {
		return x(y)
	}
	a := xa(fn (i int) string {
		return 'a' + i.str()
	}, 8)
	println(a)
	assert a == 'a8'
}
