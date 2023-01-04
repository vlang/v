struct Foo {}

fn (f Foo) a() string {
	return 'method_a'
}

fn (f Foo) b() string {
	return 'method_b'
}

fn test_comptime_call_in_fn_call() {
	f := Foo{}

	mut rets := []string{}

	$for method in Foo.methods {
		x := f.$method()
		println(x)
		println(f.$method())
		rets << get_string(f.$method())
	}
	assert rets.len == 2
	assert rets[0] == 'method_a'
	assert rets[1] == 'method_b'
}

fn get_string(s string) string {
	return s
}
