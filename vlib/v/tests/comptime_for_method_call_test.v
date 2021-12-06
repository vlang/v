struct Foo {}

fn (f Foo) a() int {
	return 1
}

fn (f Foo) b() int {
	return 2
}

fn test_comptime_for_method_call() {
	f := Foo{}
	mut rets := []string{}

	$for method in Foo.methods {
		x := f.$method()
		println(x)
		rets << x.str()
	}
	assert rets.len == 2
	assert rets[0] == '1'
	assert rets[1] == '2'
}
