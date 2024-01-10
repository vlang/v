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
		println(x.str())
		println('${x}')

		rets << x.str()
		rets << '${x}'
	}
	assert rets.len == 4
	assert rets[0] == '1'
	assert rets[1] == '1'
	assert rets[2] == '2'
	assert rets[3] == '2'
}
