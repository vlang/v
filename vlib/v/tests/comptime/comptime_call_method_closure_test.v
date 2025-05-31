struct Foo {}

fn (f Foo) foo() string {
	return 'foo'
}

fn (f Foo) bar() string {
	return 'bar'
}

fn test_main() {
	f := Foo{}
	mut results := []string{}
	$for method in Foo.methods {
		x := fn [method, f] () string {
			return f.$method()
		}
		results << x()
	}
	assert results[0] == 'foo'
	assert results[1] == 'bar'
}
