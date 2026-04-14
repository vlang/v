struct Foo {}

fn (f Foo) foo() string {
	return 'foo'
}

fn (f Foo) bar() string {
	return 'bar'
}

struct NamedFoo {
	name string
}

fn (f NamedFoo) foo() string {
	return 'foo ${f.name}'
}

fn (f NamedFoo) bar() string {
	return 'bar ${f.name}'
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

fn test_comptime_method_value() {
	f := NamedFoo{'rabbit'}
	mut results := []string{}
	$for method in NamedFoo.methods {
		x := f.$(method)
		results << x()
	}
	assert results[0] == 'foo rabbit'
	assert results[1] == 'bar rabbit'
}
