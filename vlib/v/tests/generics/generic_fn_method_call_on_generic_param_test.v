struct MyA {
	name string
}

fn (a &MyA) name() string {
	return a.name
}

struct MyB {
	name string
}

fn (b &MyB) name() string {
	return b.name
}

fn display[T](o T) string {
	return 'T is called ${o.name()}'
}

fn test_generic_fn_method_call_on_generic_param() {
	a := MyA{
		name: 'Ah'
	}
	b := MyB{
		name: 'Beh'
	}
	assert display[MyA](a) == 'T is called Ah'
	assert display[MyB](b) == 'T is called Beh'
}
