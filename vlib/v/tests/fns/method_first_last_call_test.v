struct Foo {}

fn (l &Foo) first() {}

fn (l &Foo) last() {}

fn test_method_first_last_call() {
	f := Foo{}
	f.first()
	f.last()
	assert true
}
