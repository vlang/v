pub type Foo = Bar | Baz

fn (mut f Foo) set_baz() {
	f = Baz{}
}

pub struct Bar {}

pub struct Baz {}

fn test_mut_receiver_of_sumtype() {
	mut x := Foo(Bar{})
	x.set_baz()

	println(x)
	assert '${x}' == 'Foo(Baz{})'
}
