struct Foo {
	x int
}

pub fn (f Foo) str() string {
	return 'Foo{}'
}

fn process_foo(foo &Foo) {
	println('>process_foo, called for ${foo} === ${*foo}')
}

fn get_foo() Foo {
	println('>get_foo')
	return Foo{}
}

/*
// TODO: Fix this. It 'works' only with tcc, but is not sast.
fn test_ref_fn_arg() {
	process_foo(get_foo())
	println(3434)
	assert true
}
*/

fn test_dummy() {}
