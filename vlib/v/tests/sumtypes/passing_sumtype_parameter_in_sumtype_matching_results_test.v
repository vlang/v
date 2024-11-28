struct Foo {}

type Bar = Foo | int
type Baz = Foo | f32

fn foo(a Bar) int {
	return 22
}

fn bar() Baz {
	return Foo{}
}

fn test_passing_sumtype_parameter_in_sumtype_matching_results() {
	a := bar()
	mut ret := 0
	match a {
		Foo {
			ret = foo(Bar(a))
		}
		f32 {}
	}
	println(ret)
	assert ret == 22
}
