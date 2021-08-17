struct Foo {
	foo int
}

struct Test {
	anon struct {
		foo int
	}
}

type Abc = struct {
	foo int
}
type Def = struct {
	foo int
} | int

fn test_anon_struct_init() {
	test := Test{
		anon: Foo{
			foo: 10
		}
	}

	assert test.anon.foo == 10
}

fn test_anon_fn_call() {
	assert foo(Foo{foo: 20}) == 20
}

fn foo(anon struct {
	foo int
}) int {
	return anon.foo
}

fn test_anon_sum_type_cast() {
	test := Foo{
		foo: 20
	}
	a := Abc(test)
	d := Def(test)
}