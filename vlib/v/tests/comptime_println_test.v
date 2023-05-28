struct Foo {
	a int
	b f32
	c string
}

fn print_struct[T](x &T) []string {
	mut a := []string{}
	$for field in T.fields {
		typ := typeof((*x).$(field.name)).name
		val := (*x).$(field.name)
		a << '${field.name} (${typ}) = ${val}'
	}
	return a
}

fn test_main() {
	foo := Foo{
		a: 123
		b: 4.56
		c: 'hello'
	}
	out := print_struct(&foo)
	assert out[0] == 'a (int) = 123'
	assert out[1] == 'b (f32) = 4.56'
	assert out[2] == 'c (string) = hello'
}
