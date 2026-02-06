interface Foo {
	add(x int)
}

struct Base {
mut:
	i int
}

fn (mut b Base) add(x int) {
	b.i += x
}

type Alias = Base

fn (mut a Alias) add(x int) {
	a.i += x * x
}

fn test_string_alias_of_struct() {
	mut a := Alias{
		i: 2
	}
	a.add(3)
	println(a)
	assert '${a}'.contains('Alias')
}
