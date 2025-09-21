interface Value {}

struct Foo {
mut:
	value int
}

fn test_main() {
	mut i := 0
	mut integers := []Value{}
	integers << i
	i = 1
	integers << i
	i = 2
	integers << i
	assert integers == [0, 1, 2]

	mut f := Foo{
		value: 0
	}
	mut foos := []Foo{}
	foos << f
	f.value = 1
	foos << f
	f.value = 2
	foos << f
	assert foos == [Foo{
		value: 0
	}, Foo{
		value: 1
	}, Foo{
		value: 2
	}]
}
