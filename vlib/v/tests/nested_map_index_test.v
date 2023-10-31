struct Foo {
mut:
	foo map[int]Bar
}

struct Bar {
mut:
	bar map[int]string
}

fn test_nested_map_index() {
	f := Foo{
		foo: {
			11: Bar{
				bar: {
					22: 'hello'
				}
			}
		}
	}
	ret := f.foo[11]!.bar[22]!
	println(ret)
	assert ret == 'hello'
}
