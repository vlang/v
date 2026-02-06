struct Foo {
	bar &&char
}

fn test_main() {
	foo := Foo{
		bar: unsafe {
			&&char([c'a', c'b', nil]!)
		}
	}
	println(foo)
	a := unsafe { cstring_to_vstring(*foo.bar) }
	assert a == 'a'
}
