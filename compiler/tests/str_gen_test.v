struct Foo {
	a int
}

fn test_array_str() {
	f := Foo{34}
	println(f)
	//s := f.str()
	//println(s)
	n := [1, 2, 3]
	assert n.str() == '[1, 2, 3]'
	println(n)  // make sure the array is printable
	n2 := [4,5,6]
	//assert n2.str() == '[4, 5, 6]'
	println(n2)
}
