struct MyStruct {
mut:
	v int
}

fn inc(mut s MyStruct) {
	s.v++
}

fn test_for_in_mut_with_ptrs() {
	mut ex1 := [&MyStruct{
		v: 1
	}]

	for mut c in ex1 { // c is **MyStruct whereas I would expect it to be *MyStruct
		inc(mut *c) // Need to de-reference c
	}
	assert ex1[0].v == 2

	mut ex2 := [&MyStruct{
		v: 1
	}]

	for mut c in ex2 {
		inc(mut c) // We can see that we're not incrementing the correct memory location as we're incrementing the reference to c :/
	}
	assert ex2[0].v == 2
}
