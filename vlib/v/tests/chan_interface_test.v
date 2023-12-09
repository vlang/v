interface TestInterface {
	a int
}

struct TestStruct {
	a int
}

fn test_chan_interface() {
	c := chan TestInterface{cap: 1}

	c.try_push(TestInterface(TestStruct{ a: 1 }))

	m := <-c
	println(m)
	assert m.a == 1
}
