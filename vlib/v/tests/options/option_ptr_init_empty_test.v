pub struct Test {
	a int = 10
	b int = 5
}

pub struct ABC {
	test  ?&Test
	test2 ?Test
}

fn test_main() {
	abc := ABC{
		test:  &Test{} // non option init
		test2: Test{}  // non option init
	}

	if ttt := abc.test {
		assert ttt.a == 10
		assert ttt.b == 5
	} else {
		assert false
	}

	if ttt := abc.test2 {
		assert ttt.a == 10
		assert ttt.b == 5
	} else {
		assert false
	}
}
