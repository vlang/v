pub struct Test {
	a int = 10
	b int = 5
}

pub struct ABC {
	test ?&Test
}

fn test_main() {
	abc := ABC{
		test: &Test{}
	}

	if ttt := abc.test {
		assert ttt.a == 10
		assert ttt.b == 5
	} else {
		assert false
	}
}
