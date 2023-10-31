struct Zest {
	val int
}

fn (t Zest) get_a_finger_to_the_moon() voidptr {
	return unsafe { nil }
}

fn get_the_dao_way() voidptr {
	return unsafe { nil }
}

fn test_returning_a_void_pointer_from_a_method() {
	t := &Zest{
		val: 123
	}
	z := unsafe { nil }
	assert z == t.get_a_finger_to_the_moon()
	assert t.get_a_finger_to_the_moon() == 0
}

fn test_returning_a_void_pointer_from_a_function() {
	z := unsafe { nil }
	assert z == get_the_dao_way()
	assert get_the_dao_way() == 0
}
