struct Zest {
	val int
}

fn (t Zest) get_a_finger_to_the_moon() voidptr {
	return voidptr(0)
}

fn get_the_dao_way() voidptr {
	return voidptr(0)
}

fn test_returning_a_void_pointer_from_a_method() {
	t := &Zest{
		val: 123
	}
	z := voidptr(0)
	assert z == t.get_a_finger_to_the_moon()
	assert t.get_a_finger_to_the_moon() == 0
}

fn test_returning_a_void_pointer_from_a_function() {
	z := voidptr(0)
	assert z == get_the_dao_way()
	assert get_the_dao_way() == 0
}
