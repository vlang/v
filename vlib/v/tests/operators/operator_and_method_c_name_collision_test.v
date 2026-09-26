struct ModValue {
	value int
}

fn (value ModValue) mod() int {
	return value.value
}

fn (left ModValue) % (right ModValue) ModValue {
	return ModValue{
		value: left.value % right.value
	}
}

fn test_operator_and_method_with_same_c_name() {
	left := ModValue{
		value: 7
	}
	right := ModValue{
		value: 4
	}
	assert left.mod() == 7
	assert (left % right).value == 3
}
