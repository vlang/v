import json2

// json2 declares a sum type `Any`. An interface of the program with that name is
// what `Any` means in it: the cast used to be lowered as a json2.Any sum value
// (and dropped), and the boxed value written as a json2__Any literal.
interface Any {}

struct Point {
	x int
}

fn take(value Any) bool {
	return value is Point
}

fn test_main_interface_named_like_a_module_sum_type() {
	value := Any(Point{3})
	assert value is Point
	if value is Point {
		assert value.x == 3
	}
	assert take(Point{4})
	assert json2.encode(1) == '1'
}
