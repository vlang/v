@[has_globals]
module main

struct PromotedNumber {
	value int
}

__global promoted_number &PromotedNumber = unsafe { nil }

fn (number &PromotedNumber) retain() {
	promoted_number = number
}

fn (left PromotedNumber) * (right PromotedNumber) PromotedNumber {
	return PromotedNumber{left.value * right.value}
}

fn multiply_mut_promoted_number(mut left PromotedNumber, right PromotedNumber) PromotedNumber {
	return left * right
}

fn read_promoted_number(number &PromotedNumber) !bool {
	return number.value > 0
}

fn test_operator_loads_a_promoted_value_once() {
	left := PromotedNumber{3}
	right := PromotedNumber{4}
	left.retain()
	assert read_promoted_number(&left) or { false }
	product := left * right
	assert product.value == 12
	assert promoted_number.value == 3
}

fn test_operator_still_loads_a_source_pointer_operand() {
	left := &PromotedNumber{5}
	right := PromotedNumber{4}
	product := unsafe { *left } * right
	assert product.value == 20
	mut value := PromotedNumber{5}
	assert multiply_mut_promoted_number(mut value, right).value == 20
}
