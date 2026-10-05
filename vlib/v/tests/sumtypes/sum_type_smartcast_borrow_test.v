struct BorrowSmall {
	value int
}

struct BorrowWide {
	padding [3]i64
	value   int
}

type BorrowSum = BorrowSmall | BorrowWide

struct BorrowFields {
	pointer &BorrowSum
	value   BorrowSum
}

type BorrowContainer = BorrowFields | int

fn borrowed_sum_value(value &BorrowSum) int {
	return match value {
		BorrowSmall { value.value + 100 }
		BorrowWide { value.value + 200 }
	}
}

fn replace_borrowed_sum(mut value BorrowSum) {
	value = BorrowSmall{99}
}

fn borrowed_container_value(container &BorrowContainer) int {
	if container !is BorrowFields {
		return 0
	}
	return borrowed_sum_value(container.pointer) + borrowed_sum_value(container.value)
}

fn test_smartcast_variant_fields_preserve_reference_depth() {
	pointer := &BorrowSum(BorrowSmall{3})
	fields := BorrowFields{ pointer: pointer, value: BorrowWide{ value: 4 } }
	container := BorrowContainer(fields)
	assert borrowed_container_value(container) == 307
	assert container is BorrowFields
	address := voidptr(container.pointer)
	assert container.pointer is BorrowSmall
	assert voidptr(container.pointer) == address
}

fn test_smartcast_array_element_borrows_complete_sum_storage() {
	mut values := [BorrowSum(BorrowWide{ value: 7 })]
	if values[0] is BorrowWide {
		assert borrowed_sum_value(values[0]) == 207
		assert borrowed_sum_value((values[0])) == 207
		replace_borrowed_sum(mut values[0])
	}
	assert borrowed_sum_value(values[0]) == 199
}

fn test_smartcast_fixed_array_element_borrows_complete_sum_storage() {
	mut values := [BorrowSum(BorrowWide{ value: 8 })]!
	if values[0] is BorrowWide {
		assert borrowed_sum_value(values[0]) == 208
		replace_borrowed_sum(mut values[0])
	}
	assert borrowed_sum_value(values[0]) == 199
}

fn test_smartcast_map_element_borrows_complete_sum_storage() {
	mut values := {
		'entry': BorrowSum(BorrowWide{ value: 9 })
	}
	if values['entry'] is BorrowWide {
		assert borrowed_sum_value(values['entry']) == 209
		replace_borrowed_sum(mut values['entry'])
	}
	assert borrowed_sum_value(values['entry']) == 199
}

fn test_smartcast_map_with_composite_key_borrows_complete_sum_storage() {
	key := [1, 2]!
	mut values := map[[2]int]BorrowSum{}
	values[key] = BorrowWide{ value: 10 }
	if values[key] is BorrowWide {
		assert borrowed_sum_value(values[key]) == 210
		replace_borrowed_sum(mut values[key])
	}
	assert borrowed_sum_value(values[key]) == 199
}
