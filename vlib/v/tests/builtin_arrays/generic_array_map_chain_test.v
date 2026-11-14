fn filter_mapped_strings[T](values []T) []string {
	return values.map(it.str()).filter(it != '2')
}

fn test_generic_dynamic_array_filter_uses_mapped_element_type() {
	assert filter_mapped_strings([1, 2, 3]) == ['1', '3']
	assert filter_mapped_strings(['2', '4']) == ['4']
}

fn filter_mapped_fixed_ints[T](values [3]T) []int {
	return values.map(it + 1).filter(it != 2)
}

fn test_generic_fixed_array_filter_uses_mapped_element_size() {
	assert filter_mapped_fixed_ints([1, 2, 3]!) == [3, 4]
}

fn map_fixed_string_lengths[T](values [3]T) [3]int {
	return values.map(it.str()).map(it.len)
}

fn test_generic_fixed_array_map_uses_mapped_element_type() {
	assert map_fixed_string_lengths([1, 20, 300]!) == [1, 2, 3]!
}

fn assert_mapped_string_predicates[T](values []T) {
	assert values.map(it.str()).any(it == '2')
	assert values.map(it.str()).all(it.len > 0)
	assert values.map(it.str()).count(it != '2') == 2
	assert values.map(it.str()).filter(it != '2').map(it.len) == [1, 1]
}

fn test_generic_array_predicates_use_mapped_element_type() {
	assert_mapped_string_predicates([1, 2, 3])
}

fn count_and_stringify[T](value T, mut calls []int) string {
	calls << 1
	return value.str()
}

fn filter_with_mapping_side_effect[T](values []T, mut calls []int) []string {
	return (values.map(count_and_stringify(it, mut calls))).filter(it != '2')
}

fn test_generic_array_chain_evaluates_map_once() {
	mut calls := []int{}
	assert filter_with_mapping_side_effect([1, 2, 3], mut calls) == ['1', '3']
	assert calls.len == 3
}
