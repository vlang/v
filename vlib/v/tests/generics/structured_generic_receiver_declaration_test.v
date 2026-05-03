struct ReceiverPatternExpect[T] {
	value T
}

fn (e ReceiverPatternExpect[T]) broad_receiver_returns_value() T {
	return e.value
}

fn (e ReceiverPatternExpect[[]int]) concrete_array_receiver_returns_len() int {
	return e.value.len
}

fn (e ReceiverPatternExpect[[]int]) same_name_prefers_concrete_receiver() string {
	return 'concrete'
}

fn (e ReceiverPatternExpect[[]T]) dynamic_array_receiver_pattern_returns_first_or_value(value T) T {
	if e.value.len == 0 {
		return value
	}
	return e.value[0]
}

fn (e ReceiverPatternExpect[[]T]) same_name_prefers_concrete_receiver() string {
	return 'pattern'
}

fn (e ReceiverPatternExpect[map[K]V]) map_receiver_pattern_returns_existing_or_default(key K,
	value V) V {
	if key in e.value {
		return e.value[key]
	}
	return value
}

fn test_existing_generic_receiver_declarations_still_work() {
	assert ReceiverPatternExpect[int]{
		value: 7
	}.broad_receiver_returns_value() == 7
	assert ReceiverPatternExpect[[]int]{
		value: [1, 2, 3]
	}.concrete_array_receiver_returns_len() == 3
}

fn test_concrete_receiver_method_takes_precedence_over_structured_pattern() {
	assert ReceiverPatternExpect[[]int]{
		value: [1, 2, 3]
	}.same_name_prefers_concrete_receiver() == 'concrete'
	assert ReceiverPatternExpect[[]string]{
		value: ['alpha']
	}.same_name_prefers_concrete_receiver() == 'pattern'
}

fn test_structured_generic_receiver_patterns_can_be_declared() {
	assert true
}

fn test_dynamic_array_receiver_pattern_binds_element_type() {
	assert ReceiverPatternExpect[[]int]{
		value: [1, 2, 3]
	}.dynamic_array_receiver_pattern_returns_first_or_value(9) == 1
	assert ReceiverPatternExpect[[]string]{
		value: ['alpha', 'beta']
	}.dynamic_array_receiver_pattern_returns_first_or_value('fallback') == 'alpha'
}

fn test_dynamic_array_receiver_pattern_binds_nested_element_type() {
	assert ReceiverPatternExpect[[][]int]{
		value: [[1, 2], [3, 4]]
	}.dynamic_array_receiver_pattern_returns_first_or_value([9, 10]) == [1, 2]
	assert ReceiverPatternExpect[[]map[string]int]{
		value: [
			{
				'one': 1
			},
		]
	}.dynamic_array_receiver_pattern_returns_first_or_value({
		'zero': 0
	})['one'] == 1
}

fn test_map_receiver_pattern_binds_key_and_value_types() {
	assert ReceiverPatternExpect[map[string]int]{
		value: {
			'one': 1
		}
	}.map_receiver_pattern_returns_existing_or_default('one', 0) == 1
	assert ReceiverPatternExpect[map[int]string]{
		value: {
			2: 'two'
		}
	}.map_receiver_pattern_returns_existing_or_default(3, 'fallback') == 'fallback'
}

fn test_map_receiver_pattern_binds_nested_value_type() {
	assert ReceiverPatternExpect[map[string]map[string]int]{
		value: {
			'outer': {
				'inner': 7
			}
		}
	}.map_receiver_pattern_returns_existing_or_default('outer', {
		'default': 0
	})['inner'] == 7
}
