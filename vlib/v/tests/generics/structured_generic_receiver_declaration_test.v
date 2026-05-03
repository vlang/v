struct ReceiverPatternExpect[T] {
	value T
}

struct MixedReceiverPatternBox[T, U] {
	direct T
	items  U
}

type IntListExpect = ReceiverPatternExpect[[]int]
type StringIntMapExpect = ReceiverPatternExpect[map[string]int]
type MixedIntStringListBox = MixedReceiverPatternBox[int, []string]

fn (e ReceiverPatternExpect[T]) broad_receiver_returns_value() T {
	return e.value
}

fn (e ReceiverPatternExpect[[]int]) concrete_array_receiver_returns_len() int {
	return e.value.len
}

fn (e ReceiverPatternExpect[[]int]) same_name_prefers_concrete_receiver() string {
	return 'concrete'
}

fn (e ReceiverPatternExpect[[]int]) same_name_with_method_generic_prefers_concrete_receiver[U](extra U) string {
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

fn (e ReceiverPatternExpect[[]T]) same_name_with_method_generic_prefers_concrete_receiver[U](extra U) string {
	return 'pattern'
}

fn (e ReceiverPatternExpect[[]T]) dynamic_array_receiver_pattern_pairs_with_method_generic[U](value T,
	extra U) (T, U) {
	if e.value.len == 0 {
		return value, extra
	}
	return e.value[0], extra
}

fn (e ReceiverPatternExpect[map[K]V]) map_receiver_pattern_returns_existing_or_default(key K,
	value V) V {
	if key in e.value {
		return e.value[key]
	}
	return value
}

fn (e ReceiverPatternExpect[map[K]V]) map_receiver_pattern_pairs_with_method_generic[U](key K,
	value V, extra U) (V, U) {
	if key in e.value {
		return e.value[key], extra
	}
	return value, extra
}

fn (b MixedReceiverPatternBox[T, []U]) mixed_receiver_pattern_values_with_method_generic[V](fallback U,
	marker V) (T, U, V) {
	if b.items.len == 0 {
		return b.direct, fallback, marker
	}
	return b.direct, b.items[0], marker
}

fn (b MixedReceiverPatternBox[int, []string]) same_name_with_method_generic_prefers_concrete_mixed_receiver[V](marker V) string {
	return 'concrete'
}

fn (b MixedReceiverPatternBox[T, []U]) same_name_with_method_generic_prefers_concrete_mixed_receiver[V](marker V) string {
	return 'pattern'
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

fn test_concrete_receiver_method_with_method_generic_takes_precedence_over_structured_pattern() {
	assert ReceiverPatternExpect[[]int]{
		value: [1, 2, 3]
	}.same_name_with_method_generic_prefers_concrete_receiver('extra') == 'concrete'
	assert ReceiverPatternExpect[[]int]{
		value: [1]
	}.same_name_with_method_generic_prefers_concrete_receiver[string]('extra') == 'concrete'
	assert ReceiverPatternExpect[[]string]{
		value: ['alpha']
	}.same_name_with_method_generic_prefers_concrete_receiver('extra') == 'pattern'
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

fn test_dynamic_array_receiver_pattern_method_generic_is_inferred() {
	value, extra := ReceiverPatternExpect[[]int]{
		value: [1, 2, 3]
	}.dynamic_array_receiver_pattern_pairs_with_method_generic(9, 'extra')
	assert value == 1
	assert extra == 'extra'
	explicit_value, explicit_extra := ReceiverPatternExpect[[]int]{
		value: [1, 2, 3]
	}.dynamic_array_receiver_pattern_pairs_with_method_generic[string](9, 'explicit')
	assert explicit_value == 1
	assert explicit_extra == 'explicit'
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

fn test_map_receiver_pattern_method_generic_can_be_explicit() {
	value, extra := ReceiverPatternExpect[map[string]int]{
		value: {
			'one': 1
		}
	}.map_receiver_pattern_pairs_with_method_generic[string]('missing', 7, 'extra')
	assert value == 7
	assert extra == 'extra'
}

fn test_alias_to_array_receiver_pattern_binds_element_type() {
	assert IntListExpect{
		value: [1, 2, 3]
	}.dynamic_array_receiver_pattern_returns_first_or_value(9) == 1
	value, extra := IntListExpect{
		value: [1, 2, 3]
	}.dynamic_array_receiver_pattern_pairs_with_method_generic[string](9, 'alias')
	assert value == 1
	assert extra == 'alias'
	inferred_value, inferred_extra := IntListExpect{
		value: [1, 2, 3]
	}.dynamic_array_receiver_pattern_pairs_with_method_generic(9, 'inferred')
	assert inferred_value == 1
	assert inferred_extra == 'inferred'
}

fn test_alias_to_map_receiver_pattern_binds_key_and_value_types() {
	assert StringIntMapExpect{
		value: {
			'one': 1
		}
	}.map_receiver_pattern_returns_existing_or_default('one', 0) == 1
	value, extra := StringIntMapExpect{
		value: {
			'one': 1
		}
	}.map_receiver_pattern_pairs_with_method_generic[bool]('missing', 7, true)
	assert value == 7
	assert extra
}

fn test_alias_receiver_exact_method_takes_precedence_over_structured_pattern() {
	assert IntListExpect{
		value: [1, 2, 3]
	}.same_name_prefers_concrete_receiver() == 'concrete'
	assert IntListExpect{
		value: [1]
	}.same_name_with_method_generic_prefers_concrete_receiver[string]('extra') == 'concrete'
}

fn test_mixed_direct_and_structured_receiver_pattern_method_generic() {
	direct, item, marker := MixedReceiverPatternBox[int, []string]{
		direct: 7
		items:  ['alpha']
	}.mixed_receiver_pattern_values_with_method_generic[bool]('fallback', true)
	assert direct == 7
	assert item == 'alpha'
	assert marker
	inferred_direct, inferred_item, inferred_marker := MixedReceiverPatternBox[int, []string]{
		direct: 9
		items:  []string{}
	}.mixed_receiver_pattern_values_with_method_generic('fallback', 1.5)
	assert inferred_direct == 9
	assert inferred_item == 'fallback'
	assert inferred_marker == 1.5
}

fn test_alias_to_mixed_receiver_pattern_binds_direct_and_structured_slots() {
	direct, item, marker := MixedIntStringListBox{
		direct: 7
		items:  ['alpha']
	}.mixed_receiver_pattern_values_with_method_generic[bool]('fallback', true)
	assert direct == 7
	assert item == 'alpha'
	assert marker
	assert MixedIntStringListBox{
		direct: 1
		items:  ['alpha']
	}.same_name_with_method_generic_prefers_concrete_mixed_receiver[bool](true) == 'concrete'
}

fn test_exact_mixed_receiver_method_with_method_generic_takes_precedence_over_structured_pattern() {
	assert MixedReceiverPatternBox[int, []string]{
		direct: 1
		items:  ['alpha']
	}.same_name_with_method_generic_prefers_concrete_mixed_receiver[bool](true) == 'concrete'
	assert MixedReceiverPatternBox[int, []bool]{
		direct: 1
		items:  [true]
	}.same_name_with_method_generic_prefers_concrete_mixed_receiver[string]('marker') == 'pattern'
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
