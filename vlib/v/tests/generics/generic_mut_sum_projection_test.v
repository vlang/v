struct MutablePayload {
	value int
}

type MutableVariant = []int | map[string]int | ?MutablePayload

fn update_variant[T](mut value T) string {
	$if T is []int {
		value << 42
	} $else $if T is map[string]int {
		value['answer'] = 42
	} $else $if T is $option {
		value = MutablePayload{ value: 42 }
	} $else {
		panic('expected the selected variant')
	}
	return typeof(T).name
}

fn update_selected[T](mut value T) string {
	$for variant in T.variants {
		if mut value is variant && true {
			return update_variant(mut value)
		}
	}
	panic('missing variant')
}

fn test_generic_mut_sum_projection_updates_original_storage() {
	mut array_value := MutableVariant([]int{})
	mut map_value := MutableVariant(map[string]int{})
	mut option_value := MutableVariant(?MutablePayload(none))
	assert update_selected(mut array_value) == '[]int'
	assert update_selected(mut map_value) == 'map[string]int'
	assert update_selected(mut option_value) == '?MutablePayload'
	assert array_value as []int == [42]
	assert (map_value as map[string]int)['answer'] == 42
	payload := (option_value as ?MutablePayload) or { panic('missing payload') }
	assert payload.value == 42
}
