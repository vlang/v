type Value = map[string]int | []int

fn sorted_match_keys(value Value) []string {
	match value {
		map[string]int {
			mut keys := value.keys()
			keys.sort()
			return keys
		}
		[]int { return []string{} }
	}
}

fn sorted_if_keys(value Value) []string {
	if value is map[string]int {
		mut keys := value.keys()
		keys.sort()
		return keys
	}
	return []string{}
}

fn test_map_keys_sort_retains_match_smartcast_type() {
	value := Value({
		'b': 2
		'a': 1
	})
	assert sorted_match_keys(value) == ['a', 'b']
	assert sorted_if_keys(value) == ['a', 'b']
}

fn test_map_keys_sort_without_return_type_context() {
	value := Value({
		'b': 2
		'a': 1
	})
	match value {
		map[string]int {
			mut keys := value.keys()
			keys.sort()
			assert keys == ['a', 'b']
		}
		[]int {
			assert false
		}
	}
}
