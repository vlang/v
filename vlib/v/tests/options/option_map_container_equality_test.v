fn test_array_of_optional_map_equality() {
	none_map := ?map[string]int(none)
	some_map := ?map[string]int({
		'answer': 42
	})

	assert [none_map] == [none_map]
	assert [none_map] != [some_map]
	assert [some_map] == [some_map]
}

fn test_fixed_array_of_optional_map_equality() {
	none_map := ?map[string]int(none)
	some_map := ?map[string]int({
		'answer': 42
	})

	assert [none_map]! == [none_map]!
	assert [none_map]! != [some_map]!
	assert [some_map]! == [some_map]!
}
