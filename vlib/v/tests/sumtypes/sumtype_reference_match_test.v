struct MatchEntry[T] {
	value T
}

type MatchValue = MatchEntry[int] | &MatchEntry[int] | &&MatchEntry[int]

fn reference_match_value(value MatchValue) int {
	return match value {
		MatchEntry[int] { value.value }
		&MatchEntry[int] { value.value + 10 }
		&&MatchEntry[int] { (**value).value + 20 }
	}
}

fn test_generic_reference_patterns_preserve_pointer_depth() {
	entry := MatchEntry[int]{1}
	pointer := &entry
	values := [MatchValue(entry), MatchValue(pointer), MatchValue(&pointer)]
	expected := [1, 11, 21]
	for index, value in values {
		assert reference_match_value(value) == expected[index]
	}
}
