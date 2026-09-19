fn test_missing_nested_map_lookup() {
	outer := map[string]map[string]int{}
	assert outer['missing']['child'] == 0
}

struct NestedMapEntry {
mut:
	value int
}

fn test_addressable_missing_nested_map_lookup() {
	mut outer := map[string]map[string]NestedMapEntry{}
	outer['missing']['child'].value = 42
	assert outer.len == 0
}

fn test_shared_map_value_lookup() {
	shared outer := {
		'present': {
			'answer': 42
		}
	}
	rlock outer {
		assert outer['present'].len == 1
		assert outer['present']['answer'] == 42
		assert outer['missing'].len == 0
	}
}

fn test_shared_map_pointer_value_lookup() {
	shared outer := &{
		'present': {
			'answer': 42
		}
	}
	rlock outer {
		assert outer['present'].len == 1
		assert outer['present']['answer'] == 42
	}
}
