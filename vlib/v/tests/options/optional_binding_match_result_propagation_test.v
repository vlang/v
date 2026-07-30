fn optional_binding_match_convert(value int) !int {
	return value
}

fn optional_binding_match_transform(input ?int, flag bool) !int {
	return match flag {
		true {
			if value := input {
				normalized := optional_binding_match_convert(value)!
				normalized
			} else {
				0
			}
		}
		false {
			0
		}
	}
}

fn test_optional_binding_inside_match_with_result_propagation() {
	assert optional_binding_match_transform(66, true)! == 66
	assert optional_binding_match_transform(none, true)! == 0
	assert optional_binding_match_transform(66, false)! == 0
}
