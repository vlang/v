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

fn match_result_defer_log(mut events []string, label string) !int {
	events << label
	return 1
}

fn optional_binding_match_transform_with_defer(input ?int, flag bool, mut events []string) !int {
	return match flag {
		true {
			if value := input {
				defer {
					_ := match_result_defer_log(mut events, 'defer') or { 0 }
				}
				match_result_defer_log(mut events, 'value:${value}')!
			} else {
				0
			}
		}
		false {
			0
		}
	}
}

fn test_defer_result_propagation_stays_after_match_branch_value() {
	mut events := []string{}
	assert optional_binding_match_transform_with_defer(7, true, mut events)! == 1
	assert events == ['value:7', 'defer']
}
