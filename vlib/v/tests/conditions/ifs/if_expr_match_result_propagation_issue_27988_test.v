struct First {}

struct Second {}

type Node = First | Second

fn lower_first(_ First) !int {
	return 1
}

fn lower_second(_ Second) !int {
	return 2
}

fn select_value(node ?Node) !int {
	result := if value := node {
		match value {
			First { lower_first(value)! }
			Second { lower_second(value)! }
		}
	} else {
		0
	}
	return result
}

fn test_result_propagation_in_match_nested_in_if_expression() {
	assert select_value(First{})! == 1
	assert select_value(Second{})! == 2
	assert select_value(none)! == 0
}
