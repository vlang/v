struct FlowNarrowingVariant {
	value int
}

struct FlowNarrowingOther {}

type FlowNarrowingSum = FlowNarrowingOther | FlowNarrowingVariant

fn (value FlowNarrowingSum) is_positive() bool {
	return match value {
		FlowNarrowingVariant { value.value > 0 }
		else { false }
	}
}

fn flow_narrowing_and(value FlowNarrowingSum) bool {
	return value is FlowNarrowingVariant && value.value == 42 && value.is_positive()
}

fn flow_narrowing_after_exit(value FlowNarrowingSum) int {
	if value !is FlowNarrowingVariant || value.value < 0 {
		return 0
	}
	return value.value
}

fn flow_narrowing_bool_alias(lhs FlowNarrowingSum, rhs FlowNarrowingSum) bool {
	both_variants := lhs is FlowNarrowingVariant && rhs is FlowNarrowingVariant
	return both_variants && lhs.value == rhs.value
}

fn flow_narrowing_if_value(value FlowNarrowingSum) FlowNarrowingSum {
	is_variant := value is FlowNarrowingVariant
	return if is_variant { value } else { FlowNarrowingSum(FlowNarrowingOther{}) }
}

fn test_sumtype_flow_narrowing() {
	value := FlowNarrowingSum(FlowNarrowingVariant{
		value: 42
	})
	assert flow_narrowing_and(value)
	assert flow_narrowing_after_exit(value) == 42
	assert flow_narrowing_bool_alias(value, value)
	assert flow_narrowing_if_value(value) is FlowNarrowingVariant
}
