type RecursiveFunctionSum = fn([]RecursiveFunctionSum) RecursiveFunctionSum | int

fn recursive_function_sum_count(values []RecursiveFunctionSum) RecursiveFunctionSum {
	return values.len
}

fn test_recursive_function_variant_stores_projects_and_calls() {
	value := RecursiveFunctionSum(recursive_function_sum_count)
	copy := value
	callback := copy as fn ([]RecursiveFunctionSum) RecursiveFunctionSum
	result := callback([RecursiveFunctionSum(3), RecursiveFunctionSum(4)])
	assert result is int
	assert (result as int) == 2

	if value is fn ([]RecursiveFunctionSum) RecursiveFunctionSum {
		empty := value([]RecursiveFunctionSum{})
		assert empty is int
		assert (empty as int) == 0
	} else {
		assert false
	}
}
