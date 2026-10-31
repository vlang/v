struct User {
	name string
}

// names_and_shouted has a local function and a lambda that both name their
// parameter `x`: in each instance, each one keeps its own `x`.
fn names_and_shouted[T](xs []T) ([]int, []string) {
	count := fn (x T) int {
		return x.name.len
	}
	lengths := xs.map(count)
	return lengths, xs.map(|x| x.name.to_upper())
}

fn shouted[T](xs []T) []string {
	count := fn (x T) int {
		return x.name.len
	}
	assert xs.map(count).len == xs.len
	return xs.map(|x| x.name.to_upper())
}

fn test_a_lambda_keeps_its_parameter_after_a_local_function_with_the_same_one() {
	users := [User{'ana'}, User{'bob'}]
	assert shouted(users) == ['ANA', 'BOB']
	lengths, names := names_and_shouted(users)
	assert lengths == [3, 3]
	assert names == ['ANA', 'BOB']
}
