fn test_fixed_array_string_keys() {
	mut m := map[[2]string]int{}
	key := ['foo', 'bar']!
	lookup := ['foo', 'bar']!
	m[key] = 5
	assert m[key] == 5
	assert m[lookup] == 5
	assert lookup in m
	assert m.len == 1
}

fn test_nested_fixed_array_string_keys() {
	mut m := map[[2][2]string]int{}
	key := [
		['alpha', 'beta']!,
		['gamma', 'delta']!,
	]!
	lookup := [
		['alpha', 'beta']!,
		['gamma', 'delta']!,
	]!
	m[key] = 9
	assert m[lookup] == 9
	assert key in m
}
