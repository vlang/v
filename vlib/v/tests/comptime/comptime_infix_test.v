pub fn get_first[T](arr []T) T {
	mut first := arr[0]
	for i, v in arr {
		if i == 0 {
			first = v
		}
		if first == v {
			break
		}
	}
	return first
}

fn test_main() {
	assert get_first(['foo', 'bar']) == 'foo'
	assert get_first([1, 2]) == 1
	assert get_first([1.2, 2.0]) == 1.2
}
