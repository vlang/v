fn test_unwrap_generic_params() {
	assert encode(true) == []
	assert encode([true]) == ['[]bool']
	assert encode(1) == []
	assert encode([1]) == ['[]int']
	assert encode('1') == []
	assert encode(['1']) == ['[]string']
}

fn encode[U](val U) []string {
	mut c := []string{}
	$if U is $array {
		c << g_array(val)
	}
	return c
}

fn g_array[T](t []T) string {
	return typeof(t).name
}
