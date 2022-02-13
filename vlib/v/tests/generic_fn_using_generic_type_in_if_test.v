fn test_generic_fn_using_generic_type_in_if() {
	ret := generic_bool(true)
	assert ret == 'true'
}

fn generic_bool<T>(val T) string {
	$if T is bool {
		if val {
			println('is true')
			return 'true'
		} else {
			println('is false')
			return 'false'
		}
	}
}
