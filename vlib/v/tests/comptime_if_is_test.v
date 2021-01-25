fn f<T>() int {
	$if T is int {
		return 1
	}
	$if T !is int {
		return -1
	}
	assert false
	return 0
}

fn test_generic_is() {
	assert f<int>() == 1
	assert f<bool>() == -1
}
