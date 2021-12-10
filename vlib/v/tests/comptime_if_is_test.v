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

fn g<T>(t T) int {
	$if T is byte || T is i8 {
		return 1
	}
	return 2
}

fn test_is_or() {
	assert g(byte(1)) == 1
	assert g(i8(1)) == 1
	assert g(1) == 2
}
