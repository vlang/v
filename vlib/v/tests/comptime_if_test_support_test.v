fn test_comptime_if_test() {
	mut i := 0
	$if test {
		i++
	}
	$if !test {
		i--
	}
	assert i == 1
}
