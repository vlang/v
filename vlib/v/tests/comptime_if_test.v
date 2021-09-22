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

fn test_comptime_if_parsing_in_combination_with_ordinary_if_1() {
	if true {
		$if debug {
			println('debug')
		}
	} else {
		assert false
	}
	assert true
}

fn test_comptime_if_parsing_in_combination_with_ordinary_if_2() {
	if true {
		if true {
			$if debug {
				println('debug')
			}
		} else {
			assert false
		}
	} else {
		assert false
	}
	assert true
}

fn test_comptime_if_parsing_in_combination_with_ordinary_if_3() {
	println(@LINE)
	$if true {
		println(@LINE)
		$if true {
			println(@LINE)
			$if debug {
				println('debug')
			}
		} $else {
			assert false
		}
	} $else {
		assert false
	}
	assert true
}
